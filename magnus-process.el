;;; magnus-process.el --- Process management for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module handles spawning and managing Claude Code processes
;; in vterm buffers.

;;; Code:

(require 'vterm)
(require 'magnus-instances)
(require 'magnus-coord)

(declare-function magnus-status-refresh "magnus-status")

;; Variables defined in magnus.el
(defvar magnus-claude-executable)
(defvar magnus-default-directory)
(defvar magnus-instance-name-generator)
(defvar magnus-buffer-name)

;;; Process creation

(defun magnus-process-create (&optional directory name)
  "Create a new Claude Code instance.
DIRECTORY is the working directory.  If nil, prompts for one.
NAME is the instance name.  If nil, auto-generates one."
  (interactive)
  (let* ((dir (or directory
                  (magnus-process--get-directory)))
         (instance-name (or name
                            (funcall magnus-instance-name-generator dir)))
         (instance (magnus-instances-create dir instance-name)))
    (magnus-instances-add instance)
    ;; Set up coordination files and register agent
    (magnus-coord-register-agent dir instance)
    (magnus-process--spawn instance)
    instance))

(defun magnus-process--get-directory ()
  "Get directory for new instance, prompting user."
  (let ((default (or magnus-default-directory
                     (magnus-process--project-root)
                     default-directory)))
    (read-directory-name "Directory: " default nil t)))

(defun magnus-process--project-root ()
  "Get the current project root if available.
Avoids triggering interactive prompts from Projectile."
  (or
   ;; Try projectile directly if available (non-interactive)
   (when (and (fboundp 'projectile-project-root)
              (bound-and-true-p projectile-mode))
     (ignore-errors (projectile-project-root)))
   ;; Fall back to project.el with `maybe' to avoid prompting
   (when (fboundp 'project-current)
     (when-let ((project (project-current 'maybe)))
       (if (fboundp 'project-root)
           (project-root project)
         (car (with-no-warnings (project-roots project))))))))

(defun magnus-process--spawn (instance)
  "Spawn a Claude Code process for INSTANCE."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (buffer-name (format "*claude:%s*" name))
         (default-directory directory)
         (sessions-before (magnus-process--list-sessions directory)))
    ;; Create vterm buffer
    (let ((buffer (magnus-process--create-vterm-buffer buffer-name)))
      (magnus-instances-update instance
                               :buffer buffer
                               :status 'running)
      ;; Send the claude command
      (with-current-buffer buffer
        (vterm-send-string magnus-claude-executable)
        (vterm-send-return))
      ;; Set up process sentinel
      (magnus-process--setup-sentinel instance buffer)
      ;; Send onboarding message after Claude starts
      (run-with-timer 3 nil #'magnus-process--send-onboarding instance)
      ;; Watch for new session to appear
      (magnus-process--watch-for-session instance directory sessions-before)
      buffer)))

(defun magnus-process--send-onboarding (instance)
  "Send onboarding message to INSTANCE."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (let ((msg (magnus-process--onboarding-message instance)))
        (with-current-buffer buffer
          (vterm-send-string msg)
          (vterm-send-return))))))

(defun magnus-process--onboarding-message (instance)
  "Generate onboarding message for INSTANCE."
  (let ((name (magnus-instance-name instance)))
    (format "You are agent '%s', managed by Magnus (https://github.com/hrishikeshs/magnus). \
Read .claude/magnus-instructions.md for the coordination protocol. \
Check .magnus-coord.md before starting work - other agents may be active. \
Announce your work area and files you'll touch."
            name)))

(defun magnus-process--list-sessions (directory)
  "List all session IDs for DIRECTORY.
Extracts IDs from .jsonl filenames in the project directory."
  (let* ((project-hash (magnus-process--project-hash directory))
         (sessions-dir (expand-file-name
                        (concat "projects/" project-hash)
                        (expand-file-name ".claude" (getenv "HOME")))))
    (when (file-directory-p sessions-dir)
      (mapcar (lambda (f) (file-name-sans-extension f))
              (directory-files sessions-dir nil "\\.jsonl$")))))

(defun magnus-process--watch-for-session (instance directory sessions-before)
  "Watch for a new session to appear for INSTANCE in DIRECTORY.
SESSIONS-BEFORE is the list of sessions that existed before spawning.
Uses both filenotify and polling fallback for robustness."
  (let* ((project-hash (magnus-process--project-hash directory))
         (sessions-dir (expand-file-name
                        (concat "projects/" project-hash)
                        (expand-file-name ".claude" (getenv "HOME")))))
    ;; Ensure directory exists before watching
    (unless (file-directory-p sessions-dir)
      (make-directory sessions-dir t))
    (let* ((descriptor nil)
           (poll-timer nil)
           (cleanup-timer nil)
           (detect-fn
            (lambda ()
              (magnus-process--detect-new-session
               instance directory sessions-before
               (list descriptor poll-timer cleanup-timer)))))
      ;; Primary: file-notify watcher
      (setq descriptor
            (file-notify-add-watch
             sessions-dir '(change)
             (lambda (_event) (funcall detect-fn))))
      ;; Fallback: poll every 5 seconds
      (setq poll-timer
            (run-with-timer 5 5 detect-fn))
      ;; Final cleanup after 120 seconds
      (setq cleanup-timer
            (run-with-timer 120 nil
                            (lambda ()
                              (when descriptor
                                (ignore-errors (file-notify-rm-watch descriptor)))
                              (when poll-timer
                                (cancel-timer poll-timer))))))))

(defun magnus-process--detect-new-session (instance directory sessions-before resources)
  "Try to detect a new session for INSTANCE in DIRECTORY.
SESSIONS-BEFORE is the pre-spawn session list.
RESOURCES is a list of (descriptor poll-timer cleanup-timer) to clean up on success."
  (let* ((sessions-after (magnus-process--list-sessions directory))
         (new-sessions (cl-set-difference sessions-after sessions-before :test #'string=)))
    (when new-sessions
      (let ((session-id (if (= 1 (length new-sessions))
                            (car new-sessions)
                          (magnus-process--most-recent-session directory new-sessions))))
        (magnus-instances-update instance :session-id session-id)
        (message "Captured session %s for %s"
                 session-id (magnus-instance-name instance)))
      ;; Clean up all watchers/timers
      (let ((descriptor (nth 0 resources))
            (poll-timer (nth 1 resources))
            (cleanup-timer (nth 2 resources)))
        (when descriptor
          (ignore-errors (file-notify-rm-watch descriptor)))
        (when poll-timer
          (cancel-timer poll-timer))
        (when cleanup-timer
          (cancel-timer cleanup-timer))))))

(defun magnus-process--most-recent-session (directory sessions)
  "Find the most recently modified session in DIRECTORY from SESSIONS list."
  (let* ((project-hash (magnus-process--project-hash directory))
         (sessions-dir (expand-file-name
                        (concat "projects/" project-hash)
                        (expand-file-name ".claude" (getenv "HOME")))))
    ;; Filter to sessions whose .jsonl files actually exist
    (let ((valid (cl-remove-if-not
                  (lambda (s)
                    (file-exists-p
                     (expand-file-name (concat s ".jsonl") sessions-dir)))
                  sessions)))
      (car (sort valid
                 (lambda (a b)
                   (time-less-p
                    (file-attribute-modification-time
                     (file-attributes
                      (expand-file-name (concat b ".jsonl") sessions-dir)))
                    (file-attribute-modification-time
                     (file-attributes
                      (expand-file-name (concat a ".jsonl") sessions-dir))))))))))

(defun magnus-process--create-vterm-buffer (buffer-name)
  "Create a vterm buffer with BUFFER-NAME."
  (let ((buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
      (vterm-mode)
      (magnus-process--setup-keys))
    buffer))

(defun magnus-process--setup-keys ()
  "Set up keybindings for Claude Code in the current vterm buffer.
Maps C-g to send ESC to Claude, since Emacs intercepts the real ESC key."
  (local-set-key (kbd "C-g")
                 (lambda () (interactive) (vterm-send-key "<escape>"))))

;;; Trace buffers - JSONL session viewer

(defface magnus-trace-user
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for user messages in trace buffer."
  :group 'magnus)

(defface magnus-trace-thinking
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for thinking blocks in trace buffer."
  :group 'magnus)

(defface magnus-trace-assistant
  '((t :inherit default))
  "Face for assistant responses in trace buffer."
  :group 'magnus)

(defface magnus-trace-separator
  '((t :inherit font-lock-comment-face))
  "Face for separators in trace buffer."
  :group 'magnus)

(defvar-local magnus-trace--instance nil
  "The instance this trace buffer is following.")

(defvar-local magnus-trace--last-line-count 0
  "Number of JSONL lines already processed.")

(defvar magnus-trace--timer nil
  "Timer for auto-refreshing trace buffers.")

(define-derived-mode magnus-trace-mode special-mode "Trace"
  "Major mode for viewing Claude Code thinking trace.
\\{magnus-trace-mode-map}"
  :group 'magnus
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(let ((map magnus-trace-mode-map))
  (define-key map (kbd "g") #'magnus-trace-refresh)
  (define-key map (kbd "G") #'magnus-trace-tail)
  (define-key map (kbd "q") #'quit-window))

(defun magnus-process-trace (instance)
  "Open the trace buffer for INSTANCE showing thinking and messages."
  (let* ((name (magnus-instance-name instance))
         (trace-name (format "*trace:%s*" name))
         (trace-buf (get-buffer-create trace-name)))
    (with-current-buffer trace-buf
      (unless (eq major-mode 'magnus-trace-mode)
        (magnus-trace-mode))
      (setq magnus-trace--instance instance)
      (setq magnus-trace--last-line-count 0)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (magnus-trace-refresh))
    (magnus-process--ensure-trace-timer)
    (display-buffer trace-buf '(display-buffer-in-side-window
                                (side . bottom)
                                (window-height . 0.35)))
    trace-buf))

(defun magnus-trace-refresh ()
  "Refresh the trace buffer with new JSONL content."
  (interactive)
  (when magnus-trace--instance
    (let* ((instance magnus-trace--instance)
           (session-id (magnus-instance-session-id instance))
           (directory (magnus-instance-directory instance)))
      ;; Try to detect session-id if missing
      (unless session-id
        (let ((sessions (magnus-process--list-sessions directory)))
          (when sessions
            (setq session-id (if (= 1 (length sessions))
                                 (car sessions)
                               (magnus-process--most-recent-session
                                directory sessions)))
            (when session-id
              (magnus-instances-update instance :session-id session-id)
              ;; Clear the waiting message now that we have a session
              (let ((inhibit-read-only t))
                (erase-buffer)
                (setq magnus-trace--last-line-count 0))))))
      (let ((jsonl-file (when session-id
                          (magnus-process--session-jsonl-path directory session-id))))
        (if (and jsonl-file (file-exists-p jsonl-file))
            (magnus-trace--append-new-entries jsonl-file)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (> (buffer-size) 0)
              (insert (propertize "Waiting for session to start...\n"
                                 'face 'magnus-trace-separator)))))))))

(defun magnus-trace-tail ()
  "Refresh and jump to the end of the trace buffer."
  (interactive)
  (magnus-trace-refresh)
  (goto-char (point-max))
  (recenter -3))

(defun magnus-process--session-jsonl-path (directory session-id)
  "Get the JSONL file path for SESSION-ID in DIRECTORY."
  (let* ((project-hash (magnus-process--project-hash directory))
         (jsonl-file (expand-file-name
                      (concat "projects/" project-hash "/" session-id ".jsonl")
                      (expand-file-name ".claude" (getenv "HOME")))))
    (when (file-exists-p jsonl-file)
      jsonl-file)))

(defun magnus-trace--append-new-entries (jsonl-file)
  "Append new entries from JSONL-FILE to the current trace buffer."
  (let* ((all-lines (magnus-trace--read-lines jsonl-file))
         (new-lines (nthcdr magnus-trace--last-line-count all-lines))
         (at-end (>= (point) (point-max))))
    (when new-lines
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (dolist (line new-lines)
            (condition-case nil
                (let ((entry (json-parse-string line :object-type 'alist)))
                  (magnus-trace--render-entry entry))
              (error nil)))))
      (setq magnus-trace--last-line-count (length all-lines))
      ;; Follow tail if user was at end
      (when at-end
        (goto-char (point-max))
        (let ((win (get-buffer-window (current-buffer))))
          (when win
            (set-window-point win (point-max))))))))

(defun magnus-trace--read-lines (file)
  "Read all lines from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun magnus-trace--render-entry (entry)
  "Render a JSONL ENTRY into the trace buffer."
  (let ((type (alist-get 'type entry))
        (message (alist-get 'message entry))
        (timestamp (alist-get 'timestamp entry)))
    (cond
     ((string= type "user")
      (let ((content (alist-get 'content message)))
        (when (and content (stringp content) (not (string-empty-p content)))
          (insert (propertize (format "── User [%s] ──\n"
                                     (magnus-trace--format-time timestamp))
                             'face 'magnus-trace-separator))
          (insert (propertize (concat content "\n\n")
                             'face 'magnus-trace-user)))))
     ((string= type "assistant")
      (let ((content (alist-get 'content message)))
        (when (vectorp content)
          (let ((has-output nil))
            (seq-doseq (block content)
              (let ((block-type (alist-get 'type block)))
                (cond
                 ((string= block-type "thinking")
                  (let ((thinking (alist-get 'thinking block)))
                    (when (and thinking (not (string-empty-p thinking)))
                      (unless has-output
                        (insert (propertize (format "── Thinking [%s] ──\n"
                                                   (magnus-trace--format-time timestamp))
                                           'face 'magnus-trace-separator))
                        (setq has-output t))
                      (insert (propertize (concat thinking "\n\n")
                                         'face 'magnus-trace-thinking)))))
                 ((string= block-type "text")
                  (let ((text (alist-get 'text block)))
                    (when (and text (not (string-empty-p text)))
                      (unless has-output
                        (insert (propertize (format "── Assistant [%s] ──\n"
                                                   (magnus-trace--format-time timestamp))
                                           'face 'magnus-trace-separator))
                        (setq has-output t))
                      (insert (propertize (concat text "\n\n")
                                         'face 'magnus-trace-assistant)))))))))))))))

(defun magnus-trace--format-time (timestamp)
  "Format ISO TIMESTAMP to HH:MM:SS."
  (if (and timestamp (stringp timestamp))
      (if (string-match "T\\([0-9]+:[0-9]+:[0-9]+\\)" timestamp)
          (match-string 1 timestamp)
        "")
    ""))

(defun magnus-process--ensure-trace-timer ()
  "Ensure the trace auto-refresh timer is running."
  (unless magnus-trace--timer
    (setq magnus-trace--timer
          (run-with-timer 10 10 #'magnus-process--sync-all-traces))))

(defun magnus-process--sync-all-traces ()
  "Auto-refresh all open trace buffers.
Cancels the timer when no trace buffers remain."
  (let ((any-live nil))
    (dolist (instance (magnus-instances-list))
      (let ((trace-buf (get-buffer (format "*trace:%s*" (magnus-instance-name instance)))))
        (when (and trace-buf (buffer-live-p trace-buf))
          (setq any-live t)
          (with-current-buffer trace-buf
            (condition-case nil
                (magnus-trace-refresh)
              (error nil))))))
    (unless any-live
      (when magnus-trace--timer
        (cancel-timer magnus-trace--timer)
        (setq magnus-trace--timer nil)))))

(defun magnus-process--setup-sentinel (instance buffer)
  "Set up process monitoring for INSTANCE in BUFFER."
  (when-let ((process (get-buffer-process buffer)))
    (set-process-sentinel
     process
     (lambda (proc _event)
       (unless (process-live-p proc)
         (magnus-instances-update instance :status 'stopped)
         (when (get-buffer magnus-buffer-name)
           (magnus-status-refresh)))))))

;;; Process control

(defun magnus-process-kill (instance &optional force)
  "Kill the Claude Code process for INSTANCE.
If FORCE is non-nil, forcefully terminate."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (let ((process (get-buffer-process buffer)))
        (when (and process (process-live-p process))
          (if force
              (kill-process process)
            ;; Send C-c to gracefully exit
            (with-current-buffer buffer
              (vterm-send-key "C-c")))))
      ;; Give process time to exit, then kill buffer
      (run-with-timer
       1 nil
       (lambda ()
         (when (buffer-live-p buffer)
           (kill-buffer buffer))))))
  (magnus-instances-update instance :status 'stopped :buffer nil))

(defun magnus-process-kill-and-remove (instance &optional force)
  "Kill INSTANCE and remove it from the registry.
If FORCE is non-nil, forcefully terminate."
  (let ((directory (magnus-instance-directory instance)))
    (magnus-coord-unregister-agent directory instance))
  (magnus-process-kill instance force)
  (magnus-instances-remove instance))

(defvar magnus-process--restarting nil
  "Set of instance IDs currently mid-restart, to prevent double-spawn.")

(defun magnus-process-restart (instance)
  "Restart the Claude Code process for INSTANCE."
  (let ((id (magnus-instance-id instance)))
    (when (member id magnus-process--restarting)
      (user-error "Instance '%s' is already restarting"
                  (magnus-instance-name instance)))
    (push id magnus-process--restarting)
    (magnus-process-kill instance)
    (run-with-timer
     1.5 nil
     (lambda ()
       (setq magnus-process--restarting
             (delete id magnus-process--restarting))
       (magnus-process--spawn instance)))))

(defun magnus-process-suspend (instance)
  "Suspend the Claude Code process for INSTANCE.
Sends SIGTSTP to pause the process.  Use `magnus-process-resume' to continue."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (when-let ((process (get-buffer-process buffer)))
        (when (process-live-p process)
          (signal-process process 'SIGTSTP)
          (magnus-instances-update instance :status 'suspended)
          (when (get-buffer magnus-buffer-name)
            (magnus-status-refresh))
          (message "Suspended %s" (magnus-instance-name instance)))))))

(defun magnus-process-resume (instance)
  "Resume a suspended Claude Code process for INSTANCE.
Sends SIGCONT to continue the process."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (when-let ((process (get-buffer-process buffer)))
        (signal-process process 'SIGCONT)
        (magnus-instances-update instance :status 'running)
        (when (get-buffer magnus-buffer-name)
          (magnus-status-refresh))
        (message "Resumed %s" (magnus-instance-name instance))))))

(defun magnus-process-suspended-p (instance)
  "Return non-nil if INSTANCE is suspended."
  (eq (magnus-instance-status instance) 'suspended))

(defun magnus-process-chdir (instance directory)
  "Change INSTANCE's working directory to DIRECTORY.
Uses the stored session ID, kills the instance, and respawns
in the new directory with --resume to preserve conversation history."
  (let* ((new-dir (expand-file-name directory))
         (name (magnus-instance-name instance))
         (session-id (magnus-instance-session-id instance)))
    ;; Update the directory in our records (keep session-id)
    (magnus-instances-update instance :directory new-dir)
    ;; Kill the old process and buffer
    (magnus-process-kill instance t)
    ;; Respawn after a delay with the session ID
    (run-with-timer
     2 nil
     (lambda ()
       (magnus-process--spawn-with-session instance session-id)))
    (message "Moving %s to %s%s..."
             name new-dir
             (if session-id " (resuming session)" " (no session to resume)"))))

(defun magnus-process--project-hash (directory)
  "Convert DIRECTORY to Claude's project hash format.
Replaces slashes, spaces, and tildes with hyphens."
  (let ((path (directory-file-name (expand-file-name directory))))
    (replace-regexp-in-string "[/ ~]+" "-" path)))

(defun magnus-process--spawn-with-session (instance &optional session-id)
  "Spawn a Claude Code process for INSTANCE, optionally resuming SESSION-ID."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (buffer-name (format "*claude:%s*" name))
         (default-directory directory))
    ;; Create vterm buffer
    (let ((buffer (magnus-process--create-vterm-buffer buffer-name)))
      (magnus-instances-update instance
                               :buffer buffer
                               :status 'running)
      ;; Send the claude command with optional --resume
      (with-current-buffer buffer
        (if session-id
            (vterm-send-string (format "%s --resume %s"
                                       magnus-claude-executable session-id))
          (vterm-send-string magnus-claude-executable))
        (vterm-send-return))
      ;; Set up process sentinel
      (magnus-process--setup-sentinel instance buffer)
      ;; Only send onboarding if no session (fresh start)
      (unless session-id
        (run-with-timer 3 nil #'magnus-process--send-onboarding instance))
      buffer)))

;;; Instance interaction

(defun magnus-process-switch-to (instance)
  "Switch to the buffer for INSTANCE."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (if (buffer-live-p buffer)
        (switch-to-buffer buffer)
      ;; Buffer is dead, respawn
      (magnus-process--spawn instance)
      (switch-to-buffer (magnus-instance-buffer instance)))))

(defun magnus-process-running-p (instance)
  "Return non-nil if INSTANCE has a running process."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (and (buffer-live-p buffer)
         (get-buffer-process buffer)
         (process-live-p (get-buffer-process buffer)))))

;;; Reconnection

(defun magnus-process-reconnect (instance)
  "Try to reconnect INSTANCE to an existing buffer/process."
  (let* ((name (magnus-instance-name instance))
         (buffer-name (format "*claude:%s*" name))
         (buffer (get-buffer buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (magnus-instances-update instance
                               :buffer buffer
                               :status (if (get-buffer-process buffer)
                                           'running
                                         'stopped)))))

(provide 'magnus-process)
;;; magnus-process.el ends here
