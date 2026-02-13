;;; magnus-process.el --- Process management for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module handles spawning and managing Claude Code processes
;; in vterm buffers.

;;; Code:

(require 'vterm)
(require 'magnus-instances)
(require 'magnus-coord)
(require 'magnus-trace)

(declare-function magnus-status-refresh "magnus-status")
(declare-function magnus-command--add-event "magnus-command")
(defvar magnus-command-buffer-name)

;; Variables defined in magnus.el
(defvar magnus-claude-executable)
(defvar magnus-default-directory)
(defvar magnus-instance-name-generator)
(defvar magnus-buffer-name)

;; Forward declarations for stream mode (defined later in this file)
(defvar magnus-process--stream-busy)
(defvar magnus-process--stream-queues)

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
    (format "Welcome! You are agent '%s', part of a multi-agent team managed by Magnus (an Emacs coordination tool). Other agents may be working in this project right now.

Before you start coding, please go through these steps — they keep everyone in sync:

1. Read .claude/magnus-instructions.md — it explains how agents coordinate here.
2. Read .magnus-coord.md — check the Active Work table to see what others are doing, and the Discoveries section for things they've learned.
3. In the Log section of .magnus-coord.md, announce what you plan to work on and which files you'll touch.
4. Check the Active Work table for conflicts — if another agent claims files you need, @mention them to coordinate.
5. Add your row to the Active Work table with your name, area, status 'in-progress', and files.
6. Then begin coding.

While working:
- Update .magnus-coord.md when you finish tasks or change scope.
- If you learn something non-obvious, add it to the Discoveries section — other agents will read it.
- If you need another agent's attention, @mention them in the Log — they get notified automatically.
- Check .magnus-coord.md periodically for messages and discoveries.

Please start with step 1 now."
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

;;; Trace buffer entry point

(declare-function magnus-trace-open "magnus-trace")

(defun magnus-process-trace (instance)
  "Open the trace buffer for INSTANCE showing thinking and messages."
  (require 'magnus-trace)
  (magnus-trace-open instance))

(defun magnus-process--session-jsonl-path (directory session-id)
  "Get the JSONL file path for SESSION-ID in DIRECTORY."
  (let* ((project-hash (magnus-process--project-hash directory))
         (jsonl-file (expand-file-name
                      (concat "projects/" project-hash "/" session-id ".jsonl")
                      (expand-file-name ".claude" (getenv "HOME")))))
    (when (file-exists-p jsonl-file)
      jsonl-file)))

(defun magnus-process-read-jsonl-lines (file)
  "Read all lines from JSONL FILE.
Shared by trace and command modules."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

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
            ;; Graceful exit: SIGINT for headless/stream, C-c for vterm
            (if (or (magnus-process--headless-p instance)
                    (magnus-process--stream-p instance))
                (interrupt-process process)
              (with-current-buffer buffer
                (vterm-send-key "C-c")))))))
      ;; Give process time to exit, then kill buffer
      (run-with-timer
       1 nil
       (lambda ()
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))
  ;; Clean up stream state if applicable
  (when (magnus-process--stream-p instance)
    (let ((id (magnus-instance-id instance)))
      (remhash id magnus-process--stream-busy)
      (remhash id magnus-process--stream-queues)))
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
Kills the current process and respawns in the new directory."
  (let* ((new-dir (expand-file-name directory))
         (name (magnus-instance-name instance)))
    ;; Kill old process and buffer immediately (no timers)
    (when-let ((buffer (magnus-instance-buffer instance)))
      (when (buffer-live-p buffer)
        (let ((process (get-buffer-process buffer)))
          (when (and process (process-live-p process))
            (kill-process process)))
        (kill-buffer buffer)))
    ;; Update directory, clear old session (it belongs to the old project)
    (magnus-instances-update instance
                             :directory new-dir
                             :status 'stopped
                             :buffer nil
                             :session-id nil)
    ;; Spawn fresh in the new directory
    (run-with-timer
     1 nil
     (lambda ()
       (magnus-process--spawn instance)))
    (message "Moving %s to %s (fresh start)..." name new-dir)))

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

;;; Headless mode — fire-and-forget agents

(defvar magnus-headless-allowed-tools)

(defvar-local magnus-process--headless-instance nil
  "The instance associated with this headless buffer.")

(define-derived-mode magnus-process-headless-mode special-mode "Headless"
  "Major mode for headless Claude Code output buffers."
  :group 'magnus
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(let ((map magnus-process-headless-mode-map))
  (define-key map (kbd "q") #'quit-window))

(defun magnus-process--headless-p (instance)
  "Return non-nil if INSTANCE is a headless (non-interactive) agent."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (eq major-mode 'magnus-process-headless-mode)))))

(defun magnus-process-create-headless (prompt &optional directory name)
  "Create a headless Claude Code instance with PROMPT.
DIRECTORY is the working directory.  NAME is optional.
Returns the new instance."
  (interactive "sTask prompt: ")
  (let* ((dir (or directory (magnus-process--get-directory)))
         (instance-name (or name
                            (concat "headless-"
                                    (funcall magnus-instance-name-generator dir))))
         (instance (magnus-instances-create dir instance-name)))
    (magnus-instances-add instance)
    (magnus-coord-register-agent dir instance)
    (magnus-process--spawn-headless instance prompt)
    instance))

(defun magnus-process--spawn-headless (instance prompt)
  "Spawn a headless Claude Code process for INSTANCE with PROMPT."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (buffer-name (format "*claude-headless:%s*" name))
         (default-directory directory)
         (full-prompt (magnus-process--headless-prompt instance prompt))
         (args (magnus-process--headless-args full-prompt)))
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (magnus-process-headless-mode)
        (setq magnus-process--headless-instance instance)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "Headless agent: %s\n" name)
                              'face 'font-lock-keyword-face))
          (insert (propertize (format "Directory: %s\n" directory)
                              'face 'font-lock-comment-face))
          (insert (propertize (format "Prompt: %s\n\n" prompt)
                              'face 'font-lock-comment-face))
          (insert (propertize "--- Output ---\n\n"
                              'face 'magnus-trace-separator))))
      (magnus-instances-update instance :buffer buffer :status 'running)
      (let ((partial-line ""))
        (make-process
         :name (format "claude-headless-%s" name)
         :buffer buffer
         :command (cons magnus-claude-executable args)
         :connection-type 'pipe
         :sentinel (magnus-process--headless-sentinel instance)
         :filter (lambda (proc output)
                   (setq partial-line
                         (magnus-process--headless-filter-output
                          instance proc output partial-line)))))
      buffer)))

(defun magnus-process--headless-prompt (instance prompt)
  "Build the full headless prompt for INSTANCE wrapping user PROMPT."
  (let ((name (magnus-instance-name instance)))
    (format "You are agent '%s', managed by Magnus. Before starting, \
read .magnus-coord.md to check what other agents are doing. \
Update the Active Work table with your name and planned work. \
Then execute this task:\n\n%s\n\n\
When done, update .magnus-coord.md to mark your work as complete."
            name prompt)))

(defun magnus-process--headless-args (prompt)
  "Build command-line arguments for a headless Claude process with PROMPT."
  (list "--print" prompt
        "--output-format" "stream-json"
        "--allowedTools" magnus-headless-allowed-tools))

(defun magnus-process--headless-sentinel (instance)
  "Return a process sentinel for headless INSTANCE."
  (lambda (proc event)
    (let ((event-str (string-trim event)))
      (cond
       ((string= event-str "finished")
        (magnus-instances-update instance :status 'finished)
        (message "Magnus: headless agent '%s' completed"
                 (magnus-instance-name instance)))
       ((string-prefix-p "exited abnormally" event-str)
        (magnus-instances-update instance :status 'errored)
        (message "Magnus: headless agent '%s' failed: %s"
                 (magnus-instance-name instance) event-str))
       (t
        (magnus-instances-update instance :status 'stopped)))
      ;; Append status to buffer
      (when-let ((buf (process-buffer proc)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (propertize (format "\n--- Process %s ---\n" event-str)
                                  'face 'magnus-trace-separator))))))
      ;; Update coordination
      (let ((dir (magnus-instance-directory instance))
            (name (magnus-instance-name instance)))
        (magnus-coord-add-log dir name
                              (format "Headless task %s" event-str)))
      ;; Refresh status buffer
      (when (get-buffer magnus-buffer-name)
        (magnus-status-refresh)))))

(defun magnus-process--headless-filter-output (instance proc output partial)
  "Process stream-json OUTPUT from PROC for headless INSTANCE.
PARTIAL is the incomplete line from previous call.  Returns new partial."
  (let* ((combined (concat partial output))
         (lines (split-string combined "\n"))
         (remainder (car (last lines)))
         (complete-lines (butlast lines)))
    (dolist (line complete-lines)
      (when (and (not (string-empty-p line))
                 (string-prefix-p "{" (string-trim line)))
        (condition-case nil
            (let* ((json (json-parse-string (string-trim line) :object-type 'alist))
                   (type (alist-get 'type json)))
              (magnus-process--headless-handle-event instance proc json type))
          (error nil))))
    remainder))

(defun magnus-process--headless-handle-event (instance proc json type)
  "Handle a stream-json event of TYPE for headless INSTANCE from PROC."
  (ignore instance)
  (when-let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (pcase type
            ("assistant"
             (when-let ((content (alist-get 'content (alist-get 'message json))))
               (when (vectorp content)
                 (seq-doseq (block content)
                   (when (string= (alist-get 'type block) "text")
                     (insert (alist-get 'text block)))))))
            ("result"
             (insert (propertize "\n--- Task Complete ---\n"
                                 'face 'magnus-trace-separator))
             (when-let ((cost (alist-get 'cost_usd json)))
               (insert (format "Cost: $%.4f\n" cost))))))))))

;;; Stream mode — per-message JSON subprocess agents

(defvar magnus-process--stream-busy (make-hash-table :test 'equal)
  "Hash table: instance-id -> t when a stream process is running.")

(defvar magnus-process--stream-queues (make-hash-table :test 'equal)
  "Hash table: instance-id -> list of queued messages (FIFO).")

(define-derived-mode magnus-process-stream-mode special-mode "Stream"
  "Major mode for stream-JSON Claude Code output buffers.
Shows structured output from a stream agent.  Read-only;
messages are sent via the command buffer."
  :group 'magnus
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(let ((map magnus-process-stream-mode-map))
  (define-key map (kbd "q") #'quit-window))

(defun magnus-process--stream-p (instance)
  "Return non-nil if INSTANCE is a stream-JSON agent."
  (eq (magnus-instance-type instance) 'stream))

(defun magnus-process-create-stream (&optional directory name)
  "Create a new stream-JSON Claude Code instance.
DIRECTORY is the working directory.  If nil, prompts for one.
NAME is the instance name.  If nil, auto-generates one.
Returns the new instance."
  (interactive)
  (let* ((dir (or directory (magnus-process--get-directory)))
         (instance-name (or name (funcall magnus-instance-name-generator dir)))
         (instance (magnus-instances-create dir instance-name)))
    ;; Set type before adding (struct default is 'vterm)
    (magnus-instances-update instance :type 'stream)
    (magnus-instances-add instance)
    ;; Set up coordination files and register agent
    (magnus-coord-register-agent dir instance)
    ;; Create the output buffer
    (let* ((buffer-name (format "*claude-stream:%s*" instance-name))
           (buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (magnus-process-stream-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "Stream agent: %s\n" instance-name)
                              'face 'font-lock-keyword-face))
          (insert (propertize (format "Directory: %s\n" dir)
                              'face 'font-lock-comment-face))
          (insert (propertize "--- Output ---\n\n"
                              'face 'magnus-trace-separator))))
      (magnus-instances-update instance :buffer buffer :status 'idle))
    ;; Send onboarding message
    (magnus-process-send-stream
     instance
     (magnus-process--onboarding-message instance))
    instance))

(defun magnus-process-send-stream (instance message)
  "Send MESSAGE to stream INSTANCE.
If the agent is busy (process running), queue the message.
If idle, spawn a new process with --resume."
  (let ((id (magnus-instance-id instance)))
    (if (gethash id magnus-process--stream-busy)
        ;; Busy — queue the message
        (let ((queue (gethash id magnus-process--stream-queues)))
          (puthash id (append queue (list message))
                   magnus-process--stream-queues)
          (message "Magnus: queued message for %s"
                   (magnus-instance-name instance)))
      ;; Idle — spawn process
      (magnus-process--spawn-stream-process instance message))))

(defun magnus-process--spawn-stream-process (instance message)
  "Spawn a `claude -p' process for stream INSTANCE with MESSAGE."
  (let* ((id (magnus-instance-id instance))
         (name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (session-id (magnus-instance-session-id instance))
         (buffer (magnus-instance-buffer instance))
         (default-directory directory)
         (args (magnus-process--stream-args session-id message)))
    ;; Mark busy
    (puthash id t magnus-process--stream-busy)
    (magnus-instances-update instance :status 'running)
    ;; Insert message header in output buffer
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize (format "\n[%s] user: "
                                      (format-time-string "%H:%M"))
                              'face 'font-lock-comment-face))
          (insert message "\n\n"))))
    ;; Notify command buffer
    (magnus-process--stream-notify
     instance 'stream-working
     :text (format "processing (%s)"
                   (if session-id "resume" "new session")))
    ;; Create process (pty required — claude buffers stdout on pipes)
    (let ((partial-line ""))
      (make-process
       :name (format "claude-stream-%s" name)
       :buffer buffer
       :command (cons magnus-claude-executable args)
       :connection-type 'pty
       :sentinel (magnus-process--stream-sentinel-fn instance)
       :filter (lambda (proc output)
                 (setq partial-line
                       (magnus-process--stream-filter
                        instance proc output partial-line)))))))

(defun magnus-process--stream-args (session-id message)
  "Build CLI args for a stream process.
SESSION-ID is used for --resume if non-nil.  MESSAGE is the prompt.
No --allowedTools: all tools go through the PermissionRequest hook
for project-scoped auto-approve and interactive prompts."
  (let ((args (list "--print" message
                    "--output-format" "stream-json"
                    "--verbose")))
    (when session-id
      (setq args (append (list "--resume" session-id) args)))
    args))

(defun magnus-process--stream-filter (instance proc output partial)
  "Process stream-json OUTPUT from PROC for stream INSTANCE.
PARTIAL is the incomplete line from previous call.  Returns new partial."
  (let* ((combined (concat partial output))
         (lines (split-string combined "\n"))
         (remainder (car (last lines)))
         (complete-lines (butlast lines)))
    (dolist (line complete-lines)
      (let ((trimmed (string-trim line)))
        (unless (string-empty-p trimmed)
          (if (string-prefix-p "{" trimmed)
              ;; JSON line — parse and handle
              (condition-case err
                  (let* ((json (json-parse-string trimmed
                                                  :object-type 'alist))
                         (type (alist-get 'type json)))
                    (magnus-process--stream-handle-event
                     instance proc json type))
                (error
                 (message "Magnus: JSON parse error: %s" err)))
            ;; Non-JSON line — show it (stderr, errors, etc.)
            (when-let ((buf (process-buffer proc)))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (goto-char (point-max))
                    (insert (propertize (concat trimmed "\n")
                                        'face 'font-lock-warning-face))))))))))
    remainder))

(defun magnus-process--stream-handle-event (instance proc json type)
  "Handle a stream-json event of TYPE for stream INSTANCE from PROC."
  (when-let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (pcase type
            ("system"
             ;; Capture session_id from init event
             (when-let ((sid (alist-get 'session_id json)))
               (unless (magnus-instance-session-id instance)
                 (magnus-instances-update instance :session-id sid)
                 (message "Magnus: captured session %s for %s"
                          sid (magnus-instance-name instance)))))
            ("assistant"
             (when-let ((content (alist-get 'content
                                            (alist-get 'message json))))
               (when (vectorp content)
                 (seq-doseq (block content)
                   (pcase (alist-get 'type block)
                     ("text"
                      (let ((text (alist-get 'text block)))
                        (insert text)
                        ;; Notify command buffer
                        (magnus-process--stream-notify
                         instance 'agent-reply :text text)))
                     ("tool_use"
                      (let ((tool-name (alist-get 'name block))
                            (tool-input (alist-get 'input block)))
                        (if (string= tool-name "AskUserQuestion")
                            ;; Special rendering for questions
                            (magnus-process--stream-render-question
                             instance tool-input)
                          (insert
                           (propertize
                            (format "  [%s] %s\n" tool-name
                                    (magnus-process--stream-tool-summary
                                     tool-name tool-input))
                            'face 'font-lock-type-face))))))))))
            ("user"
             ;; Tool results — brief indicator
             (when-let ((content (alist-get 'content
                                            (alist-get 'message json))))
               (when (vectorp content)
                 (seq-doseq (block content)
                   (when (string= (alist-get 'type block) "tool_result")
                     (let ((is-error (eq (alist-get 'is_error block) t)))
                       (insert
                        (propertize
                         (if is-error "  [error]\n" "  [done]\n")
                         'face (if is-error
                                   'font-lock-warning-face
                                 'font-lock-comment-face)))))))))
            ("result"
             (let ((turns (alist-get 'num_turns json))
                   (denials (alist-get 'permission_denials json)))
               (insert
                (propertize
                 (format "\n--- Done (%d turn%s) ---\n"
                         (or turns 0)
                         (if (= (or turns 0) 1) "" "s"))
                 'face 'magnus-trace-separator))
               ;; Report permission denials
               (when (and denials (> (length denials) 0))
                 (insert
                  (propertize
                   (format "  Permission denied: %d tool(s)\n"
                           (length denials))
                   'face 'font-lock-warning-face)))
               ;; Notify command buffer
               (magnus-process--stream-notify
                instance 'stream-done
                :text (format "done (%d turn%s)"
                              (or turns 0)
                              (if (= (or turns 0) 1) "" "s")))))))))))

(defun magnus-process--stream-tool-summary (tool-name tool-input)
  "Format a brief summary of TOOL-NAME with TOOL-INPUT for display."
  (pcase tool-name
    ((or "Bash" "Zsh")
     (or (alist-get 'command tool-input) ""))
    ((or "Read" "Edit" "Write")
     (or (alist-get 'file_path tool-input)
         (alist-get 'filePath tool-input) ""))
    ("Glob" (or (alist-get 'pattern tool-input) ""))
    ("Grep" (or (alist-get 'pattern tool-input) ""))
    ("Task" (or (alist-get 'description tool-input) ""))
    (_ "")))

(defun magnus-process--stream-render-question (instance tool-input)
  "Render an AskUserQuestion TOOL-INPUT and prompt the user.
Renders in the output buffer, notifies the command buffer,
and schedules a minibuffer prompt to collect the answer.
INSTANCE is the stream agent asking."
  (let ((questions (alist-get 'questions tool-input))
        (full-text ""))
    (when (and questions (vectorp questions))
      ;; Render in output buffer
      (insert (propertize "\n  === Question ===\n" 'face 'font-lock-warning-face))
      (seq-doseq (q questions)
        (let ((question (alist-get 'question q))
              (options (alist-get 'options q)))
          (when question
            (insert (propertize (format "  %s\n" question)
                                'face '(:weight bold)))
            (setq full-text
                  (concat full-text
                          (if (string-empty-p full-text) "" "; ")
                          question)))
          (when (and options (vectorp options))
            (let ((idx 1))
              (seq-doseq (opt options)
                (let ((label (alist-get 'label opt))
                      (desc (alist-get 'description opt)))
                  (insert (propertize (format "    %d. %s" idx label)
                                      'face 'font-lock-constant-face))
                  (when desc
                    (insert (propertize (format " — %s" desc)
                                        'face 'font-lock-comment-face)))
                  (insert "\n")
                  (setq idx (1+ idx))))))
          (insert "\n")))
      ;; Notify command buffer
      (magnus-process--stream-notify
       instance 'stream-question :text full-text)
      ;; Schedule interactive prompt (can't block in process filter)
      (run-at-time 0.5 nil
                   #'magnus-process--stream-ask-user
                   instance questions))))

(defun magnus-process--stream-ask-user (instance questions)
  "Prompt user for answers to QUESTIONS from stream INSTANCE.
Pops up `completing-read' with options (works with vertico/ivy/helm).
Sends the answer back as a follow-up message via the stream."
  (condition-case nil
      (let ((answers nil)
            (name (magnus-instance-name instance)))
        (seq-doseq (q questions)
          (let* ((question (alist-get 'question q))
                 (options (alist-get 'options q))
                 (multi (eq (alist-get 'multiSelect q) t))
                 (choices (when (and options (vectorp options))
                            (mapcar (lambda (opt) (alist-get 'label opt))
                                    options)))
                 (answer
                  (cond
                   ;; Multi-select: completing-read-multiple
                   ((and choices multi)
                    (string-join
                     (completing-read-multiple
                      (format "[%s] %s: " name question)
                      choices)
                     ", "))
                   ;; Single-select: completing-read (allows custom input)
                   (choices
                    (completing-read
                     (format "[%s] %s: " name question)
                     choices nil nil))
                   ;; No options: free text
                   (t
                    (read-string
                     (format "[%s] %s: " name question))))))
            (push (cons question answer) answers)))
        ;; Format and send
        (let* ((pairs (nreverse answers))
               (reply (mapconcat
                       (lambda (qa)
                         (format "Re: \"%s\" — %s" (car qa) (cdr qa)))
                       pairs "\n")))
          (unless (string-empty-p (string-trim reply))
            (magnus-process-send-stream instance reply)
            ;; Log in output buffer
            (when-let ((buf (magnus-instance-buffer instance)))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (goto-char (point-max))
                    (insert (propertize
                             (format "  → answered: %s\n\n"
                                     (string-trim reply))
                             'face 'font-lock-string-face)))))))))
    ;; User cancelled with C-g
    (quit
     (message "Magnus: question from %s dismissed"
              (magnus-instance-name instance)))))

(defun magnus-process--stream-sentinel-fn (instance)
  "Return a process sentinel for stream INSTANCE."
  (lambda (proc event)
    (let* ((id (magnus-instance-id instance))
           (event-str (string-trim event)))
      ;; ALWAYS clear busy first — even if rest of sentinel errors
      (remhash id magnus-process--stream-busy)
      (message "Magnus: stream sentinel for '%s': %s"
               (magnus-instance-name instance) event-str)
      ;; Update status
      (cond
       ((string= event-str "finished")
        (magnus-instances-update instance :status 'idle))
       ((string-prefix-p "exited abnormally" event-str)
        (magnus-instances-update instance :status 'idle)
        (message "Magnus: stream process for '%s' failed: %s"
                 (magnus-instance-name instance) event-str)
        ;; Log error to output buffer
        (when-let ((buf (process-buffer proc)))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert (propertize
                         (format "\n--- Error: %s ---\n" event-str)
                         'face 'font-lock-warning-face)))))))
       (t
        (magnus-instances-update instance :status 'idle)))
      ;; Check message queue — send next if any
      (let ((queue (gethash id magnus-process--stream-queues)))
        (when queue
          (let ((next-msg (car queue)))
            (puthash id (cdr queue) magnus-process--stream-queues)
            ;; Small delay to let things settle
            (run-with-timer
             0.5 nil
             (lambda ()
               (when (magnus-instances-get id)
                 (magnus-process--spawn-stream-process
                  instance next-msg)))))))
      ;; Refresh status buffer
      (when (get-buffer magnus-buffer-name)
        (magnus-status-refresh)))))

(defun magnus-process--stream-notify (instance type &rest props)
  "Send an event of TYPE for INSTANCE to the command buffer.
PROPS is a plist of additional event properties."
  (when-let ((buf (get-buffer (or (bound-and-true-p magnus-command-buffer-name)
                                   "*magnus-command*"))))
    (let ((event (append
                  (list :type type
                        :timestamp (float-time)
                        :instance-id (magnus-instance-id instance)
                        :instance-name (magnus-instance-name instance)
                        :handled t)
                  props)))
      (with-current-buffer buf
        (magnus-command--add-event event)))))

(provide 'magnus-process)
;;; magnus-process.el ends here
