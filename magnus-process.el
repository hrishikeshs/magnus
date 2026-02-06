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
  "Get the current project root if available."
  (when (fboundp 'project-current)
    (when-let ((project (project-current)))
      (if (fboundp 'project-root)
          (project-root project)
        ;; Emacs < 28
        (car (project-roots project))))))

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
      ;; Detect and store session ID after Claude creates it
      (run-with-timer 5 nil #'magnus-process--detect-session
                      instance directory sessions-before)
      buffer)))

(defun magnus-process--send-onboarding (instance)
  "Send onboarding message to INSTANCE."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (let ((name (magnus-instance-name instance))
            (msg (magnus-process--onboarding-message instance)))
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
  "List all session IDs for DIRECTORY."
  (let* ((project-hash (magnus-process--project-hash directory))
         (sessions-dir (expand-file-name
                        (concat "projects/" project-hash)
                        (expand-file-name ".claude" (getenv "HOME")))))
    (when (file-directory-p sessions-dir)
      (directory-files sessions-dir nil "^[^.]"))))

(defun magnus-process--detect-session (instance directory sessions-before)
  "Detect new session for INSTANCE in DIRECTORY.
SESSIONS-BEFORE is the list of sessions that existed before spawning."
  (let* ((sessions-after (magnus-process--list-sessions directory))
         (new-sessions (cl-set-difference sessions-after sessions-before :test #'string=)))
    (when new-sessions
      ;; If multiple new sessions (unlikely), pick the most recent
      (let ((session-id (if (= 1 (length new-sessions))
                            (car new-sessions)
                          (magnus-process--most-recent-session directory new-sessions))))
        (magnus-instances-update instance :session-id session-id)
        (message "Captured session %s for %s"
                 session-id (magnus-instance-name instance))))))

(defun magnus-process--most-recent-session (directory sessions)
  "Find the most recently modified session in DIRECTORY from SESSIONS list."
  (let* ((project-hash (magnus-process--project-hash directory))
         (sessions-dir (expand-file-name
                        (concat "projects/" project-hash)
                        (expand-file-name ".claude" (getenv "HOME")))))
    (car (sort sessions
               (lambda (a b)
                 (time-less-p
                  (file-attribute-modification-time
                   (file-attributes (expand-file-name b sessions-dir)))
                  (file-attribute-modification-time
                   (file-attributes (expand-file-name a sessions-dir)))))))))

(defun magnus-process--create-vterm-buffer (buffer-name)
  "Create a vterm buffer with BUFFER-NAME."
  (let ((buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
      (vterm-mode))
    buffer))

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
              (vterm-send-C-c)))))
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

(defun magnus-process-restart (instance)
  "Restart the Claude Code process for INSTANCE."
  (magnus-process-kill instance)
  (run-with-timer
   1.5 nil
   (lambda ()
     (magnus-process--spawn instance))))

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
  (let* ((old-dir (magnus-instance-directory instance))
         (new-dir (expand-file-name directory))
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
  (let ((path (expand-file-name directory)))
    ;; Remove leading slash and replace special chars with hyphens
    (replace-regexp-in-string
     "^-+" ""
     (replace-regexp-in-string "[/ ~]+" "-" path))))

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
