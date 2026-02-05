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
         (default-directory directory))
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
      buffer)))

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
