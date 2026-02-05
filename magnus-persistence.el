;;; magnus-persistence.el --- State persistence for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S

;;; Commentary:

;; This module handles saving and restoring magnus instance state
;; across Emacs sessions.

;;; Code:

(require 'magnus-instances)

(declare-function magnus-process-reconnect "magnus-process")

;;; Persistence

(defun magnus-persistence-save ()
  "Save current instance state to disk."
  (let ((state (mapcar #'magnus-instances-serialize
                       (magnus-instances-list))))
    (with-temp-file magnus-state-file
      (insert ";; Magnus state file - do not edit manually\n")
      (insert ";; Generated at: " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
      (pp state (current-buffer)))
    (message "Magnus: saved %d instance(s)" (length state))))

(defun magnus-persistence-load ()
  "Load instance state from disk."
  (when (file-exists-p magnus-state-file)
    (condition-case err
        (let ((state (with-temp-buffer
                       (insert-file-contents magnus-state-file)
                       (goto-char (point-min))
                       (read (current-buffer)))))
          (magnus-instances-clear)
          (dolist (plist (reverse state))
            (let ((instance (magnus-instances-deserialize plist)))
              (magnus-instances-add instance)
              ;; Try to reconnect to running process
              (magnus-persistence--try-reconnect instance)))
          (message "Magnus: loaded %d instance(s)" (length state)))
      (error
       (message "Magnus: failed to load state: %s" (error-message-string err))))))

(defun magnus-persistence--try-reconnect (instance)
  "Try to reconnect INSTANCE to a running Claude process.
This is a best-effort operation that looks for existing vterm buffers."
  (let* ((name (magnus-instance-name instance))
         (buffer-name (format "*claude:%s*" name))
         (buffer (get-buffer buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      ;; Found an existing buffer, check if process is running
      (when (get-buffer-process buffer)
        (magnus-instances-update instance
                                 :buffer buffer
                                 :status 'running)))))

;;; Auto-save hooks

(defun magnus-persistence--setup-autosave ()
  "Set up automatic saving on changes."
  (add-hook 'magnus-instances-changed-hook #'magnus-persistence--schedule-save)
  (add-hook 'kill-emacs-hook #'magnus-persistence-save))

(defvar magnus-persistence--save-timer nil
  "Timer for debounced saving.")

(defun magnus-persistence--schedule-save ()
  "Schedule a save after a short delay (debounced)."
  (when magnus-persistence--save-timer
    (cancel-timer magnus-persistence--save-timer))
  (setq magnus-persistence--save-timer
        (run-with-idle-timer 2 nil #'magnus-persistence--do-save)))

(defun magnus-persistence--do-save ()
  "Actually perform the save."
  (setq magnus-persistence--save-timer nil)
  (magnus-persistence-save))

;; Set up autosave when loaded
(magnus-persistence--setup-autosave)

(provide 'magnus-persistence)
;;; magnus-persistence.el ends here
