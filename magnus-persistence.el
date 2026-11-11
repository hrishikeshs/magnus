;;; magnus-persistence.el --- State persistence for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module handles saving and restoring magnus instance state
;; across Emacs sessions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magnus-instances)

(declare-function magnus-process-reconnect "magnus-process")

;; Defined in magnus.el
(defvar magnus-state-file)

;; Dynamically binding this reader control is harmless on Emacs versions that
;; reject reader evaluation unconditionally and protects versions that expose
;; it as an option.
(defvar read-eval)

;;; Persistence

(defun magnus-persistence--default-directory-p (directory)
  "Return non-nil when DIRECTORY is Magnus's default state directory."
  (string-equal
   (directory-file-name (expand-file-name directory))
   (directory-file-name
    (expand-file-name ".magnus" (or (getenv "HOME") "~")))))

(defun magnus-persistence--ensure-private-directory (file)
  "Ensure FILE's parent exists and return its path.
New directories and Magnus's default state directory are made private.  An
existing custom parent keeps its permissions because `magnus-state-file' may
deliberately point into a shared directory."
  (let* ((directory (file-name-directory (expand-file-name file)))
         (created (not (file-exists-p directory))))
    (when (file-remote-p directory)
      (error "Magnus state path may not be remote: %s" directory))
    (when (file-symlink-p directory)
      (error "Refusing symlinked Magnus state directory: %s" directory))
    (if (file-exists-p directory)
        (unless (file-directory-p directory)
          (error "Magnus state parent is not a directory: %s" directory))
      (make-directory directory t))
    (when (or created (magnus-persistence--default-directory-p directory))
      (set-file-modes directory #o700))
    directory))

(defun magnus-persistence--atomic-write (file contents)
  "Atomically replace FILE with CONTENTS using private permissions."
  (let* ((file (expand-file-name file))
         (directory (magnus-persistence--ensure-private-directory file))
         temporary)
    (when (file-symlink-p file)
      (error "Refusing to overwrite symlinked Magnus state file: %s" file))
    (when (file-directory-p file)
      (error "Magnus state path is a directory: %s" file))
    (setq temporary
          (make-temp-file (expand-file-name ".magnus-state-tmp-" directory)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region contents nil temporary nil 'quiet))
          (set-file-modes temporary #o600)
          (rename-file temporary file t)
          (setq temporary nil)
          (set-file-modes file #o600))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun magnus-persistence-save ()
  "Save current instance state to disk."
  (let ((state (mapcar #'magnus-instances-serialize
                       (magnus-instances-list))))
    (magnus-persistence--atomic-write
     magnus-state-file
     (with-temp-buffer
       (insert ";; Magnus state file - do not edit manually\n")
       (insert ";; Generated at: "
               (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")
       (pp state (current-buffer))
       (buffer-string)))
    (message "Magnus: saved %d instance(s)" (length state))))

(defun magnus-persistence--timestamp-p (value)
  "Return non-nil when VALUE is nil or a serialized Emacs timestamp."
  (or (null value)
      (numberp value)
      (and (proper-list-p value)
           (memq (length value) '(2 3 4))
           (let ((parts value)
                 (valid t))
             (while parts
               (unless (integerp (pop parts))
                 (setq valid nil
                       parts nil)))
             valid))))

(defun magnus-persistence--nonempty-string-p (value)
  "Return non-nil when VALUE is a nonempty string."
  (and (stringp value) (not (string-empty-p value))))

(defun magnus-persistence--deserialize-record (record index)
  "Validate and deserialize RECORD at one-based INDEX."
  (unless (and (proper-list-p record) (zerop (% (length record) 2)))
    (error "Invalid Magnus state record %d: expected a property list" index))
  (let ((properties record))
    (while properties
      (unless (keywordp (pop properties))
        (error "Invalid Magnus state record %d: non-keyword property" index))
      (pop properties)))
  (let ((instance (magnus-instances-deserialize record)))
    (unless (magnus-instances-valid-id-p (magnus-instance-id instance))
      (error "Invalid Magnus state record %d: missing or invalid id" index))
    (unless (magnus-instances-valid-name-p (magnus-instance-name instance))
      (error "Invalid Magnus state record %d: missing or invalid name" index))
    (unless (magnus-persistence--nonempty-string-p
             (magnus-instance-directory instance))
      (error "Invalid Magnus state record %d: missing or invalid directory" index))
    ;; Creation time is display metadata, not identity or lifecycle authority.
    ;; Historical or hand-repaired state must not make every persisted agent
    ;; disappear because this one nonessential field has an obsolete shape.
    (unless (and (magnus-instance-created-at instance)
                 (magnus-persistence--timestamp-p
                  (magnus-instance-created-at instance)))
      (setf (magnus-instance-created-at instance) (current-time))
      (message "Magnus: repaired creation timestamp in state record %d" index))
    (unless (symbolp (magnus-instance-provider instance))
      (error "Invalid Magnus state record %d: invalid provider" index))
    (unless (magnus-instances-valid-kind-p (magnus-instance-kind instance))
      (error "Invalid Magnus state record %d: invalid instance kind" index))
    (unless (memq (magnus-instance-status instance)
                  '(running stopped suspended purged finished errored))
      (error "Invalid Magnus state record %d: invalid status" index))
    (dolist (session (list (magnus-instance-session-id instance)
                           (magnus-instance-previous-session-id instance)))
      (unless (or (null session) (stringp session))
        (error "Invalid Magnus state record %d: invalid session id" index)))
    (unless (magnus-persistence--timestamp-p
             (magnus-instance-purged-at instance))
      (error "Invalid Magnus state record %d: invalid archive timestamp" index))
    instance))

(defun magnus-persistence--deserialize-state (state)
  "Validate and deserialize all records in STATE without changing the registry."
  (unless (proper-list-p state)
    (error "Invalid Magnus state: expected a list of records"))
  (let ((seen-ids (make-hash-table :test #'equal))
        (index 0)
        instances)
    (dolist (record state (nreverse instances))
      (let* ((instance (magnus-persistence--deserialize-record
                        record (cl-incf index)))
             (id (magnus-instance-id instance)))
        (when (gethash id seen-ids)
          (error "Invalid Magnus state record %d: duplicate id %s" index id))
        (puthash id t seen-ids)
        (push instance instances)))))

(defun magnus-persistence--read-state ()
  "Read and validate `magnus-state-file' into a temporary instance list."
  (with-temp-buffer
    (insert-file-contents magnus-state-file)
    (goto-char (point-min))
    (let ((read-eval nil)
          state)
      (setq state (read (current-buffer)))
      (condition-case err
          (progn
            (read (current-buffer))
            (error "Invalid Magnus state: more than one Lisp form"))
        (end-of-file nil)
        (error (signal (car err) (cdr err))))
      (magnus-persistence--deserialize-state state))))

(defun magnus-persistence-load ()
  "Load instance state from disk."
  (when (file-exists-p magnus-state-file)
    (condition-case err
        (let ((instances (magnus-persistence--read-state)))
          ;; Suppress incremental notifications so observers never see a
          ;; partially restored registry.  Reconnect every provider only after
          ;; the complete validated replacement is live.
          (let ((magnus-instances-changed-hook nil))
            (setq magnus-instances instances)
            (dolist (instance instances)
              (condition-case reconnect-error
                  (magnus-persistence--try-reconnect instance)
                (error
                 (message "Magnus: failed to reconnect %s: %s"
                          (magnus-instance-name instance)
                          (error-message-string reconnect-error))))))
          (run-hooks 'magnus-instances-changed-hook)
          (message "Magnus: loaded %d instance(s)" (length instances)))
      (error
       (message "Magnus: failed to load state: %s" (error-message-string err))))))

(defun magnus-persistence--try-reconnect (instance)
  "Delegate INSTANCE reconnection to the provider-aware process layer."
  (magnus-process-reconnect instance))

;;; Auto-save hooks

(defvar magnus-persistence--save-timer nil
  "Timer for debounced saving.")

(defvar magnus-persistence--autosave-active nil
  "Non-nil while Magnus persistence owns its hooks and autosave timer.")

(defun magnus-persistence--setup-autosave ()
  "Install automatic persistence hooks."
  (add-hook 'magnus-instances-changed-hook #'magnus-persistence--schedule-save)
  (add-hook 'kill-emacs-hook #'magnus-persistence-save)
  (setq magnus-persistence--autosave-active t))

(defun magnus-persistence--schedule-save ()
  "Schedule a save after a short delay (debounced)."
  (when (timerp magnus-persistence--save-timer)
    (cancel-timer magnus-persistence--save-timer))
  (setq magnus-persistence--save-timer
        (run-with-idle-timer 2 nil #'magnus-persistence--do-save)))

(defun magnus-persistence--do-save ()
  "Actually perform the save."
  (setq magnus-persistence--save-timer nil)
  (magnus-persistence-save))

(defun magnus-persistence-shutdown ()
  "Stop persistence hooks and timers, flushing owned or pending state once."
  (let ((flush-state (or magnus-persistence--autosave-active
                         (timerp magnus-persistence--save-timer))))
    ;; Relinquish lifecycle ownership before writing.  Cleanup therefore stays
    ;; complete and a repeated shutdown is harmless even if the write fails.
    (when (timerp magnus-persistence--save-timer)
      (cancel-timer magnus-persistence--save-timer))
    (setq magnus-persistence--save-timer nil
          magnus-persistence--autosave-active nil)
    (remove-hook 'magnus-instances-changed-hook
                 #'magnus-persistence--schedule-save)
    (remove-hook 'kill-emacs-hook #'magnus-persistence-save)
    (when flush-state
      (magnus-persistence-save))))

(provide 'magnus-persistence)
;;; magnus-persistence.el ends here
