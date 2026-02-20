;;; magnus-status.el --- Status buffer for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides the main status buffer showing all Claude Code
;; instances with magit-style keybindings.

;;; Code:

(require 'magnus-instances)
(require 'magnus-process)
(require 'magnus-coord)
(require 'magnus-attention)
(require 'magnus-health)

(declare-function magnus-dispatch "magnus-transient")
(declare-function magnus-coord-agent-busy-p "magnus-coord")
(declare-function magnus-coord--neglected-p "magnus-coord")
(declare-function magnus-retro "magnus-coord")

(defvar magnus-coord--do-not-disturb)
(declare-function magnus-context "magnus-context")
(declare-function magnus-chat "magnus-chat")

;; Defined in magnus.el
(defvar magnus-buffer-name)
(defvar magnus-default-directory)

;;; Faces

(defface magnus-status-instance-name
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for instance names."
  :group 'magnus)

(defface magnus-status-instance-dir
  '((t :inherit font-lock-comment-face))
  "Face for instance directories."
  :group 'magnus)

(defface magnus-status-running
  '((t :inherit success))
  "Face for running status."
  :group 'magnus)

(defface magnus-status-stopped
  '((t :inherit error))
  "Face for stopped status."
  :group 'magnus)

(defface magnus-status-suspended
  '((t :inherit warning))
  "Face for suspended status."
  :group 'magnus)

(defface magnus-status-section-heading
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section headings."
  :group 'magnus)

(defface magnus-status-empty-hint
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for empty state hints."
  :group 'magnus)

(defface magnus-status-finished
  '((t :inherit success :slant italic))
  "Face for finished status (headless completed)."
  :group 'magnus)

(defface magnus-status-errored
  '((t :inherit error :slant italic))
  "Face for errored status (headless failed)."
  :group 'magnus)

;;; Mode definition

(defvar magnus-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'magnus-status-visit)
    (define-key map (kbd "c") #'magnus-status-create)
    (define-key map (kbd "k") #'magnus-status-kill)
    (define-key map (kbd "K") #'magnus-status-kill-force)
    (define-key map (kbd "r") #'magnus-status-rename)
    (define-key map (kbd "R") #'magnus-status-restart)
    (define-key map (kbd "s") #'magnus-status-suspend)
    (define-key map (kbd "S") #'magnus-status-resume)
    (define-key map (kbd "d") #'magnus-status-chdir)
    (define-key map (kbd "m") #'magnus-status-send-message)
    (define-key map (kbd "M") #'magnus-chat)
    (define-key map (kbd "t") #'magnus-status-trace)
    (define-key map (kbd "g") #'magnus-status-refresh)
    (define-key map (kbd "x") #'magnus-status-context)
    (define-key map (kbd "C") #'magnus-status-coordination)
    (define-key map (kbd "n") #'magnus-status-next)
    (define-key map (kbd "p") #'magnus-status-previous)
    (define-key map (kbd "a") #'magnus-attention-next)
    (define-key map (kbd "A") #'magnus-attention-show-queue)
    (define-key map (kbd "P") #'magnus-status-purge)
    (define-key map (kbd "z") #'magnus-coord-toggle-dnd)
    (define-key map (kbd "F") #'magnus-retro)
    (define-key map (kbd "?") #'magnus-dispatch)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `magnus-status-mode'.")

(define-derived-mode magnus-status-mode special-mode "Magnus"
  "Major mode for the magnus status buffer.

\\{magnus-status-mode-map}"
  :group 'magnus
  (setq-local revert-buffer-function #'magnus-status--revert)
  (setq-local truncate-lines t)
  (add-hook 'magnus-instances-changed-hook #'magnus-status--maybe-refresh))

;;; Buffer creation

(defun magnus-status ()
  "Open or switch to the magnus status buffer."
  (interactive)
  (let ((buffer (get-buffer-create magnus-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magnus-status-mode)
        (magnus-status-mode))
      (magnus-status-refresh))
    (switch-to-buffer buffer)))

(defun magnus-status-refresh ()
  "Refresh the magnus status buffer."
  (interactive)
  ;; Reconcile only on interactive (manual `g') refresh
  (when (called-interactively-p 'interactive)
    (magnus-coord-reconcile-all))
  (when-let ((buffer (get-buffer magnus-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (line (line-number-at-pos)))
        (erase-buffer)
        (magnus-status--insert-header)
        (magnus-status--insert-instances)
        (magnus-status--insert-coordination)
        (goto-char (point-min))
        (forward-line (1- line))
        (magnus-status--goto-instance-line)))))

(defun magnus-status--revert (_ignore-auto _noconfirm)
  "Revert function for status buffer."
  (magnus-status-refresh))

(defun magnus-status--maybe-refresh ()
  "Refresh if the status buffer is visible."
  (when-let ((buffer (get-buffer magnus-buffer-name)))
    (when (get-buffer-window buffer)
      (magnus-status-refresh))))

;;; Buffer content

(defun magnus-status--insert-header ()
  "Insert the status buffer header."
  (insert (propertize "Magnus" 'face 'magnus-status-section-heading))
  (insert " - Claude Code Instance Manager\n")
  (insert (format "Instances: %d" (magnus-instances-count)))
  (when magnus-coord--do-not-disturb
    (insert (propertize "  [DND]" 'face 'font-lock-warning-face)))
  (let ((attention-count (magnus-attention-pending-count)))
    (when (> attention-count 0)
      (insert (propertize (format "  [%d need attention]" attention-count)
                         'face 'magnus-status-running))))
  (insert "\n\n"))

(defun magnus-status--insert-instances ()
  "Insert the list of instances."
  (let ((instances (magnus-instances-list)))
    (if (null instances)
        (magnus-status--insert-empty-state)
      (insert (propertize "Instances\n" 'face 'magnus-status-section-heading))
      (dolist (instance instances)
        (magnus-status--insert-instance instance)))))

(defun magnus-status--insert-empty-state ()
  "Insert the empty state message."
  (insert "\n")
  (insert (propertize "  No Claude Code instances.\n" 'face 'magnus-status-empty-hint))
  (insert (propertize "  Press 'c' to create one.\n" 'face 'magnus-status-empty-hint)))

(defun magnus-status--insert-instance (instance)
  "Insert a line for INSTANCE."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (status (magnus-instance-status instance))
         (suspended (eq status 'suspended))
         (finished (eq status 'finished))
         (errored (eq status 'errored))
         (running (or (eq status 'running)
                      (magnus-process-running-p instance)))
         (status-str (cond (suspended "suspended")
                           (finished "finished")
                           (errored "errored")
                           (running "running")
                           (t "stopped")))
         (status-face (cond (suspended 'magnus-status-suspended)
                            (finished 'magnus-status-finished)
                            (errored 'magnus-status-errored)
                            (running 'magnus-status-running)
                            (t 'magnus-status-stopped)))
         (health-ind (magnus-health-indicator instance))
         (age (magnus-status--format-age (magnus-instance-created-at instance))))
    (insert "  ")
    (insert (propertize name 'face 'magnus-status-instance-name))
    (insert " ")
    (insert (propertize (format "[%s]" status-str) 'face status-face))
    (when (magnus-coord-agent-busy-p instance)
      (insert " ")
      (insert (propertize "busy" 'face 'font-lock-warning-face)))
    (when (magnus-coord--neglected-p instance)
      (insert " ")
      (insert (propertize "!" 'face 'font-lock-warning-face)))
    (insert " ")
    (insert health-ind)
    (insert " ")
    (insert (propertize age 'face 'magnus-status-instance-dir))
    (insert "\n")
    (insert "    ")
    (insert (propertize (abbreviate-file-name directory) 'face 'magnus-status-instance-dir))
    (insert "\n")
    ;; Store instance ID as text property for commands
    (put-text-property (line-beginning-position -1) (point)
                       'magnus-instance-id (magnus-instance-id instance))))

(defun magnus-status--format-age (time)
  "Format TIME as a human-readable age."
  (let* ((seconds (float-time (time-subtract (current-time) time)))
         (minutes (/ seconds 60))
         (hours (/ minutes 60))
         (days (/ hours 24)))
    (cond
     ((< seconds 60) "just now")
     ((< minutes 60) (format "%dm ago" (floor minutes)))
     ((< hours 24) (format "%dh ago" (floor hours)))
     (t (format "%dd ago" (floor days))))))

;;; Coordination display

(defun magnus-status--insert-coordination ()
  "Insert coordination status from all project directories."
  (let ((directories (magnus-status--get-project-directories)))
    (when directories
      (insert "\n")
      (insert (propertize "Coordination\n" 'face 'magnus-status-section-heading))
      (dolist (dir directories)
        (magnus-status--insert-coordination-for-dir dir)))))

(defun magnus-status--get-project-directories ()
  "Get unique project directories from all instances."
  (let ((dirs (mapcar #'magnus-instance-directory (magnus-instances-list))))
    (delete-dups dirs)))

(defun magnus-status--insert-coordination-for-dir (directory)
  "Insert coordination info for DIRECTORY."
  (let ((coord-file (magnus-coord-file-path directory)))
    (when (file-exists-p coord-file)
      (let* ((parsed (magnus-coord-parse directory))
             (active (plist-get parsed :active))
             (log (plist-get parsed :log)))
        ;; Show directory
        (insert "  ")
        (insert (propertize (abbreviate-file-name directory)
                           'face 'magnus-status-instance-dir))
        (insert "\n")
        ;; Show active work
        (when active
          (insert (propertize "  Active Work:\n" 'face 'font-lock-comment-face))
          (dolist (entry active)
            (insert (format "    %s: %s [%s]\n"
                           (propertize (plist-get entry :agent)
                                      'face 'magnus-status-instance-name)
                           (plist-get entry :area)
                           (propertize (plist-get entry :status)
                                      'face (if (string= (plist-get entry :status)
                                                        "in-progress")
                                               'magnus-status-running
                                             'magnus-status-instance-dir))))))
        ;; Show recent log (last 3 entries)
        (when log
          (insert (propertize "  Recent:\n" 'face 'font-lock-comment-face))
          (let ((recent (seq-take (reverse log) 3)))
            (dolist (entry (reverse recent))
              (insert (format "    [%s] %s: %s\n"
                             (propertize (plist-get entry :time)
                                        'face 'magnus-status-instance-dir)
                             (propertize (plist-get entry :agent)
                                        'face 'magnus-status-instance-name)
                             (plist-get entry :message))))))
        (insert "\n")))))

;;; Navigation

(defun magnus-status--get-instance-at-point ()
  "Get the instance at point."
  (when-let ((id (get-text-property (point) 'magnus-instance-id)))
    (magnus-instances-get id)))

(defun magnus-status--goto-instance-line ()
  "Move point to the nearest instance line."
  (unless (get-text-property (point) 'magnus-instance-id)
    (or (magnus-status--find-instance-forward)
        (magnus-status--find-instance-backward))))

(defun magnus-status--find-instance-forward ()
  "Find next instance and move point there."
  (let ((start (point)))
    (while (and (not (eobp))
                (not (get-text-property (point) 'magnus-instance-id)))
      (forward-line 1))
    (if (get-text-property (point) 'magnus-instance-id)
        t
      (goto-char start)
      nil)))

(defun magnus-status--find-instance-backward ()
  "Find previous instance and move point there."
  (let ((start (point)))
    (while (and (not (bobp))
                (not (get-text-property (point) 'magnus-instance-id)))
      (forward-line -1))
    (if (get-text-property (point) 'magnus-instance-id)
        t
      (goto-char start)
      nil)))

(defun magnus-status-next ()
  "Move to the next instance."
  (interactive)
  (forward-line 1)
  (while (and (not (eobp))
              (not (get-text-property (point) 'magnus-instance-id)))
    (forward-line 1))
  (unless (get-text-property (point) 'magnus-instance-id)
    (magnus-status--find-instance-backward)))

(defun magnus-status-previous ()
  "Move to the previous instance."
  (interactive)
  (forward-line -1)
  (while (and (not (bobp))
              (not (get-text-property (point) 'magnus-instance-id)))
    (forward-line -1)))

;;; Commands

(defun magnus-status-visit ()
  "Switch to the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (magnus-process-switch-to instance)
    (user-error "No instance at point")))

(defvar magnus--creation-task)

(defun magnus-status-create ()
  "Create a new Claude Code instance.
Prompts for a task description to enable smart resurrection of
dormant agents with relevant expertise.  Press RET to skip.
Uses the directory of the instance at point, or the first instance's
directory, or `magnus-default-directory', or `default-directory'."
  (interactive)
  (let* ((dir (or (when-let ((inst (magnus-status--get-instance-at-point)))
                    (magnus-instance-directory inst))
                  (when-let ((inst (car (magnus-instances-list))))
                    (magnus-instance-directory inst))
                  magnus-default-directory
                  default-directory))
         (task (read-string "What will this agent work on? (RET to skip): "))
         (magnus--creation-task (unless (string-empty-p task) task)))
    (magnus-process-create dir)
    (magnus-status-refresh)))

(defun magnus-status-kill ()
  "Kill the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (when (yes-or-no-p (format "Kill instance '%s'? "
                                 (magnus-instance-name instance)))
        (magnus-process-kill-and-remove instance)
        (magnus-status-refresh))
    (user-error "No instance at point")))

(defun magnus-status-kill-force ()
  "Forcefully kill the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (when (yes-or-no-p (format "Force kill instance '%s'? "
                                 (magnus-instance-name instance)))
        (magnus-process-kill-and-remove instance t)
        (magnus-status-refresh))
    (user-error "No instance at point")))

(defun magnus-status-rename ()
  "Rename the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (let* ((old-name (magnus-instance-name instance))
             (new-name (read-string "New name: " old-name)))
        (unless (string-empty-p new-name)
          (magnus-instances-update instance :name new-name)
          (magnus-status-refresh)))
    (user-error "No instance at point")))

(defun magnus-status-restart ()
  "Restart the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (progn
        (magnus-process-restart instance)
        (message "Restarting '%s'..." (magnus-instance-name instance)))
    (user-error "No instance at point")))

(defun magnus-status-context ()
  "Open the shared context buffer for the current project."
  (interactive)
  (magnus-context))

(defun magnus-status-coordination ()
  "Open the coordination file for the current project."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (magnus-coord-open (magnus-instance-directory instance))
    ;; No instance at point, try to use first instance's directory
    (if-let ((first-instance (car (magnus-instances-list))))
        (magnus-coord-open (magnus-instance-directory first-instance))
      (user-error "No instances to get project directory from"))))

(defun magnus-status-suspend ()
  "Suspend the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (if (magnus-process-suspended-p instance)
          (user-error "Instance '%s' is already suspended"
                     (magnus-instance-name instance))
        (magnus-process-suspend instance)
        (magnus-status-refresh))
    (user-error "No instance at point")))

(defun magnus-status-resume ()
  "Resume the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (if (magnus-process-suspended-p instance)
          (progn
            (magnus-process-resume instance)
            (magnus-status-refresh))
        (user-error "Instance '%s' is not suspended"
                   (magnus-instance-name instance)))
    (user-error "No instance at point")))

(defun magnus-status-trace ()
  "Open the thinking trace for the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (magnus-process-trace instance)
    (user-error "No instance at point")))

(defun magnus-status-send-message ()
  "Send a message to the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (let ((msg (read-string (format "Message to %s: "
                                      (magnus-instance-name instance)))))
        (unless (string-empty-p msg)
          (magnus-coord-nudge-agent instance msg)
          (message "Sent to %s" (magnus-instance-name instance))))
    (user-error "No instance at point")))

(defun magnus-status-chdir ()
  "Change the working directory of the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (let* ((new-dir (read-directory-name "New directory: " nil nil t)))
        (magnus-process-chdir instance new-dir)
        (magnus-status-refresh))
    (user-error "No instance at point")))

(defun magnus-status-purge ()
  "Kill and remove all instances."
  (interactive)
  (let ((count (magnus-instances-count)))
    (if (zerop count)
        (user-error "No instances to purge")
      (when (yes-or-no-p (format "Kill and remove all %d instance%s? "
                                 count (if (= count 1) "" "s")))
        (dolist (instance (copy-sequence (magnus-instances-list)))
          (magnus-process-kill-and-remove instance t))
        (magnus-status-refresh)
        (message "Purged %d instance%s" count (if (= count 1) "" "s"))))))

(provide 'magnus-status)
;;; magnus-status.el ends here
