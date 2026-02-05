;;; magnus-attention.el --- Attention queue for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module manages attention requests from Claude Code instances.
;; When an instance needs user input (permissions, confirmations, etc.),
;; it joins an attention queue.  Magnus focuses instances one at a time,
;; preventing multiple instances from competing for attention.
;;
;; Detection methods:
;; 1. Pattern matching in vterm buffers for permission prompts
;; 2. Coordination file status (agents announce awaiting-input)
;;
;; The attention queue ensures orderly handling of requests.

;;; Code:

(require 'magnus-instances)
(require 'magnus-coord)

;;; Customization

(defcustom magnus-attention-check-interval 2
  "Seconds between checks for instances needing attention."
  :type 'number
  :group 'magnus)

(defcustom magnus-attention-patterns
  '("\\[y/n\\]"
    "\\[Y/n\\]"
    "\\[yes/no\\]"
    "(y/n)"
    "(Y/n)"
    "Press Enter to continue"
    "Allow\\?"
    "Proceed\\?"
    "Continue\\?"
    "approve"
    "permission"
    "Do you want to")
  "Patterns that indicate an instance is waiting for input.
These are matched against the last few lines of the vterm buffer."
  :type '(repeat regexp)
  :group 'magnus)

(defcustom magnus-attention-scan-lines 10
  "Number of lines from end of buffer to scan for attention patterns."
  :type 'integer
  :group 'magnus)

(defcustom magnus-attention-notify t
  "If non-nil, show notifications when instances need attention."
  :type 'boolean
  :group 'magnus)

;;; Attention queue

(defvar magnus-attention-queue nil
  "Queue of instance IDs waiting for user attention.
First element is the instance currently having the floor.")

(defvar magnus-attention-current nil
  "Instance ID that currently has the user's attention, or nil.")

(defvar magnus-attention--timer nil
  "Timer for checking instances for attention needs.")

(defvar magnus-attention--last-notify nil
  "Last notification time to avoid spam.")

;;; Queue management

(defun magnus-attention-request (instance)
  "Add INSTANCE to the attention queue if not already present."
  (let ((id (magnus-instance-id instance)))
    (unless (member id magnus-attention-queue)
      (setq magnus-attention-queue (append magnus-attention-queue (list id)))
      (magnus-attention--log instance "needs attention")
      (when (and magnus-attention-notify
                 (= (length magnus-attention-queue) 1))
        ;; First in queue, notify immediately
        (magnus-attention--notify instance))
      ;; Update coordination file
      (magnus-attention--update-coord instance 'awaiting-input))))

(defun magnus-attention-release (instance)
  "Release INSTANCE from the attention queue."
  (let ((id (magnus-instance-id instance)))
    (setq magnus-attention-queue (delete id magnus-attention-queue))
    (when (string= magnus-attention-current id)
      (setq magnus-attention-current nil)
      ;; Update coordination file
      (magnus-attention--update-coord instance 'ready)
      ;; Focus next in queue if any
      (magnus-attention--focus-next))))

(defun magnus-attention-release-current ()
  "Release the current instance from attention (call when done with it)."
  (interactive)
  (when magnus-attention-current
    (when-let ((instance (magnus-instances-get magnus-attention-current)))
      (magnus-attention-release instance))))

(defun magnus-attention--focus-next ()
  "Focus the next instance in the attention queue."
  (when-let ((next-id (car magnus-attention-queue)))
    (when-let ((instance (magnus-instances-get next-id)))
      (setq magnus-attention-current next-id)
      (magnus-attention--notify instance)
      (magnus-attention--focus instance))))

(defun magnus-attention--focus (instance)
  "Focus INSTANCE's buffer."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (let ((window (get-buffer-window buffer)))
        (if window
            (select-window window)
          (switch-to-buffer buffer))))))

;;; Detection

(defun magnus-attention-check-all ()
  "Check all instances for attention needs."
  (dolist (instance (magnus-instances-list))
    (when (magnus-attention--needs-input-p instance)
      (magnus-attention-request instance))))

(defun magnus-attention--needs-input-p (instance)
  "Check if INSTANCE appears to need user input."
  (let ((buffer (magnus-instance-buffer instance)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (magnus-attention--buffer-has-prompt-p)))))

(defun magnus-attention--buffer-has-prompt-p ()
  "Check if current buffer has a prompt waiting for input."
  (save-excursion
    (goto-char (point-max))
    (let ((search-start (save-excursion
                          (forward-line (- magnus-attention-scan-lines))
                          (point)))
          (found nil))
      (while (and (not found)
                  (re-search-backward
                   (regexp-opt magnus-attention-patterns)
                   search-start t))
        (setq found t))
      found)))

(defun magnus-attention--is-prompt-line-p (line)
  "Check if LINE contains a prompt pattern."
  (let ((case-fold-search t))
    (cl-some (lambda (pattern)
               (string-match-p pattern line))
             magnus-attention-patterns)))

;;; Coordination integration

(defun magnus-attention--update-coord (instance status)
  "Update INSTANCE's status in coordination file.
STATUS is either `awaiting-input' or `ready'."
  (let* ((directory (magnus-instance-directory instance))
         (name (magnus-instance-name instance))
         (status-str (if (eq status 'awaiting-input)
                         "awaiting-input"
                       "ready")))
    (magnus-coord-update-active directory name "attention" status-str "")))

(defun magnus-attention--log (instance message)
  "Log MESSAGE for INSTANCE in coordination file."
  (let ((directory (magnus-instance-directory instance))
        (name (magnus-instance-name instance)))
    (magnus-coord-add-log directory name
                          (format "[ATTENTION] %s" message))))

;;; Notifications

(defun magnus-attention--notify (instance)
  "Notify user that INSTANCE needs attention."
  (let ((name (magnus-instance-name instance))
        (now (float-time)))
    ;; Avoid notification spam (at least 5 seconds between notifications)
    (when (or (null magnus-attention--last-notify)
              (> (- now magnus-attention--last-notify) 5))
      (setq magnus-attention--last-notify now)
      (message "Magnus: %s needs your attention" name)
      ;; Ring bell for urgent notification
      (ding t))))

;;; Queue display

(defun magnus-attention-queue-string ()
  "Return a string describing the current attention queue."
  (if magnus-attention-queue
      (let ((names (mapcar (lambda (id)
                            (if-let ((instance (magnus-instances-get id)))
                                (magnus-instance-name instance)
                              id))
                          magnus-attention-queue)))
        (format "Attention queue: %s" (string-join names " <- ")))
    "No instances waiting for attention"))

(defun magnus-attention-pending-count ()
  "Return the number of instances waiting for attention."
  (length magnus-attention-queue))

;;; Timer management

(defun magnus-attention-start ()
  "Start the attention monitoring timer."
  (interactive)
  (magnus-attention-stop)
  (setq magnus-attention--timer
        (run-with-timer magnus-attention-check-interval
                        magnus-attention-check-interval
                        #'magnus-attention-check-all))
  (message "Magnus attention monitoring started"))

(defun magnus-attention-stop ()
  "Stop the attention monitoring timer."
  (interactive)
  (when magnus-attention--timer
    (cancel-timer magnus-attention--timer)
    (setq magnus-attention--timer nil))
  (message "Magnus attention monitoring stopped"))

(defun magnus-attention-toggle ()
  "Toggle attention monitoring."
  (interactive)
  (if magnus-attention--timer
      (magnus-attention-stop)
      (magnus-attention-start)))

;;; Interactive commands

(defun magnus-attention-next ()
  "Move to the next instance in the attention queue."
  (interactive)
  (if magnus-attention-queue
      (progn
        (magnus-attention-release-current)
        (if magnus-attention-queue
            (message "Moving to next: %s"
                     (magnus-instance-name
                      (magnus-instances-get (car magnus-attention-queue))))
          (message "Attention queue empty")))
    (message "No instances in attention queue")))

(defun magnus-attention-show-queue ()
  "Display the current attention queue."
  (interactive)
  (message (magnus-attention-queue-string)))

;;; Hooks

(defun magnus-attention--on-instance-kill (instance)
  "Handle INSTANCE being killed."
  (magnus-attention-release instance))

;; Start monitoring by default when loaded
(add-hook 'magnus-instances-changed-hook
          (lambda ()
            (when (null (magnus-instances-list))
              (magnus-attention-stop))))

(provide 'magnus-attention)
;;; magnus-attention.el ends here
