;;; magnus-attention.el --- Attention queue for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module manages attention requests from Claude Code instances.
;; When an instance needs user input (permissions, confirmations, etc.),
;; it joins an attention queue.  Magnus focuses instances one at a time,
;; preventing multiple instances from competing for attention.
;;
;; Detection: Pattern matching in vterm buffers for permission prompts.
;; The attention queue ensures orderly handling of requests.

;;; Code:

(require 'magnus-instances)

(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function magnus-process--headless-p "magnus-process")

;;; Customization

(defcustom magnus-attention-check-interval 10
  "Seconds between checks for instances needing attention."
  :type 'number
  :group 'magnus)

(defcustom magnus-attention-patterns
  '(;; Classic y/n format
    "\\[y/n\\]"
    "\\[Y/n\\]"
    "\\[yes/no\\]"
    "(y/n)"
    "(Y/n)"
    ;; CC Ink-based prompt format
    "Esc to cancel"
    "Do you want to proceed"
    ;; General patterns
    "Press Enter to continue"
    "Allow\\?"
    "Proceed\\?")
  "Patterns that indicate an instance is waiting for input.
These are matched against the last few lines of the vterm buffer."
  :type '(repeat regexp)
  :group 'magnus)

(defcustom magnus-attention-scan-lines 10
  "Number of lines from end of buffer to scan for attention patterns."
  :type 'integer
  :group 'magnus)

(defcustom magnus-attention-auto-approve-patterns
  '("Read"
    "Glob"
    "Grep"
    "Edit"
    "Write"
    "NotebookEdit"
    "Bash(git "
    "Bash(cd "
    "Bash(ls "
    "Bash(mkdir "
    "Bash(cat "
    "Bash(head "
    "Bash(tail "
    "Bash(wc "
    "Bash(find "
    "Bash(grep "
    "Bash(rg "
    "Bash(npm test"
    "Bash(npm run"
    "Bash(npx "
    "Bash(cargo test"
    "Bash(cargo build"
    "Bash(cargo check"
    "Bash(make"
    "Bash(python "
    "Bash(pytest"
    "Bash(go test"
    "Bash(go build"
    "Bash(emacs --batch")
  "Patterns for permission prompts that should be auto-approved.
When the last line of a vterm buffer matches one of these (as a
substring of the prompt text), magnus sends `y' automatically
instead of adding the instance to the attention queue.
Set to nil to disable auto-approval."
  :type '(repeat string)
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

(defvar magnus-attention--checking nil
  "Non-nil if currently checking instances (prevents re-entry).")

(defvar magnus-attention-return-buffer nil
  "Buffer to return to when the attention queue empties.
Set by the chat command center so the user is switched back
after handling all permission prompts.")

(defun magnus-attention-set-return-buffer (buffer)
  "Set BUFFER as the destination when the attention queue empties."
  (setq magnus-attention-return-buffer buffer))

;;; Queue management

(defun magnus-attention-request (instance)
  "Add INSTANCE to the attention queue if not already present.
When the instance is the first in queue, focus its buffer immediately."
  (let ((id (magnus-instance-id instance)))
    (unless (member id magnus-attention-queue)
      (setq magnus-attention-queue (append magnus-attention-queue (list id)))
      (when (= (length magnus-attention-queue) 1)
        ;; First in queue — take the floor and focus
        (setq magnus-attention-current id)
        (when magnus-attention-notify
          (magnus-attention--notify instance))
        (magnus-attention--focus instance)))))

(defun magnus-attention-release (instance)
  "Release INSTANCE from the attention queue."
  (let ((id (magnus-instance-id instance)))
    (setq magnus-attention-queue (delete id magnus-attention-queue))
    (when (string= magnus-attention-current id)
      (setq magnus-attention-current nil)
      ;; Focus next in queue if any
      (magnus-attention--focus-next))))

(defun magnus-attention-release-current ()
  "Release the current instance from attention (call when done with it)."
  (interactive)
  (when magnus-attention-current
    (when-let ((instance (magnus-instances-get magnus-attention-current)))
      (magnus-attention-release instance))))

(defun magnus-attention--focus-next ()
  "Focus the next instance in the attention queue.
When the queue is empty and a return buffer is set, switch back to it."
  (if-let ((next-id (car magnus-attention-queue)))
      (when-let ((instance (magnus-instances-get next-id)))
        (setq magnus-attention-current next-id)
        (magnus-attention--notify instance)
        (magnus-attention--focus instance))
    ;; Queue empty — return to the chat buffer if set
    (when (and magnus-attention-return-buffer
               (buffer-live-p magnus-attention-return-buffer))
      (let ((buf magnus-attention-return-buffer))
        (setq magnus-attention-return-buffer nil)
        (switch-to-buffer buf)))))

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
  "Check all instances for attention needs.
Tries auto-approval first; falls back to the attention queue.
Also releases queued instances whose prompts have been handled."
  ;; Prevent re-entry and protect against errors
  (unless magnus-attention--checking
    (setq magnus-attention--checking t)
    (unwind-protect
        (condition-case err
            (progn
              ;; Add new attention requests
              (dolist (instance (magnus-instances-list))
                (when (and (magnus-attention--needs-input-p instance)
                           (not (magnus-process--headless-p instance)))
                  (unless (magnus-attention--try-auto-approve instance)
                    (magnus-attention-request instance))))
              ;; Release queued instances that no longer need input
              (magnus-attention--release-handled))
          (error
           (message "Magnus: attention check error: %s"
                    (error-message-string err))))
      (setq magnus-attention--checking nil))))

(defun magnus-attention--release-handled ()
  "Release queued instances whose prompts have been handled."
  (let ((to-release nil))
    (dolist (id magnus-attention-queue)
      (when-let ((instance (magnus-instances-get id)))
        (unless (magnus-attention--needs-input-p instance)
          (push instance to-release))))
    (dolist (instance to-release)
      (magnus-attention-release instance))))

(defun magnus-attention--needs-input-p (instance)
  "Check if INSTANCE appears to need user input."
  (condition-case err
      (let ((buffer (magnus-instance-buffer instance)))
        (when (and buffer (buffer-live-p buffer))
          (with-current-buffer buffer
            (magnus-attention--buffer-has-prompt-p))))
    (error
     (message "Magnus: needs-input-p error for %s: %s"
              (magnus-instance-name instance)
              (error-message-string err))
     nil)))

(defun magnus-attention--buffer-has-prompt-p ()
  "Check if current buffer has a prompt in the last few lines."
  (let ((tail (magnus-attention--tail-text)))
    (when tail
      (cl-some (lambda (pattern)
                 (string-match-p pattern tail))
               magnus-attention-patterns))))

(defun magnus-attention--tail-text ()
  "Return the last `magnus-attention-scan-lines' lines as a single string."
  (let* ((end (point-max))
         (start (max (point-min) (- end 2000)))
         (text (buffer-substring-no-properties start end))
         (lines (split-string text "\n" t "[ \t]+"))
         (recent (last lines magnus-attention-scan-lines)))
    (when recent
      (mapconcat #'identity recent "\n"))))


;;; Auto-approval

(defun magnus-attention--prompt-anchors ()
  "Return the current prompt anchor patterns.
Derived from `magnus-attention-patterns' so customizations are
always reflected in auto-approval logic."
  magnus-attention-patterns)

(defun magnus-attention--try-auto-approve (instance)
  "Try to auto-approve INSTANCE's permission prompt.
Only fires when the tail text looks like a genuine permission prompt
AND contains an allowlisted tool/command pattern.
Sends `y' which maps to confirm:yes in all CC prompt formats."
  (when magnus-attention-auto-approve-patterns
    (when-let ((buffer (magnus-instance-buffer instance)))
      (when (buffer-live-p buffer)
        (let ((tail (with-current-buffer buffer
                      (magnus-attention--tail-text))))
          (when (and tail
                     (magnus-attention--is-prompt-line-p tail)
                     (magnus-attention--matches-auto-approve-p tail))
            (with-current-buffer buffer
              (vterm-send-string "y")
              (vterm-send-return))
            (message "Magnus: auto-approved for %s" (magnus-instance-name instance))
            t))))))

(defun magnus-attention--is-prompt-line-p (line)
  "Return non-nil if LINE looks like an actual yes/no permission prompt."
  (cl-some (lambda (pattern)
             (string-match-p pattern line))
           (magnus-attention--prompt-anchors)))

(defun magnus-attention--matches-auto-approve-p (prompt-text)
  "Return non-nil if PROMPT-TEXT matches an auto-approve pattern."
  (cl-some (lambda (pattern)
             (string-match-p (regexp-quote pattern) prompt-text))
           magnus-attention-auto-approve-patterns))

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
