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

(defvar magnus-attention-prompt-detected-hook nil
  "Hook called with (INSTANCE PROMPT-TEXT AUTO-APPROVED-P).
Fired when a permission prompt is detected in an instance.
AUTO-APPROVED-P is t if the prompt was auto-approved, nil otherwise.")

;;; Queue management

(defun magnus-attention-request (instance)
  "Add INSTANCE to the attention queue if not already present."
  (let ((id (magnus-instance-id instance)))
    (unless (member id magnus-attention-queue)
      (setq magnus-attention-queue (append magnus-attention-queue (list id)))
      (when (and magnus-attention-notify
                 (= (length magnus-attention-queue) 1))
        ;; First in queue, notify immediately
        (magnus-attention--notify instance)))))

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
  "Check all instances for attention needs.
Tries auto-approval first; falls back to the attention queue."
  ;; Prevent re-entry and protect against errors
  (unless magnus-attention--checking
    (setq magnus-attention--checking t)
    (unwind-protect
        (condition-case nil
            (dolist (instance (magnus-instances-list))
              (when (and (magnus-attention--needs-input-p instance)
                         (not (magnus-process--headless-p instance)))
                (let ((prompt-text (with-current-buffer (magnus-instance-buffer instance)
                                     (magnus-attention--last-line))))
                  (if (magnus-attention--try-auto-approve instance)
                      (run-hook-with-args 'magnus-attention-prompt-detected-hook
                                          instance prompt-text t)
                    (magnus-attention-request instance)
                    (run-hook-with-args 'magnus-attention-prompt-detected-hook
                                        instance prompt-text nil)))))
          (error nil))  ; Silently ignore errors
      (setq magnus-attention--checking nil))))

(defun magnus-attention--needs-input-p (instance)
  "Check if INSTANCE appears to need user input."
  (condition-case nil
      (let ((buffer (magnus-instance-buffer instance)))
        (when (and buffer (buffer-live-p buffer))
          (with-current-buffer buffer
            (magnus-attention--buffer-has-prompt-p))))
    (error nil)))

(defun magnus-attention--buffer-has-prompt-p ()
  "Check if current buffer has a yes/no prompt on the last line."
  (let ((last-line (magnus-attention--last-line)))
    (when last-line
      (cl-some (lambda (pattern)
                 (string-match-p pattern last-line))
               magnus-attention-patterns))))

(defun magnus-attention--last-line ()
  "Return the last non-empty line in the current buffer, or nil."
  (let* ((end (point-max))
         (start (max (point-min) (- end 500)))
         (text (buffer-substring-no-properties start end))
         (lines (split-string text "\n" t "[ \t]+")))
    (car (last lines))))


;;; Auto-approval

(defvar magnus-attention--prompt-anchors
  '("\\[y/n\\]" "\\[Y/n\\]" "\\[yes/no\\]" "(y/n)" "(Y/n)" "Allow\\?" "Proceed\\?")
  "Patterns confirming a real yes/no prompt on the last line.
Auto-approve only fires when the last line matches one of these
AND an allowlist entry.")

(defun magnus-attention--try-auto-approve (instance)
  "Try to auto-approve INSTANCE's permission prompt.
Only fires when the last line looks like a genuine yes/no prompt
AND contains an allowlisted tool/command pattern."
  (when magnus-attention-auto-approve-patterns
    (when-let ((buffer (magnus-instance-buffer instance)))
      (when (buffer-live-p buffer)
        (let ((last-line (with-current-buffer buffer
                           (magnus-attention--last-line))))
          (when (and last-line
                     (magnus-attention--is-prompt-line-p last-line)
                     (magnus-attention--matches-auto-approve-p last-line))
            (with-current-buffer buffer
              (vterm-send-string "y")
              (vterm-send-return))
            (message "Magnus: auto-approved for %s" (magnus-instance-name instance))
            t))))))

(defun magnus-attention--is-prompt-line-p (line)
  "Return non-nil if LINE looks like an actual yes/no permission prompt."
  (cl-some (lambda (pattern)
             (string-match-p pattern line))
           magnus-attention--prompt-anchors))

(defun magnus-attention--matches-auto-approve-p (prompt-text)
  "Return non-nil if PROMPT-TEXT matches an auto-approve pattern."
  (cl-some (lambda (pattern)
             (string-match-p (regexp-quote pattern) prompt-text))
           magnus-attention-auto-approve-patterns))

;;; Notifications

(defun magnus-attention--notify (instance)
  "Notify user that INSTANCE needs attention.
Suppressed when the command buffer is visible (prompts show there)."
  (let ((name (magnus-instance-name instance))
        (now (float-time)))
    ;; Skip if command buffer is visible — it shows prompts inline
    (unless (get-buffer-window "*magnus-command*")
      ;; Avoid notification spam (at least 5 seconds between notifications)
      (when (or (null magnus-attention--last-notify)
                (> (- now magnus-attention--last-notify) 5))
        (setq magnus-attention--last-notify now)
        (message "Magnus: %s needs your attention" name)
        ;; Ring bell for urgent notification
        (ding t)))))

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
