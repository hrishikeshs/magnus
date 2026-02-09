;;; magnus-trace.el --- Thinking trace viewer for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides a JSONL session viewer for Claude Code thinking
;; traces.  It reads the session JSONL files that Claude Code writes and
;; renders user messages, thinking blocks, and assistant responses in a
;; scrollable Emacs buffer with auto-refresh.

;;; Code:

(require 'magnus-instances)

(declare-function magnus-process--list-sessions "magnus-process")
(declare-function magnus-process--most-recent-session "magnus-process")
(declare-function magnus-process--session-jsonl-path "magnus-process")

;;; Faces

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

;;; Variables

(defvar-local magnus-trace--instance nil
  "The instance this trace buffer is following.")

(defvar-local magnus-trace--last-line-count 0
  "Number of JSONL lines already processed.")

(defvar magnus-trace--timer nil
  "Timer for auto-refreshing trace buffers.")

;;; Major mode

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

;;; Core functions

(defun magnus-trace-open (instance)
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
    (magnus-trace--ensure-timer)
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

;;; Internal helpers

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

;;; Timer management

(defun magnus-trace--ensure-timer ()
  "Ensure the trace auto-refresh timer is running."
  (unless magnus-trace--timer
    (setq magnus-trace--timer
          (run-with-timer 10 10 #'magnus-trace--sync-all))))

(defun magnus-trace--sync-all ()
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

(provide 'magnus-trace)
;;; magnus-trace.el ends here
