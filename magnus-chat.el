;;; magnus-chat.el --- iMessage-style command center for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A chat-style command center for messaging Claude Code agents.
;; Messages typed here get sent directly to an agent's vterm buffer
;; via `vterm-send-string'.  The buffer displays a conversation log
;; with iMessage-inspired styling — outgoing messages in colored
;; bubbles, system events in dim centered text.
;;
;; Usage: M-x magnus-chat  (or press `M' in the magnus status buffer)

;;; Code:

(require 'magnus-instances)

(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function magnus-coord-send-message "magnus-coord")

;;; Faces

(defface magnus-chat-outgoing
  '((((background dark))
     :background "#1a3a5c" :extend t)
    (t
     :background "#d4e8ff" :extend t))
  "Face for outgoing (user) message bubbles."
  :group 'magnus)

(defface magnus-chat-outgoing-header
  '((((background dark))
     :foreground "#7eb8ff" :weight bold)
    (t
     :foreground "#0A64CC" :weight bold))
  "Face for outgoing message headers."
  :group 'magnus)

(defface magnus-chat-event
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for system event messages."
  :group 'magnus)

(defface magnus-chat-agent-label
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for agent name labels."
  :group 'magnus)

(defface magnus-chat-prompt
  '((t :inherit minibuffer-prompt))
  "Face for the input prompt."
  :group 'magnus)

(defface magnus-chat-timestamp
  '((t :inherit font-lock-comment-face))
  "Face for timestamps."
  :group 'magnus)

;;; Customization

(defcustom magnus-chat-buffer-name "*magnus-chat*"
  "Name of the chat command center buffer."
  :type 'string
  :group 'magnus)

;;; Buffer-local state

(defvar-local magnus-chat--target nil
  "Currently selected agent instance, or nil.")

(defvar-local magnus-chat--input-marker nil
  "Marker at the start of the input area.")

;;; Major mode

(defvar magnus-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'magnus-chat-send)
    (define-key map (kbd "C-c C-a") #'magnus-chat-select-agent)
    (define-key map (kbd "TAB") #'magnus-chat-cycle-agent)
    (define-key map (kbd "C-c C-k") #'magnus-chat-clear)
    (define-key map (kbd "C-c C-q") #'quit-window)
    map)
  "Keymap for `magnus-chat-mode'.")

(define-derived-mode magnus-chat-mode nil "Chat"
  "Major mode for the magnus chat command center.
Send messages to Claude Code agents from one place.

\\{magnus-chat-mode-map}"
  :group 'magnus
  (setq-local magnus-chat--input-marker (make-marker))
  (setq-local magnus-chat--target nil)
  (setq truncate-lines nil)
  (setq word-wrap t))

;;; Buffer setup

(defun magnus-chat--setup-buffer (buffer)
  "Set up BUFFER as the chat command center."
  (with-current-buffer buffer
    (magnus-chat-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Welcome header
      (insert (propertize " Magnus Chat " 'face '(:weight bold :height 1.2))
              "\n")
      (insert (propertize " Send messages to any agent. TAB to switch, RET to send.\n"
                          'face 'magnus-chat-event))
      (insert (propertize (make-string 50 ?━) 'face 'font-lock-comment-face))
      (insert "\n\n")
      ;; Make everything above read-only
      (add-text-properties (point-min) (point)
                           '(read-only t rear-nonsticky t)))
    ;; Insert the prompt (also read-only)
    (magnus-chat--insert-prompt)
    ;; Auto-select first running agent
    (unless magnus-chat--target
      (magnus-chat--select-first-agent))
    (magnus-chat--update-header-line)))

(defun magnus-chat--insert-prompt ()
  "Insert the input prompt at the end of the buffer."
  (goto-char (point-max))
  (let ((inhibit-read-only t)
        (agent-name (if magnus-chat--target
                        (magnus-instance-name magnus-chat--target)
                      "no agent")))
    (insert (propertize (format "\n[%s] ▸ " agent-name)
                        'face 'magnus-chat-prompt
                        'read-only t
                        'rear-nonsticky t
                        'magnus-chat-prompt t)))
  ;; Mark where user input starts
  (set-marker magnus-chat--input-marker (point)))

(defun magnus-chat--redraw-prompt ()
  "Redraw the prompt with the current agent name."
  ;; Find and delete the old prompt
  (save-excursion
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      ;; Delete from the prompt to end (including any user input)
      (when-let ((prompt-start
                  (previous-single-property-change
                   (point-max) 'magnus-chat-prompt)))
        (let ((input-text (buffer-substring-no-properties
                           magnus-chat--input-marker (point-max))))
          (delete-region prompt-start (point-max))
          ;; Re-insert prompt
          (magnus-chat--insert-prompt)
          ;; Restore user input
          (goto-char (point-max))
          (insert input-text)))))
  (magnus-chat--update-header-line))

;;; Header line

(defun magnus-chat--update-header-line ()
  "Update the header line to show the current target agent."
  (let* ((agents (magnus-chat--running-agents))
         (target-name (if magnus-chat--target
                         (propertize (magnus-instance-name magnus-chat--target)
                                     'face 'magnus-chat-agent-label)
                       (propertize "none" 'face 'font-lock-warning-face)))
         (count (length agents)))
    (setq header-line-format
          (list " "
                (propertize "▸ " 'face 'success)
                target-name
                (propertize (format "  (%d agent%s)"
                                    count (if (= count 1) "" "s"))
                            'face 'font-lock-comment-face)
                "  "
                (propertize "TAB:switch  RET:send  C-c C-a:pick"
                            'face 'font-lock-comment-face)))))

;;; Agent selection

(defun magnus-chat--running-agents ()
  "Return list of instances that have live vterm buffers."
  (cl-remove-if-not
   (lambda (inst)
     (and (eq (magnus-instance-type inst) 'vterm)
          (magnus-instance-buffer inst)
          (buffer-live-p (magnus-instance-buffer inst))))
   (magnus-instances-list)))

(defun magnus-chat--select-first-agent ()
  "Select the first running agent as target."
  (when-let ((agents (magnus-chat--running-agents)))
    (setq magnus-chat--target (car agents))))

(defun magnus-chat-select-agent ()
  "Select which agent to message via completing-read."
  (interactive)
  (let* ((agents (magnus-chat--running-agents))
         (names (mapcar #'magnus-instance-name agents))
         (choice (completing-read "Send to: " names nil t)))
    (when-let ((inst (magnus-instances-get-by-name choice)))
      (setq magnus-chat--target inst)
      (magnus-chat--redraw-prompt)
      (message "Now talking to %s" choice))))

(defun magnus-chat-cycle-agent ()
  "Cycle to the next agent."
  (interactive)
  (let* ((agents (magnus-chat--running-agents))
         (count (length agents)))
    (cond
     ((zerop count)
      (message "No agents running"))
     ((= count 1)
      (setq magnus-chat--target (car agents))
      (message "Only one agent: %s"
               (magnus-instance-name (car agents))))
     (t
      (let* ((current-idx (cl-position magnus-chat--target agents))
             (next-idx (if current-idx
                          (mod (1+ current-idx) count)
                        0)))
        (setq magnus-chat--target (nth next-idx agents))
        (message "Switched to %s"
                 (magnus-instance-name magnus-chat--target)))))
    (magnus-chat--redraw-prompt)))

;;; Sending messages

(defun magnus-chat-send ()
  "Send the current input to the selected agent."
  (interactive)
  (unless magnus-chat--target
    (magnus-chat-select-agent))
  (unless magnus-chat--target
    (user-error "No agent selected"))
  (let ((input (string-trim
                (buffer-substring-no-properties
                 magnus-chat--input-marker (point-max)))))
    (when (string-empty-p input)
      (user-error "Nothing to send"))
    ;; Clear input area
    (let ((inhibit-read-only t))
      (delete-region magnus-chat--input-marker (point-max)))
    ;; Render outgoing message in chat log
    (magnus-chat--render-outgoing input magnus-chat--target)
    ;; Send to vterm
    (magnus-chat--send-to-vterm magnus-chat--target input)
    ;; Also write to coord file so other agents can see it
    (condition-case nil
        (magnus-coord-send-message magnus-chat--target input)
      (error nil))
    ;; Redraw prompt
    (magnus-chat--redraw-prompt)
    ;; Scroll to bottom
    (goto-char (point-max))))

(defun magnus-chat--send-to-vterm (instance message)
  "Send MESSAGE to INSTANCE's vterm buffer."
  (let ((buffer (magnus-instance-buffer instance)))
    (if (and buffer (buffer-live-p buffer))
        (with-current-buffer buffer
          (vterm-send-string message)
          (vterm-send-return))
      (user-error "Agent %s has no live buffer"
                  (magnus-instance-name instance)))))

;;; Message rendering

(defun magnus-chat--render-outgoing (text target)
  "Render outgoing TEXT sent to TARGET in the chat log."
  (save-excursion
    ;; Insert before the prompt
    (goto-char (point-max))
    (let ((inhibit-read-only t)
          (prompt-start (previous-single-property-change
                         (point-max) 'magnus-chat-prompt)))
      (when prompt-start
        (goto-char prompt-start))
      ;; Timestamp + target
      (insert (propertize (format "  %s  " (format-time-string "%H:%M"))
                          'face 'magnus-chat-timestamp
                          'read-only t 'rear-nonsticky t))
      (insert (propertize (format "you → %s"
                                  (magnus-instance-name target))
                          'face 'magnus-chat-outgoing-header
                          'read-only t 'rear-nonsticky t))
      (insert (propertize "\n" 'read-only t 'rear-nonsticky t))
      ;; Message bubble
      (let* ((lines (split-string text "\n"))
             (bubble-text
              (mapconcat (lambda (line)
                           (format "  %s  " line))
                         lines "\n")))
        (insert (propertize (concat bubble-text "\n\n")
                            'face 'magnus-chat-outgoing
                            'read-only t 'rear-nonsticky t))))))

(defun magnus-chat--render-event (text)
  "Render a system event TEXT in the chat log."
  (when-let ((buf (get-buffer magnus-chat-buffer-name)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t)
              (prompt-start (previous-single-property-change
                             (point-max) 'magnus-chat-prompt)))
          (when prompt-start
            (goto-char prompt-start))
          (insert (propertize (format "  ── %s ──\n\n" text)
                              'face 'magnus-chat-event
                              'read-only t 'rear-nonsticky t)))))))

;;; Clear

(defun magnus-chat-clear ()
  "Clear the chat history."
  (interactive)
  (when (y-or-n-p "Clear chat history? ")
    (magnus-chat--setup-buffer (current-buffer))))

;;; Entry point

;;;###autoload
(defun magnus-chat ()
  "Open the magnus chat command center."
  (interactive)
  (let ((buf (get-buffer-create magnus-chat-buffer-name)))
    (unless (eq (buffer-local-value 'major-mode buf) 'magnus-chat-mode)
      (magnus-chat--setup-buffer buf))
    ;; Refresh agent list and header
    (with-current-buffer buf
      (magnus-chat--update-header-line))
    (pop-to-buffer buf)
    (goto-char (point-max))))

(provide 'magnus-chat)
;;; magnus-chat.el ends here
