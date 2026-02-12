;;; magnus-command.el --- Command buffer for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides a unified command buffer — a group-chat-style
;; interface for interacting with all Claude Code instances.  Permission
;; prompts, auto-approvals, and messages appear in a single chronological
;; event stream with an inline input area at the bottom.
;;
;; One agent "has the floor" at a time; the user responds to the active
;; prompt and the next queued prompt takes over.  Free-form messages
;; can be sent via @agent-name or C-c C-t to pick a target.

;;; Code:

(require 'magnus-instances)
(require 'magnus-coord)

(declare-function magnus-attention--last-line "magnus-attention")
(declare-function magnus-attention-release "magnus-attention")
(declare-function magnus-process-running-p "magnus-process")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

;;; Customization

(defcustom magnus-command-show-auto-approved t
  "Show auto-approved prompts in the command buffer."
  :type 'boolean
  :group 'magnus)

;;; Faces

(defface magnus-command-prompt-active
  '((t :inherit (warning bold)))
  "Face for the currently active prompt."
  :group 'magnus)

(defface magnus-command-prompt-queued
  '((t :inherit warning))
  "Face for queued prompts waiting for the floor."
  :group 'magnus)

(defface magnus-command-prompt-handled
  '((t :inherit shadow))
  "Face for prompts after the user responded."
  :group 'magnus)

(defface magnus-command-auto-approved
  '((t :inherit (font-lock-comment-face italic)))
  "Face for auto-approved prompts."
  :group 'magnus)

(defface magnus-command-agent-name
  '((t :inherit (font-lock-function-name-face bold)))
  "Face for agent names."
  :group 'magnus)

(defface magnus-command-timestamp
  '((t :inherit font-lock-comment-face))
  "Face for HH:MM timestamps."
  :group 'magnus)

(defface magnus-command-user
  '((t :inherit (font-lock-keyword-face bold)))
  "Face for `you ->' prefix on user messages."
  :group 'magnus)

(defface magnus-command-system
  '((t :inherit (font-lock-comment-face italic)))
  "Face for join/leave/status events."
  :group 'magnus)

(defface magnus-command-separator
  '((t :inherit font-lock-comment-face))
  "Face for the separator line above input."
  :group 'magnus)

(defface magnus-command-input-prompt
  '((t :inherit minibuffer-prompt))
  "Face for the [agent] > prompt in the input area."
  :group 'magnus)

;;; Buffer-local variables

(defvar-local magnus-command--events nil
  "List of event plists, newest last.")

(defvar-local magnus-command--input-marker nil
  "Marker at start of the editable input area.")

(defvar-local magnus-command--active-prompt nil
  "Event plist of the current floor-holder prompt, or nil.")

(defvar-local magnus-command--prompt-queue nil
  "List of prompt events waiting for the floor.")

(defvar-local magnus-command--target nil
  "Instance-id for free-form messages when no active prompt.")

(defvar-local magnus-command--prev-instances nil
  "Snapshot of instance IDs for detecting joins/leaves.")

;;; Global variables

(defvar magnus-command--seen-prompts (make-hash-table :test 'equal)
  "Hash table for dedup: (instance-id . md5) -> t.")

(defvar magnus-command--reconcile-timer nil
  "30s timer for stale prompt cleanup.")

;;; Buffer name

(defconst magnus-command-buffer-name "*magnus-command*"
  "Name of the command buffer.")

;;; Keymap

(defvar magnus-command-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'magnus-command-send)
    (define-key map (kbd "C-c C-t") #'magnus-command-set-target)
    (define-key map (kbd "C-c C-a") #'magnus-command-broadcast)
    (define-key map (kbd "TAB") #'magnus-command-next-prompt)
    (define-key map (kbd "C-c C-v") #'magnus-command-visit)
    (define-key map (kbd "C-c C-g") #'magnus-command-refresh)
    (define-key map (kbd "C-c C-e") #'magnus-command-tail)
    (define-key map (kbd "q") #'magnus-command--maybe-quit)
    map)
  "Keymap for `magnus-command-mode'.")

;;; Mode definition

(define-derived-mode magnus-command-mode fundamental-mode "Magnus-Cmd"
  "Major mode for the magnus command buffer.

A group-chat style interface for interacting with Claude Code instances.
Permission prompts appear in a chronological stream; type responses
in the input area at the bottom.

\\{magnus-command-mode-map}"
  :group 'magnus
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (add-hook 'post-command-hook #'magnus-command--keep-cursor-in-input nil t))

;;; Entry point

;;;###autoload
(defun magnus-command ()
  "Open or switch to the magnus command buffer."
  (interactive)
  (let ((buffer (get-buffer-create magnus-command-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'magnus-command-mode)
        (magnus-command-mode)
        (magnus-command--setup-buffer)
        (magnus-command--register-hooks)
        (magnus-command--start-reconcile-timer)
        (magnus-command--snapshot-instances)))
    (pop-to-buffer buffer)))

;;; Buffer setup

(defun magnus-command--setup-buffer ()
  "Initialize the command buffer layout."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Header
    (insert (propertize "Magnus Command Buffer" 'face 'magnus-command-agent-name))
    (insert "\n\n")
    ;; Separator
    (magnus-command--insert-separator)
    ;; Input area starts here
    (setq magnus-command--input-marker (point-marker))
    (set-marker-insertion-type magnus-command--input-marker nil)
    (magnus-command--insert-input-prompt)
    ;; Make everything before marker read-only
    (magnus-command--apply-read-only)))

(defun magnus-command--insert-separator ()
  "Insert the separator line between log and input."
  (insert (propertize (make-string 56 ?─)
                      'face 'magnus-command-separator
                      'magnus-command-separator t)
          "\n"))

(defun magnus-command--insert-input-prompt ()
  "Insert the input prompt text based on active prompt or target."
  (let ((prompt-text
         (cond
          (magnus-command--active-prompt
           (let ((name (plist-get magnus-command--active-prompt :instance-name)))
             (format "[%s] > " name)))
          (magnus-command--target
           (if-let ((inst (magnus-instances-get magnus-command--target)))
               (format "[%s] > " (magnus-instance-name inst))
             "> "))
          (t "> "))))
    (insert (propertize prompt-text
                        'face 'magnus-command-input-prompt
                        'magnus-command-input-prompt t
                        'rear-nonsticky t))))

(defun magnus-command--apply-read-only ()
  "Make everything before the input marker read-only."
  (when (and magnus-command--input-marker
             (marker-position magnus-command--input-marker))
    (let ((inhibit-read-only t))
      ;; Remove old read-only
      (remove-text-properties (point-min) (point-max) '(read-only nil))
      ;; Set read-only on log region
      (when (> (marker-position magnus-command--input-marker) (point-min))
        (add-text-properties (point-min) (marker-position magnus-command--input-marker)
                             '(read-only t front-sticky (read-only)))))))

(defun magnus-command--keep-cursor-in-input ()
  "Bounce cursor to input area if it enters the read-only region."
  (when (and magnus-command--input-marker
             (marker-position magnus-command--input-marker)
             (< (point) (marker-position magnus-command--input-marker))
             ;; Only bounce for self-insert-command and similar
             (memq this-command '(self-insert-command
                                  newline
                                  yank
                                  yank-pop)))
    (goto-char (point-max))))

;;; Header-line

(defun magnus-command--header-line ()
  "Return the header-line string with pending count."
  (let ((pending (length magnus-command--prompt-queue))
        (active (if magnus-command--active-prompt 1 0)))
    (format " Magnus Command Buffer%s"
            (let ((total (+ pending active)))
              (if (> total 0)
                  (format "  [%d pending]" total)
                "")))))

;;; Hook registration

(defun magnus-command--register-hooks ()
  "Register hooks for event ingestion."
  (add-hook 'magnus-attention-prompt-detected-hook
            #'magnus-command--on-prompt-detected)
  (add-hook 'magnus-instances-changed-hook
            #'magnus-command--on-instances-changed)
  (setq header-line-format '(:eval (magnus-command--header-line))))

(defun magnus-command--unregister-hooks ()
  "Remove hooks."
  (remove-hook 'magnus-attention-prompt-detected-hook
               #'magnus-command--on-prompt-detected)
  (remove-hook 'magnus-instances-changed-hook
               #'magnus-command--on-instances-changed))

;;; Instance snapshot (for detecting joins/leaves)

(defun magnus-command--snapshot-instances ()
  "Take a snapshot of current instance IDs."
  (when-let ((buf (get-buffer magnus-command-buffer-name)))
    (with-current-buffer buf
      (setq magnus-command--prev-instances
            (mapcar #'magnus-instance-id (magnus-instances-list))))))

;;; Event ingestion

(defun magnus-command--on-prompt-detected (instance prompt-text auto-approved-p)
  "Handle a prompt detected from INSTANCE.
PROMPT-TEXT is the prompt line, AUTO-APPROVED-P is t if auto-approved."
  (when-let ((buf (get-buffer magnus-command-buffer-name)))
    (let* ((id (magnus-instance-id instance))
           (dedup-key (cons id (md5 (or prompt-text ""))))
           (name (magnus-instance-name instance)))
      ;; Dedup
      (unless (gethash dedup-key magnus-command--seen-prompts)
        (puthash dedup-key t magnus-command--seen-prompts)
        (with-current-buffer buf
          (if auto-approved-p
              ;; Informational event
              (when magnus-command-show-auto-approved
                (let ((event (list :type 'auto-approved
                                   :timestamp (float-time)
                                   :instance-id id
                                   :instance-name name
                                   :text prompt-text
                                   :handled t)))
                  (magnus-command--add-event event)))
            ;; Real prompt — queue it
            (let ((event (list :type 'prompt
                               :timestamp (float-time)
                               :instance-id id
                               :instance-name name
                               :text prompt-text
                               :handled nil
                               :response nil)))
              (magnus-command--add-event event)
              (setq magnus-command--prompt-queue
                    (append magnus-command--prompt-queue (list event)))
              (unless magnus-command--active-prompt
                (magnus-command--activate-next)))))))))

(defun magnus-command--on-instances-changed ()
  "Detect joins and leaves by comparing with previous snapshot."
  (when-let ((buf (get-buffer magnus-command-buffer-name)))
    (with-current-buffer buf
      (let* ((current-ids (mapcar #'magnus-instance-id (magnus-instances-list)))
             (prev-ids magnus-command--prev-instances)
             (joined (cl-set-difference current-ids prev-ids :test #'string=))
             (left (cl-set-difference prev-ids current-ids :test #'string=)))
        ;; Record joins
        (dolist (id joined)
          (when-let ((inst (magnus-instances-get id)))
            (magnus-command--add-event
             (list :type 'status-change
                   :timestamp (float-time)
                   :instance-id id
                   :instance-name (magnus-instance-name inst)
                   :text "joined"
                   :handled t))))
        ;; Record leaves
        (dolist (id left)
          (magnus-command--add-event
           (list :type 'status-change
                 :timestamp (float-time)
                 :instance-id id
                 :instance-name (or (when-let ((inst (magnus-instances-get id)))
                                      (magnus-instance-name inst))
                                    id)
                 :text "left"
                 :handled t)))
        ;; Update snapshot
        (setq magnus-command--prev-instances current-ids)))))

;;; Event management

(defun magnus-command--add-event (event)
  "Add EVENT to the event list and render it incrementally."
  (push event magnus-command--events)
  (magnus-command--append-event event)
  (magnus-command--update-header-line))

(defun magnus-command--update-header-line ()
  "Force header-line redisplay."
  (force-mode-line-update))

;;; Floor management

(defun magnus-command--activate-next ()
  "Pop the next prompt from the queue and make it the active prompt."
  (if magnus-command--prompt-queue
      (let ((next (pop magnus-command--prompt-queue)))
        (setq magnus-command--active-prompt next)
        (magnus-command--update-input-prompt))
    (magnus-command--clear-active)))

(defun magnus-command--clear-active ()
  "Clear the active prompt, update input prompt."
  (setq magnus-command--active-prompt nil)
  (magnus-command--update-input-prompt))

;;; Sending

(defun magnus-command-send ()
  "Send the text in the input area.
If an active prompt exists, respond to that agent.
If in the log region, visit the agent at point.
Otherwise, send as a free-form message."
  (interactive)
  ;; If in log region, visit agent at point
  (when (and magnus-command--input-marker
             (< (point) (marker-position magnus-command--input-marker)))
    (magnus-command-visit)
    (cl-return-from magnus-command-send))
  ;; Get input text
  (let ((text (magnus-command--get-input-text)))
    (when (string-empty-p text)
      (user-error "Nothing to send"))
    (cond
     ;; Active prompt — respond to that agent
     (magnus-command--active-prompt
      (magnus-command--send-to-active text))
     ;; Parse @mention or use target
     (t
      (magnus-command--send-free-form text)))
    ;; Clear input area
    (magnus-command--clear-input)
    ;; Scroll to bottom
    (goto-char (point-max))))

(defun magnus-command--get-input-text ()
  "Return the text in the input area (after the prompt, trimmed)."
  (when magnus-command--input-marker
    (let* ((start (marker-position magnus-command--input-marker))
           (raw (buffer-substring-no-properties start (point-max)))
           ;; Strip the input prompt prefix
           (stripped (if (string-match "^\\(?:\\[.*?\\] \\)?> " raw)
                        (substring raw (match-end 0))
                      raw)))
      (string-trim stripped))))

(defun magnus-command--send-to-active (text)
  "Send TEXT as a response to the active prompt's agent."
  (let* ((event magnus-command--active-prompt)
         (id (plist-get event :instance-id))
         (name (plist-get event :instance-name))
         (instance (magnus-instances-get id)))
    ;; Send to vterm
    (when instance
      (when-let ((buffer (magnus-instance-buffer instance)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (vterm-send-string text)
            (vterm-send-return)))))
    ;; Mark prompt as handled
    (plist-put event :handled t)
    (plist-put event :response text)
    ;; Release from attention queue
    (when instance
      (magnus-attention-release instance))
    ;; Log user response
    (magnus-command--add-event
     (list :type 'user-response
           :timestamp (float-time)
           :instance-id id
           :instance-name name
           :text text
           :handled t))
    ;; Activate next
    (magnus-command--activate-next)))

(defun magnus-command--send-free-form (text)
  "Send TEXT as a free-form message, parsing @mention or using target."
  (let (target-id target-name)
    ;; Try to parse @agent-name at start
    (if (string-match "^@\\([a-zA-Z][-a-zA-Z0-9_]*\\)\\s-+" text)
        (let* ((mention-name (match-string 1 text))
               (msg (substring text (match-end 0)))
               (instance (magnus-instances-get-by-name mention-name)))
          (if instance
              (progn
                (setq target-id (magnus-instance-id instance))
                (setq target-name mention-name)
                (setq text msg))
            (user-error "No instance named '%s'" mention-name)))
      ;; Use stored target
      (if magnus-command--target
          (if-let ((inst (magnus-instances-get magnus-command--target)))
              (progn
                (setq target-id magnus-command--target)
                (setq target-name (magnus-instance-name inst)))
            (user-error "Target instance no longer exists"))
        (user-error "No target — use @agent-name or C-c C-t to pick one")))
    ;; Send the message
    (when-let ((instance (magnus-instances-get target-id)))
      (magnus-coord-send-message instance text))
    ;; Log it
    (magnus-command--add-event
     (list :type 'message-sent
           :timestamp (float-time)
           :instance-id target-id
           :instance-name target-name
           :text text
           :handled t))))

(defun magnus-command-broadcast ()
  "Send the input text to ALL running agents."
  (interactive)
  (let ((text (magnus-command--get-input-text)))
    (when (string-empty-p text)
      (user-error "Nothing to send"))
    (let ((sent 0))
      (dolist (instance (magnus-instances-list))
        (when (magnus-process-running-p instance)
          (magnus-coord-send-message instance text)
          (cl-incf sent)))
      ;; Log it
      (magnus-command--add-event
       (list :type 'message-sent
             :timestamp (float-time)
             :instance-id nil
             :instance-name "all"
             :text text
             :handled t))
      (magnus-command--clear-input)
      (goto-char (point-max))
      (message "Broadcast to %d agents" sent))))

(defun magnus-command-set-target ()
  "Pick a target agent for free-form messages."
  (interactive)
  (let* ((instances (magnus-instances-list))
         (names (mapcar #'magnus-instance-name instances))
         (choice (completing-read "Target agent: " names nil t)))
    (when-let ((inst (magnus-instances-get-by-name choice)))
      (setq magnus-command--target (magnus-instance-id inst))
      (magnus-command--update-input-prompt)
      (message "Target set to %s" choice))))

;;; Input area management

(defun magnus-command--clear-input ()
  "Clear the input area and re-insert the prompt."
  (let ((inhibit-read-only t))
    (delete-region (marker-position magnus-command--input-marker) (point-max))
    (goto-char (marker-position magnus-command--input-marker))
    (magnus-command--insert-input-prompt)))

(defun magnus-command--update-input-prompt ()
  "Redraw the input prompt prefix based on active prompt or target."
  (when magnus-command--input-marker
    (let ((inhibit-read-only t)
          (user-text (magnus-command--get-input-text)))
      (delete-region (marker-position magnus-command--input-marker) (point-max))
      (goto-char (marker-position magnus-command--input-marker))
      (magnus-command--insert-input-prompt)
      (insert user-text)
      (magnus-command--apply-read-only))))

;;; Rendering

(defun magnus-command--append-event (event)
  "Insert EVENT text just above the separator line."
  (when-let ((buf (get-buffer magnus-command-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          ;; Find the separator
          (goto-char (point-min))
          (let ((sep-pos (text-property-any (point-min) (point-max)
                                            'magnus-command-separator t)))
            (when sep-pos
              (goto-char sep-pos)
              (beginning-of-line)
              ;; Insert event text before separator
              (magnus-command--render-event event)
              (insert "\n"))))
        ;; Update marker and read-only
        (magnus-command--apply-read-only)
        ;; Scroll to bottom if visible
        (magnus-command--scroll-to-bottom)))))

(defun magnus-command--render-event (event)
  "Insert formatted text for EVENT at point."
  (let ((type (plist-get event :type))
        (ts (magnus-command--format-timestamp (plist-get event :timestamp)))
        (name (plist-get event :instance-name))
        (text (plist-get event :text)))
    (pcase type
      ('prompt
       (let* ((handled (plist-get event :handled))
              (active (eq event magnus-command--active-prompt))
              (face (cond (handled 'magnus-command-prompt-handled)
                          (active 'magnus-command-prompt-active)
                          (t 'magnus-command-prompt-queued)))
              (label (cond (handled (format "(approved: %s)" (or (plist-get event :response) "?")))
                           (active "PROMPT")
                           (t "queued"))))
         (insert (propertize ts 'face 'magnus-command-timestamp))
         (insert "  ")
         (insert (propertize (format "[%s]" name)
                             'face 'magnus-command-agent-name
                             'magnus-command-instance-id (plist-get event :instance-id)))
         (insert " ")
         (insert (propertize label 'face face))
         (when active
           (insert (propertize " ← active" 'face 'magnus-command-prompt-active)))
         (insert "\n")
         (insert "  " (or text ""))))

      ('auto-approved
       (insert (propertize ts 'face 'magnus-command-timestamp))
       (insert "  ")
       (insert (propertize (format "[%s]" name) 'face 'magnus-command-agent-name))
       (insert " ")
       (insert (propertize "auto-approved" 'face 'magnus-command-auto-approved))
       (insert "\n")
       (insert (propertize (concat "  " (or text ""))
                           'face 'magnus-command-auto-approved)))

      ('user-response
       (insert (propertize ts 'face 'magnus-command-timestamp))
       (insert "  ")
       (insert (propertize "you" 'face 'magnus-command-user))
       (insert (propertize " -> " 'face 'magnus-command-user))
       (insert (propertize name 'face 'magnus-command-agent-name))
       (insert ": ")
       (insert (or text "")))

      ('message-sent
       (insert (propertize ts 'face 'magnus-command-timestamp))
       (insert "  ")
       (insert (propertize "you" 'face 'magnus-command-user))
       (insert (propertize " -> " 'face 'magnus-command-user))
       (insert (propertize name 'face 'magnus-command-agent-name))
       (insert ": ")
       (insert (or text "")))

      ('status-change
       (insert (propertize ts 'face 'magnus-command-timestamp))
       (insert "  ")
       (insert (propertize (format "%s %s" name (or text ""))
                           'face 'magnus-command-system
                           'magnus-command-instance-id (plist-get event :instance-id)))))))

(defun magnus-command--format-timestamp (time)
  "Format TIME as HH:MM."
  (format-time-string "%H:%M" (seconds-to-time (or time (float-time)))))

(defun magnus-command--scroll-to-bottom ()
  "Scroll the command buffer window to show the latest events."
  (when-let ((win (get-buffer-window magnus-command-buffer-name)))
    (with-selected-window win
      (goto-char (point-max)))))

;;; Full re-render

(defun magnus-command-refresh ()
  "Full re-render of the command buffer."
  (interactive)
  (when-let ((buf (get-buffer magnus-command-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (user-text (magnus-command--get-input-text)))
        (erase-buffer)
        ;; Header
        (insert (propertize "Magnus Command Buffer" 'face 'magnus-command-agent-name))
        (insert "\n\n")
        ;; Events (oldest first — our list is newest-first)
        (dolist (event (reverse magnus-command--events))
          (magnus-command--render-event event)
          (insert "\n"))
        ;; Separator
        (magnus-command--insert-separator)
        ;; Input area
        (setq magnus-command--input-marker (point-marker))
        (set-marker-insertion-type magnus-command--input-marker nil)
        (magnus-command--insert-input-prompt)
        (when user-text
          (insert user-text))
        (magnus-command--apply-read-only)
        (goto-char (point-max))))))

(defun magnus-command-tail ()
  "Refresh and jump to the end."
  (interactive)
  (magnus-command-refresh)
  (goto-char (point-max)))

;;; Navigation

(defun magnus-command-next-prompt ()
  "Jump to the next pending prompt in the log."
  (interactive)
  ;; Find next prompt event text property in buffer
  (let ((pos (point)))
    (goto-char (point-min))
    ;; Simple: search for "PROMPT" or "queued" face
    (if (text-property-any (point) (point-max)
                           'face 'magnus-command-prompt-active)
        (goto-char (text-property-any (point) (point-max)
                                      'face 'magnus-command-prompt-active))
      (goto-char pos)
      (message "No pending prompts"))))

(defun magnus-command-visit ()
  "Visit the agent at point in their vterm buffer."
  (interactive)
  (if-let ((id (get-text-property (point) 'magnus-command-instance-id)))
      (if-let ((instance (magnus-instances-get id)))
          (when-let ((buffer (magnus-instance-buffer instance)))
            (when (buffer-live-p buffer)
              (pop-to-buffer buffer)))
        (user-error "Instance no longer exists"))
    (user-error "No agent at point")))

(defun magnus-command--maybe-quit ()
  "Quit if in log area, self-insert if in input area."
  (interactive)
  (if (and magnus-command--input-marker
           (>= (point) (marker-position magnus-command--input-marker)))
      (self-insert-command 1)
    (quit-window)))

;;; Reconciliation

(defun magnus-command--start-reconcile-timer ()
  "Start the 30s reconciliation timer."
  (magnus-command--stop-reconcile-timer)
  (setq magnus-command--reconcile-timer
        (run-with-timer 30 30 #'magnus-command--reconcile)))

(defun magnus-command--stop-reconcile-timer ()
  "Stop the reconciliation timer."
  (when magnus-command--reconcile-timer
    (cancel-timer magnus-command--reconcile-timer)
    (setq magnus-command--reconcile-timer nil)))

(defun magnus-command--reconcile ()
  "Check if pending prompts are still present in their vterm buffers.
Remove stale prompts and activate next if needed."
  (when-let ((buf (get-buffer magnus-command-buffer-name)))
    (with-current-buffer buf
      (let ((stale nil))
        ;; Check active prompt
        (when magnus-command--active-prompt
          (when (magnus-command--prompt-stale-p magnus-command--active-prompt)
            (push magnus-command--active-prompt stale)
            (plist-put magnus-command--active-prompt :handled t)
            (plist-put magnus-command--active-prompt :response "(resolved externally)")
            (setq magnus-command--active-prompt nil)))
        ;; Check queued prompts
        (let ((remaining nil))
          (dolist (event magnus-command--prompt-queue)
            (if (magnus-command--prompt-stale-p event)
                (progn
                  (push event stale)
                  (plist-put event :handled t)
                  (plist-put event :response "(resolved externally)"))
              (push event remaining)))
          (setq magnus-command--prompt-queue (nreverse remaining)))
        ;; Clean up seen-prompts for stale entries
        (dolist (event stale)
          (let ((id (plist-get event :instance-id))
                (text (plist-get event :text)))
            (remhash (cons id (md5 (or text ""))) magnus-command--seen-prompts)))
        ;; If active was cleared, activate next
        (when (and stale (null magnus-command--active-prompt))
          (magnus-command--activate-next))
        ;; Re-render if anything changed
        (when stale
          (magnus-command-refresh))))))

(defun magnus-command--prompt-stale-p (event)
  "Return non-nil if EVENT's prompt is no longer present in the agent's vterm."
  (let* ((id (plist-get event :instance-id))
         (instance (magnus-instances-get id)))
    (or (null instance)
        (let ((buffer (magnus-instance-buffer instance)))
          (or (null buffer)
              (not (buffer-live-p buffer))
              (not (with-current-buffer buffer
                     (magnus-attention--last-line))))))))

;;; Cleanup

(defun magnus-command--cleanup ()
  "Clean up hooks and timers."
  (magnus-command--unregister-hooks)
  (magnus-command--stop-reconcile-timer))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (eq major-mode 'magnus-command-mode)
              (magnus-command--cleanup))))

(provide 'magnus-command)
;;; magnus-command.el ends here
