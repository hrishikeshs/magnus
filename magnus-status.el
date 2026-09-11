;;; magnus-status.el --- Status buffer for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides the main status buffer showing interactive agents and
;; completed review lineages with magit-style keybindings.

;;; Code:

(require 'cl-lib)
(require 'eldoc)
(require 'seq)
(require 'subr-x)
(require 'magnus-instances)
(require 'magnus-process)
(require 'magnus-coord)
(require 'magnus-attention)
(require 'magnus-health)
(require 'magnus-review)
(require 'magnus-onboarding)

(declare-function magnus-dispatch "magnus-transient")
(declare-function magnus-review-request-dispatch "magnus-transient")
(declare-function magnus-review-actions "magnus-transient")
(autoload 'magnus-transient-create-codex "magnus-transient"
  "Create a Codex agent in the selected status row's project." t)
(declare-function magnus-review-ui-open "magnus-review-ui")
(declare-function magnus-review-controller-candidate-round
                  "magnus-review-controller" (review))
(declare-function magnus-review-controller-error
                  "magnus-review-controller" (review))
(declare-function magnus-coord-agent-busy-p "magnus-coord")
(declare-function magnus-coord--neglected-p "magnus-coord")
(declare-function magnus-coord-has-state-p "magnus-coord" (directory))
(declare-function magnus-coord-refresh-all "magnus-coord")
(declare-function magnus-coord-reconcile-all "magnus-coord")
(declare-function magnus-retro "magnus-coord")
(declare-function magnus-persistence-save "magnus-persistence")
(declare-function magnus--agents-index-get "magnus")

(defvar magnus-coord--do-not-disturb)
(defvar magnus-persistence--autosave-active nil)
(declare-function magnus-context "magnus-context")

;; Defined in magnus.el
(defvar magnus-buffer-name "*magnus*")
(defvar magnus-default-directory nil)

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

(defface magnus-status-purged
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for purged (archived) instances."
  :group 'magnus)

(defface magnus-status-expertise
  '((t :inherit font-lock-doc-face :slant italic))
  "Face for expertise tags in status buffer."
  :group 'magnus)

(defface magnus-status-review-unread
  '((t :inherit warning :weight bold))
  "Face for the unread marker on completed reviews."
  :group 'magnus)

(defface magnus-status-reviewer-name
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for durable reviewer identities."
  :group 'magnus)

(defface magnus-status-reviewing
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for an active independent-review badge."
  :group 'magnus)

(defun magnus-status--set-review-animation-interval (symbol value)
  "Set SYMBOL to VALUE and immediately normalize the visible status UI."
  (set-default symbol value)
  (when (fboundp 'magnus-status-stop-review-animation)
    (magnus-status-stop-review-animation))
  (when (and (boundp 'magnus-buffer-name)
             (fboundp 'magnus-status-refresh))
    (when-let ((buffer (get-buffer magnus-buffer-name)))
      (with-current-buffer buffer
        (when (derived-mode-p 'magnus-status-mode)
          (magnus-status-refresh))))))

(defcustom magnus-status-review-animation-interval 0.4
  "Seconds between active-review animation frames.
Set this to nil to keep the textual review badge but disable its animation."
  :type '(choice (const :tag "Static badge" nil)
                 (number :tag "Frame interval"))
  :set #'magnus-status--set-review-animation-interval
  :group 'magnus)

(defcustom magnus-status-show-context-hints t
  "Whether the Magnus status buffer shows point-sensitive action hints.
Hints use ElDoc's echo-area ownership, so they wait for ordinary command and
asynchronous messages instead of replacing them."
  :type 'boolean
  :group 'magnus)

(defconst magnus-status--review-animation-frames ["|" "/" "-" "\\"]
  "Fixed-width frames used by the active-review status badge.")

(defvar magnus-status--review-animation-timer nil
  "Single presentation timer for active-review badges.")

(defvar magnus-status--review-animation-frame 0
  "Index of the current active-review animation frame.")

(defvar-local magnus-status--last-context-hint nil
  "Last echo-area string displayed by Magnus's ElDoc backend.")

(defun magnus-status--context-hint-message (format-string &rest args)
  "Display an ElDoc message without replacing unrelated echo-area output.
FORMAT-STRING and ARGS follow `message'.  Magnus may replace silence or its
own prior hint; ordinary command, timer, and process messages retain ownership."
  (let ((current (current-message)))
    (if (or (null current)
            (and magnus-status--last-context-hint
                 (string=
                  (substring-no-properties current)
                  (substring-no-properties
                   magnus-status--last-context-hint))))
        (progn
          (apply #'eldoc-minibuffer-message format-string args)
          ;; Record the rendered message rather than the source hint: ElDoc can
          ;; truncate it to the frame width before it reaches the echo area.
          (setq magnus-status--last-context-hint
                (and format-string (current-message))))
      ;; ElDoc records a message before invoking its display function.  If we
      ;; declined that display, forget both ownership records so a later
      ;; pre-command refresh cannot resurrect the skipped hint.
      (setq magnus-status--last-context-hint nil
            eldoc-last-message nil))))

(defun magnus-status--context-hint (_callback)
  "Return an ElDoc action hint for the status entity at point.
The displayed keys come from the active keymaps, so user rebindings remain
truthful.  CALLBACK is accepted for ElDoc's documentation-function protocol."
  (when magnus-status-show-context-hints
    (let* ((review (magnus-status--get-review-at-point))
           (instance (and (not review)
                          (magnus-status--get-instance-at-point)))
           (hint
            (cond
              (review
               (let* ((round (magnus-status--review-round-to-open review))
                      (identity
                       (if round
                           (format "%s · round %d"
                                   (or (magnus-review-reviewer-name review)
                                       "Review")
                                   (magnus-review-scope-number round))
                         (or (magnus-review-reviewer-name review) "Review"))))
                 (concat
                  identity " — "
                  (when round "\\[magnus-status-visit] open · ")
                  "\\[magnus-review-request-dispatch] review actions · "
                  "\\[magnus-dispatch] all actions")))
              (instance
               (if (eq (magnus-instance-status instance) 'purged)
                   (format
                    (concat
                     "%s (archived) — "
                     "\\[magnus-status-resurrect-purged] resurrect · "
                     "\\[magnus-status-rename] rename · "
                     "\\[magnus-dispatch] all actions")
                    (magnus-instance-name instance))
                 (format
                  (concat
                   "%s — \\[magnus-status-visit] visit · "
                   "\\[magnus-status-send-message] message · "
                   "\\[magnus-review-request-dispatch] request review · "
                   "\\[magnus-status-trace] thinking trace · "
                   "\\[magnus-dispatch] all actions")
                  (magnus-instance-name instance))))
              (t
               (concat
                "Magnus — \\[magnus-status-next]/"
                "\\[magnus-status-previous] navigate · "
                "\\[magnus-status-create] create Claude · "
                "\\[magnus-transient-create-codex] create Codex · "
                "\\[magnus-dispatch] all actions")))))
      (substitute-command-keys hint))))

(defun magnus-status--selection-error (target action)
  "Explain that point must be on TARGET to perform ACTION.
Signal an actionable `user-error' that follows the active navigation
bindings, so custom keymaps do not make the guidance stale."
  (user-error
   "Put point on %s to %s; use %s to select one, then retry"
   target action
   (substitute-command-keys
    "\\[magnus-status-next]/\\[magnus-status-previous]")))

(defun magnus-status-explain-unavailable-key ()
  "Explain an unbound printable key in the status buffer.
Magnus inherits `special-mode', where an unbound printable key normally
reports only that it is undefined.  Name the selected entity and direct the
user to its actions instead."
  (interactive)
  (let* ((description (key-description (this-command-keys-vector)))
         (key (if (string-empty-p description) "That key" description))
         (actions (substitute-command-keys "\\[magnus-dispatch]")))
    (cond
     ((magnus-status--get-instance-at-point)
      (user-error "%s is not available for this agent; press %s for its actions"
                  key actions))
     ((magnus-status--get-review-at-point)
      (user-error "%s is not available for this review; press %s for its actions"
                  key actions))
     (t
      (user-error
       "%s is not available here; use %s to select an agent or review, or press %s for all actions"
       key
       (substitute-command-keys
        "\\[magnus-status-next]/\\[magnus-status-previous]")
       actions)))))

;;; Mode definition

(defvar magnus-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'magnus-status-visit)
    (define-key map (kbd "c") #'magnus-status-create)
    (define-key map (kbd "X") #'magnus-transient-create-codex)
    (define-key map (kbd "k") #'magnus-status-archive)
    (define-key map (kbd "r") #'magnus-status-rename)
    (define-key map (kbd "R") #'magnus-status-resurrect-purged)
    (define-key map (kbd "s") #'magnus-status-suspend)
    (define-key map (kbd "S") #'magnus-status-resume)
    (define-key map (kbd "d") #'magnus-status-chdir)
    (define-key map (kbd "m") #'magnus-status-send-message)
    (define-key map (kbd "t") #'magnus-status-trace)
    (define-key map (kbd "v") #'magnus-review-request-dispatch)
    (define-key map (kbd "V") #'magnus-review-actions)
    (define-key map (kbd "g") #'magnus-status-refresh)
    (define-key map (kbd "x") #'magnus-status-context)
    (define-key map (kbd "C") #'magnus-status-coordination)
    (define-key map (kbd "n") #'magnus-status-next)
    (define-key map (kbd "p") #'magnus-status-previous)
    (define-key map (kbd "a") #'magnus-attention-next)
    (define-key map (kbd "A") #'magnus-attention-show-queue)
    (define-key map (kbd "P") #'magnus-status-archive-all)
    (define-key map (kbd "z") #'magnus-coord-toggle-dnd)
    (define-key map (kbd "F") #'magnus-retro)
    (define-key map (kbd "?") #'magnus-dispatch)
    (define-key map (kbd "q") #'quit-window)
    ;; `special-mode' remaps otherwise unbound printable keys to `undefined'.
    ;; Replace only that self-insertion fallback; explicit Magnus bindings,
    ;; navigation keys, prefixes, and user overrides continue to win.
    (define-key map [remap self-insert-command]
                #'magnus-status-explain-unavailable-key)
    map)
  "Keymap for `magnus-status-mode'.")

(define-derived-mode magnus-status-mode special-mode "Magnus"
  "Major mode for the magnus status buffer.

\\{magnus-status-mode-map}"
  :group 'magnus
  (setq-local revert-buffer-function #'magnus-status--revert)
  (setq-local truncate-lines t)
  (when magnus-status-show-context-hints
    (add-hook 'eldoc-documentation-functions
              #'magnus-status--context-hint nil t)
    (setq-local eldoc-message-function
                #'magnus-status--context-hint-message)
    (setq-local magnus-status--last-context-hint nil)
    ;; ElDoc already recognizes ordinary cursor motion.  Magnus's semantic
    ;; item motion uses package-specific command names, so register those too.
    (eldoc-add-command 'magnus-status-next 'magnus-status-previous)
    (eldoc-mode 1))
  (add-hook 'magnus-instances-changed-hook #'magnus-status--maybe-refresh)
  (add-hook 'kill-buffer-hook #'magnus-status-stop-review-animation nil t)
  (add-hook 'change-major-mode-hook
            #'magnus-status-stop-review-animation nil t)
  (add-hook 'window-state-change-functions
            #'magnus-status--window-state-change nil t))

;;; Buffer creation

(defun magnus-status ()
  "Open or switch to the magnus status buffer."
  (interactive)
  ;; `magnus--shutdown' removes this global hook.  Reinstall it here as well as
  ;; in the mode constructor so an existing status buffer can be reused after
  ;; a clean initialize/shutdown/initialize lifecycle.
  (add-hook 'magnus-instances-changed-hook #'magnus-status--maybe-refresh)
  (let ((buffer (get-buffer-create magnus-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'magnus-status-mode)
        (magnus-status-mode))
      (magnus-status-refresh))
    (switch-to-buffer buffer)
    ;; The initial render occurs before `switch-to-buffer', so synchronize once
    ;; more now that a visible status buffer can own the presentation timer.
    (magnus-status--sync-review-animation)))

(defun magnus-status-refresh ()
  "Refresh the magnus status buffer."
  (interactive)
  ;; Re-read shared coordination files only on interactive (manual `g')
  ;; refresh.  Presentation-only refreshes use the watcher-maintained cache.
  (when (called-interactively-p 'interactive)
    (magnus-coord-reconcile-all)
    (magnus-coord-refresh-all))
  (when-let ((buffer (get-buffer magnus-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (line (line-number-at-pos))
            (instance-id (get-text-property (point) 'magnus-instance-id))
            (review-id (get-text-property (point) 'magnus-review-id)))
        (erase-buffer)
        (magnus-status--insert-header)
        (magnus-status--insert-instances)
        (magnus-status--insert-reviews)
        (magnus-status--insert-coordination)
        (magnus-status--insert-purged)
        (cond
         ((and review-id
               (text-property-any (point-min) (point-max)
                                  'magnus-review-id review-id))
          (goto-char (text-property-any (point-min) (point-max)
                                        'magnus-review-id review-id)))
         ((and instance-id
               (text-property-any (point-min) (point-max)
                                  'magnus-instance-id instance-id))
          (goto-char (text-property-any (point-min) (point-max)
                                        'magnus-instance-id instance-id)))
         (t
          (goto-char (point-min))
          (forward-line (1- line))))
        (magnus-status--goto-instance-line)
        (magnus-status--sync-review-animation)))))

(defun magnus-status--revert (_ignore-auto _noconfirm)
  "Revert function for status buffer."
  (magnus-status-refresh))

(defun magnus-status--maybe-refresh ()
  "Refresh if the status buffer is visible."
  (when-let ((buffer (get-buffer magnus-buffer-name)))
    (when (get-buffer-window buffer t)
      (magnus-status-refresh))))

;;; Active review animation

(defun magnus-status--review-animation-enabled-p ()
  "Return non-nil when active-review animation is enabled."
  (and (numberp magnus-status-review-animation-interval)
       (> magnus-status-review-animation-interval 0)))

(defun magnus-status-stop-review-animation ()
  "Stop the global active-review presentation timer."
  (when (timerp magnus-status--review-animation-timer)
    (cancel-timer magnus-status--review-animation-timer))
  (setq magnus-status--review-animation-timer nil
        magnus-status--review-animation-frame 0))

(defun magnus-status-shutdown ()
  "Stop global status presentation resources and detach refresh hooks."
  (magnus-status-stop-review-animation)
  (remove-hook 'magnus-instances-changed-hook #'magnus-status--maybe-refresh))

(defun magnus-status--review-animation-slot-p ()
  "Return non-nil when the current buffer contains an animation slot."
  (text-property-any (point-min) (point-max)
                     'magnus-review-animation-slot t))

(defun magnus-status--sync-review-animation ()
  "Start or stop active-review animation for the current status buffer."
  (let ((slot-p (and (derived-mode-p 'magnus-status-mode)
                     (magnus-status--review-animation-slot-p))))
    (cond
     ;; A runtime motion opt-out must remove the stale frame as well as stop
     ;; the timer.  Refresh renders the same accessible badge as `[review]'.
     ((and slot-p (not (magnus-status--review-animation-enabled-p)))
      (magnus-status-stop-review-animation)
      (magnus-status-refresh))
     ((and slot-p
           (get-buffer-window (current-buffer) t))
      (unless (timerp magnus-status--review-animation-timer)
        (setq magnus-status--review-animation-timer
              (run-with-timer magnus-status-review-animation-interval
                              magnus-status-review-animation-interval
                              #'magnus-status--review-animation-tick))))
     (t
      (magnus-status-stop-review-animation)))))

(defun magnus-status--window-state-change (_window)
  "Resynchronize review animation when the status buffer becomes visible."
  (when (derived-mode-p 'magnus-status-mode)
    (magnus-status--sync-review-animation)))

(defun magnus-status--review-animation-tick ()
  "Advance every visible active-review badge without rebuilding status."
  (let ((buffer (and (boundp 'magnus-buffer-name)
                     (get-buffer magnus-buffer-name))))
    (if (and (buffer-live-p buffer)
             (get-buffer-window buffer t))
        (with-current-buffer buffer
          (cond
           ((not (magnus-status--review-animation-enabled-p))
            (magnus-status--sync-review-animation))
           ((not (magnus-status--review-animation-slot-p))
            (magnus-status-stop-review-animation))
           (t
            (setq magnus-status--review-animation-frame
                  (mod (1+ magnus-status--review-animation-frame)
                       (length magnus-status--review-animation-frames)))
            (let ((position (point-min))
                  (frame
                   (aref magnus-status--review-animation-frames
                         magnus-status--review-animation-frame)))
              (with-silent-modifications
                (while (setq position
                             (text-property-any
                              position (point-max)
                              'magnus-review-animation-slot t))
                  (put-text-property position (1+ position) 'display frame)
                  (setq position (1+ position))))))))
      (magnus-status-stop-review-animation))))

;;; Buffer content

(defun magnus-status--insert-header ()
  "Insert the status buffer header."
  (insert (propertize "Magnus" 'face 'magnus-status-section-heading))
  (insert " - AI Agent Manager\n")
  (insert (format "Instances: %d" (length (magnus-instances-active-list))))
  (let ((unread
         (cl-count-if
          (lambda (review)
            (eq (magnus-review-read-state review) 'unread))
          (magnus-review-list))))
    (when (> unread 0)
      (insert
       (propertize
        (format "  [%d unread review%s]" unread
                (if (= unread 1) "" "s"))
        'face 'magnus-status-review-unread))))
  (when magnus-coord--do-not-disturb
    (insert (propertize "  [DND]" 'face 'font-lock-warning-face)))
  (let ((attention-count (magnus-attention-pending-count)))
    (when (> attention-count 0)
      (insert (propertize (format "  [%d need attention]" attention-count)
                         'face 'magnus-status-running))))
  (insert "\n\n"))

(defun magnus-status--insert-instances ()
  "Insert the list of active (non-purged) instances."
  (let ((instances (magnus-instances-active-list)))
    (if (and (null instances) (null (magnus-instances-purged-list)))
        (magnus-status--insert-empty-state)
      (insert (propertize "Instances" 'face 'magnus-status-section-heading))
      (insert (propertize "  (RET visit · v review · ? actions)\n"
                          'face 'magnus-status-empty-hint))
      (if (null instances)
          (progn
            (insert (propertize "  No active instances.\n"
                                'face 'magnus-status-empty-hint))
            (insert (propertize "  Press 'c' for Claude, 'X' for Codex, or 'R' to resurrect.\n"
                                'face 'magnus-status-empty-hint)))
        (dolist (instance instances)
          (magnus-status--insert-instance instance))))))

(defun magnus-status--insert-empty-state ()
  "Insert the empty state message."
  (insert "\n")
  (insert (propertize "  No agent instances.\n"
                      'face 'magnus-status-empty-hint))
  (insert (propertize "  Press 'c' for Claude or 'X' for Codex.\n"
                      'face 'magnus-status-empty-hint)))

(defun magnus-status--insert-reviews ()
  "Insert review lineages after the instance list."
  (let* ((all (magnus-review-list))
         (reviews
          (cl-remove-if
           (lambda (review)
             (eq (magnus-review-lifecycle review) 'archived))
           all))
         (archived
          (cl-remove-if-not
           (lambda (review)
             (eq (magnus-review-lifecycle review) 'archived))
           all)))
    (insert "\n")
    (insert (propertize "Reviews\n" 'face 'magnus-status-section-heading))
    (if (or reviews archived)
        (progn
          (dolist (review reviews)
            (magnus-status--insert-review review))
          (when archived
            (insert (propertize "  Archived reviews\n"
                                'face 'magnus-status-empty-hint))
            (dolist (review archived)
              (magnus-status--insert-review review))))
      (insert
       (propertize
        "  No reviews yet. On an agent, press 'v', then RET, to request one.\n"
        'face 'magnus-status-empty-hint)))))

(defun magnus-status--review-state-label (review round)
  "Return a concise state label for REVIEW and its latest ROUND."
  (let* ((lifecycle (magnus-review-lifecycle review))
         (execution (magnus-review-execution review))
         (verdict (and (eq execution 'complete)
                       round (magnus-review-round-verdict round))))
    (replace-regexp-in-string
     "-" " "
     (symbol-name (if (eq lifecycle 'archived)
                      'archived
                    (or verdict
                        (and (eq execution 'asking-scope) 'asking-author)
                        execution
                        'pending))))))

(defun magnus-status--review-state-face (review round)
  "Return the status face for REVIEW and its latest ROUND."
  (let* ((lifecycle (magnus-review-lifecycle review))
         (execution (magnus-review-execution review))
         (verdict (and (eq execution 'complete)
                       round (magnus-review-round-verdict round))))
    (cond
     ((eq lifecycle 'archived) 'magnus-status-purged)
     ((eq verdict 'approve) 'magnus-status-running)
     ((eq verdict 'changes-requested) 'magnus-status-suspended)
     ((memq execution '(failed interrupted)) 'magnus-status-errored)
     ((eq execution 'complete) 'magnus-status-finished)
     ((memq execution '(asking-scope running))
      'magnus-status-running)
     (t 'magnus-status-instance-dir))))

(defun magnus-status--review-runtime-p (execution)
  "Return non-nil when EXECUTION describes disposable review work."
  (memq execution
        '(asking-scope running failed interrupted)))

(defun magnus-status--review-runtime-round (review execution)
  "Return REVIEW's prepared candidate round while EXECUTION is disposable."
  (when (and (magnus-status--review-runtime-p execution)
             (fboundp 'magnus-review-controller-candidate-round))
    (magnus-review-controller-candidate-round review)))

(defun magnus-status--review-runtime-round-number
    (execution candidate completed)
  "Return the round number STATUS should show.
EXECUTION selects disposable versus completed work.  CANDIDATE is the prepared
ephemeral round and COMPLETED is the latest successful round."
  (cond
   (candidate (magnus-review-scope-number candidate))
   ((magnus-status--review-runtime-p execution)
    ;; Scope discovery precedes candidate preparation, but it is already work
    ;; on the next round.  Name that round without pretending a candidate has
    ;; been frozen yet.
    (1+ (if completed (magnus-review-scope-number completed) 0)))
   (completed (magnus-review-scope-number completed))))

(defun magnus-status--review-error-summary (review execution)
  "Return REVIEW's bounded one-line diagnostic for failed EXECUTION."
  (when (and (eq execution 'failed)
             (fboundp 'magnus-review-controller-error))
    (when-let* ((value (magnus-review-controller-error review))
                (one-line
                 (string-trim
                  (replace-regexp-in-string
                   "[[:space:]]+" " " (format "%s" value)))))
      (unless (string-empty-p one-line)
        (if (> (length one-line) 96)
            (concat (substring one-line 0 93) "...")
          one-line)))))

(defun magnus-status--insert-review (review)
  "Insert a two-line status row for REVIEW's lineage and runtime state."
  (let* ((start (point))
         (completed-round (magnus-review-latest-round review))
         (execution (magnus-review-execution review))
         (runtime-p (magnus-status--review-runtime-p execution))
         (candidate-round
          (magnus-status--review-runtime-round review execution))
         (unread (eq (magnus-review-read-state review) 'unread))
         (provider (or (magnus-review-reviewer-provider review) 'unknown))
         (effort (or (magnus-review-effort review) 'default))
         (task
          (replace-regexp-in-string
           "[\n\r]+" " "
           (string-trim (or (magnus-review-task review) ""))))
         (round-number
          (magnus-status--review-runtime-round-number
           execution candidate-round completed-round))
         (finding-count
          (and (not runtime-p)
               completed-round
               (magnus-review-round-finding-count completed-round)))
         (diagnostic
          (magnus-status--review-error-summary review execution))
         (age
          (unless runtime-p
            (magnus-status--format-age
             (or (magnus-review-updated-at review)
                 (magnus-review-created-at review)))))
         (state (magnus-status--review-state-label review completed-round)))
    (insert "  ")
    (insert (propertize (if unread "●" "·")
                        'face (if unread
                                  'magnus-status-review-unread
                                'magnus-status-instance-dir)))
    (insert " ")
    (insert (propertize (or (magnus-review-reviewer-name review) "reviewer")
                        'face 'magnus-status-reviewer-name))
    (insert " ")
    (insert (propertize (format "[%s/%s]" provider effort)
                        'face 'font-lock-type-face))
    (insert " → ")
    (insert (propertize (or (magnus-review-author-name review) "unknown")
                        'face 'magnus-status-instance-name))
    (unless (string-empty-p task)
      (insert " — ")
      (insert (propertize
               (if (> (length task) 42)
                   (concat (substring task 0 39) "...")
                 task)
               'face 'magnus-status-instance-dir)))
    (insert "\n    ")
    (when round-number
      (insert (format "round %d · " round-number)))
    (insert (propertize state
                        'face (magnus-status--review-state-face
                               review completed-round)))
    (when (numberp finding-count)
      (insert (format " · %d finding%s" finding-count
                      (if (= finding-count 1) "" "s"))))
    (when diagnostic
      (insert " · ")
      (insert (propertize diagnostic 'face 'magnus-status-errored)))
    (when age
      (insert " · ")
      (insert (propertize age 'face 'magnus-status-instance-dir)))
    (insert "\n")
    (put-text-property start (point)
                       'magnus-review-id (magnus-review-id review))))

(defun magnus-status--active-reviews-for-instance (instance)
  "Return review workflows currently active for INSTANCE."
  (let ((instance-id (magnus-instance-id instance)))
    (cl-remove-if-not
     (lambda (review)
       (and (eq (magnus-review-lifecycle review) 'open)
            (string= (or (magnus-review-author-instance-id review) "")
                     instance-id)
            (memq (magnus-review-execution review)
                  '(asking-scope running))))
     (magnus-review-list))))

(defun magnus-status--insert-active-review-badge (reviews)
  "Insert an accessible active-review badge for REVIEWS."
  (let* ((count (length reviews))
         (reviewers
          (mapcar (lambda (review)
                    (or (magnus-review-reviewer-name review) "reviewer"))
                  reviews))
         (help
          (if (= count 1)
              (pcase (magnus-review-execution (car reviews))
                ('asking-scope
                 (format
                  "Magnus is asking this agent which commits %s should review"
                  (car reviewers)))
                (_ (format "%s is reviewing this agent's committed work"
                           (car reviewers))))
            (format "%s have review work pending for this agent"
                    (string-join reviewers ", "))))
         (start (point)))
    (insert " [")
    (insert (if (= count 1) "review" (format "%d reviews" count)))
    (when (magnus-status--review-animation-enabled-p)
      (insert " ")
      (let ((slot (point)))
        (insert (aref magnus-status--review-animation-frames
                      magnus-status--review-animation-frame))
        (put-text-property slot (point)
                           'magnus-review-animation-slot t)))
    (insert "]")
    (add-text-properties
     start (point)
     `(face magnus-status-reviewing help-echo ,help))))

(defun magnus-status--insert-instance (instance)
  "Insert a line for INSTANCE."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (provider (or (magnus-instance-provider instance) 'claude))
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
         (active-reviews (magnus-status--active-reviews-for-instance instance))
         (health-ind (magnus-health-indicator instance))
         (age (magnus-status--format-age (magnus-instance-created-at instance))))
    (insert "  ")
    (insert (propertize name 'face 'magnus-status-instance-name))
    (unless (eq provider 'claude)
      (insert " ")
      (insert (propertize (format "[%s]" provider)
                          'face 'font-lock-type-face)))
    (insert " ")
    (insert (propertize (format "[%s]" status-str) 'face status-face))
    (when active-reviews
      (magnus-status--insert-active-review-badge active-reviews))
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
    (when-let ((tags (magnus--agents-index-get directory name)))
      (insert " ")
      (insert (propertize (magnus-status--truncate-tags tags)
                          'face 'magnus-status-expertise)))
    (insert "\n")
    (insert "    ")
    (insert (propertize (abbreviate-file-name directory) 'face 'magnus-status-instance-dir))
    (when-let ((sid (magnus-instance-session-id instance)))
      (insert " ")
      (insert (propertize (format "[%.8s]" sid) 'face 'magnus-status-instance-dir)))
    (insert "\n")
    ;; Store instance ID as text property for commands
    (put-text-property (line-beginning-position -1) (point)
                       'magnus-instance-id (magnus-instance-id instance))))

(defun magnus-status--insert-purged ()
  "Insert the purged instances section."
  (let ((purged (magnus-instances-purged-list)))
    (when purged
      (insert "\n")
      (insert (propertize "Purged\n" 'face 'magnus-status-section-heading))
      (dolist (instance purged)
        (magnus-status--insert-purged-instance instance)))))

(defun magnus-status--insert-purged-instance (instance)
  "Insert a line for purged INSTANCE."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (session-id (magnus-instance-session-id instance))
         (age (if (magnus-instance-purged-at instance)
                  (magnus-status--format-age
                   (seconds-to-time (magnus-instance-purged-at instance)))
                "unknown")))
    (insert "  ")
    (insert (propertize name 'face 'magnus-status-purged))
    (when session-id
      (insert " ")
      (insert (propertize (format "[%.8s]" session-id)
                          'face 'magnus-status-purged)))
    (when-let ((tags (magnus--agents-index-get directory name)))
      (insert " ")
      (insert (propertize (magnus-status--truncate-tags tags)
                          'face 'magnus-status-expertise)))
    (insert " ")
    (insert (propertize (abbreviate-file-name directory)
                        'face 'magnus-status-purged))
    (insert " ")
    (insert (propertize age 'face 'magnus-status-purged))
    (insert "\n")
    (put-text-property (line-beginning-position 0) (point)
                       'magnus-instance-id (magnus-instance-id instance))))

(defun magnus-status--format-age (time)
  "Format TIME as a human-readable age."
  (when (numberp time)
    (setq time (seconds-to-time time)))
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
  (let ((directories
         (seq-filter #'magnus-coord-has-state-p
                     (magnus-status--get-project-directories))))
    (when directories
      (insert "\n")
      (insert (propertize "Coordination\n" 'face 'magnus-status-section-heading))
      (dolist (dir directories)
        (magnus-status--insert-coordination-for-dir dir)))))

(defun magnus-status--get-project-directories ()
  "Get unique physical project directories from active instances."
  (let ((dirs
         (mapcar (lambda (instance)
                   (magnus-coord--normalized-directory
                    (magnus-instance-directory instance)))
                 (magnus-instances-active-list))))
    (delete-dups dirs)))

(defun magnus-status--insert-coordination-for-dir (directory)
  "Insert coordination info for DIRECTORY."
  (let* ((section-start (point))
         (parsed (magnus-coord-parse directory))
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
      (let ((recent (magnus-coord-recent-log log 3)))
        (dolist (entry recent)
          (insert (format "    [%s] %s: %s\n"
                          (propertize (plist-get entry :time)
                                     'face 'magnus-status-instance-dir)
                          (propertize (plist-get entry :agent)
                                     'face 'magnus-status-instance-name)
                          (plist-get entry :message))))))
    (insert "\n")
    (put-text-property section-start (point)
                       'magnus-project-directory directory)))

(defun magnus-status--truncate-tags (tags)
  "Truncate TAGS string to 50 characters if needed."
  (if (> (length tags) 50)
      (concat (substring tags 0 47) "...")
    tags))

;;; Navigation

(defun magnus-status--get-instance-at-point ()
  "Get the instance at point."
  (when-let ((id (get-text-property (point) 'magnus-instance-id)))
    (magnus-instances-get id)))

(defun magnus-status--get-review-at-point ()
  "Get the review lineage at point."
  (when-let ((id (get-text-property (point) 'magnus-review-id)))
    (magnus-review-get id)))

(defun magnus-status--selection-key-at-point ()
  "Return the stable status item key at point, or nil."
  (cond
   ((get-text-property (point) 'magnus-review-id)
    (cons 'review (get-text-property (point) 'magnus-review-id)))
   ((get-text-property (point) 'magnus-instance-id)
    (cons 'instance (get-text-property (point) 'magnus-instance-id)))))

(defun magnus-status--review-round-to-open (review)
  "Return REVIEW's newest unread, or newest completed, round."
  (let* ((completed (magnus-review-rounds review))
         (unread
          (cl-remove-if-not
           (lambda (round)
             (eq (magnus-review-round-read-state round) 'unread))
           completed)))
    (car (last (or unread completed)))))

(defun magnus-status--goto-instance-line ()
  "Move point to the nearest selectable instance or review row."
  (unless (magnus-status--selection-key-at-point)
    (or (magnus-status--find-instance-forward)
        (magnus-status--find-instance-backward))))

(defun magnus-status--find-instance-forward ()
  "Find the next selectable instance or review and move point there."
  (let ((start (point)))
    (while (and (not (eobp))
                (not (magnus-status--selection-key-at-point)))
      (forward-line 1))
    (if (magnus-status--selection-key-at-point)
        t
      (goto-char start)
      nil)))

(defun magnus-status--find-instance-backward ()
  "Find the previous selectable instance or review and move point there."
  (let ((start (point)))
    (while (and (not (bobp))
                (not (magnus-status--selection-key-at-point)))
      (forward-line -1))
    (if (magnus-status--selection-key-at-point)
        t
      (goto-char start)
      nil)))

(defun magnus-status-next ()
  "Move to the next instance or review."
  (interactive)
  (let ((start (point))
        (current (magnus-status--selection-key-at-point)))
    (forward-line 1)
    (while (and (not (eobp))
                (let ((candidate (magnus-status--selection-key-at-point)))
                  (or (null candidate) (equal candidate current))))
      (forward-line 1))
    (unless (magnus-status--selection-key-at-point)
      (goto-char start))))

(defun magnus-status-previous ()
  "Move to the previous instance or review."
  (interactive)
  (let ((start (point))
        (current (magnus-status--selection-key-at-point)))
    (forward-line -1)
    (while (and (not (bobp))
                (let ((candidate (magnus-status--selection-key-at-point)))
                  (or (null candidate) (equal candidate current))))
      (forward-line -1))
    (unless (magnus-status--selection-key-at-point)
      (goto-char start))))

;;; Commands

(defun magnus-status-visit ()
  "Visit the instance or review lineage at point."
  (interactive)
  (cond
   ((magnus-status--get-review-at-point)
    (let* ((review (magnus-status--get-review-at-point))
           (round (magnus-status--review-round-to-open review)))
      (unless round
        (user-error "Review has no completed round yet (%s)"
                    (magnus-status--review-state-label
                     review (magnus-review-latest-round review))))
      (require 'magnus-review-ui)
      (magnus-review-ui-open review round)))
   ((magnus-status--get-instance-at-point)
    (magnus-process-switch-to (magnus-status--get-instance-at-point)))
   (t (magnus-status--selection-error
       "an agent or review row" "visit it"))))

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

(defun magnus-status-archive ()
  "Archive the instance at point.
Stops the process but preserves the session ID for later resurrection."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (if (eq (magnus-instance-status instance) 'purged)
          (user-error "Instance '%s' is already archived"
                      (magnus-instance-name instance))
        (when (yes-or-no-p (format "Archive instance '%s'? "
                                   (magnus-instance-name instance)))
          (magnus-process-archive instance)
          (magnus-status-refresh)
          (message "Archived '%s' — resurrect with R"
                   (magnus-instance-name instance))))
    (magnus-status--selection-error "an agent row" "archive it")))

(defun magnus-status-resurrect-purged ()
  "Resurrect the purged instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (if (eq (magnus-instance-status instance) 'purged)
          (progn
            (magnus-process-resurrect-purged instance)
            (magnus-status-refresh)
            (message "Resurrected '%s'" (magnus-instance-name instance)))
        (user-error "Instance '%s' is not archived"
                    (magnus-instance-name instance)))
    (magnus-status--selection-error
     "an archived agent row" "resurrect it")))

(defun magnus-status--rename-archived-instance (instance new-name)
  "Transactionally rename archived INSTANCE to NEW-NAME.
Move its project-local memory home before publishing the new registry name.
If either the registry notification or home migration fails, restore both."
  (unless (eq (magnus-instance-status instance) 'purged)
    (user-error "Archive %s before renaming it"
                (magnus-instance-name instance)))
  (magnus-instances--validate-name new-name)
  (magnus-instances--ensure-name-available
   (magnus-instance-directory instance) new-name instance)
  (let* ((old-name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (old-home
          (directory-file-name
           (file-name-directory
            (expand-file-name
             (magnus-onboarding-memory-relative-path old-name) directory))))
         (new-home
          (directory-file-name
           (file-name-directory
            (expand-file-name
             (magnus-onboarding-memory-relative-path new-name) directory))))
         moved)
    (unless (string= old-name new-name)
      (when (or (file-exists-p new-home) (file-symlink-p new-home))
        (user-error "Agent home already exists for %s" new-name))
      (unwind-protect
          (condition-case err
              (progn
                (when (or (file-exists-p old-home)
                          (file-symlink-p old-home))
                  (make-directory (file-name-directory new-home) t)
                  (rename-file old-home new-home)
                  (setq moved t))
                (setf (magnus-instance-name instance) new-name)
                (run-hooks 'magnus-instances-changed-hook)
                ;; During a normal initialized session, make the home move and
                ;; registry rename durable before reporting success.  The
                ;; persistence writer is atomic, so a failure can still roll
                ;; the in-memory and filesystem halves back together.
                (when (and (boundp 'magnus-persistence--autosave-active)
                           magnus-persistence--autosave-active
                           (fboundp 'magnus-persistence-save))
                  (magnus-persistence-save)))
            (error
             (setf (magnus-instance-name instance) old-name)
             (when moved
               (condition-case rollback-error
                   (rename-file new-home old-home)
                 (error
                  (error "Rename failed (%s); home rollback also failed (%s)"
                         (error-message-string err)
                         (error-message-string rollback-error)))))
             ;; Observers may have seen the tentative name before one failed.
             ;; Best-effort notification restores their final projection.
             (condition-case observer-error
                 (run-hooks 'magnus-instances-changed-hook)
               (error
                (message "Magnus: rename rollback observer failed: %s"
                         (error-message-string observer-error))))
             (signal (car err) (cdr err))))
        ;; Nothing should clean either path: the transaction owns a rename,
        ;; not newly allocated data.
        nil))
    instance))

(defun magnus-status-rename ()
  "Rename the archived instance at point, preserving its memory home."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (progn
        (unless (eq (magnus-instance-status instance) 'purged)
          (user-error "Archive %s before renaming it"
                      (magnus-instance-name instance)))
        (let* ((old-name (magnus-instance-name instance))
               (new-name (read-string "New name: " old-name)))
          (unless (or (string-empty-p new-name) (string= old-name new-name))
            (magnus-status--rename-archived-instance instance new-name)
            (magnus-status-refresh)
            (message "Renamed '%s' to '%s'" old-name new-name))))
    (magnus-status--selection-error
     "an archived agent row" "rename it")))

(defun magnus-status-context ()
  "Open the shared context buffer for the current project."
  (interactive)
  (magnus-context))

(defun magnus-status--coordination-directory ()
  "Return the project directory selected by the current status context."
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (magnus-instance-directory instance)
    (or (get-text-property (point) 'magnus-project-directory)
        (if-let ((first-instance (car (magnus-instances-list))))
            (magnus-instance-directory first-instance)
          (user-error "No instances to get project directory from")))))

(defun magnus-status-coordination ()
  "Open the shared coordination file for the current project."
  (interactive)
  (magnus-coord-open (magnus-status--coordination-directory)))

(defun magnus-status-suspend ()
  "Suspend the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (if (magnus-process-suspended-p instance)
          (user-error "Instance '%s' is already suspended"
                     (magnus-instance-name instance))
        (magnus-process-suspend instance)
        (magnus-status-refresh))
    (magnus-status--selection-error "an agent row" "suspend it")))

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
    (magnus-status--selection-error "an agent row" "resume it")))

(defun magnus-status-trace ()
  "Open the thinking trace for the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (magnus-process-trace instance)
    (magnus-status--selection-error
     "an agent row" "open its thinking trace")))

(defun magnus-status-send-message ()
  "Send a message to the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (let ((msg (read-string (format "Message to %s: "
                                      (magnus-instance-name instance)))))
        (unless (string-empty-p msg)
          (magnus-coord-nudge-agent instance msg)
          (message "Sent to %s" (magnus-instance-name instance))))
    (magnus-status--selection-error "an agent row" "send it a message")))

(defun magnus-status-chdir ()
  "Change the working directory of the instance at point."
  (interactive)
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (let* ((new-dir (read-directory-name "New directory: " nil nil t)))
        (magnus-process-chdir instance new-dir)
        (magnus-status-refresh))
    (magnus-status--selection-error
     "an agent row" "change its working directory")))

(defun magnus-status-archive-all ()
  "Archive all active instances."
  (interactive)
  (let* ((active (magnus-instances-active-list))
         (count (length active)))
    (if (zerop count)
        (user-error "No active instances to archive")
      (when (yes-or-no-p (format "Archive all %d instance%s? "
                                 count (if (= count 1) "" "s")))
        (dolist (instance active)
          (magnus-process-archive instance))
        (magnus-status-refresh)
        (message "Archived %d instance%s" count (if (= count 1) "" "s"))))))

(provide 'magnus-status)
;;; magnus-status.el ends here
