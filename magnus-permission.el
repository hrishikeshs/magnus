;;; magnus-permission.el --- Hook-based permission handling -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module handles Claude Code PermissionRequest hooks.  When CC
;; needs a tool permission, an inline bash command (configured in
;; ~/.claude/settings.json) notifies Emacs via emacsclient.  This
;; module receives the notification, checks auto-approve rules, and
;; either responds immediately or SKIPs the hook and switches the
;; user to the agent's vterm so they can use CC's native dialog.
;;
;; The command buffer is a pure log/awareness layer — it shows what
;; happened but doesn't handle y/n responses.  The user interacts
;; with CC's native prompt dialog which has better UX (allow always,
;; full context, interactive questions, etc.).
;;
;; Call `magnus-permission-ensure-hook' on startup to auto-configure
;; the hook in CC settings.  No external script files needed.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'magnus-instances)

(declare-function magnus-command--add-event "magnus-command")
(declare-function magnus-attention--is-prompt-line-p "magnus-attention")
(declare-function magnus-attention--tail-text "magnus-attention")

(defvar magnus-command-show-auto-approved)
(defvar magnus-command-buffer-name)

;;; Customization

(defcustom magnus-permission-auto-approve-tools
  '("Read" "Glob" "Grep" "LSP")
  "Tool names to auto-approve without prompting the user."
  :type '(repeat string)
  :group 'magnus)

(defcustom magnus-permission-auto-approve-bash-patterns
  '("^git " "^cd " "^ls " "^mkdir " "^cat " "^head " "^tail "
    "^wc " "^find " "^grep " "^rg "
    "^npm test" "^npm run" "^npx "
    "^cargo test" "^cargo build" "^cargo check"
    "^make" "^python " "^pytest" "^go test" "^go build"
    "^emacs --batch")
  "Regexps matched against the Bash/Zsh command string for auto-approval.
When a Bash or Zsh tool request matches any of these, it is
auto-approved without prompting."
  :type '(repeat regexp)
  :group 'magnus)

;;; Switch-to-vterm state

(defvar magnus-permission--return-buffer nil
  "Buffer to switch back to after the user handles a prompt in vterm.")

(defvar magnus-permission--return-timer nil
  "1-second poll timer watching for the vterm prompt to resolve.")

(defvar magnus-permission--return-instance nil
  "Instance whose vterm we switched to, or nil.")

(defvar magnus-permission--return-event nil
  "The `vterm-prompt' event logged when we switched, for marking handled.")

(defvar magnus-permission--return-state nil
  "State: nil, `waiting' (prompt not visible), `active' (visible).")

(defvar magnus-permission--return-start nil
  "Float-time when the switch happened, for timeout.")

(defvar magnus-permission--return-interval 1
  "Current poll interval in seconds.  Grows via exponential backoff.")

;;; Core: hook entry point

(defun magnus-permission-notify (request-file response-file)
  "Handle a PermissionRequest from Claude Code.
REQUEST-FILE contains the hook JSON.  RESPONSE-FILE is where the
decision must be written.  Called by emacsclient from the hook script.

Three outcomes:
  1. Not a Magnus instance → SKIP (CC shows normal dialog)
  2. Auto-approve match → allow via hook response
  3. Everything else → SKIP + switch to agent's vterm"
  (condition-case err
      (let* ((json-string (with-temp-buffer
                            (insert-file-contents request-file)
                            (buffer-string)))
             (request (json-parse-string json-string :object-type 'alist))
             (tool-name (alist-get 'tool_name request))
             (tool-input (alist-get 'tool_input request))
             (session-id (alist-get 'session_id request))
             (instance (magnus-permission--find-instance session-id)))
        (if (null instance)
            ;; Not a Magnus-managed instance — skip so CC shows normal dialog
            (with-temp-file response-file
              (insert "SKIP"))
          (if (magnus-permission--auto-approve-p tool-name tool-input)
              ;; Auto-approve: write response immediately
              (progn
                (magnus-permission--write-response response-file "allow")
                (magnus-permission--log-auto-approved instance tool-name tool-input))
            ;; Need user input: SKIP hook and switch to agent's vterm
            (with-temp-file response-file
              (insert "SKIP"))
            (magnus-permission--switch-to-vterm instance tool-name tool-input))))
    (error
     (message "Magnus permission error: %s" (error-message-string err))
     ;; Write SKIP on error so CC falls back to normal dialog
     (condition-case nil
         (with-temp-file response-file
           (insert "SKIP"))
       (error nil))))
  nil)

;;; Instance lookup

(defun magnus-permission--find-instance (session-id)
  "Find the Magnus instance matching SESSION-ID, or nil."
  (cl-find-if (lambda (inst)
                (equal (magnus-instance-session-id inst) session-id))
              (magnus-instances-list)))

;;; Auto-approve

(defun magnus-permission--auto-approve-p (tool-name tool-input)
  "Return non-nil if TOOL-NAME with TOOL-INPUT should be auto-approved."
  (or
   ;; Exact tool match
   (member tool-name magnus-permission-auto-approve-tools)
   ;; Bash/Zsh command pattern match
   (and (or (equal tool-name "Bash") (equal tool-name "Zsh"))
        (let ((command (alist-get 'command tool-input)))
          (when (stringp command)
            (cl-some (lambda (pattern)
                       (string-match-p pattern command))
                     magnus-permission-auto-approve-bash-patterns))))))

;;; Response writing

(defun magnus-permission--write-response (response-file behavior &optional message)
  "Write a CC hook decision to RESPONSE-FILE.
BEHAVIOR is \"allow\" or \"deny\".  MESSAGE is optional denial reason."
  (let* ((decision (if message
                       `((behavior . ,behavior) (message . ,message))
                     `((behavior . ,behavior))))
         (output `((hookSpecificOutput
                    . ((hookEventName . "PermissionRequest")
                       (decision . ,decision))))))
    (with-temp-file response-file
      (insert (json-encode output)))))

;;; Command buffer logging

(defun magnus-permission--log-auto-approved (instance tool-name tool-input)
  "Log an auto-approved event in the command buffer."
  (when-let ((buf (and (bound-and-true-p magnus-command-show-auto-approved)
                       (get-buffer (or (bound-and-true-p magnus-command-buffer-name)
                                       "*magnus-command*")))))
    (let* ((id (when instance (magnus-instance-id instance)))
           (name (if instance (magnus-instance-name instance) "unknown"))
           (summary (magnus-permission--format-tool-summary tool-name tool-input)))
      (with-current-buffer buf
        (magnus-command--add-event
         (list :type 'auto-approved
               :timestamp (float-time)
               :instance-id id
               :instance-name name
               :text summary
               :handled t))))))

(defun magnus-permission--log-vterm-prompt (instance tool-name tool-input)
  "Log a vterm-prompt event in the command buffer.
Returns the event plist so we can mark it handled later."
  (let* ((id (when instance (magnus-instance-id instance)))
         (name (if instance (magnus-instance-name instance) "unknown"))
         (event (list :type 'vterm-prompt
                      :timestamp (float-time)
                      :instance-id id
                      :instance-name name
                      :tool-name tool-name
                      :tool-input tool-input
                      :handled nil)))
    (when-let ((buf (get-buffer (or (bound-and-true-p magnus-command-buffer-name)
                                    "*magnus-command*"))))
      (with-current-buffer buf
        (magnus-command--add-event event)))
    event))

;;; Switch-to-vterm mechanism

(defun magnus-permission--switch-to-vterm (instance tool-name tool-input)
  "Switch to INSTANCE's vterm so the user can handle the prompt natively.
Logs a `vterm-prompt' event and starts a return timer."
  ;; Cancel any existing return timer
  (magnus-permission--cancel-return-timer)
  ;; Log the event
  (let ((event (magnus-permission--log-vterm-prompt instance tool-name tool-input)))
    ;; Save return state
    (setq magnus-permission--return-buffer (current-buffer)
          magnus-permission--return-instance instance
          magnus-permission--return-event event
          magnus-permission--return-state 'waiting
          magnus-permission--return-start (float-time)
          magnus-permission--return-interval 1)
    ;; Switch to vterm
    (when-let ((buffer (magnus-instance-buffer instance)))
      (when (buffer-live-p buffer)
        (pop-to-buffer buffer)))
    ;; Start polling (one-shot timer, re-scheduled with backoff)
    (magnus-permission--schedule-tick)))

(defun magnus-permission--schedule-tick ()
  "Schedule the next return-tick after the current interval."
  (setq magnus-permission--return-timer
        (run-with-timer magnus-permission--return-interval nil
                        #'magnus-permission--return-tick)))

(defun magnus-permission--return-tick ()
  "Poll callback: watch for the vterm prompt to appear then disappear.
State machine:
  `waiting' — prompt not yet visible in vterm → transition to `active'
  `active'  — prompt visible → when it disappears, switch back

Uses narrow anchor patterns (e.g. [y/n], Esc to cancel) rather than
the broad `magnus-attention-patterns' to avoid false positives from
CC's normal output.  Exponential backoff: 1s → 2s → 4s → ... → 30s max."
  (condition-case nil
      (let* ((instance magnus-permission--return-instance)
             (buffer (when instance (magnus-instance-buffer instance)))
             (has-prompt (and buffer (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (let ((tail (magnus-attention--tail-text)))
                                  (and tail
                                       (magnus-attention--is-prompt-line-p tail)))))))
        (pcase magnus-permission--return-state
          ('waiting
           (if has-prompt
               ;; Prompt appeared — transition to active, reset interval
               (setq magnus-permission--return-state 'active
                     magnus-permission--return-interval 1)
             ;; Check timeout (120s) — prompt never appeared
             (when (> (- (float-time) magnus-permission--return-start) 120)
               (magnus-permission--cancel-return-timer)
               (cl-return-from magnus-permission--return-tick)))
           (magnus-permission--backoff-and-reschedule))
          ('active
           (if (not has-prompt)
               ;; Prompt resolved — switch back
               (magnus-permission--do-return)
             (magnus-permission--backoff-and-reschedule)))
          (_ (magnus-permission--cancel-return-timer))))
    (error (magnus-permission--cancel-return-timer))))

(defun magnus-permission--backoff-and-reschedule ()
  "Increase the poll interval (max 30s) and schedule next tick."
  (setq magnus-permission--return-interval
        (min 30 (* 2 magnus-permission--return-interval)))
  (magnus-permission--schedule-tick))

(defun magnus-permission--do-return ()
  "Switch back to the return buffer and clean up."
  ;; Mark the event as handled
  (when magnus-permission--return-event
    (plist-put magnus-permission--return-event :handled t))
  ;; Switch back
  (when (and magnus-permission--return-buffer
             (buffer-live-p magnus-permission--return-buffer))
    (pop-to-buffer magnus-permission--return-buffer))
  (magnus-permission--cancel-return-timer))

(defun magnus-permission--cancel-return-timer ()
  "Cancel the return timer and clear state."
  (when magnus-permission--return-timer
    (cancel-timer magnus-permission--return-timer))
  (setq magnus-permission--return-timer nil
        magnus-permission--return-instance nil
        magnus-permission--return-event nil
        magnus-permission--return-state nil
        magnus-permission--return-start nil
        magnus-permission--return-buffer nil
        magnus-permission--return-interval 1))

;;; Formatting helpers

(defun magnus-permission--format-tool-summary (tool-name tool-input)
  "Format a one-line summary of TOOL-NAME with TOOL-INPUT."
  (pcase tool-name
    ((or "Bash" "Zsh")
     (let ((cmd (alist-get 'command tool-input)))
       (if cmd (format "%s: %s" tool-name cmd) tool-name)))
    ((or "Edit" "Write" "Read")
     (let ((file (or (alist-get 'file_path tool-input)
                     (alist-get 'filePath tool-input))))
       (if file (format "%s: %s" tool-name file) tool-name)))
    ("Glob"
     (let ((pattern (alist-get 'pattern tool-input)))
       (if pattern (format "Glob: %s" pattern) "Glob")))
    ("Grep"
     (let ((pattern (alist-get 'pattern tool-input)))
       (if pattern (format "Grep: %s" pattern) "Grep")))
    ("AskUserQuestion"
     (let* ((questions (alist-get 'questions tool-input))
            (first-q (and (vectorp questions) (> (length questions) 0)
                          (aref questions 0)))
            (text (and first-q (alist-get 'question first-q))))
       (if text (format "Question: %s" (truncate-string-to-width text 60 nil nil "..."))
         "AskUserQuestion")))
    ("Task"
     (let ((desc (alist-get 'description tool-input)))
       (if desc (format "Task: %s" (truncate-string-to-width desc 60 nil nil "..."))
         "Task")))
    ("WebFetch"
     (let ((url (alist-get 'url tool-input)))
       (if url (format "WebFetch: %s" url) "WebFetch")))
    ("WebSearch"
     (let ((query (alist-get 'query tool-input)))
       (if query (format "WebSearch: %s" query) "WebSearch")))
    (_ tool-name)))

(defun magnus-permission--format-tool-detail (tool-name tool-input)
  "Format multi-line detail of TOOL-NAME with TOOL-INPUT for display."
  (let ((lines (list (format "  Tool: %s" tool-name))))
    (pcase tool-name
      ((or "Bash" "Zsh")
       (when-let ((cmd (alist-get 'command tool-input)))
         (push (format "  Command: %s" cmd) lines))
       (when-let ((desc (alist-get 'description tool-input)))
         (push (format "  Description: %s" desc) lines)))
      ((or "Edit" "Write")
       (when-let ((file (or (alist-get 'file_path tool-input)
                            (alist-get 'filePath tool-input))))
         (push (format "  File: %s" file) lines))
       (when-let ((content (alist-get 'content tool-input)))
         (let ((preview (if (> (length content) 200)
                            (concat (substring content 0 200) "...")
                          content)))
           (push (format "  Content: %s" preview) lines))))
      ("Read"
       (when-let ((file (or (alist-get 'file_path tool-input)
                            (alist-get 'filePath tool-input))))
         (push (format "  File: %s" file) lines)))
      ("AskUserQuestion"
       (when-let ((questions (alist-get 'questions tool-input)))
         (seq-doseq (q (if (vectorp questions) questions (vector questions)))
           (when-let ((text (alist-get 'question q)))
             (push (format "  Question: %s" text) lines))
           (when-let ((options (alist-get 'options q)))
             (let ((n 1))
               (seq-doseq (opt options)
                 (let ((label (alist-get 'label opt))
                       (desc (alist-get 'description opt)))
                   (push (format "    %d. %s%s" n (or label "")
                                 (if desc (format " — %s" desc) ""))
                         lines))
                 (setq n (1+ n))))))))
      ("Task"
       (when-let ((desc (alist-get 'description tool-input)))
         (push (format "  Description: %s" desc) lines))
       (when-let ((st (alist-get 'subagent_type tool-input)))
         (push (format "  Agent: %s" st) lines))
       (when-let ((prompt (alist-get 'prompt tool-input)))
         (push (format "  Prompt: %s"
                       (if (> (length prompt) 200)
                           (concat (substring prompt 0 200) "...")
                         prompt))
               lines)))
      ("WebFetch"
       (when-let ((url (alist-get 'url tool-input)))
         (push (format "  URL: %s" url) lines))
       (when-let ((prompt (alist-get 'prompt tool-input)))
         (push (format "  Prompt: %s" prompt) lines)))
      ("WebSearch"
       (when-let ((query (alist-get 'query tool-input)))
         (push (format "  Query: %s" query) lines)))
      (_
       ;; Generic: show all input keys with string/number values
       (when (listp tool-input)
         (dolist (pair tool-input)
           (when (consp pair)
             (let* ((val (cdr pair))
                    (val-str (cond ((stringp val) val)
                                   ((numberp val) (number-to-string val))
                                   (t nil))))
               (when val-str
                 (push (format "  %s: %s"
                               (car pair)
                               (if (> (length val-str) 100)
                                   (concat (substring val-str 0 100) "...")
                                 val-str))
                       lines))))))))
    (mapconcat #'identity (nreverse lines) "\n")))

;;; Cleanup

(defun magnus-permission-cleanup ()
  "Cancel any active return timer.
Called when the command buffer is killed."
  (magnus-permission--cancel-return-timer))

;;; Hook command (inline — no external script file needed)

(defconst magnus-permission--hook-command
  (concat
   "bash -c '"
   "F=$(mktemp /tmp/mp-XXXXXX); R=${F}.resp; cat>$F; "
   "trap \"rm -f $F $R\" EXIT; "
   "emacsclient -n --eval "
   "\"(magnus-permission-notify \\\"$F\\\" \\\"$R\\\")\" "
   ">/dev/null 2>&1 || exit 1; "
   "for i in $(seq 1 1200); do "
   "[ -f \"$R\" ] && { "
   "head -1 \"$R\" | grep -q \"^SKIP$\" && exit 1; "
   "cat \"$R\"; exit 0; }; sleep 0.5; "
   "done; exit 1'")
  "Inline bash command for the CC PermissionRequest hook.
Reads JSON from stdin, notifies Emacs via emacsclient, polls for
response.  Falls back to normal CC dialog if Emacs is unavailable.")

;;; Setup

(defun magnus-permission-ensure-hook ()
  "Ensure the PermissionRequest hook is configured in CC settings.
Idempotent — safe to call on every Magnus startup."
  (let* ((settings-file (expand-file-name "~/.claude/settings.json"))
         (settings (if (file-exists-p settings-file)
                       (with-temp-buffer
                         (insert-file-contents settings-file)
                         (json-parse-string (buffer-string)
                                            :object-type 'hash-table
                                            :false-object :json-false
                                            :null-object :json-null))
                     (make-hash-table :test 'equal)))
         (hooks (or (gethash "hooks" settings)
                    (let ((h (make-hash-table :test 'equal)))
                      (puthash "hooks" h settings)
                      h))))
    ;; Check if already configured with our command
    (let ((existing (gethash "PermissionRequest" hooks)))
      (unless (and existing (magnus-permission--hook-matches-p existing))
        ;; Install or update
        (let ((perm-hooks
               (vector
                (let ((h (make-hash-table :test 'equal)))
                  (puthash "hooks"
                           (vector
                            (let ((inner (make-hash-table :test 'equal)))
                              (puthash "type" "command" inner)
                              (puthash "command"
                                       magnus-permission--hook-command inner)
                              (puthash "timeout" 600 inner)
                              inner))
                           h)
                  h))))
          (puthash "PermissionRequest" perm-hooks hooks)
          (with-temp-file settings-file
            (insert (json-encode settings)))
          (message "Magnus: PermissionRequest hook configured"))))))

(defun magnus-permission--hook-matches-p (hook-config)
  "Return non-nil if HOOK-CONFIG already has our exact magnus hook command."
  (condition-case nil
      (let* ((entry (aref hook-config 0))
             (inner-hooks (gethash "hooks" entry))
             (cmd-entry (aref inner-hooks 0))
             (command (gethash "command" cmd-entry)))
        (and command (string= command magnus-permission--hook-command)))
    (error nil)))

(defun magnus-permission-setup ()
  "Interactively set up CC PermissionRequest hooks for Magnus."
  (interactive)
  (magnus-permission-ensure-hook)
  (message "Magnus: PermissionRequest hook ready.  Restart CC instances to activate."))

(provide 'magnus-permission)
;;; magnus-permission.el ends here
