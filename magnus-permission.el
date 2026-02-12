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
;; either responds immediately or creates a prompt in the command
;; buffer for the user to decide.
;;
;; Call `magnus-permission-ensure-hook' on startup to auto-configure
;; the hook in CC settings.  No external script files needed.

;;; Code:

(require 'json)
(require 'magnus-instances)

(declare-function magnus-command--add-event "magnus-command")
(declare-function magnus-command--activate-next "magnus-command")

(defvar magnus-command--active-prompt)
(defvar magnus-command--prompt-queue)
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

;;; State

(defvar magnus-permission--pending (make-hash-table :test 'equal)
  "Pending permission requests.
Hash table: request-file -> plist with keys:
  :response-file  STRING  path to write the decision
  :instance       STRUCT  magnus-instance or nil
  :tool-name      STRING
  :tool-input     ALIST
  :session-id     STRING
  :timestamp      FLOAT")

;;; Core: hook entry point

(defun magnus-permission-notify (request-file response-file)
  "Handle a PermissionRequest from Claude Code.
REQUEST-FILE contains the hook JSON.  RESPONSE-FILE is where the
decision must be written.  Called by emacsclient from the hook script."
  (condition-case err
      (let* ((json-string (with-temp-buffer
                            (insert-file-contents request-file)
                            (buffer-string)))
             (request (json-parse-string json-string :object-type 'alist))
             (tool-name (alist-get 'tool_name request))
             (tool-input (alist-get 'tool_input request))
             (session-id (alist-get 'session_id request))
             (instance (magnus-permission--find-instance session-id)))
        (if (magnus-permission--auto-approve-p tool-name tool-input)
            ;; Auto-approve: write response immediately
            (progn
              (magnus-permission--write-response response-file "allow")
              (magnus-permission--log-auto-approved instance tool-name tool-input))
          ;; Need user input: queue in command buffer
          (let ((plist (list :response-file response-file
                             :instance instance
                             :tool-name tool-name
                             :tool-input tool-input
                             :session-id session-id
                             :timestamp (float-time))))
            (puthash request-file plist magnus-permission--pending)
            (magnus-permission--create-prompt instance tool-name tool-input
                                              request-file response-file))))
    (error
     (message "Magnus permission error: %s" (error-message-string err))
     ;; Write deny on error so the hook script doesn't hang
     (condition-case nil
         (magnus-permission--write-response response-file "deny"
                                            (error-message-string err))
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

;;; Command buffer integration

(defun magnus-permission--create-prompt (instance tool-name tool-input
                                                  request-file response-file)
  "Create a permission-request event in the command buffer."
  (when-let ((buf (get-buffer (or (bound-and-true-p magnus-command-buffer-name)
                                  "*magnus-command*"))))
    (with-current-buffer buf
      (let* ((id (when instance (magnus-instance-id instance)))
             (name (if instance
                       (magnus-instance-name instance)
                     "unknown"))
             (event (list :type 'permission-request
                          :timestamp (float-time)
                          :instance-id id
                          :instance-name name
                          :tool-name tool-name
                          :tool-input tool-input
                          :request-file request-file
                          :response-file response-file
                          :handled nil
                          :response nil)))
        (magnus-command--add-event event)
        (setq magnus-command--prompt-queue
              (append magnus-command--prompt-queue (list event)))
        (unless magnus-command--active-prompt
          (magnus-command--activate-next))))))

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
      (_
       ;; Generic: show all input keys
       (when (listp tool-input)
         (dolist (pair tool-input)
           (when (and (consp pair) (stringp (cdr pair)))
             (push (format "  %s: %s"
                           (car pair)
                           (if (> (length (cdr pair)) 100)
                               (concat (substring (cdr pair) 0 100) "...")
                             (cdr pair)))
                   lines))))))
    (mapconcat #'identity (nreverse lines) "\n")))

;;; Respond to a pending request

(defun magnus-permission-respond (request-file behavior)
  "Respond to the pending request identified by REQUEST-FILE.
BEHAVIOR is \"allow\" or \"deny\"."
  (when-let ((plist (gethash request-file magnus-permission--pending)))
    (let ((response-file (plist-get plist :response-file)))
      (magnus-permission--write-response
       response-file behavior
       (when (equal behavior "deny") "Denied by user via Magnus"))
      (remhash request-file magnus-permission--pending))))

;;; Cleanup

(defun magnus-permission-cleanup-pending ()
  "Deny all pending permission requests.
Called when the command buffer is killed to avoid hanging hook scripts."
  (maphash (lambda (_request-file plist)
             (condition-case nil
                 (magnus-permission--write-response
                  (plist-get plist :response-file) "deny"
                  "Magnus command buffer closed")
               (error nil)))
           magnus-permission--pending)
  (clrhash magnus-permission--pending))

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
   "[ -f \"$R\" ] && { cat \"$R\"; exit 0; }; sleep 0.5; "
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
  "Return non-nil if HOOK-CONFIG already has our magnus hook."
  (condition-case nil
      (let* ((entry (aref hook-config 0))
             (inner-hooks (gethash "hooks" entry))
             (cmd-entry (aref inner-hooks 0))
             (command (gethash "command" cmd-entry)))
        (and command (string-match-p "magnus-permission-notify" command)))
    (error nil)))

(defun magnus-permission-setup ()
  "Interactively set up CC PermissionRequest hooks for Magnus."
  (interactive)
  (magnus-permission-ensure-hook)
  (message "Magnus: PermissionRequest hook ready.  Restart CC instances to activate."))

(provide 'magnus-permission)
;;; magnus-permission.el ends here
