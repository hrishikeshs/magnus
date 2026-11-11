;;; magnus-provider-codex.el --- Native Codex TUI provider for Magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Opt-in Codex support using the native Codex TUI as the single owner of each
;; session.  Magnus launches Codex directly in vterm, delivers coordination
;; messages through that terminal, and discovers new session IDs from Codex's
;; local rollout records so archived agents can be resumed later.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'magnus-environment)
(require 'magnus-instances)
(require 'magnus-onboarding)
(require 'magnus-provider)
(require 'magnus-terminal)

(declare-function magnus-status-refresh "magnus-status")
(declare-function vterm-send-key "vterm" (key &optional shift meta ctrl))
(declare-function vterm-send-return "vterm" ())
(declare-function vterm-send-string "vterm" (string &optional paste-p))
(defvar magnus-buffer-name)
(defvar magnus-process-ready-hook nil)
(defvar magnus-process--transaction-runtime-buffer nil)

(defcustom magnus-codex-executable "codex"
  "Codex executable or command prefix used for native TUI sessions.
The value may include established command-line arguments."
  :type 'string
  :group 'magnus)

(defcustom magnus-codex-extra-developer-instructions nil
  "Optional extra instructions for Magnus-managed Codex sessions."
  :type '(choice (const :tag "None" nil) string)
  :group 'magnus)

(defcustom magnus-codex-tui-ready-delay 1.0
  "Seconds after launch before queued input may be sent to Codex.
This protects the shell command and initial prompt from coordination messages
arriving during startup."
  :type 'number
  :group 'magnus)

(defcustom magnus-codex-session-capture-timeout 30
  "Seconds to wait for a new Codex TUI to record its session ID."
  :type 'number
  :group 'magnus)

(defun magnus-codex--command-prefix ()
  "Return configured Codex command as a nonempty argv prefix."
  (magnus-environment-command-prefix magnus-codex-executable "Codex"))

(defconst magnus-codex--session-scan-limit (* 1024 1024)
  "Maximum rollout bytes inspected while identifying a new session.")

(defconst magnus-codex--metadata-scan-limit (* 64 1024)
  "Maximum rollout bytes inspected while reading session metadata.")

(defvar magnus-codex--session-file-cache (make-hash-table :test #'equal)
  "Codex session-root/session-ID keys to captured root rollout paths.")

(defvar magnus-codex--trace-file-cache (make-hash-table :test #'equal)
  "Codex session-root/session-ID keys to active trace rollout state.")

(defvar-local magnus-codex--instance nil
  "Magnus instance represented by the current Codex TUI buffer.")

(defun magnus-codex--tui-process (instance)
  "Return INSTANCE's live vterm process, or nil."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (and (buffer-live-p buffer)
         (let ((process (get-buffer-process buffer)))
           (and (process-live-p process) process)))))

(defun magnus-codex--current-process-p (process instance)
  "Return non-nil when PROCESS is INSTANCE's current live terminal."
  (eq process (magnus-codex--tui-process instance)))

(defun magnus-codex--cancel-timer (process property)
  "Cancel PROCESS timer stored under PROPERTY, if any."
  (when-let ((timer (process-get process property)))
    (cancel-timer timer)
    (process-put process property nil)))

(defun magnus-codex--session-root ()
  "Return the root containing Codex rollout records."
  (expand-file-name
   "sessions" (or (getenv "CODEX_HOME") (expand-file-name ".codex" "~"))))

(defun magnus-codex--session-directories ()
  "Return possible recent local and UTC Codex session directories."
  (let ((root (magnus-codex--session-root))
        (now (current-time)))
    (mapcar (lambda (date) (expand-file-name date root))
            (delete-dups
             (cl-loop for time in (list now (time-subtract now 86400))
                      append (list (format-time-string "%Y/%m/%d" time)
                                   (format-time-string "%Y/%m/%d" time t)))))))

(defun magnus-codex--session-files ()
  "Return current Codex rollout files that may belong to a new launch."
  (apply
   #'append
   (mapcar
    (lambda (directory)
      (when (file-directory-p directory)
        (directory-files directory t "\\`rollout-.*\\.jsonl\\'")))
    (magnus-codex--session-directories))))

(defun magnus-codex--read-json-line ()
  "Read the JSON object on the current line, returning nil when incomplete."
  (condition-case nil
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol)
            (json-false nil)
            (json-null nil))
        (json-read-from-string
         (buffer-substring-no-properties
          (line-beginning-position) (line-end-position))))
    (error
     ;; Rollout lines may be observed before Codex finishes writing them.
     nil)))

(defun magnus-codex--same-directory-p (first second)
  "Return non-nil when FIRST and SECOND name the same directory."
  (when (and (stringp first) (stringp second))
    (condition-case nil
        (equal (file-truename first) (file-truename second))
      (file-error
       (equal (directory-file-name (expand-file-name first))
              (directory-file-name (expand-file-name second)))))))

(defun magnus-codex--rollout-session-id (file)
  "Return the stable session ID from FILE's first metadata record."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file nil 0 magnus-codex--metadata-scan-limit)
        (goto-char (point-min))
        (let* ((metadata (magnus-codex--read-json-line))
               (payload (and (equal (alist-get 'type metadata) "session_meta")
                             (alist-get 'payload metadata))))
          (alist-get 'session_id payload)))
    (error
     ;; A newly created continuation may not have a complete first line yet.
     nil)))

(defun magnus-codex--file-modification-time (file)
  "Return FILE's modification time, or the epoch when FILE vanished."
  (condition-case nil
      (or (file-attribute-modification-time (file-attributes file))
          (seconds-to-time 0))
    (file-error (seconds-to-time 0))))

(defun magnus-codex--scan-trace-rollouts (files session-id after)
  "Scan FILES for the newest SESSION-ID rollout modified at or after AFTER.
Return a cons of the matching file and the latest modification time seen."
  (let (matching matching-time (latest-time after))
    (dolist (file files)
      (let ((modified (magnus-codex--file-modification-time file)))
        (when (or (null latest-time) (time-less-p latest-time modified))
          (setq latest-time modified))
        (when (and (or (null after) (not (time-less-p modified after)))
                   (equal (magnus-codex--rollout-session-id file) session-id)
                   (or (null matching-time)
                       (time-less-p matching-time modified)
                       (and (not (time-less-p modified matching-time))
                            (string> file matching))))
          (setq matching file matching-time modified))))
    (cons matching latest-time)))

(defun magnus-codex--session-id-from-file (file directory prompt)
  "Return FILE's top-level Codex session ID for DIRECTORY and PROMPT.
Only the initial metadata and first user message are examined."
  (condition-case nil
      (with-temp-buffer
        ;; The initial prompt is recorded near the beginning of a rollout.
        (insert-file-contents file nil 0 magnus-codex--session-scan-limit)
        (goto-char (point-min))
        (let* ((metadata (magnus-codex--read-json-line))
               (payload (and (equal (alist-get 'type metadata) "session_meta")
                             (alist-get 'payload metadata)))
               (id (alist-get 'id payload))
               (session-id (alist-get 'session_id payload))
               user-message)
          (when (and id
                     (equal id session-id)
                     (null (alist-get 'parent_thread_id payload))
                     (magnus-codex--same-directory-p
                      directory (alist-get 'cwd payload)))
            (forward-line 1)
            (while (and (not user-message) (not (eobp)))
              (let* ((record (magnus-codex--read-json-line))
                     (event (and (equal (alist-get 'type record) "event_msg")
                                 (alist-get 'payload record))))
                (when (equal (alist-get 'type event) "user_message")
                  (setq user-message (alist-get 'message event))))
              (forward-line 1))
            (when (equal user-message prompt)
              (puthash (cons (magnus-codex--session-root) id) file
                       magnus-codex--session-file-cache)
              id))))
    (error
     ;; A concurrently written rollout is retried on the next bounded poll.
     nil)))

(defun magnus-codex--find-session-id (instance prompt files-before)
  "Find INSTANCE's new session for PROMPT, excluding FILES-BEFORE."
  (let ((candidates
         (cl-set-difference (magnus-codex--session-files) files-before
                            :test #'string=)))
    (cl-loop for file in candidates
             thereis (magnus-codex--session-id-from-file
                      file (magnus-instance-directory instance) prompt))))

(defun magnus-codex--finish-session-capture (process)
  "Release session-capture state owned by PROCESS."
  (magnus-codex--cancel-timer process 'magnus-codex-capture-timer)
  (process-put process 'magnus-codex-capture-prompt nil)
  (process-put process 'magnus-codex-files-before nil)
  (process-put process 'magnus-codex-capture-deadline nil))

(defun magnus-codex--poll-session (process)
  "Capture the new Codex session ID owned by PROCESS."
  (let ((instance (process-get process 'magnus-codex-instance)))
    (cond
     ((not (magnus-codex--current-process-p process instance))
      (magnus-codex--finish-session-capture process))
     ((when-let ((session-id
                  (magnus-codex--find-session-id
                   instance
                   (process-get process 'magnus-codex-capture-prompt)
                   (process-get process 'magnus-codex-files-before))))
        (magnus-instances-update instance :session-id session-id)
        (message "Magnus: captured Codex session %s for %s"
                 session-id (magnus-instance-name instance))
        (magnus-codex--finish-session-capture process)
        t))
     ((> (float-time) (process-get process 'magnus-codex-capture-deadline))
      (magnus-codex--finish-session-capture process)
      (message
       "Magnus: could not capture the Codex session for %s; the TUI remains usable"
       (magnus-instance-name instance))))))

(defun magnus-codex--watch-for-session (process instance prompt files-before)
  "Watch PROCESS for INSTANCE's session matching PROMPT.
FILES-BEFORE contains rollout files that predate this launch."
  (process-put process 'magnus-codex-instance instance)
  (process-put process 'magnus-codex-capture-prompt prompt)
  (process-put process 'magnus-codex-files-before files-before)
  (process-put process 'magnus-codex-capture-deadline
               (+ (float-time) magnus-codex-session-capture-timeout))
  (process-put process 'magnus-codex-capture-timer
               (run-at-time 0.5 0.5 #'magnus-codex--poll-session process)))

(defun magnus-codex--instructions (instance)
  "Build Magnus identity and coordination instructions for Codex INSTANCE."
  (concat
   (magnus-onboarding-prompt instance)
   (when magnus-codex-extra-developer-instructions
     (concat "\n\nCodex-specific developer instructions:\n"
             magnus-codex-extra-developer-instructions))))

(defun magnus-codex--launch-marker (instance)
  "Return a unique session-correlation marker for INSTANCE."
  (concat "magnus-session-marker:"
          (secure-hash
           'sha256
           (format "%s:%s:%s" (magnus-instance-id instance)
                   (float-time) (random)))))

(defun magnus-codex--onboarding-prompt (instance initial-message marker)
  "Return first-turn onboarding for INSTANCE, INITIAL-MESSAGE, and MARKER."
  (concat
   (magnus-codex--instructions instance)
   (if initial-message
       (concat "\n\nInitial task from the user:\n" initial-message)
     (concat "\n\nNo separate task was supplied. Complete your orientation, "
             "report that you are ready, and then wait for the user."))
   "\n\nInternal Magnus session marker: " marker
   ". Leave this marker in the session history; do not repeat it in replies."))

(defun magnus-codex--tui-command (instance &optional initial-message)
  "Return the shell command for INSTANCE and optional INITIAL-MESSAGE."
  (let ((session-id (magnus-instance-session-id instance)))
    (mapconcat
     #'shell-quote-argument
     (append (list "exec") (magnus-codex--command-prefix)
             (when session-id (list "resume"))
             (list "-C" (magnus-instance-directory instance))
             (when session-id (list session-id))
             (when initial-message (list initial-message)))
     " ")))

(defun magnus-codex--setup-tui-sentinel (instance buffer)
  "Track INSTANCE's interactive Codex process in BUFFER."
  (when-let ((process (get-buffer-process buffer)))
    (process-put process 'magnus-codex-instance instance)
    (set-process-sentinel
     process
     (lambda (terminal _event)
       (unless (process-live-p terminal)
         (dolist (property '(magnus-codex-capture-timer
                             magnus-codex-ready-timer))
           (magnus-codex--cancel-timer terminal property))
         (magnus-terminal-release-process terminal)
         ;; A replaced vterm may report its exit after a new TUI is running.
         ;; Emacs normally detaches a dead process before its sentinel runs, so
         ;; nil still means this captured runtime exited normally.  A different
         ;; process in the same buffer is a replacement and must win the race.
         (let ((current (and (buffer-live-p buffer)
                             (get-buffer-process buffer))))
           (when (and (eq buffer (magnus-instance-buffer instance))
                      (or (null current) (eq terminal current)))
             (unless (eq (magnus-instance-status instance) 'purged)
               (magnus-instances-update instance :status 'stopped))
             (when (and (boundp 'magnus-buffer-name)
                        (get-buffer magnus-buffer-name))
               (magnus-status-refresh)))))))))

(defun magnus-codex--rollback-tui-spawn
    (instance buffer process command-timer submit-timer)
  "Roll back one failed Codex TUI launch for INSTANCE.
BUFFER and PROCESS are the exact runtime allocated by this launch.  The timer
arguments are startup callbacks which must not survive a synchronous failure.
Never detach or kill a replacement process that has since claimed BUFFER."
  (dolist (timer (list command-timer submit-timer))
    (when (timerp timer)
      (cancel-timer timer)))
  (when (processp process)
    (dolist (property '(magnus-codex-capture-timer
                        magnus-codex-ready-timer))
      (ignore-errors (magnus-codex--cancel-timer process property))))
  (when (processp process)
    (magnus-terminal-release-process process))
  (let* ((current (and (buffer-live-p buffer)
                       (get-buffer-process buffer)))
         (owned (and (bufferp buffer)
                     (eq buffer (magnus-instance-buffer instance))
                     (or (null current) (eq process current)))))
    ;; Detach first so deleting PROCESS cannot let its sentinel publish another
    ;; transition.  `magnus-instances-update' mutates before running hooks, so a
    ;; hook error is diagnostic and must not replace the startup failure.
    (when owned
      (condition-case err
          (magnus-instances-update instance :buffer nil :status 'stopped)
        (error
         (message "Magnus: Codex startup rollback hook failed for %s: %s"
                  (magnus-instance-name instance)
                  (error-message-string err)))))
    (when (processp process)
      (ignore-errors (set-process-query-on-exit-flag process nil))
      (when (process-live-p process)
        (ignore-errors (delete-process process))))
    ;; The buffer belongs to this allocation, but a distinct current process is
    ;; evidence that it has been adopted as a replacement runtime.
    (when (and (buffer-live-p buffer)
               (let ((attached (get-buffer-process buffer)))
                 (or (null attached) (eq attached process))))
      (ignore-errors (kill-buffer buffer)))))

(defun magnus-codex--spawn-tui (instance prompt &optional marker files-before)
  "Launch INSTANCE's native TUI with PROMPT.
When MARKER is non-nil, capture its new session against FILES-BEFORE."
  (let* ((buffer-name (format "*codex:%s*" (magnus-instance-name instance)))
         (default-directory (magnus-instance-directory instance))
         (command (magnus-codex--tui-command instance prompt))
         ;; Finish pure command construction before allocating a terminal so a
         ;; malformed instance cannot strand a vterm buffer.
         buffer
         process
         command-timer
         submit-timer
         started)
    (unwind-protect
        (progn
          (setq buffer
                (magnus-terminal-create-buffer buffer-name))
          (setq process (get-buffer-process buffer))
          ;; Outer lifecycle transactions consume this exact allocation if a
          ;; later step fails after provider startup has returned successfully.
          (when (and (consp magnus-process--transaction-runtime-buffer)
                     (null (car magnus-process--transaction-runtime-buffer)))
            (setcar magnus-process--transaction-runtime-buffer
                    (cons buffer process)))
          (unless (process-live-p process)
            (user-error "Could not start a vterm for Codex instance `%s'"
                        (magnus-instance-name instance)))
          (with-current-buffer buffer
            (setq-local magnus-codex--instance instance))
          (magnus-instances-update instance :buffer buffer :status 'running)
          (magnus-codex--setup-tui-sentinel instance buffer)
          (when marker
            (magnus-codex--watch-for-session
             process instance prompt files-before))
          ;; Give vterm's login shell one tick before replacing it with Codex.
          (setq command-timer
                (run-with-timer
                 0.1 nil
                 (lambda ()
                   (when (magnus-codex--current-process-p process instance)
                     (with-current-buffer buffer
                       (vterm-send-string command))))))
          (setq submit-timer
                (run-with-timer
                 0.5 nil
                 (lambda ()
                   (when (magnus-codex--current-process-p process instance)
                     (with-current-buffer buffer
                       (vterm-send-return))
                     (process-put
                      process 'magnus-codex-ready-timer
                      (run-with-timer
                       magnus-codex-tui-ready-delay nil
                       (lambda ()
                         (when (magnus-codex--current-process-p
                                process instance)
                           (process-put process 'magnus-codex-ready-timer nil)
                           (process-put process 'magnus-codex-ready t)
                           ;; Existing queued input predates durable ready-hook
                           ;; deliveries.  Defer the hook until that queue has
                           ;; actually drained so an automated notice cannot
                           ;; jump ahead of user input.
                           (process-put
                            process 'magnus-codex-ready-hook-pending t)
                           (magnus-terminal-drain process)
                           (magnus-codex--maybe-run-ready-hook process)))))))))
          (when (and (boundp 'magnus-buffer-name)
                     (get-buffer magnus-buffer-name))
            (magnus-status-refresh))
          (setq started t)
          buffer)
      (unless started
        (magnus-codex--rollback-tui-spawn
         instance buffer process command-timer submit-timer)))))

(defun magnus-codex-start (instance &optional initial-message)
  "Start or resume Codex INSTANCE with optional INITIAL-MESSAGE."
  (when (magnus-codex-running-p instance)
    (user-error "Codex instance `%s' is already running"
                (magnus-instance-name instance)))
  (let* ((prefix (magnus-codex--command-prefix))
         (program (car prefix)))
    (unless (or (executable-find program)
                (and (file-name-absolute-p program)
                     (file-executable-p program)))
      (user-error "Cannot find Codex executable: %s" program)))
  (when-let ((old-buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p old-buffer)
      (magnus-terminal--discard-buffer old-buffer)))
  ;; A failed replacement must never leave INSTANCE pointing at its killed old
  ;; terminal or claiming that a runtime is still live.
  (unless (and (null (magnus-instance-buffer instance))
               (eq (magnus-instance-status instance) 'stopped))
    (magnus-instances-update instance :buffer nil :status 'stopped))
  (if (magnus-instance-session-id instance)
      (magnus-codex--spawn-tui instance initial-message)
    (let* ((marker (magnus-codex--launch-marker instance))
           (prompt (magnus-codex--onboarding-prompt
                    instance initial-message marker))
           (files-before (magnus-codex--session-files)))
      (magnus-codex--spawn-tui instance prompt marker files-before))))

(defun magnus-codex--delivery-ready-p (process)
  "Return non-nil when PROCESS's Codex composer accepts automation."
  (process-get process 'magnus-codex-ready))

(defun magnus-codex-send (instance text &optional accepted scope)
  "Queue TEXT for serialized delivery through INSTANCE's native TUI.
When ACCEPTED is non-nil, call it only after bracketed paste and Return have
reached vterm.  SCOPE identifies this delivery for selective cancellation and
defaults to `codex'.  Return `submitted' for immediate submission or `queued'
while the message is waiting for readiness, serialization, or user TUI
ownership."
  (let ((process (magnus-codex--tui-process instance)))
    (unless process
      (user-error "Codex instance `%s' is not running"
                  (magnus-instance-name instance)))
    (magnus-terminal-submit
     instance text accepted
     :ready-p #'magnus-codex--delivery-ready-p
     :settle-delay 0.1
     :idle #'magnus-codex--maybe-run-ready-hook
     :scope (or scope 'codex))))

(defun magnus-codex-try-send (instance text &optional accepted)
  "Try to submit ephemeral TEXT to INSTANCE without pre-queuing it.
Call ACCEPTED only after bracketed paste and Return have both reached vterm.
Return `submitted', `queued', or nil according to
`magnus-terminal-try-submit'."
  (let ((process (magnus-codex--tui-process instance)))
    (unless process
      (user-error "Codex instance `%s' is not running"
                  (magnus-instance-name instance)))
    (magnus-terminal-try-submit
     instance text
     :ready-p #'magnus-codex--delivery-ready-p
     :settle-delay 0.1
     :accepted accepted
     :idle #'magnus-codex--maybe-run-ready-hook)))

(defun magnus-codex--maybe-run-ready-hook (process)
  "Run PROCESS's deferred ready hook once earlier input has drained."
  (let ((instance (process-get process 'magnus-codex-instance)))
    (when (and (process-get process 'magnus-codex-ready-hook-pending)
               (process-get process 'magnus-codex-ready)
               (magnus-terminal-delivery-idle-p process)
               (magnus-codex--current-process-p process instance))
      (process-put process 'magnus-codex-ready-hook-pending nil)
      (condition-case err
          (run-hook-with-args 'magnus-process-ready-hook instance)
        (error
         (message "Magnus: process-ready hook failed for %s: %s"
                  (magnus-instance-name instance)
                  (error-message-string err)))))))

(defun magnus-codex-interrupt (instance)
  "Interrupt Codex INSTANCE through its native TUI."
  (let ((buffer (magnus-instance-buffer instance)))
    (unless (magnus-codex--tui-process instance)
      (user-error "Codex instance `%s' is not running"
                  (magnus-instance-name instance)))
    (with-current-buffer buffer
      (vterm-send-key "C-c"))))

(defun magnus-codex--finish-stop (instance buffer process force)
  "Finish stopping INSTANCE's captured BUFFER and exact PROCESS.
FORCE selects immediate process killing.  A replacement process which has
since claimed BUFFER is left running and remains attached to INSTANCE."
  (when (processp process)
    (set-process-query-on-exit-flag process nil)
    (when (process-live-p process)
      (if force (kill-process process) (delete-process process))))
  ;; Deleting PROCESS can synchronously run its sentinel and lifecycle hooks.
  ;; Resolve ownership only afterwards so a same-buffer replacement wins.
  (when (and (buffer-live-p buffer)
             (let ((attached (get-buffer-process buffer)))
               (or (null attached) (eq attached process))))
    (kill-buffer buffer))
  (when (and (eq buffer (magnus-instance-buffer instance))
             (or (not (buffer-live-p buffer))
                 (let ((attached (get-buffer-process buffer)))
                   (or (null attached) (eq attached process)))))
    (unless (eq (magnus-instance-status instance) 'purged)
      (magnus-instances-update instance :status 'stopped :buffer nil))))

(defun magnus-codex-stop (instance &optional force)
  "Stop Codex INSTANCE's native TUI.
FORCE kills the terminal immediately; otherwise interrupt before closing it."
  (let* ((buffer (magnus-instance-buffer instance))
         (process (magnus-codex--tui-process instance)))
    ;; Give a very short-lived session one final chance to become resumable.
    (when (and process
               (not (magnus-instance-session-id instance))
               (process-get process 'magnus-codex-capture-prompt))
      (magnus-codex--poll-session process))
    (unless (eq (magnus-instance-status instance) 'purged)
      (magnus-instances-update instance :status 'stopped))
    (if (and (buffer-live-p buffer) (process-live-p process))
        (if force
            (magnus-codex--finish-stop instance buffer process t)
          (with-current-buffer buffer
            (vterm-send-key "C-c"))
          (run-with-timer 0.5 nil
                          #'magnus-codex--finish-stop
                          instance buffer process nil))
      (magnus-codex--finish-stop instance buffer process force))))

(defun magnus-codex-running-p (instance)
  "Return non-nil when Codex INSTANCE has a live native TUI."
  (and (magnus-codex--tui-process instance) t))

(defun magnus-codex-switch-to (instance)
  "Switch to Codex INSTANCE, resuming its TUI when necessary."
  (unless (magnus-codex-running-p instance)
    (magnus-codex-start instance))
  (pop-to-buffer (magnus-instance-buffer instance)))

(defun magnus-codex-trace-file (instance)
  "Return the newest rollout JSONL file for Codex INSTANCE, or nil.
Root rollout filenames contain the stable session ID, but continuation
filenames contain a new per-launch ID.  Continuations are therefore matched
through their first `session_meta.session_id' record."
  (when-let ((session-id (magnus-instance-session-id instance)))
    (let* ((root (magnus-codex--session-root))
           (cache-key (cons root session-id))
           (state (gethash cache-key magnus-codex--trace-file-cache))
           (cached (plist-get state :file))
           (scan-time (plist-get state :scan-time)))
      (unless (and cached (file-exists-p cached))
        (setq cached nil scan-time nil)
        (remhash cache-key magnus-codex--trace-file-cache))
      (when (file-directory-p root)
        (let* ((files
                (condition-case nil
                    (if cached
                        ;; A continuation is always created in a current date
                        ;; directory.  Full traversal is only needed once.
                        (magnus-codex--session-files)
                      (directory-files-recursively
                       root "rollout-.*\\.jsonl\\'"))
                  (file-error nil)))
               (root-pattern
                (concat "-" (regexp-quote session-id) "\\.jsonl\\'"))
               (filename-root
                (and (not cached)
                     (cl-find-if
                      (lambda (file) (string-match-p root-pattern file))
                      files)))
               (fallback (or cached filename-root))
               (threshold
                (or scan-time
                    (and fallback
                         (magnus-codex--file-modification-time fallback))))
               (scan (magnus-codex--scan-trace-rollouts
                      files session-id threshold))
               (matching (car scan))
               (selected (or matching fallback))
               (latest-time (cdr scan)))
          (if selected
              (progn
                (puthash cache-key
                         (list :file selected :scan-time latest-time)
                         magnus-codex--trace-file-cache)
                selected)
            (remhash cache-key magnus-codex--trace-file-cache)
            nil))))))

(defun magnus-codex--canonical-trace-entry (role timestamp text)
  "Build a shared trace entry for ROLE, TIMESTAMP, and TEXT."
  (if (eq role 'user)
      `((type . "user")
        (timestamp . ,timestamp)
        (message . ((content . ,text))))
    `((type . "assistant")
      (timestamp . ,timestamp)
      (message . ((content . [((type . "text") (text . ,text))]))))))

(defun magnus-codex-trace-entry (_instance entry)
  "Normalize one visible Codex rollout ENTRY for the shared trace viewer.
Codex `response_item' records are deliberately ignored: assistant output is
duplicated in `event_msg' records, and raw reasoning content is encrypted."
  (when (equal (alist-get 'type entry) "event_msg")
    (let* ((payload (alist-get 'payload entry))
           (event-type (alist-get 'type payload))
           (text (alist-get 'message payload))
           (timestamp (alist-get 'timestamp entry)))
      (when (and (stringp text) (not (string-empty-p text)))
        (pcase event-type
          ("user_message"
           (magnus-codex--canonical-trace-entry 'user timestamp text))
          ("agent_message"
           (magnus-codex--canonical-trace-entry
            'assistant timestamp text)))))))

(defun magnus-codex--headless-option-string (value)
  "Return optional Codex CLI VALUE as a string."
  (cond
   ((null value) nil)
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   (t (format "%s" value))))

(defun magnus-codex--headless-environment ()
  "Return an isolated environment for a nested Codex headless process."
  (magnus-environment-without
   process-environment
   '("CLAUDECODE" "CODEX_THREAD_ID" "CODEX_CI")
   ;; Preserve CODEX_HOME and Codex/OpenAI authentication.  Only inherited
   ;; runner identity and opposite-provider credentials are removed.
   '("ANTHROPIC_" "CLAUDE_CODE_" "CODEX_SANDBOX")))

(defun magnus-codex--make-headless-review-decoder ()
  "Return a stateful decoder for one Codex review process.

`codex exec --json' may emit more than one completed agent message during a
turn.  Codex itself defines the final response as the last such message when
the turn completes, so each process needs an independent last-message cell."
  (let ((last-message nil))
    (lambda (event request)
      (let* ((type (alist-get 'type event))
             (item (alist-get 'item event))
             (item-type (and (listp item) (alist-get 'type item)))
             (text (and (equal item-type "agent_message")
                        (alist-get 'text item))))
        (when (and (equal type "item.completed")
                   (equal item-type "agent_message")
                   (stringp text))
          (setq last-message text))
        (magnus-codex-headless-decode-event
         event request
         (and (equal type "turn.completed") last-message))))))

(defun magnus-codex-headless-review-spec (request)
  "Return a Codex headless launch specification for REQUEST."
  (let* ((session-id (plist-get request :session-id))
         (base (plist-get request :base))
         (schema-file (plist-get request :schema-file))
         (directory (plist-get request :directory))
         (prompt (plist-get request :prompt))
         (model (magnus-codex--headless-option-string
                 (plist-get request :model)))
         (effort (magnus-codex--headless-option-string
                  (plist-get request :effort)))
         (name (magnus-codex--headless-option-string
                (plist-get request :name))))
    (unless (and (stringp schema-file) (file-readable-p schema-file))
      (user-error "Codex headless reviews require a JSON schema file"))
    (if session-id
        (unless (and (stringp session-id) (not (string-empty-p session-id)))
          (user-error "Codex review session ID is invalid"))
      (unless (and (stringp base) (not (string-empty-p base)))
        (user-error "A fresh Codex review requires an exact base revision")))
    (list
     :command
     (append
      (magnus-codex--command-prefix)
      (list "exec"
            "--json"
            "--color" "never"
            "--sandbox" "read-only"
            "--cd" (expand-file-name directory)
            "--output-schema" schema-file)
      (when model (list "--model" model))
      (when effort
        (list "--config"
              (format "model_reasoning_effort=%S" effort)))
      (if session-id
          (list "resume" session-id prompt)
        ;; Use an ordinary exec session so the documented `exec resume' path
        ;; can preserve reviewer continuity.  The `exec review' subcommand
        ;; treats a custom prompt as its sole target and rejects combining it
        ;; with --base/--commit/--uncommitted; --title requires --commit.
        ;; Magnus already pins the immutable base/head in the detached checkout
        ;; and PROMPT, then validates the echoed object IDs, so the built-in
        ;; review target machinery would be redundant.
        (list prompt)))
     :environment (magnus-codex--headless-environment)
     :decoder (magnus-codex--make-headless-review-decoder)
     :success-requires '(terminal structured-result)
     :session-id session-id
     :name (and name (format "magnus-codex-review-%s" name)))))

(defun magnus-codex-headless-spec (request)
  "Return a Codex headless launch specification for REQUEST's purpose."
  (pcase (plist-get request :purpose)
    ('review (magnus-codex-headless-review-spec request))
    ('agent (user-error "Codex does not support headless agent work"))
    (purpose (user-error "Codex does not support headless purpose `%s'"
                         purpose))))

(defun magnus-codex--parse-structured-result (text)
  "Parse Codex structured result TEXT into alists and lists."
  (json-parse-string text
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun magnus-codex-headless-decode-event (event request &optional final-message)
  "Normalize one Codex exec JSONL EVENT for REQUEST.
FINAL-MESSAGE, when non-nil, is the last agent message selected for a completed
turn by the process-local decoder returned by
`magnus-codex--make-headless-review-decoder'."
  (let* ((type (alist-get 'type event))
         (item (alist-get 'item event))
         (item-type (and (listp item) (alist-get 'type item)))
         (text (and (equal item-type "agent_message")
                    (alist-get 'text item)))
         (canonical (list :type (or type "unknown")
                          :provider 'codex
                          :raw event)))
    (when (stringp text)
      (setq canonical (plist-put canonical :text text)))
    (when (equal type "thread.started")
      (setq canonical
            (plist-put canonical :session-id
                       (or (alist-get 'thread_id event)
                           (alist-get 'threadId event)))))
    ;; `codex exec' may emit prose agent messages before its schema-constrained
    ;; final response.  Codex selects the last agent message at turn completion;
    ;; parsing earlier messages would turn valid preambles into sticky errors.
    (when (and (equal type "turn.completed")
               (plist-get request :schema-file))
      (if (not (stringp final-message))
          (setq canonical
                (plist-put canonical :decode-error
                           "Codex completed without a structured final message"))
        (condition-case err
            (setq canonical
                  (plist-put canonical :structured-result
                             (magnus-codex--parse-structured-result
                              final-message)))
          (error
           (setq canonical
                 (plist-put canonical :decode-error
                            (format "Codex result is not schema JSON: %s"
                                    (error-message-string err))))))))
    (when (member type '("turn.completed" "turn.failed" "turn.cancelled"))
      (setq canonical (plist-put canonical :terminal t)))
    (when (member type '("error" "turn.failed" "turn.cancelled"))
      (let* ((detail (alist-get 'error event))
             (message (or (alist-get 'message event)
                          (and (listp detail) (alist-get 'message detail))
                          (and (stringp detail) detail)
                          (format "Codex emitted %s" type))))
        (setq canonical
              (plist-put canonical :error
                         (list :type type :message message :detail detail)))))
    canonical))

(magnus-provider-register
 'codex
 '((start . magnus-codex-start)
   (resume . magnus-codex-start)
   (send . magnus-codex-send)
   (try-send . magnus-codex-try-send)
   (interrupt . magnus-codex-interrupt)
   (stop . magnus-codex-stop)
   (running-p . magnus-codex-running-p)
   (switch-to . magnus-codex-switch-to)
   (trace-file . magnus-codex-trace-file)
   (trace-entry . magnus-codex-trace-entry)
   (headless-spec . magnus-codex-headless-spec)))

(provide 'magnus-provider-codex)
;;; magnus-provider-codex.el ends here
