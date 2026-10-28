;;; magnus-process.el --- Process management for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module handles spawning and managing Claude Code processes
;; in vterm buffers.

;;; Code:

(require 'cl-lib)
(require 'filenotify)
(require 'subr-x)
(require 'magnus-environment)
(require 'magnus-instances)
(require 'magnus-provider)
(require 'magnus-headless)
(require 'magnus-coord)
(require 'magnus-onboarding)
(require 'magnus-terminal)
(require 'magnus-trace)

(declare-function magnus-instances-create "magnus-instances"
                  (directory name &optional provider kind))

(declare-function magnus-status-refresh "magnus-status")
(declare-function magnus-project-root "magnus")
(declare-function magnus--agents-index-update "magnus")
(declare-function magnus-claude--fresh-session-id "magnus-provider-claude")
(declare-function vterm-send-key "vterm" (key &optional shift meta ctrl))

;; Variables defined in magnus.el
(defvar magnus-claude-executable)
(defvar magnus-default-directory)
(defvar magnus-instance-name-generator)
(defvar magnus-buffer-name)
(defvar magnus--summon-context)

(defvar magnus-process-ready-hook nil
  "Hook run when an interactive agent can receive automated input.
Functions are called with the ready `magnus-instance' as their sole argument.")

(defvar magnus-process--claude-session-id-support-cache
  (make-hash-table :test #'equal)
  "Claude executable path to cached `--session-id' support result.")

(defvar magnus-process--legacy-session-launches
  (make-hash-table :test #'equal)
  "Physical project roots with an unresolved legacy Claude launch.")

(defvar magnus-process--transaction-runtime-buffer nil
  "Dynamic one-cell list recording an exact (BUFFER . PROCESS) owner.")

(defvar-local magnus-process--instance-id nil
  "Durable Magnus instance ID owning this interactive Claude terminal.")

(defun magnus-process--current-instance (instance)
  "Return the registered incarnation of INSTANCE, or INSTANCE itself.
Persistence replaces instance objects while live local terminals survive.  A
delayed callback must therefore resolve the durable ID before reading or
publishing lifecycle state."
  (or (and (magnus-instance-id instance)
           (magnus-instances-get (magnus-instance-id instance)))
      instance))

(defun magnus-process--callback-owner-p (instance buffer &optional process)
  "Return non-nil when BUFFER and optional PROCESS still own INSTANCE.
Unlike `magnus-process--runtime-owner-p', this accepts a detached process so a
normal exit callback can publish its final state.  It still rejects a different
process which has claimed the same buffer."
  (let* ((current-instance (magnus-process--current-instance instance))
         (attached (and (buffer-live-p buffer)
                        (get-buffer-process buffer))))
    (and (bufferp buffer)
         (eq buffer (magnus-instance-buffer current-instance))
         (or (null process)
             (null attached)
             (eq attached process)))))

(defun magnus-process--runtime-owner-p (instance buffer &optional process)
  "Return non-nil when BUFFER and optional PROCESS still own INSTANCE."
  (let ((current-instance (magnus-process--current-instance instance)))
    (and (buffer-live-p buffer)
         (eq buffer (magnus-instance-buffer current-instance))
         (when-let ((current (get-buffer-process buffer)))
           (and (process-live-p current)
                (or (null process) (eq process current)))))))

(defun magnus-process--runtime-replaced-p (instance owner-runtime)
  "Return non-nil when a live runtime has replaced OWNER-RUNTIME for INSTANCE.
OWNER-RUNTIME is the exact (BUFFER . PROCESS) pair acquired by an earlier
startup attempt.  Detect both a different buffer and a different process which
has claimed the same buffer."
  (let* ((current-instance (magnus-process--current-instance instance))
         (current-buffer (magnus-instance-buffer current-instance))
         (current-process
          (and (buffer-live-p current-buffer)
               (get-buffer-process current-buffer))))
    (and (consp owner-runtime)
         (processp current-process)
         (process-live-p current-process)
         (or (not (eq current-buffer (car owner-runtime)))
             (not (eq current-process (cdr owner-runtime)))))))

(defun magnus-process--record-transaction-runtime (buffer process)
  "Record exact BUFFER and PROCESS ownership for the current transaction.
The first acquired runtime wins so a nested or reentrant launch cannot redirect
the outer transaction's rollback at a later replacement."
  (when (and (consp magnus-process--transaction-runtime-buffer)
             (null (car magnus-process--transaction-runtime-buffer)))
    (setcar magnus-process--transaction-runtime-buffer
            (cons buffer process))))

(defun magnus-process--run-ready-hook
    (instance &optional owner-buffer owner-process)
  "Run the ready hook when INSTANCE still has its expected terminal owner.
OWNER-BUFFER and OWNER-PROCESS, when non-nil, prevent a delayed callback from
publishing readiness for a replacement runtime."
  (let* ((current-instance (magnus-process--current-instance instance))
         (buffer (or owner-buffer
                     (magnus-instance-buffer current-instance))))
    (when (magnus-process--runtime-owner-p
           current-instance buffer owner-process)
      (run-hook-with-args 'magnus-process-ready-hook current-instance))))

;;; Process creation

(defun magnus-process--shell-command (&rest arguments)
  "Return shell command string built from safely quoted ARGUMENTS."
  (mapconcat #'shell-quote-argument arguments " "))

(defun magnus-process--claude-command-prefix ()
  "Return configured Claude command as a nonempty argv prefix.
An executable path which resolves as configured remains one token, including
paths containing spaces.  Otherwise parse the string as shell words to retain
the shipped ability to customize `magnus-claude-executable' with extra flags."
  (magnus-environment-command-prefix magnus-claude-executable "Claude"))

(defun magnus-process--claude-command (&rest arguments)
  "Return a safely quoted Claude shell command with ARGUMENTS appended."
  (let* ((prefix (magnus-process--claude-command-prefix))
         ;; A direct vterm launch does not source an interactive shell.  Prefer
         ;; the executable Emacs already resolved so startup does not depend on
         ;; shell initialization or a second PATH lookup.
         (program (or (executable-find (car prefix)) (car prefix))))
    (apply #'magnus-process--shell-command
           (append (cons program (cdr prefix)) arguments))))

(defun magnus-process--claude-command-with-onboarding
    (instance arguments &optional summon-context)
  "Return Claude command for INSTANCE, ARGUMENTS, and SUMMON-CONTEXT.
The complete current onboarding is the final positional argument, making it
Claude's first turn for a new session and first new turn after resumed history.
Flatten line breaks before shell quoting so vterm launches one physical command
without an intermediate interactive shell composer."
  (apply #'magnus-process--claude-command
         (append
          arguments
          (list
           (replace-regexp-in-string
            "[\n\r]+" " "
            (magnus-process--onboarding-message instance summon-context))))))

(defun magnus-process--claude-session-id-supported-p ()
  "Return non-nil when the configured Claude CLI supports `--session-id'.
Probe each resolved executable at most once per Emacs session.  A failed probe
conservatively selects the legacy unique-delta capture path."
  (let* ((prefix (magnus-process--claude-command-prefix))
         (program (or (executable-find (car prefix)) (car prefix)))
         (missing (make-symbol "not-probed"))
         (cached (gethash prefix
                          magnus-process--claude-session-id-support-cache
                          missing)))
    (if (not (eq cached missing))
        (eq cached 'supported)
      (let ((supported
             (condition-case err
                 (with-temp-buffer
                   (and (eq 0 (apply #'call-process program nil t nil
                                     (append (cdr prefix) '("--help"))))
                        (goto-char (point-min))
                        (re-search-forward
                         "\\(?:^\\|[[:space:]]\\)--session-id\\(?:[=[:space:]]\\|$\\)"
                         nil t)))
               (error
                (message "Magnus: could not probe Claude session-ID support: %s"
                         (error-message-string err))
                nil))))
        (puthash prefix (if supported 'supported 'unsupported)
                 magnus-process--claude-session-id-support-cache)
        (and supported t)))))

(defun magnus-process--fresh-claude-session-id ()
  "Return a fresh UUID v4 candidate for one interactive Claude launch."
  (require 'magnus-provider-claude)
  (magnus-claude--fresh-session-id))

(defun magnus-process--reserve-legacy-session-launch (directory token)
  "Reserve legacy session inference in DIRECTORY for launch TOKEN."
  (let* ((key (magnus-coord--normalized-directory directory))
         (existing (gethash key magnus-process--legacy-session-launches)))
    (when existing
      (user-error
       "A fresh legacy Claude launch is already resolving in %s; wait for its session capture"
       directory))
    (puthash key token magnus-process--legacy-session-launches)
    token))

(defun magnus-process--release-legacy-session-launch (directory token)
  "Release DIRECTORY's legacy launch reservation when owned by TOKEN."
  (when token
    (let ((key (magnus-coord--normalized-directory directory)))
      (when (eq (gethash key magnus-process--legacy-session-launches) token)
        (remhash key magnus-process--legacy-session-launches)))))

(defun magnus-process--discard-created-runtime
    (instance external &optional owner-runtime)
  "Discard runtime resources acquired while creating INSTANCE.
When EXTERNAL is non-nil, first give the provider a chance to release its
own timers and transport state.  The buffer cleanup is an intentional
fallback: a provider start may fail after attaching a terminal but before its
normal `stop' operation is fully usable.
OWNER-RUNTIME, when non-nil, is an exact (BUFFER . PROCESS) pair.  A bare
buffer is not sufficient because it cannot distinguish a same-buffer process
replacement."
  (unless (or (null owner-runtime)
              (and (consp owner-runtime)
                   (bufferp (car owner-runtime))
                   (or (null (cdr owner-runtime))
                       (processp (cdr owner-runtime)))))
    (error "Invalid runtime owner: %S" owner-runtime))
  ;; Capture the buffer before provider cleanup: a provider may clear the
  ;; instance slot even when its own partial-start cleanup cannot kill the
  ;; terminal it already created.
  (let* ((exact-owner-p (consp owner-runtime))
         (current-buffer (magnus-instance-buffer instance))
         (buffer (if exact-owner-p (car owner-runtime) current-buffer))
         (expected-process (and exact-owner-p (cdr owner-runtime)))
         (attached (and (buffer-live-p buffer)
                        (get-buffer-process buffer)))
         (owns-current
          (and (eq buffer current-buffer)
               (or (not exact-owner-p)
                   (null attached)
                   (eq expected-process attached)))))
    (when (and external owns-current)
      (condition-case err
          (magnus-provider-call instance 'stop t)
        (error
         (message "Magnus: provider creation rollback failed for %s: %s"
                  (magnus-instance-name instance)
                  (error-message-string err)))))
    (when-let ((process (if exact-owner-p expected-process attached)))
      (when (processp process)
        (ignore-errors (set-process-query-on-exit-flag process nil))
        (when (process-live-p process)
          (ignore-errors (delete-process process)))))
    (when (and (buffer-live-p buffer)
               (let ((current (get-buffer-process buffer)))
                 (or (not exact-owner-p)
                     (null current)
                     (eq current expected-process))))
      (ignore-errors (kill-buffer buffer)))
    ;; Clear lifecycle slots only while they still name the runtime being
    ;; rolled back.  A reentrant observer may already have installed a new one.
    (when (and owns-current
               (eq buffer (magnus-instance-buffer instance))
               (let ((current (and (buffer-live-p buffer)
                                   (get-buffer-process buffer))))
                 (or (not exact-owner-p)
                     (null current)
                     (eq current expected-process))))
      (setf (magnus-instance-buffer instance) nil
            (magnus-instance-status instance) 'stopped))))

(defun magnus-process--coord-row-existed-p (directory name)
  "Return non-nil when DIRECTORY already has an Active Work row for NAME.
Read the shared file directly so a watched-but-stale cache cannot authorize
destructive rollback. If the snapshot is ambiguous, return non-nil and leave
the row untouched."
  (let ((file (magnus-coord-file-path directory)))
    (and
     (file-exists-p file)
     (condition-case err
         (with-temp-buffer
           (insert-file-contents file)
           (cl-some
            (lambda (entry)
              (string= (or (plist-get entry :agent) "") name))
            (plist-get (magnus-coord--parse-buffer) :active)))
       (error
        (message "Magnus: preserving ambiguous coordination row for %s: %s"
                 name (error-message-string err))
        t)))))

(defun magnus-process--rollback-creation
    (instance directory coordination-attempted runtime-attempted external
              runtime-owner coordination-snapshot)
  "Release resources acquired by one failed INSTANCE creation transaction."
  ;; Reverse acquisition order: runtime, coordination, registry.
  (when runtime-attempted
    (magnus-process--discard-created-runtime
     instance external runtime-owner))
  (when coordination-attempted
    (magnus-process--restore-coord-ownership
     directory coordination-snapshot))
  (when (memq instance (magnus-instances-list))
    (condition-case err
        (magnus-instances-remove instance)
      (error
       ;; `magnus-instances-remove' mutates the registry before running its
       ;; hook, so an error here is diagnostic rather than a ghost instance.
       (message "Magnus: registry rollback hook failed for %s: %s"
                (magnus-instance-name instance)
                (error-message-string err))))))

(defun magnus-process--create-transaction (instance starter)
  "Register INSTANCE and call STARTER as one rollback-safe transaction.
STARTER receives INSTANCE and a non-nil external-provider flag.  On failure,
release only the runtime, coordination state, and registry entry acquired by
this call.  Return INSTANCE after successful startup."
  (let* ((directory (magnus-instance-directory instance))
         (project-key (magnus-coord--normalized-directory directory))
         (coordination-snapshot
          (magnus-process--coord-ownership-snapshot
           project-key (magnus-instance-name instance)))
         coordination-attempted
         runtime-attempted
         (magnus-process--transaction-runtime-buffer (list nil))
         external
         committed)
    (unwind-protect
        (progn
          (magnus-instances-add instance)
          (setq coordination-attempted t)
          (magnus-coord-register-agent directory instance)
          (setq external (magnus-provider-external-p instance)
                runtime-attempted t)
          (funcall starter instance external)
          (setq committed t)
          instance)
      (unless committed
        (magnus-process--rollback-creation
         instance project-key coordination-attempted runtime-attempted external
         (car magnus-process--transaction-runtime-buffer)
         coordination-snapshot)))))

(defun magnus-process-create (&optional directory name provider initial-message)
  "Create a new agent instance.
DIRECTORY is the working directory.  If nil, prompts for one.
NAME is the instance name.  If nil, auto-generates one.
PROVIDER defaults to `claude', preserving the original vterm behavior.
INITIAL-MESSAGE is passed to external providers when non-nil."
  (interactive)
  (let* ((dir (or directory
                  (magnus-process--get-directory)))
         (instance-name (or name
                            (funcall magnus-instance-name-generator dir)))
         (instance (magnus-instances-create dir instance-name provider)))
    (magnus-process--create-transaction
     instance
     (lambda (candidate external)
       (if external
           (if initial-message
               (magnus-provider-call candidate 'start initial-message)
             (magnus-provider-call candidate 'start))
         (magnus-process--spawn candidate))))))

(defun magnus-process-create-codex (&optional directory name initial-message)
  "Create an opt-in Codex instance in DIRECTORY with NAME.
When INITIAL-MESSAGE is non-nil, include it in the native TUI's first turn."
  (interactive)
  (magnus-process-create directory name 'codex initial-message))

(defun magnus-process--get-directory ()
  "Get directory for new instance, prompting user."
  (let ((default (or magnus-default-directory
                     (magnus-project-root)
                     default-directory)))
    (read-directory-name "Directory: " default nil t)))

(defun magnus-process--spawn (instance)
  "Spawn a Claude Code process for INSTANCE."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (buffer-name (format "*claude:%s*" name))
         (default-directory directory)
         (explicit-session-p
          (magnus-process--claude-session-id-supported-p))
         (candidate-session-id
          (when explicit-session-p
            (magnus-process--fresh-claude-session-id)))
         (legacy-launch-token
          (unless explicit-session-p
            (cons instance (make-symbol "legacy-claude-launch"))))
         (command
          (magnus-process--claude-command-with-onboarding
           instance
           (when candidate-session-id
             (list "--session-id" candidate-session-id))
           magnus--summon-context))
         sessions-before
         ready-timer
         buffer
         owner-process
         spawned)
    (unwind-protect
        (progn
          (when legacy-launch-token
            (magnus-process--reserve-legacy-session-launch
             directory legacy-launch-token)
            (setq sessions-before
                  (magnus-process--list-sessions directory)))
          ;; Make onboarding part of process launch.  A user can visit the
          ;; native TUI immediately without racing a delayed Magnus paste.
          (setq buffer
                (magnus-terminal-create-command-buffer buffer-name command))
          (setq owner-process (get-buffer-process buffer))
          (magnus-process--record-transaction-runtime buffer owner-process)
          (with-current-buffer buffer
            (setq-local magnus-process--instance-id
                        (magnus-instance-id instance)))
          (magnus-instances-update instance
                                   :buffer buffer
                                   :status 'running)
          ;; Set up process sentinel.
          (magnus-process--setup-sentinel instance buffer)
          ;; Readiness notification is separate from first-turn ordering.  Keep
          ;; the existing delayed hook, fenced to this exact runtime, while the
          ;; argv already owns Claude's first turn.
          (setq ready-timer
                (run-with-timer 5 nil #'magnus-process--run-ready-hook
                                instance buffer owner-process))
          ;; Watch for a new session to appear.  This is deliberately last, so
          ;; no later synchronous failure can orphan a successful watcher.
          (magnus-process--watch-for-session
           instance directory sessions-before candidate-session-id buffer
           legacy-launch-token owner-process)
          (setq spawned t)
          buffer)
      (unless spawned
        (when (timerp ready-timer)
          (cancel-timer ready-timer))
        (magnus-process--discard-created-runtime
         instance nil (and buffer (cons buffer owner-process)))
        (magnus-process--release-legacy-session-launch
         directory legacy-launch-token)))))

(defun magnus-process--agent-memory-path (instance)
  "Return the memory file path for INSTANCE.
Path is <directory>/.claude/agents/<name>/memory.md."
  (magnus-onboarding-memory-path instance))

(defun magnus-process--agent-busy-path (instance)
  "Return the busy signal file path for INSTANCE.
Path is <directory>/.claude/agents/<name>/busy."
  (magnus-onboarding-busy-path instance))

(defun magnus-process--ensure-agent-dir (instance)
  "Ensure the agent directory exists for INSTANCE."
  (let ((dir (file-name-directory (magnus-process--agent-memory-path instance))))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun magnus-process--onboarding-message (instance &optional summon-context)
  "Generate onboarding message for INSTANCE.
SUMMON-CONTEXT, if non-nil, is a plist with :sender and :reason.
Tailors the message for new vs returning agents, and includes
summon context when the agent was brought back by another agent."
  (let* ((prev-session (magnus-instance-previous-session-id instance))
         (prev-trace (when prev-session
                       (magnus-process--session-jsonl-path
                        (magnus-instance-directory instance)
                        prev-session))))
    (magnus-onboarding-prompt instance prev-trace summon-context)))

(defun magnus-process--onboarding-returning
    (name prev-trace summon-context &optional instance)
  "Generate onboarding for a returning agent NAME.
PREV-TRACE is the path to the previous session trace, or nil.
SUMMON-CONTEXT is a plist with :sender and :reason, or nil.
INSTANCE is accepted for internal callers; legacy callers are resolved by NAME."
  (let ((candidate (or instance (magnus-instances-get-by-name name))))
    (if candidate
        (magnus-onboarding-build
         (magnus-instance-id candidate) name
         :directory (magnus-instance-directory candidate)
         :provider (magnus-instance-provider candidate)
         :returning t
         :previous-trace prev-trace
         :summon-context summon-context)
      (magnus-onboarding-build
       nil name
       :directory default-directory
       :provider 'claude
       :returning t
       :previous-trace prev-trace
       :summon-context summon-context))))

(defun magnus-process--onboarding-new (name summon-context &optional instance)
  "Generate onboarding for a new agent NAME.
SUMMON-CONTEXT is a plist with :sender and :reason, or nil.
INSTANCE is accepted for internal callers; legacy callers are resolved by NAME."
  (let ((candidate (or instance (magnus-instances-get-by-name name))))
    (magnus-onboarding-build
     (if candidate
         (magnus-instance-id candidate)
       nil)
     name
     :directory (if candidate
                    (magnus-instance-directory candidate)
                  default-directory)
     :provider (if candidate
                   (magnus-instance-provider candidate)
                 'claude)
     :returning nil
     :summon-context summon-context)))

(defun magnus-process--list-sessions (directory)
  "List all session IDs for DIRECTORY.
Extracts IDs from .jsonl filenames in the project directory."
  (let* ((project-hash (magnus-process--project-hash directory))
         (sessions-dir (expand-file-name
                        (concat "projects/" project-hash)
                        (expand-file-name ".claude" (getenv "HOME")))))
    (when (file-directory-p sessions-dir)
      (mapcar (lambda (f) (file-name-sans-extension f))
              (directory-files sessions-dir nil "\\.jsonl$")))))

(defun magnus-process--watch-for-session
    (instance directory sessions-before &optional candidate-session-id
              owner-buffer legacy-launch-token owner-process)
  "Watch for a new session to appear for INSTANCE in DIRECTORY.
SESSIONS-BEFORE is the list of sessions that existed before spawning.
When CANDIDATE-SESSION-ID is non-nil, watch only its exact JSONL file.
OWNER-BUFFER identifies the launch allowed to persist the captured session.
LEGACY-LAUNCH-TOKEN serializes old-CLI inference within one physical project.
OWNER-PROCESS distinguishes a later process reusing the same terminal buffer.
Uses both filenotify and polling fallback for robustness."
  (let* ((project-hash (magnus-process--project-hash directory))
         (sessions-dir (expand-file-name
                        (concat "projects/" project-hash)
                        (expand-file-name ".claude" (getenv "HOME")))))
    ;; Ensure directory exists before watching
    (unless (file-directory-p sessions-dir)
      (make-directory sessions-dir t))
    (let* ((descriptor nil)
           (poll-timer nil)
           (cleanup-timer nil)
           (watching nil)
           (detect-fn
            (lambda ()
              (magnus-process--detect-new-session
               instance directory sessions-before
               (list descriptor poll-timer cleanup-timer)
               candidate-session-id
               (or owner-buffer (magnus-instance-buffer instance))
               legacy-launch-token
               owner-process))))
      (unwind-protect
          (progn
            ;; Primary: file-notify watcher.
            (setq descriptor
                  (file-notify-add-watch
                   sessions-dir '(change)
                   (lambda (_event) (funcall detect-fn))))
            ;; Fallback: poll every 5 seconds.
            (setq poll-timer
                  (run-with-timer 5 5 detect-fn))
            ;; Final cleanup after 120 seconds.
            (setq cleanup-timer
                  (run-with-timer
                   120 nil
                   (lambda ()
                     (magnus-process--cleanup-session-watch
                      (list descriptor poll-timer nil))
                     (magnus-process--release-legacy-session-launch
                      directory legacy-launch-token))))
            (setq watching t)
            ;; Close the race where Claude creates its JSONL before the file
            ;; notification watcher is fully installed.
            (funcall detect-fn))
        (unless watching
          (when descriptor
            (ignore-errors (file-notify-rm-watch descriptor)))
          (when (timerp poll-timer)
            (cancel-timer poll-timer))
          (when (timerp cleanup-timer)
            (cancel-timer cleanup-timer))
          (magnus-process--release-legacy-session-launch
           directory legacy-launch-token))))))

(defun magnus-process--cleanup-session-watch (resources)
  "Release file notification and timer RESOURCES for a session watcher."
  (when-let ((descriptor (nth 0 resources)))
    (ignore-errors (file-notify-rm-watch descriptor)))
  (dolist (timer (cdr resources))
    (when (timerp timer)
      (cancel-timer timer))))

(defun magnus-process--session-watch-owner-p
    (instance directory owner-buffer &optional owner-process)
  "Return non-nil when OWNER-BUFFER still owns INSTANCE's launch in DIRECTORY.
When OWNER-PROCESS is non-nil, reject a different process which has since
claimed the same buffer.  A detached process remains eligible to publish a
session file created during normal shutdown."
  (let* ((current-instance (magnus-process--current-instance instance))
         (attached (and (buffer-live-p owner-buffer)
                        (get-buffer-process owner-buffer))))
    (and (buffer-live-p owner-buffer)
         (eq owner-buffer (magnus-instance-buffer current-instance))
         (or (null owner-process)
             (null attached)
             (eq owner-process attached))
         (string=
          (directory-file-name (expand-file-name directory))
          (directory-file-name
           (expand-file-name
            (magnus-instance-directory current-instance)))))))

(defun magnus-process--session-candidate-path (directory session-id)
  "Return the expected Claude JSONL path for SESSION-ID in DIRECTORY."
  (expand-file-name
   (format "projects/%s/%s.jsonl"
           (magnus-process--project-hash directory) session-id)
   (expand-file-name ".claude" (getenv "HOME"))))

(defun magnus-process--detect-new-session
    (instance directory sessions-before resources
              &optional candidate-session-id owner-buffer
              legacy-launch-token owner-process)
  "Try to detect a new session for INSTANCE in DIRECTORY.
SESSIONS-BEFORE is the pre-spawn session list.  RESOURCES is
a list of (descriptor poll-timer cleanup-timer) to clean up.
CANDIDATE-SESSION-ID selects exact-file capture for a supporting CLI.
OWNER-BUFFER and OWNER-PROCESS must still identify INSTANCE's launch before any
ID is persisted."
  (let* ((instance (magnus-process--current-instance instance))
         (owner (or owner-buffer (magnus-instance-buffer instance))))
    (cond
     ((not (magnus-process--session-watch-owner-p
            instance directory owner owner-process))
      (magnus-process--cleanup-session-watch resources)
      (magnus-process--release-legacy-session-launch
       directory legacy-launch-token)
      'stale)
     ((magnus-instance-session-id instance)
      (magnus-process--cleanup-session-watch resources)
      (magnus-process--release-legacy-session-launch
       directory legacy-launch-token)
      'settled)
     (candidate-session-id
      (when (file-exists-p
             (magnus-process--session-candidate-path
              directory candidate-session-id))
        (unwind-protect
            (progn
              (magnus-instances-update
               instance :session-id candidate-session-id)
              (message "Captured session %s for %s"
                       candidate-session-id
                       (magnus-instance-name instance))
              'captured)
          (magnus-process--cleanup-session-watch resources)
          (magnus-process--release-legacy-session-launch
           directory legacy-launch-token))))
     (t
      (let* ((sessions-after (magnus-process--list-sessions directory))
             (new-sessions
              (cl-set-difference sessions-after sessions-before
                                 :test #'string=)))
        (cond
         ((= 1 (length new-sessions))
          (let ((session-id (car new-sessions)))
            (unwind-protect
                (progn
                  (magnus-instances-update instance :session-id session-id)
                  (message "Captured legacy session %s for %s"
                           session-id (magnus-instance-name instance))
                  'captured)
              (magnus-process--cleanup-session-watch resources)
              (magnus-process--release-legacy-session-launch
               directory legacy-launch-token))))
         ((> (length new-sessions) 1)
          (message
           "Magnus: ambiguous Claude session capture for %s; leaving unresolved"
           (magnus-instance-name instance))
          (magnus-process--cleanup-session-watch resources)
          (magnus-process--release-legacy-session-launch
           directory legacy-launch-token)
          'ambiguous)))))))

(defun magnus-process--create-vterm-buffer (buffer-name)
  "Create a vterm buffer with BUFFER-NAME.
Compatibility wrapper for `magnus-terminal-create-buffer'."
  (magnus-terminal-create-buffer buffer-name))

(defun magnus-process-send-escape ()
  "Send ESC to Claude Code (mapped from \\`keyboard-quit')."
  (interactive)
  (magnus-terminal-send-escape))

(defun magnus-process--setup-keys ()
  "Set up keybindings for Claude Code in the current vterm buffer.
Maps \\`keyboard-quit' to send ESC, since Emacs intercepts the real ESC key."
  (magnus-terminal-setup-keys))

;;; Trace buffer entry point

(declare-function magnus-trace-open "magnus-trace")

(defun magnus-process-trace (instance)
  "Open the trace buffer for INSTANCE showing thinking and messages."
  (when (and (magnus-provider-external-p instance)
             (not (magnus-provider-operation-p instance 'trace-file)))
    (user-error "Provider `%s' does not support thinking traces"
                (magnus-instance-provider instance)))
  (require 'magnus-trace)
  (magnus-trace-open instance))

(defun magnus-process--session-jsonl-path (directory session-id)
  "Get the JSONL file path for SESSION-ID in DIRECTORY."
  (let* ((project-hash (magnus-process--project-hash directory))
         (jsonl-file (expand-file-name
                      (concat "projects/" project-hash "/" session-id ".jsonl")
                      (expand-file-name ".claude" (getenv "HOME")))))
    (when (file-exists-p jsonl-file)
      jsonl-file)))

(defun magnus-process--setup-sentinel (instance buffer)
  "Set up process monitoring for INSTANCE in BUFFER."
  (when-let ((process (get-buffer-process buffer)))
    (set-process-sentinel
     process
     (lambda (proc _event)
       (unless (process-live-p proc)
         ;; A delayed exit from a killed or archived terminal must not mutate
         ;; a replacement that already owns INSTANCE.  A normally exiting
         ;; process is often detached before this sentinel runs, while a
         ;; same-buffer replacement remains observable as a different process.
         (let ((current-instance
                (magnus-process--current-instance instance)))
           (when (magnus-process--callback-owner-p
                  current-instance buffer proc)
             ;; Don't overwrite `purged': archive publishes it intentionally.
             (unless (eq (magnus-instance-status current-instance) 'purged)
               (magnus-instances-update current-instance :status 'stopped))
             (when (and (boundp 'magnus-buffer-name)
                        (get-buffer magnus-buffer-name))
               (magnus-status-refresh)))))))))

;;; Process control

(defun magnus-process--finish-local-stop (buffer process)
  "Finish stopping exact PROCESS without disturbing a replacement in BUFFER."
  (when (processp process)
    (set-process-query-on-exit-flag process nil)
    (when (process-live-p process)
      (delete-process process)))
  (when (and (buffer-live-p buffer)
             (let ((attached (get-buffer-process buffer)))
               (or (null attached) (eq attached process))))
    (kill-buffer buffer)))

(defun magnus-process-kill (instance &optional force)
  "Kill the agent process for INSTANCE.
If FORCE is non-nil, forcefully terminate."
  (if (magnus-provider-external-p instance)
      (magnus-provider-call instance 'stop force)
    (when-let ((buffer (magnus-instance-buffer instance)))
      (when (buffer-live-p buffer)
        (let ((process (get-buffer-process buffer)))
          (when (and process (process-live-p process))
            (if force
                (kill-process process)
              ;; Graceful exit: SIGINT for headless, C-c for vterm
              (if (magnus-process--headless-p instance)
                  (interrupt-process process)
                (with-current-buffer buffer
                  (vterm-send-key "C-c")))))
          ;; Give the captured process time to exit, then clean up only that
          ;; runtime.  Another process may claim the same buffer meanwhile.
          (run-with-timer
           1 nil #'magnus-process--finish-local-stop buffer process))))
    (magnus-instances-update instance :status 'stopped :buffer nil)))

(defun magnus-process-archive (instance)
  "Archive INSTANCE: stop its process but keep it in the registry.
The session ID is preserved so the agent can be resurrected later
  via `magnus-process-resurrect-purged'."
  (if (magnus-provider-external-p instance)
      (progn
        (magnus-provider-call instance 'stop)
        (magnus-instances-update instance
                                 :status 'purged
                                 :buffer nil
                                 :purged-at (float-time))
        ;; Record the archived status before the last agent releases the
        ;; project's coordination watcher.
        (magnus-coord-unregister-agent
         (magnus-instance-directory instance) instance))
    ;; Graceful stop (same as kill, but we keep the instance)
    (when-let ((buffer (magnus-instance-buffer instance)))
      (when (buffer-live-p buffer)
        (let ((process (get-buffer-process buffer)))
          (when (and process (process-live-p process))
            (if (magnus-process--headless-p instance)
                (interrupt-process process)
              (with-current-buffer buffer
                (vterm-send-key "C-c"))))
          (run-with-timer
           1 nil #'magnus-process--finish-local-stop buffer process))))
    (magnus-instances-update instance
                             :status 'purged
                             :buffer nil
                             :purged-at (float-time))
    ;; Keep coordination ordering identical across local and external providers.
    (magnus-coord-unregister-agent
     (magnus-instance-directory instance) instance))
  ;; Index expertise tags asynchronously for every provider.
  (magnus--agents-index-update instance))

(defun magnus-process-resurrect-purged (instance)
  "Resurrect a purged INSTANCE and resend its current Magnus onboarding.
The provider session is resumed so its conversation survives.  The complete
onboarding is regenerated so identity, coordination, authorization, and the
configured user-facing guidance are current."
  (when (magnus-process--headless-p instance)
    (user-error "Headless task '%s' cannot be resurrected interactively"
                (magnus-instance-name instance)))
  (let* ((session-id (magnus-instance-session-id instance))
         (directory (magnus-instance-directory instance))
         (external (magnus-provider-external-p instance))
         (coord-snapshot
          (magnus-process--coord-ownership-snapshot
           directory (magnus-instance-name instance)))
         (original-status (magnus-instance-status instance))
         (original-buffer (magnus-instance-buffer instance))
         (original-purged-at (magnus-instance-purged-at instance))
         (magnus-process--transaction-runtime-buffer (list nil))
         coordination-attempted
         runtime-attempted
         runtime-superseded
         committed)
    (unless session-id
      (user-error "No session ID for '%s' — cannot resume"
                  (magnus-instance-name instance)))
    (unwind-protect
        (progn
          ;; Acquire project coordination while the instance is still archived,
          ;; then expose it immediately before its provider runtime starts.
          (setq coordination-attempted t)
          (magnus-coord-register-agent directory instance)
          (magnus-instances-update instance :status 'running :purged-at nil)
          (setq runtime-attempted t)
          (if external
              (progn
                (let ((resumed-runtime
                       (magnus-provider-call instance 'resume)))
                  ;; Older adapters may return their terminal without using
                  ;; Magnus's transaction recorder.  Adopt that exact return
                  ;; value when possible; first-writer-wins preserves a more
                  ;; precise owner already recorded during provider startup.
                  (when (bufferp resumed-runtime)
                    (magnus-process--record-transaction-runtime
                     resumed-runtime
                     (and (buffer-live-p resumed-runtime)
                          (get-buffer-process resumed-runtime)))))
                (let* ((owner-runtime
                        (car magnus-process--transaction-runtime-buffer))
                       (current-instance
                        (magnus-process--current-instance instance)))
                  ;; Provider startup can run public instance hooks.  Never
                  ;; steer a reentrant replacement which claimed the instance
                  ;; before RESUME returned.
                  (unless (and (consp owner-runtime)
                               (bufferp (car owner-runtime))
                               (processp (cdr owner-runtime)))
                    (error
                     "Provider `%s' did not expose an exact live runtime while restoring `%s'"
                     (magnus-instance-provider current-instance)
                     (magnus-instance-name current-instance)))
                  (unless (magnus-process--runtime-owner-p
                           current-instance
                           (car owner-runtime)
                           (cdr owner-runtime))
                    (setq runtime-superseded
                          (magnus-process--runtime-replaced-p
                           current-instance owner-runtime))
                    (error
                     "Provider `%s' lost exact runtime ownership while restoring `%s'"
                     (magnus-instance-provider current-instance)
                     (magnus-instance-name current-instance)))
                  ;; External providers own their input transport.  Queue the
                  ;; complete shared onboarding through that transport so
                  ;; Codex retains its single-writer and readiness guarantees.
                  (magnus-provider-call
                   current-instance 'send
                   (magnus-process--onboarding-message current-instance))))
            (magnus-process--spawn-with-session instance session-id))
          (setq committed t))
      (unless committed
        (let* ((owner-runtime
                (car magnus-process--transaction-runtime-buffer))
               (replacement-won
                (or runtime-superseded
                    (magnus-process--runtime-replaced-p
                     instance owner-runtime))))
          (when runtime-attempted
            (magnus-process--discard-created-runtime
             instance external owner-runtime))
          ;; Provider cleanup can itself run public hooks.  Recheck after it so
          ;; a replacement installed during STOP receives the same protection.
          (setq replacement-won
                (or replacement-won
                    (magnus-process--runtime-replaced-p
                     instance owner-runtime)))
          ;; A live replacement owns both the durable lifecycle state and the
          ;; coordination registration acquired by this attempt.  Clean only
          ;; the stale runtime; rolling back shared state would damage it.
          (unless replacement-won
            (when coordination-attempted
              (condition-case err
                  (magnus-process--restore-coord-ownership
                   directory coord-snapshot)
                (error
                 (message
                  "Magnus: resurrection ownership rollback failed for %s: %s"
                  directory (error-message-string err)))))
            (condition-case err
                (magnus-instances-update
                 instance
                 :status original-status
                 :buffer (and (buffer-live-p original-buffer) original-buffer)
                 :session-id session-id
                 :purged-at original-purged-at)
              (error
               ;; As with directory moves, notification hooks run after
               ;; mutation.  Complete rollback directly without replacing the
               ;; startup error.
               (setf (magnus-instance-status instance) original-status
                     (magnus-instance-buffer instance)
                     (and (buffer-live-p original-buffer) original-buffer)
                     (magnus-instance-session-id instance) session-id
                     (magnus-instance-purged-at instance) original-purged-at)
               (message
                "Magnus: resurrection registry rollback failed for %s: %s"
                (magnus-instance-name instance)
                (error-message-string err))))))))
    instance))

(defun magnus-process-suspend (instance)
  "Suspend the Claude Code process for INSTANCE.
Sends SIGTSTP to pause the process.  Use `magnus-process-resume' to continue."
  (when (magnus-provider-external-p instance)
    (user-error "Suspend is not supported by the `%s' provider"
                (magnus-instance-provider instance)))
  (when (magnus-process--headless-p instance)
    (user-error "Headless task '%s' cannot be suspended"
                (magnus-instance-name instance)))
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (when-let ((process (get-buffer-process buffer)))
        (when (process-live-p process)
          (signal-process process 'SIGTSTP)
          (magnus-instances-update instance :status 'suspended)
          (when (get-buffer magnus-buffer-name)
            (magnus-status-refresh))
          (message "Suspended %s" (magnus-instance-name instance)))))))

(defun magnus-process-resume (instance)
  "Resume a suspended Claude Code process for INSTANCE.
Sends SIGCONT to continue the process."
  (when (magnus-provider-external-p instance)
    (user-error "Process resume is not supported by the `%s' provider"
                (magnus-instance-provider instance)))
  (when (magnus-process--headless-p instance)
    (user-error "Headless task '%s' cannot be resumed interactively"
                (magnus-instance-name instance)))
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (when-let ((process (get-buffer-process buffer)))
        (when (process-live-p process)
          (signal-process process 'SIGCONT)
          (magnus-instances-update instance :status 'running)
          (when (get-buffer magnus-buffer-name)
            (magnus-status-refresh))
          (message "Resumed %s" (magnus-instance-name instance)))))))

(defun magnus-process-suspended-p (instance)
  "Return non-nil if INSTANCE is suspended."
  (eq (magnus-instance-status instance) 'suspended))

(defun magnus-process--coord-ownership-snapshot
    (directory &optional agent-name)
  "Snapshot coordination ownership and AGENT-NAME's row in DIRECTORY."
  (setq directory (magnus-coord--normalized-directory directory))
  (let* ((missing (make-symbol "missing-session-start"))
         (session-start
          (gethash directory magnus-coord--session-start-times missing)))
    (list :watching (and (member directory magnus-coord--watched-dirs) t)
          :session-start-present (not (eq session-start missing))
          :session-start session-start
          :agent-name agent-name
          :row-existed
          (and agent-name
               (magnus-process--coord-row-existed-p directory agent-name)))))

(defun magnus-process--restore-coord-ownership (directory snapshot)
  "Restore DIRECTORY's coordination ownership from SNAPSHOT.
A same-name Active Work row is removed only when it did not exist before the
failed attempt. Shared log history remains as an audit trail."
  (setq directory (magnus-coord--normalized-directory directory))
  (let ((agent-name (plist-get snapshot :agent-name)))
    (when (and agent-name (not (plist-get snapshot :row-existed)))
      (condition-case err
          (when (file-exists-p (magnus-coord-file-path directory))
            (magnus-coord-clear-agent directory agent-name)
            ;; Keep an existing watcher's presentation cache truthful without
            ;; consuming ordinary coordination messages.
            (when (member directory magnus-coord--watched-dirs)
              (magnus-coord--cache-content
               directory (magnus-coord--read-content directory))))
        (error
         (message "Magnus: coordination row rollback failed for %s: %s"
                  agent-name (error-message-string err))))))
  (condition-case err
      (if (plist-get snapshot :session-start-present)
          (puthash directory (plist-get snapshot :session-start)
                   magnus-coord--session-start-times)
        (remhash directory magnus-coord--session-start-times))
    (error
     (message "Magnus: session-state rollback failed for %s: %s"
              directory (error-message-string err))))
  (when (and (not (plist-get snapshot :watching))
             (member directory magnus-coord--watched-dirs))
    (condition-case err
        (magnus-coord-stop-watching directory)
      (error
       (message "Magnus: watcher rollback failed for %s: %s"
                directory (error-message-string err))))))

(defun magnus-process--stop-local-for-chdir (instance)
  "Synchronously stop INSTANCE's local terminal before a directory move."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (when-let ((process (get-buffer-process buffer)))
        (set-process-query-on-exit-flag process nil)
        (when (process-live-p process)
          (kill-process process)))
      (kill-buffer buffer)))
  (magnus-instances-update instance :status 'stopped :buffer nil))

(defun magnus-process--restore-chdir-instance
    (instance directory session-id previous-session-id purged-at)
  "Restore a failed move of INSTANCE to DIRECTORY as a stopped agent."
  (condition-case err
      (magnus-instances-update
       instance
       :directory directory
       :status 'stopped
       :buffer nil
       :session-id session-id
       :previous-session-id previous-session-id
       :purged-at purged-at)
    (error
     ;; `magnus-instances-update' mutates before running its notification hook.
     ;; Finish the restoration directly so a hook failure cannot replace the
     ;; startup error or leave a cross-project ghost behind.
     (setf (magnus-instance-directory instance) directory
           (magnus-instance-status instance) 'stopped
           (magnus-instance-buffer instance) nil
           (magnus-instance-session-id instance) session-id
           (magnus-instance-previous-session-id instance) previous-session-id
           (magnus-instance-purged-at instance) purged-at)
     (message "Magnus: instance rollback hook failed for %s: %s"
              (magnus-instance-name instance)
              (error-message-string err))
     instance)))

(defun magnus-process-chdir (instance directory)
  "Change INSTANCE's working directory to DIRECTORY.
Stop the old runtime, provision coordination and onboarding in the new root,
then synchronously start a fresh provider session.  The old root remains
owned until the new runtime is live.  If setup fails, restore the instance and
both roots' coordination ownership coherently; the old runtime remains
stopped so callers can retry or explicitly resurrect it."
  (magnus-process--ensure-changeable-lifecycle instance "change its project")
  (let* ((old-dir (directory-file-name
                   (expand-file-name (magnus-instance-directory instance))))
         (new-dir (directory-file-name (expand-file-name directory)))
         (old-project-key (magnus-coord--normalized-directory old-dir))
         (new-project-key (magnus-coord--normalized-directory new-dir))
         (name (magnus-instance-name instance))
         (external (magnus-provider-external-p instance))
         (original-session-id (magnus-instance-session-id instance))
         (original-previous-session-id
          (magnus-instance-previous-session-id instance))
         (original-purged-at (magnus-instance-purged-at instance)))
    (if (string= old-project-key new-project-key)
        (progn
          (message "%s is already in %s" name new-dir)
          instance)
      ;; Stop first while INSTANCE still names the old root.  Provider cleanup
      ;; and process sentinels must never observe a partially moved identity.
      (if external
          (magnus-provider-call instance 'stop t)
        (magnus-process--stop-local-for-chdir instance))
      (let* ((old-session-id
              (or (magnus-instance-session-id instance) original-session-id))
             (new-coord-snapshot
              (magnus-process--coord-ownership-snapshot
               new-project-key name))
             (magnus-process--transaction-runtime-buffer (list nil))
             new-coord-attempted
             new-runtime-attempted
             old-release-attempted
             committed)
        (unwind-protect
            (progn
              ;; Updating the durable instance first makes every path derived
              ;; during provisioning point at the destination project.
              (magnus-instances-update
               instance
               :directory new-dir
               :status 'stopped
               :buffer nil
               :session-id nil
               :previous-session-id old-session-id
               :purged-at nil)
              (magnus-process--ensure-agent-dir instance)
              (setq new-coord-attempted t)
              ;; Registration creates the shared coordination file, current
              ;; onboarding instructions, and destination watcher before startup.
              (magnus-coord-register-agent new-dir instance)
              (setq new-runtime-attempted t)
              (if external
                  (magnus-provider-call instance 'start)
                ;; Synchronous spawn is deliberate: setup failures belong to
                ;; this transaction rather than an unreportable timer callback.
                (magnus-process--spawn instance))
              ;; There is now no observation gap: destination ownership and its
              ;; provider are live before the source watcher is released.
              (setq old-release-attempted t)
              (magnus-coord-unregister-agent old-dir instance)
              (setq committed t)
              (message "Moved %s to %s (fresh %s thread)"
                       name new-dir
                       (if external
                           (magnus-instance-provider instance)
                         "Claude"))
              instance)
          (unless committed
            ;; Reverse destination acquisition before restoring the durable
            ;; instance.  Provider starts can fail after attaching a buffer.
            (when new-runtime-attempted
              (magnus-process--discard-created-runtime
               instance external
               (car magnus-process--transaction-runtime-buffer)))
            (when new-coord-attempted
              (condition-case err
                  (magnus-process--restore-coord-ownership
                   new-project-key new-coord-snapshot)
                (error
                 (message "Magnus: destination ownership rollback failed for %s: %s"
                          new-dir (error-message-string err)))))
            (magnus-process--restore-chdir-instance
             instance old-dir old-session-id original-previous-session-id
             original-purged-at)
            ;; A failing source release may have stopped its watcher before
            ;; signaling.  Re-register only in that ambiguous case; otherwise
            ;; the source ownership was deliberately kept throughout setup.
            (when old-release-attempted
              (condition-case err
                  (magnus-coord-register-agent old-dir instance)
                (error
                 (message "Magnus: source ownership rollback failed for %s: %s"
                          old-dir (error-message-string err)))))))))))

(defun magnus-process--project-hash (directory)
  "Convert DIRECTORY's physical root to Claude's project hash format.
Claude records Node's physical working directory, so symlink aliases must map
to the same session directory.  Replace slashes, spaces, tildes, and
underscores with hyphens."
  (let ((path (magnus-coord--normalized-directory directory)))
    (replace-regexp-in-string "[/ ~_]+" "-" path)))

(defun magnus-process--spawn-with-session (instance &optional session-id)
  "Spawn Claude Code for INSTANCE, optionally resuming SESSION-ID.
The complete current Magnus onboarding is part of process launch, before the
delayed ready notification."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (buffer-name (format "*claude:%s*" name))
         (default-directory directory)
         (command
          (magnus-process--claude-command-with-onboarding
           instance
           (when session-id (list "--resume" session-id))))
         buffer
         owner-process
         ready-timer
         spawned)
    (unwind-protect
        (progn
          (setq buffer
                (magnus-terminal-create-command-buffer buffer-name command))
          (setq owner-process (get-buffer-process buffer))
          (magnus-process--record-transaction-runtime buffer owner-process)
          (with-current-buffer buffer
            (setq-local magnus-process--instance-id
                        (magnus-instance-id instance)))
          (magnus-instances-update instance
                                   :buffer buffer
                                   :status 'running)
          (magnus-process--setup-sentinel instance buffer)
          ;; First-turn ordering is already guaranteed by COMMAND.  This timer
          ;; only publishes the existing delayed readiness notification.
          (setq ready-timer
                (run-with-timer
                 5 nil #'magnus-process--run-ready-hook
                 instance buffer owner-process))
          (setq spawned t)
          buffer)
      (unless spawned
        (when (timerp ready-timer)
          (cancel-timer ready-timer))
        (magnus-process--discard-created-runtime
         instance nil (and buffer (cons buffer owner-process)))))))

;;; Instance interaction

(defun magnus-process--ensure-changeable-lifecycle (instance action)
  "Reject lifecycle states where INSTANCE cannot safely ACTION."
  (when (magnus-process--headless-p instance)
    (user-error "Headless task '%s' cannot %s"
                (magnus-instance-name instance) action))
  (pcase (magnus-instance-status instance)
    ('purged
     (user-error "Instance '%s' is archived; resurrect it with R before trying to %s"
                 (magnus-instance-name instance) action))
    ((or 'finished 'errored)
     (user-error
      "Instance '%s' is a completed headless task; create an agent to %s"
      (magnus-instance-name instance) action))))

(defun magnus-process--discard-stale-local-buffer (instance)
  "Release INSTANCE's stale local terminal and publish a stopped state."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (magnus-terminal--discard-buffer buffer)))
  (magnus-instances-update instance :buffer nil :status 'stopped))

(defun magnus-process-switch-to (instance)
  "Switch to the buffer for INSTANCE.
If the buffer is nil (e.g. after Emacs restart), resume the session
if a session ID exists, or spawn a fresh process."
  (let ((status (magnus-instance-status instance))
        (buffer (magnus-instance-buffer instance)))
    (cond
     ((eq status 'purged)
      (user-error "Instance '%s' is archived; resurrect it with R"
                  (magnus-instance-name instance)))
     ;; A headless task can only display the output buffer owned by its one
     ;; execution.  Never turn a persisted or bufferless task into a TUI.
     ((magnus-process--headless-p instance)
      (if (buffer-live-p buffer)
          (switch-to-buffer buffer)
        (user-error "Headless output for '%s' is no longer available"
                    (magnus-instance-name instance))))
     ;; A completed headless buffer is useful output, not an interactive agent
     ;; to restart.  Retain the old visit behavior only while that output lives.
     ((memq status '(finished errored))
      (if (buffer-live-p buffer)
          (switch-to-buffer buffer)
        (user-error "Headless output for '%s' is no longer available"
                    (magnus-instance-name instance))))
     ((magnus-provider-external-p instance)
      (magnus-provider-call instance 'switch-to))
     ((magnus-process-running-p instance)
      (switch-to-buffer buffer))
     (t
      ;; A sentinel can leave a readable but process-less vterm behind.  It is
      ;; not a runtime to revisit and would also collide with the replacement.
      (when (or buffer (memq status '(running suspended)))
        (magnus-process--discard-stale-local-buffer instance))
      (if-let ((session-id (magnus-instance-session-id instance)))
          (magnus-process--spawn-with-session instance session-id)
        (magnus-process--spawn instance))
      (switch-to-buffer (magnus-instance-buffer instance))))))

(defun magnus-process-running-p (instance)
  "Return non-nil if INSTANCE has a running process."
  (if (magnus-provider-external-p instance)
      (magnus-provider-call instance 'running-p)
    (when-let ((buffer (magnus-instance-buffer instance)))
      (and (buffer-live-p buffer)
           (get-buffer-process buffer)
           (process-live-p (get-buffer-process buffer))))))

;;; Reconnection

(defun magnus-process--owned-terminal-p (instance buffer)
  "Return non-nil when BUFFER is INSTANCE's live tagged Claude terminal."
  (and (buffer-live-p buffer)
       (equal (buffer-local-value 'magnus-process--instance-id buffer)
              (magnus-instance-id instance))
       (when-let ((process (get-buffer-process buffer)))
         (process-live-p process))))

(defun magnus-process--find-owned-terminal (instance)
  "Return INSTANCE's unambiguous live Claude terminal, or nil.
Prefer the current runtime pointer.  After persistence replaces the instance
object, recover a uniquely tagged buffer regardless of its display name."
  (let ((current (magnus-instance-buffer instance)))
    (if (magnus-process--owned-terminal-p instance current)
        current
      (let ((matches
             (cl-remove-if-not
              (lambda (buffer)
                (magnus-process--owned-terminal-p instance buffer))
              (buffer-list))))
        (cond
         ((null (cdr matches)) (car matches))
         (t
          (message "Magnus: multiple live terminals claim instance %s; refusing to guess"
                   (magnus-instance-name instance))
          nil))))))

(defun magnus-process-reconnect (instance)
  "Try to reconnect INSTANCE to an existing buffer/process."
  (if (magnus-provider-external-p instance)
      (progn
        ;; A provider's terminal cannot survive Emacs.  Preserve its session ID;
        ;; visiting the instance will resume it in a fresh display buffer.
        (when (memq (magnus-instance-status instance) '(running suspended))
          (magnus-instances-update instance :status 'stopped :buffer nil))
        nil)
    (unless (memq (magnus-instance-status instance)
                  '(purged finished errored))
      (let ((buffer (magnus-process--find-owned-terminal instance)))
        (if buffer
            (progn
              (magnus-instances-update
               instance
               :buffer buffer
               :status (if (eq (magnus-instance-status instance) 'suspended)
                           'suspended
                         'running))
              ;; Persistence replaces the registry object.  Re-home both the
              ;; durable terminal tag and its exit callback to that new object.
              (with-current-buffer buffer
                (setq-local magnus-process--instance-id
                            (magnus-instance-id instance)))
              (magnus-process--setup-sentinel instance buffer))
          (when (or (memq (magnus-instance-status instance)
                          '(running suspended))
                    (magnus-instance-buffer instance))
            (magnus-instances-update instance :status 'stopped :buffer nil)))))))

;;; Headless mode — fire-and-forget agents

(defvar magnus-headless-allowed-tools)

(defvar-local magnus-process--headless-instance nil
  "The instance associated with this headless buffer.")

(define-derived-mode magnus-process-headless-mode special-mode "Headless"
  "Major mode for headless Claude Code output buffers."
  :group 'magnus
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(let ((map magnus-process-headless-mode-map))
  (define-key map (kbd "q") #'quit-window))

(defun magnus-process--headless-p (instance)
  "Return non-nil if INSTANCE is a headless (non-interactive) agent."
  (eq (magnus-instance-effective-kind instance) 'headless))

(defvar magnus--creation-task)

(defun magnus-process-create-headless (prompt &optional directory name)
  "Create a headless Claude Code instance with PROMPT.
DIRECTORY is the working directory.  NAME is optional.
Binds the prompt as `magnus--creation-task' for smart resurrection.
Returns the new instance."
  (interactive "sTask prompt: ")
  (let* ((dir (or directory (magnus-process--get-directory)))
         (magnus--creation-task prompt)
         (instance-name (or name
                            (concat "headless-"
                                    (funcall magnus-instance-name-generator dir))))
         (instance (magnus-instances-create
                    dir instance-name nil 'headless)))
    (magnus-process--create-transaction
     instance
     (lambda (candidate _external)
       (magnus-process--spawn-headless candidate prompt)))))

(defun magnus-process--spawn-headless (instance prompt)
  "Spawn a headless Claude Code process for INSTANCE with PROMPT."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (buffer-name (format "*claude-headless:%s*" name))
         (full-prompt (magnus-process--headless-prompt instance prompt))
         (buffer (generate-new-buffer buffer-name))
         process
         started)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (magnus-process-headless-mode)
            (setq magnus-process--headless-instance instance)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (propertize (format "Headless agent: %s\n" name)
                                  'face 'font-lock-keyword-face))
              (insert (propertize (format "Directory: %s\n" directory)
                                  'face 'font-lock-comment-face))
              (insert (propertize (format "Prompt: %s\n\n" prompt)
                                  'face 'font-lock-comment-face))
              (insert (propertize "--- Output ---\n\n"
                                  'face 'magnus-trace-separator))))
          (magnus-instances-update instance :buffer buffer :status 'running)
          (setq
           process
           (magnus-headless-start
            'claude
            (list :purpose 'agent
                  :directory directory
                  :prompt full-prompt
                  :allowed-tools magnus-headless-allowed-tools
                  :name name
                  :buffer buffer)
            (list
             :on-event
             (lambda (event-process event)
               (magnus-process--headless-render-event
                instance event-process event buffer))
             :on-complete
             (lambda (completed-process result)
               (magnus-process--headless-complete
                instance completed-process result buffer)))))
          (magnus-process--record-transaction-runtime buffer process)
          (setq started t)
          buffer)
      (unless started
        (when (buffer-live-p buffer)
          (when-let ((process (get-buffer-process buffer)))
            (ignore-errors (set-process-query-on-exit-flag process nil))
            (when (process-live-p process)
              (ignore-errors (delete-process process))))
          (ignore-errors (kill-buffer buffer)))))))

(defun magnus-process--headless-prompt (instance prompt)
  "Build the full headless prompt for INSTANCE wrapping user PROMPT."
  (magnus-onboarding-task-prompt instance prompt))

(defun magnus-process--headless-render-event
    (instance process event &optional owner-buffer)
  "Render canonical headless EVENT from PROCESS for INSTANCE.
OWNER-BUFFER preserves the launch identity if PROCESS is later detached."
  (let* ((current-instance (magnus-process--current-instance instance))
         (buf (or owner-buffer (process-buffer process))))
    (when (and (buffer-live-p buf)
               (magnus-process--callback-owner-p
                current-instance buf process))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (pcase (plist-get event :type)
            ("assistant"
             (when-let ((text (plist-get event :text)))
               (insert text)))
            ("result"
             (insert (propertize "\n--- Task Complete ---\n"
                                 'face 'magnus-trace-separator))
             (when-let ((cost (plist-get event :cost-usd)))
               (insert (format "Cost: $%.4f\n" cost))))))))))

(defun magnus-process--headless-complete
    (instance process result &optional owner-buffer)
  "Publish RESULT for headless INSTANCE completed by PROCESS.
OWNER-BUFFER preserves the runtime identity after its output buffer is killed."
  (let* ((instance (magnus-process--current-instance instance))
         (buffer (or owner-buffer (process-buffer process))))
    ;; Completion can arrive after stop/archive detached this runtime and a
    ;; replacement began.  Only the exact buffer still owned by INSTANCE may
    ;; publish terminal state or append output.
    (when (magnus-process--callback-owner-p instance buffer process)
      (let* ((process-status (plist-get result :status))
             (event-str (string-trim
                         (or (plist-get result :process-event)
                             (symbol-name (or process-status 'stopped)))))
             (new-status
              (cond
               ((plist-get result :success-p) 'finished)
               ((eq process-status 'exit) 'errored)
               (t 'stopped))))
        ;; Archiving intentionally wins a race with a late completion.
        (unless (eq (magnus-instance-status instance) 'purged)
          (magnus-instances-update instance :status new-status))
        (pcase new-status
          ('finished
           (message "Magnus: headless agent '%s' completed"
                    (magnus-instance-name instance)))
          ('errored
           (message "Magnus: headless agent '%s' failed: %s"
                    (magnus-instance-name instance) event-str)))
        (unless (eq (magnus-instance-status instance) 'purged)
          (condition-case err
              (magnus-coord-add-log
               (magnus-instance-directory instance)
               (magnus-instance-name instance)
               (format "Headless task %s"
                       (replace-regexp-in-string "[\r\n]+" " " event-str)))
            (error
             (message "Magnus: could not log headless completion for %s: %s"
                      (magnus-instance-name instance)
                      (error-message-string err)))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert
               (propertize (format "\n--- Process %s ---\n" event-str)
                           'face 'magnus-trace-separator)))))
        (when (and (boundp 'magnus-buffer-name)
                   (get-buffer magnus-buffer-name))
          (magnus-status-refresh))))))

(provide 'magnus-process)
;;; magnus-process.el ends here
