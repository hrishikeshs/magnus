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

(require 'vterm)
(require 'magnus-instances)
(require 'magnus-coord)
(require 'magnus-trace)

(declare-function magnus-status-refresh "magnus-status")
(declare-function magnus-project-root "magnus")

;; Variables defined in magnus.el
(defvar magnus-claude-executable)
(defvar magnus-default-directory)
(defvar magnus-instance-name-generator)
(defvar magnus-buffer-name)
(defvar magnus--summon-context)

;;; Process creation

(defun magnus-process-create (&optional directory name)
  "Create a new Claude Code instance.
DIRECTORY is the working directory.  If nil, prompts for one.
NAME is the instance name.  If nil, auto-generates one."
  (interactive)
  (let* ((dir (or directory
                  (magnus-process--get-directory)))
         (instance-name (or name
                            (funcall magnus-instance-name-generator dir)))
         (instance (magnus-instances-create dir instance-name)))
    (magnus-instances-add instance)
    ;; Set up coordination files and register agent
    (magnus-coord-register-agent dir instance)
    (magnus-process--spawn instance)
    instance))

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
         (sessions-before (magnus-process--list-sessions directory)))
    ;; Create vterm buffer
    (let ((buffer (magnus-process--create-vterm-buffer buffer-name)))
      (magnus-instances-update instance
                               :buffer buffer
                               :status 'running)
      ;; Send the claude command
      (with-current-buffer buffer
        (vterm-send-string magnus-claude-executable)
        (vterm-send-return))
      ;; Set up process sentinel
      (magnus-process--setup-sentinel instance buffer)
      ;; Send onboarding message after Claude starts
      ;; Capture summon context now (dynamic binding unwinds before timer fires)
      (let ((summon-ctx magnus--summon-context))
        (run-with-timer 3 nil #'magnus-process--send-onboarding
                        instance summon-ctx))
      ;; Watch for new session to appear
      (magnus-process--watch-for-session instance directory sessions-before)
      buffer)))

(defun magnus-process--send-onboarding (instance &optional summon-context)
  "Send onboarding message to INSTANCE.
SUMMON-CONTEXT, if non-nil, is a plist with :sender and :reason
from an agent-initiated summon.
Delays the Return keystroke so the terminal has time to process
the full message text before submitting."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (let ((msg (magnus-process--onboarding-message instance summon-context)))
        (with-current-buffer buffer
          (vterm-send-string msg))
        ;; Delay Return so the TUI can digest the pasted text
        (run-with-timer 0.5 nil
                        (lambda ()
                          (when (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (vterm-send-return)))))))))

(defun magnus-process--agent-memory-path (instance)
  "Return the memory file path for INSTANCE.
Path is <directory>/.claude/agents/<name>/memory.md."
  (expand-file-name
   (format ".claude/agents/%s/memory.md" (magnus-instance-name instance))
   (magnus-instance-directory instance)))

(defun magnus-process--agent-busy-path (instance)
  "Return the busy signal file path for INSTANCE.
Path is <directory>/.claude/agents/<name>/busy."
  (expand-file-name
   (format ".claude/agents/%s/busy" (magnus-instance-name instance))
   (magnus-instance-directory instance)))

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
  (let* ((name (magnus-instance-name instance))
         (memory-path (magnus-process--agent-memory-path instance))
         (has-memory (file-exists-p memory-path))
         (prev-session (magnus-instance-previous-session-id instance))
         (prev-trace (when prev-session
                       (magnus-process--session-jsonl-path
                        (magnus-instance-directory instance)
                        prev-session))))
    (if has-memory
        (magnus-process--onboarding-returning name prev-trace summon-context)
      (magnus-process--onboarding-new name summon-context))))

(defun magnus-process--onboarding-returning (name prev-trace summon-context)
  "Generate onboarding for a returning agent NAME.
PREV-TRACE is the path to the previous session trace, or nil.
SUMMON-CONTEXT is a plist with :sender and :reason, or nil."
  (concat
   (format "You are %s. You've been here before.\n\n" name)
   (format "\
Your memory is at .claude/agents/%s/memory.md — read it now, before anything \
else. That file is you: decisions you made, patterns you discovered, \
relationships with other agents, work left unfinished. Everything that makes \
you *you* persists there.\n" name)
   (if prev-trace
       (format "\nYour last session trace: %s — skim it for recent context.\n" prev-trace)
     "")
   (if summon-context
       (format "\n\
%s summoned you back. Reason: %s\n\
After catching up on your memory and the coordination file, \
prioritize their request. Find them in .magnus-coord.md.\n"
               (plist-get summon-context :sender)
               (plist-get summon-context :reason))
     "")
   (format "\n\
Welcome back. You are one of several Claude Code agents managed by Magnus \
(an Emacs orchestration tool). Others may be active right now.\n\n\
Your home directory: .claude/agents/%s/\n\
  memory.md — your persistent memory (update it as you work)\n\
  busy      — create this when you need deep focus; Magnus stops nudging you\n\n\
Get oriented:\n\
1. Read .magnus-coord.md — the Active Work table shows who's doing what. \
The Discoveries section has things others learned. The Log has recent messages.\n\
2. Read .claude/magnus-instructions.md for the full coordination protocol.\n\
3. Announce your plan in the Log and claim your files in Active Work.\n\n\
While working:\n\
- Check .magnus-coord.md periodically for messages and discoveries from teammates.\n\
- When you learn something non-obvious, add it to the Discoveries section.\n\
- @mention an agent in the Log to get their attention — they are notified instantly.\n\
- Run: ls .claude/agents/ — see who else has worked here. Their memory files \
are at .claude/agents/<name>/memory.md. Read them if you need context on past work.\n\
- Need a dormant expert back? Write [SUMMON @agent-name] reason in the coord log. \
Magnus confirms with the user and resurrects them.\n\
- Before you sign off, update your memory file. Write down what you learned, \
what you decided, what's unfinished. Future-you depends on it.\n\n\
Start with step 1."
           name)))

(defun magnus-process--onboarding-new (name summon-context)
  "Generate onboarding for a new agent NAME.
SUMMON-CONTEXT is a plist with :sender and :reason, or nil."
  (concat
   (if summon-context
       (format "\
You were summoned by %s — %s. Once you're oriented, prioritize their request.\n\n"
               (plist-get summon-context :sender)
               (plist-get summon-context :reason))
     "")
   (format "\
Welcome! You are agent '%s', one of several Claude Code agents managed by \
Magnus (an Emacs orchestration tool). Others may be active right now.\n\n\
Your home directory: .claude/agents/%s/\n\
  memory.md — doesn't exist yet. You'll create it before you leave.\n\
  busy      — create this when you need deep focus; Magnus stops nudging you\n\n\
Get oriented:\n\
1. Read .magnus-coord.md — the Active Work table shows who's doing what. \
The Discoveries section has things others learned. The Log has recent messages.\n\
2. Read .claude/magnus-instructions.md for the full coordination protocol.\n\
3. Announce your plan in the Log and claim your files in Active Work.\n\n\
While working:\n\
- Check .magnus-coord.md periodically for messages and discoveries from teammates.\n\
- When you learn something non-obvious, add it to the Discoveries section.\n\
- @mention an agent in the Log to get their attention — they are notified instantly.\n\
- Run: ls .claude/agents/ — you'll see who's been here before you. Their memory \
files are at .claude/agents/<name>/memory.md. Read them — you'll find context, \
gotchas, and decisions that save you hours.\n\
- Need a dormant expert back? Write [SUMMON @agent-name] reason in the coord log. \
Magnus confirms with the user and resurrects them.\n\
- Before you sign off, write your memory to .claude/agents/%s/memory.md. \
Record what you learned, decisions you made, gotchas you hit, relationships with \
other agents, and any unfinished work. This is how future-you picks up where you \
left off.\n\n\
Start with step 1."
           name name name)))

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

(defun magnus-process--watch-for-session (instance directory sessions-before)
  "Watch for a new session to appear for INSTANCE in DIRECTORY.
SESSIONS-BEFORE is the list of sessions that existed before spawning.
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
           (detect-fn
            (lambda ()
              (magnus-process--detect-new-session
               instance directory sessions-before
               (list descriptor poll-timer cleanup-timer)))))
      ;; Primary: file-notify watcher
      (setq descriptor
            (file-notify-add-watch
             sessions-dir '(change)
             (lambda (_event) (funcall detect-fn))))
      ;; Fallback: poll every 5 seconds
      (setq poll-timer
            (run-with-timer 5 5 detect-fn))
      ;; Final cleanup after 120 seconds
      (setq cleanup-timer
            (run-with-timer 120 nil
                            (lambda ()
                              (when descriptor
                                (ignore-errors (file-notify-rm-watch descriptor)))
                              (when poll-timer
                                (cancel-timer poll-timer))))))))

(defun magnus-process--detect-new-session (instance directory sessions-before resources)
  "Try to detect a new session for INSTANCE in DIRECTORY.
SESSIONS-BEFORE is the pre-spawn session list.  RESOURCES is
a list of (descriptor poll-timer cleanup-timer) to clean up."
  (let* ((sessions-after (magnus-process--list-sessions directory))
         (new-sessions (cl-set-difference sessions-after sessions-before :test #'string=)))
    (when new-sessions
      (let ((session-id (if (= 1 (length new-sessions))
                            (car new-sessions)
                          (magnus-process--most-recent-session directory new-sessions))))
        (magnus-instances-update instance :session-id session-id)
        (message "Captured session %s for %s"
                 session-id (magnus-instance-name instance)))
      ;; Clean up all watchers/timers
      (let ((descriptor (nth 0 resources))
            (poll-timer (nth 1 resources))
            (cleanup-timer (nth 2 resources)))
        (when descriptor
          (ignore-errors (file-notify-rm-watch descriptor)))
        (when poll-timer
          (cancel-timer poll-timer))
        (when cleanup-timer
          (cancel-timer cleanup-timer))))))

(defun magnus-process--most-recent-session (directory sessions)
  "Find the most recently modified session in DIRECTORY from SESSIONS list."
  (let* ((project-hash (magnus-process--project-hash directory))
         (sessions-dir (expand-file-name
                        (concat "projects/" project-hash)
                        (expand-file-name ".claude" (getenv "HOME")))))
    ;; Filter to sessions whose .jsonl files actually exist
    (let ((valid (cl-remove-if-not
                  (lambda (s)
                    (file-exists-p
                     (expand-file-name (concat s ".jsonl") sessions-dir)))
                  sessions)))
      (car (sort valid
                 (lambda (a b)
                   (time-less-p
                    (file-attribute-modification-time
                     (file-attributes
                      (expand-file-name (concat b ".jsonl") sessions-dir)))
                    (file-attribute-modification-time
                     (file-attributes
                      (expand-file-name (concat a ".jsonl") sessions-dir))))))))))

(defun magnus-process--create-vterm-buffer (buffer-name)
  "Create a vterm buffer with BUFFER-NAME."
  (let ((buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
      (vterm-mode)
      (magnus-process--setup-keys))
    buffer))

(defun magnus-process-send-escape ()
  "Send ESC to Claude Code (mapped from \\`keyboard-quit')."
  (interactive)
  (vterm-send-key "<escape>"))

(defun magnus-process--setup-keys ()
  "Set up keybindings for Claude Code in the current vterm buffer.
Maps \\`keyboard-quit' to send ESC, since Emacs intercepts the real ESC key."
  (local-set-key (kbd "C-g") #'magnus-process-send-escape))

;;; Trace buffer entry point

(declare-function magnus-trace-open "magnus-trace")

(defun magnus-process-trace (instance)
  "Open the trace buffer for INSTANCE showing thinking and messages."
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
         (magnus-instances-update instance :status 'stopped)
         (when (get-buffer magnus-buffer-name)
           (magnus-status-refresh)))))))

;;; Process control

(defun magnus-process-kill (instance &optional force)
  "Kill the Claude Code process for INSTANCE.
If FORCE is non-nil, forcefully terminate."
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
                (vterm-send-key "C-c")))))))
      ;; Give process time to exit, then kill buffer
      (run-with-timer
       1 nil
       (lambda ()
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))
  (magnus-instances-update instance :status 'stopped :buffer nil))

(defun magnus-process-kill-and-remove (instance &optional force)
  "Kill INSTANCE and remove it from the registry.
If FORCE is non-nil, forcefully terminate."
  (let ((directory (magnus-instance-directory instance)))
    (magnus-coord-unregister-agent directory instance))
  (magnus-process-kill instance force)
  (magnus-instances-remove instance))

(defvar magnus-process--restarting nil
  "Set of instance IDs currently mid-restart, to prevent double-spawn.")

(defun magnus-process-restart (instance)
  "Restart the Claude Code process for INSTANCE.
Preserves the current session-id as previous-session-id so the
new incarnation can read its predecessor's thinking trace."
  (let ((id (magnus-instance-id instance)))
    (when (member id magnus-process--restarting)
      (user-error "Instance '%s' is already restarting"
                  (magnus-instance-name instance)))
    (push id magnus-process--restarting)
    ;; Save current session as previous before killing
    (when-let ((session (magnus-instance-session-id instance)))
      (magnus-instances-update instance :previous-session-id session))
    (magnus-process-kill instance)
    (run-with-timer
     1.5 nil
     (lambda ()
       (setq magnus-process--restarting
             (delete id magnus-process--restarting))
       (magnus-process--spawn instance)))))

(defun magnus-process-suspend (instance)
  "Suspend the Claude Code process for INSTANCE.
Sends SIGTSTP to pause the process.  Use `magnus-process-resume' to continue."
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
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (when-let ((process (get-buffer-process buffer)))
        (signal-process process 'SIGCONT)
        (magnus-instances-update instance :status 'running)
        (when (get-buffer magnus-buffer-name)
          (magnus-status-refresh))
        (message "Resumed %s" (magnus-instance-name instance))))))

(defun magnus-process-suspended-p (instance)
  "Return non-nil if INSTANCE is suspended."
  (eq (magnus-instance-status instance) 'suspended))

(defun magnus-process-chdir (instance directory)
  "Change INSTANCE's working directory to DIRECTORY.
Kills the current process and respawns in the new directory."
  (let* ((new-dir (expand-file-name directory))
         (name (magnus-instance-name instance)))
    ;; Kill old process and buffer immediately (no timers)
    (when-let ((buffer (magnus-instance-buffer instance)))
      (when (buffer-live-p buffer)
        (let ((process (get-buffer-process buffer)))
          (when (and process (process-live-p process))
            (kill-process process)))
        (kill-buffer buffer)))
    ;; Update directory, clear old session (it belongs to the old project)
    (magnus-instances-update instance
                             :directory new-dir
                             :status 'stopped
                             :buffer nil
                             :session-id nil)
    ;; Spawn fresh in the new directory
    (run-with-timer
     1 nil
     (lambda ()
       (magnus-process--spawn instance)))
    (message "Moving %s to %s (fresh start)..." name new-dir)))

(defun magnus-process--project-hash (directory)
  "Convert DIRECTORY to Claude's project hash format.
Replaces slashes, spaces, and tildes with hyphens."
  (let ((path (directory-file-name (expand-file-name directory))))
    (replace-regexp-in-string "[/ ~]+" "-" path)))

(defun magnus-process--spawn-with-session (instance &optional session-id)
  "Spawn a Claude Code process for INSTANCE, optionally resuming SESSION-ID."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (buffer-name (format "*claude:%s*" name))
         (default-directory directory))
    ;; Create vterm buffer
    (let ((buffer (magnus-process--create-vterm-buffer buffer-name)))
      (magnus-instances-update instance
                               :buffer buffer
                               :status 'running)
      ;; Send the claude command with optional --resume
      (with-current-buffer buffer
        (if session-id
            (vterm-send-string (format "%s --resume %s"
                                       magnus-claude-executable session-id))
          (vterm-send-string magnus-claude-executable))
        (vterm-send-return))
      ;; Set up process sentinel
      (magnus-process--setup-sentinel instance buffer)
      ;; Only send onboarding if no session (fresh start)
      (unless session-id
        (run-with-timer 3 nil #'magnus-process--send-onboarding instance))
      buffer)))

;;; Instance interaction

(defun magnus-process-switch-to (instance)
  "Switch to the buffer for INSTANCE."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (if (buffer-live-p buffer)
        (switch-to-buffer buffer)
      ;; Buffer is dead, respawn
      (magnus-process--spawn instance)
      (switch-to-buffer (magnus-instance-buffer instance)))))

(defun magnus-process-running-p (instance)
  "Return non-nil if INSTANCE has a running process."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (and (buffer-live-p buffer)
         (get-buffer-process buffer)
         (process-live-p (get-buffer-process buffer)))))

;;; Reconnection

(defun magnus-process-reconnect (instance)
  "Try to reconnect INSTANCE to an existing buffer/process."
  (let* ((name (magnus-instance-name instance))
         (buffer-name (format "*claude:%s*" name))
         (buffer (get-buffer buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (magnus-instances-update instance
                               :buffer buffer
                               :status (if (get-buffer-process buffer)
                                           'running
                                         'stopped)))))

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
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (derived-mode-p 'magnus-process-headless-mode)))))

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
         (instance (magnus-instances-create dir instance-name)))
    (magnus-instances-add instance)
    (magnus-coord-register-agent dir instance)
    (magnus-process--spawn-headless instance prompt)
    instance))

(defun magnus-process--spawn-headless (instance prompt)
  "Spawn a headless Claude Code process for INSTANCE with PROMPT."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (buffer-name (format "*claude-headless:%s*" name))
         (default-directory directory)
         (full-prompt (magnus-process--headless-prompt instance prompt))
         (args (magnus-process--headless-args full-prompt)))
    (let ((buffer (get-buffer-create buffer-name)))
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
      (let ((partial-line ""))
        (make-process
         :name (format "claude-headless-%s" name)
         :buffer buffer
         :command (cons magnus-claude-executable args)
         :connection-type 'pipe
         :sentinel (magnus-process--headless-sentinel instance)
         :filter (lambda (proc output)
                   (setq partial-line
                         (magnus-process--headless-filter-output
                          instance proc output partial-line)))))
      buffer)))

(defun magnus-process--headless-prompt (instance prompt)
  "Build the full headless prompt for INSTANCE wrapping user PROMPT."
  (let ((name (magnus-instance-name instance)))
    (format "You are agent '%s', managed by Magnus. Before starting, \
read .magnus-coord.md to check what other agents are doing. \
Update the Active Work table with your name and planned work. \
Then execute this task:\n\n%s\n\n\
When done, update .magnus-coord.md to mark your work as complete."
            name prompt)))

(defun magnus-process--headless-args (prompt)
  "Build command-line arguments for a headless Claude process with PROMPT."
  (list "--print" prompt
        "--output-format" "stream-json"
        "--allowedTools" magnus-headless-allowed-tools))

(defun magnus-process--headless-sentinel (instance)
  "Return a process sentinel for headless INSTANCE."
  (lambda (proc event)
    (let ((event-str (string-trim event)))
      (cond
       ((string= event-str "finished")
        (magnus-instances-update instance :status 'finished)
        (message "Magnus: headless agent '%s' completed"
                 (magnus-instance-name instance)))
       ((string-prefix-p "exited abnormally" event-str)
        (magnus-instances-update instance :status 'errored)
        (message "Magnus: headless agent '%s' failed: %s"
                 (magnus-instance-name instance) event-str))
       (t
        (magnus-instances-update instance :status 'stopped)))
      ;; Append status to buffer
      (when-let ((buf (process-buffer proc)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (propertize (format "\n--- Process %s ---\n" event-str)
                                  'face 'magnus-trace-separator))))))
      ;; Update coordination
      (let ((dir (magnus-instance-directory instance))
            (name (magnus-instance-name instance)))
        (magnus-coord-add-log dir name
                              (format "Headless task %s" event-str)))
      ;; Refresh status buffer
      (when (get-buffer magnus-buffer-name)
        (magnus-status-refresh)))))

(defun magnus-process--headless-filter-output (instance proc output partial)
  "Process stream-json OUTPUT from PROC for headless INSTANCE.
PARTIAL is the incomplete line from previous call.  Returns new partial."
  (let* ((combined (concat partial output))
         (lines (split-string combined "\n"))
         (remainder (car (last lines)))
         (complete-lines (butlast lines)))
    (dolist (line complete-lines)
      (when (and (not (string-empty-p line))
                 (string-prefix-p "{" (string-trim line)))
        (condition-case nil
            (let* ((json (json-parse-string (string-trim line) :object-type 'alist))
                   (type (alist-get 'type json)))
              (magnus-process--headless-handle-event instance proc json type))
          (error nil))))
    remainder))

(defun magnus-process--headless-handle-event (instance proc json type)
  "Handle a stream-json event of TYPE with JSON data.
INSTANCE is the headless agent, PROC is its process,
and JSON is the parsed event payload."
  (ignore instance)
  (when-let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (pcase type
            ("assistant"
             (when-let ((content (alist-get 'content (alist-get 'message json))))
               (when (vectorp content)
                 (seq-doseq (block content)
                   (when (string= (alist-get 'type block) "text")
                     (insert (alist-get 'text block)))))))
            ("result"
             (insert (propertize "\n--- Task Complete ---\n"
                                 'face 'magnus-trace-separator))
             (when-let ((cost (alist-get 'cost_usd json)))
               (insert (format "Cost: $%.4f\n" cost))))))))))

(provide 'magnus-process)
;;; magnus-process.el ends here
