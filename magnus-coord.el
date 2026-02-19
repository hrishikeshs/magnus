;;; magnus-coord.el --- Agent coordination for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides inter-agent coordination through a shared
;; coordination file (.magnus-coord.md).  Agents use this file to:
;; - Announce what they're working on
;; - Communicate with other agents
;; - Record decisions and agreements
;; - Avoid stepping on each other's work

;;; Code:

(require 'magnus-instances)
(require 'filenotify)

(declare-function vterm-send-string "vterm")
(declare-function project-root "project")
(declare-function vterm-send-return "vterm")

;;; Customization

(defcustom magnus-coord-file ".magnus-coord.md"
  "Name of the coordination file in project directories."
  :type 'string
  :group 'magnus)

(defcustom magnus-coord-instructions-file ".claude/magnus-instructions.md"
  "Path to the instructions file for agents (relative to project)."
  :type 'string
  :group 'magnus)

(defcustom magnus-coord-mention-notify t
  "If non-nil, automatically notify agents when they are @mentioned."
  :type 'boolean
  :group 'magnus)

(defcustom magnus-coord-skill-file ".claude/skills/coordinate/SKILL.md"
  "Path to the coordination skill file (relative to project)."
  :type 'string
  :group 'magnus)

(defcustom magnus-coord-reminder-interval 600
  "Seconds between coordination file reminders to agents.
Set to nil to disable.  Default is 600 (10 minutes)."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'magnus)

(defcustom magnus-coord-log-max-entries 25
  "Maximum number of log entries to keep in the coordination file.
Older entries are trimmed automatically.  Set to nil to disable."
  :type '(choice (integer :tag "Entries")
                 (const :tag "Unlimited" nil))
  :group 'magnus)

(defcustom magnus-coord-active-cooldown 120
  "Seconds after last chat message before nudges resume.
When the user sends a message via the chat buffer, all
coordination nudges are suppressed for this many seconds."
  :type 'integer
  :group 'magnus)

(defcustom magnus-coord-idle-threshold 300
  "Seconds of inactivity before telling agents to sleep.
When the user is idle for this long, a sleep message is sent to
all running agents and periodic nudges are suppressed.  When the
user returns, a wake-up message is sent.  Set to nil to disable."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'magnus)

(defcustom magnus-coord-context-warn-threshold 0.80
  "Context utilization ratio at which to warn an agent.
When an agent's context usage exceeds this fraction of the
maximum window, a memory consolidation warning is sent.
Set to nil to disable.  Default is 0.80 (80%)."
  :type '(choice (float :tag "Ratio (0.0-1.0)")
                 (const :tag "Disabled" nil))
  :group 'magnus)

(defcustom magnus-coord-context-max-tokens 200000
  "Maximum context window size in tokens.
Used to calculate context utilization percentage."
  :type 'integer
  :group 'magnus)

;;; Atomic file writes

(defun magnus-coord--write-file-atomic (file)
  "Write the current buffer to FILE atomically.
Writes to a temporary file in the same directory, then renames.
This prevents partial reads when agents write concurrently."
  (let ((tmp (make-temp-file
              (expand-file-name ".magnus-coord-tmp" (file-name-directory file)))))
    (write-region (point-min) (point-max) tmp nil 'quiet)
    (rename-file tmp file t)))

;;; Sending messages to agents

(defun magnus-coord-nudge-agent (instance message)
  "Nudge INSTANCE by sending MESSAGE to its vterm buffer."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (vterm-send-string message)
        (vterm-send-return)))))

;;; Periodic reminders

(defvar magnus-coord--reminder-timer nil
  "Timer for periodic coordination reminders.")

(defun magnus-coord-start-reminders ()
  "Start periodic coordination reminders and AFK detection."
  (magnus-coord-stop-reminders)
  (when magnus-coord-reminder-interval
    (setq magnus-coord--reminder-timer
          (run-with-timer magnus-coord-reminder-interval
                         magnus-coord-reminder-interval
                         #'magnus-coord--send-reminders)))
  (magnus-coord--start-idle-watch))

(defun magnus-coord-stop-reminders ()
  "Stop periodic coordination reminders and AFK detection."
  (when magnus-coord--reminder-timer
    (cancel-timer magnus-coord--reminder-timer)
    (setq magnus-coord--reminder-timer nil))
  (magnus-coord--stop-idle-watch))

(defvar magnus-coord--reminder-templates
  '("Hey %s — take a quick look at .magnus-coord.md. Any messages for you? Anything you've learned that other agents should know? Drop it in the Discoveries section."
    "Coordination check, %s. Read .magnus-coord.md — has anyone posted something useful in Discoveries? If you've figured out something non-obvious, share it there."
    "%s, heads up: peek at .magnus-coord.md. Other agents may have left insights in Discoveries that save you time. And if you've hit a gotcha or found a pattern, pay it forward."
    "Quick sync, %s. Open .magnus-coord.md — update your status, check for messages, and if you've learned anything surprising about this codebase, add it to Discoveries.")
  "Rotating reminder messages. %s is replaced with the agent name.")

(defvar magnus-coord--reminder-index 0
  "Index into the rotating reminder templates.")

(defvar magnus-coord--last-contact (make-hash-table :test 'equal)
  "Hash table: instance-id -> `float-time' of last user message.
Agents contacted recently are skipped by periodic nudges.")

(defun magnus-coord-record-contact (instance-id)
  "Record that INSTANCE-ID was just contacted by the user.
Suppresses periodic nudges for this agent until the next interval."
  (puthash instance-id (float-time) magnus-coord--last-contact))

(defun magnus-coord--recently-contacted-p (instance)
  "Return non-nil if INSTANCE was contacted within the reminder interval."
  (let* ((id (magnus-instance-id instance))
         (last (gethash id magnus-coord--last-contact))
         (interval (or magnus-coord-reminder-interval 600)))
    (and last (< (- (float-time) last) interval))))

(defvar magnus-coord--user-idle-p nil
  "Non-nil when the user has been detected as AFK.
Set by `magnus-coord--on-user-idle', cleared by `magnus-coord--on-user-return'.")

(defvar magnus-coord--user-active-until 0
  "Float-time until which the user is considered actively chatting.
Nudges are suppressed while `float-time' is less than this value.")

(defvar magnus-coord--do-not-disturb nil
  "Non-nil when do-not-disturb mode is active.
All periodic nudges are suppressed.  Toggle with `magnus-coord-toggle-dnd'.")

(defun magnus-coord-record-user-active ()
  "Record that the user is actively chatting.
Suppresses all nudges for `magnus-coord-active-cooldown' seconds."
  (setq magnus-coord--user-active-until
        (+ (float-time) magnus-coord-active-cooldown)))

(defun magnus-coord--user-active-p ()
  "Return non-nil if the user has been actively chatting recently."
  (> magnus-coord--user-active-until (float-time)))

(defun magnus-coord-toggle-dnd ()
  "Toggle do-not-disturb mode.
When active, all periodic coordination nudges are suppressed.
Agents keep working but are not poked."
  (interactive)
  (setq magnus-coord--do-not-disturb (not magnus-coord--do-not-disturb))
  (message "Magnus: Do Not Disturb %s"
           (if magnus-coord--do-not-disturb "ON" "OFF"))
  (run-hooks 'magnus-instances-changed-hook))

(defun magnus-coord-agent-busy-p (instance)
  "Return non-nil if INSTANCE has signaled it is busy.
Agents create a busy file to tell Magnus to stop nudging them."
  (file-exists-p (magnus-process--agent-busy-path instance)))

(declare-function magnus-chat-active-target-id "magnus-chat")

(defun magnus-coord--send-reminders ()
  "Send a coordination reminder to idle instances.
Skips agents that were recently contacted, are busy, or are the
current chat target while the user is typing.
Suppressed entirely when AFK, actively chatting, or DND is on."
  (unless (or magnus-coord--user-idle-p
              (magnus-coord--user-active-p)
              magnus-coord--do-not-disturb)
    (let ((template (nth (mod magnus-coord--reminder-index
                              (length magnus-coord--reminder-templates))
                         magnus-coord--reminder-templates))
          (chat-target (and (fboundp 'magnus-chat-active-target-id)
                            (magnus-chat-active-target-id))))
      (dolist (instance (magnus-instances-list))
        (when (and (eq (magnus-instance-status instance) 'running)
                   (not (magnus-coord--recently-contacted-p instance))
                   (not (magnus-coord-agent-busy-p instance))
                   (not (equal (magnus-instance-id instance) chat-target)))
          (magnus-coord-nudge-agent
           instance
           (format template (magnus-instance-name instance)))))
      (setq magnus-coord--reminder-index
            (1+ magnus-coord--reminder-index))))
  ;; Trim logs while we're at it (even when idle)
  (magnus-coord-trim-all)
  ;; Check context utilization (runs even when idle/active)
  (magnus-coord-check-context-all))

;;; Log trimming

(defun magnus-coord-trim-log (directory)
  "Trim the Log section in DIRECTORY's coordination file.
Keeps only the last `magnus-coord-log-max-entries' entries."
  (when magnus-coord-log-max-entries
    (let ((file (magnus-coord-file-path directory)))
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (when (re-search-forward "^## Log" nil t)
            (let* ((log-start (match-beginning 0))
                   (section-end (save-excursion
                                  (if (re-search-forward "^## " nil t)
                                      (match-beginning 0)
                                    (point-max))))
                   (entries nil))
              ;; Collect log entries (timestamp lines)
              (goto-char log-start)
              (while (re-search-forward
                      "^\\[\\([0-9:]+\\)\\] .+$" section-end t)
                (push (match-beginning 0) entries))
              (setq entries (nreverse entries))
              ;; If over limit, delete everything before the Nth-from-last
              (when (> (length entries) magnus-coord-log-max-entries)
                (let ((cut-point (nth (- (length entries)
                                         magnus-coord-log-max-entries)
                                      entries)))
                  ;; Delete from after the Log header to the cut point
                  (goto-char log-start)
                  (forward-line 1)
                  ;; Skip blank lines and comments after header
                  (while (and (< (point) cut-point)
                              (or (looking-at "^$")
                                  (looking-at "^<!--")))
                    (if (looking-at "^<!--")
                        (if (re-search-forward "-->" nil t)
                            (forward-line 1)
                          (goto-char cut-point))
                      (forward-line 1)))
                  (when (< (point) cut-point)
                    (delete-region (point) cut-point))
                  (magnus-coord--write-file-atomic file))))))))))

(defun magnus-coord-trim-all ()
  "Trim coordination file logs for all active project directories."
  (let ((dirs (delete-dups
               (mapcar #'magnus-instance-directory
                       (magnus-instances-list)))))
    (dolist (dir dirs)
      (magnus-coord-trim-log dir))))

;;; AFK detection

(defvar magnus-coord--idle-timer nil
  "Idle timer that fires after `magnus-coord-idle-threshold' seconds.")

(defun magnus-coord--start-idle-watch ()
  "Start watching for user idleness."
  (magnus-coord--stop-idle-watch)
  (when magnus-coord-idle-threshold
    (setq magnus-coord--idle-timer
          (run-with-idle-timer magnus-coord-idle-threshold nil
                              #'magnus-coord--on-user-idle))))

(defun magnus-coord--stop-idle-watch ()
  "Stop watching for user idleness."
  (when magnus-coord--idle-timer
    (cancel-timer magnus-coord--idle-timer)
    (setq magnus-coord--idle-timer nil))
  (remove-hook 'pre-command-hook #'magnus-coord--on-user-return)
  (setq magnus-coord--user-idle-p nil))

(declare-function magnus-process--agent-memory-path "magnus-process")
(declare-function magnus-process--agent-busy-path "magnus-process")
(declare-function magnus-process--ensure-agent-dir "magnus-process")
(declare-function magnus-process--session-jsonl-path "magnus-process")

(defun magnus-coord--on-user-idle ()
  "Called when the user has been idle for `magnus-coord-idle-threshold'.
Tells agents to consolidate their memory and go to sleep."
  (setq magnus-coord--user-idle-p t)
  (dolist (instance (magnus-instances-list))
    (when (eq (magnus-instance-status instance) 'running)
      (let* ((name (magnus-instance-name instance))
             (memory-rel (format ".claude/agents/%s/memory.md" name)))
        (magnus-process--ensure-agent-dir instance)
        (magnus-coord-nudge-agent
         instance
         (format "The user is away. Before you sleep, update your memory file at %s — write down what matters from this session: key decisions, things you learned, gotchas, unfinished work, your relationships with other agents. This file persists across restarts — it's how future-you remembers. Then go to sleep and wait quietly."
                 memory-rel)))))
  (add-hook 'pre-command-hook #'magnus-coord--on-user-return))

(defun magnus-coord--on-user-return ()
  "Called when the user presses a key after being idle.
Sends a wake-up message to running agents and re-arms the idle timer."
  (remove-hook 'pre-command-hook #'magnus-coord--on-user-return)
  (setq magnus-coord--user-idle-p nil)
  (dolist (instance (magnus-instances-list))
    (when (eq (magnus-instance-status instance) 'running)
      (magnus-coord-nudge-agent
       instance
       "The user is back! Resume normal operation — check .magnus-coord.md for any updates.")))
  ;; Re-arm the idle timer for the next AFK period
  (magnus-coord--start-idle-watch))

;;; Context window monitoring

(defvar magnus-coord--context-warned (make-hash-table :test 'equal)
  "Hash table of instance-id to t for agents already warned about context.
Cleared when the agent's session changes (restart or compaction).")

(defun magnus-coord-check-context-all ()
  "Check context utilization for all running agents.
Warns agents approaching the context window limit."
  (when magnus-coord-context-warn-threshold
    (dolist (instance (magnus-instances-list))
      (when (eq (magnus-instance-status instance) 'running)
        (condition-case nil
            (magnus-coord--check-context-one instance)
          (error nil))))))

(defun magnus-coord--check-context-one (instance)
  "Check context utilization for INSTANCE and warn if needed."
  (let ((id (magnus-instance-id instance)))
    (unless (gethash id magnus-coord--context-warned)
      (when-let ((usage (magnus-coord--read-context-usage instance)))
        (let ((ratio (/ (float usage) magnus-coord-context-max-tokens)))
          (when (>= ratio magnus-coord-context-warn-threshold)
            (puthash id t magnus-coord--context-warned)
            (let* ((name (magnus-instance-name instance))
                   (pct (round (* ratio 100)))
                   (memory-rel (format ".claude/agents/%s/memory.md" name)))
              (magnus-process--ensure-agent-dir instance)
              (magnus-coord-nudge-agent
               instance
               (format "Heads up: you're at %d%% context. Compaction is coming — write everything important to your memory file at %s NOW, while you still have full context. Key decisions, unfinished work, what you've learned, relationships with other agents. After compaction you'll lose detail."
                       pct memory-rel)))))))))

(defun magnus-coord--read-context-usage (instance)
  "Read the latest context token count from INSTANCE's session trace.
Returns total input tokens or nil if unavailable.  Only reads the
last 4KB of the file for efficiency."
  (when-let ((session-id (magnus-instance-session-id instance))
             (jsonl (magnus-process--session-jsonl-path
                     (magnus-instance-directory instance) session-id)))
    (let* ((attrs (file-attributes jsonl))
           (size (file-attribute-size attrs))
           (start (max 0 (- size 4096))))
      (with-temp-buffer
        (insert-file-contents jsonl nil start size)
        (magnus-coord--parse-last-usage)))))

(defun magnus-coord--parse-last-usage ()
  "Parse the last usage entry from the current buffer.
Looks for cache_read_input_tokens in the last complete JSON lines
and sums input_tokens + cache_creation_input_tokens +
cache_read_input_tokens."
  (goto-char (point-max))
  (let ((found nil))
    (while (and (not found)
                (re-search-backward "cache_read_input_tokens" nil t))
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        ;; Extract the three token counts via regex (avoid full JSON parse)
        (let ((input (magnus-coord--extract-number
                      "\"input_tokens\":\\s*\\([0-9]+\\)" line-start line-end))
              (cache-create (magnus-coord--extract-number
                             "\"cache_creation_input_tokens\":\\s*\\([0-9]+\\)" line-start line-end))
              (cache-read (magnus-coord--extract-number
                           "\"cache_read_input_tokens\":\\s*\\([0-9]+\\)" line-start line-end)))
          (when (and input cache-read)
            (setq found (+ input (or cache-create 0) cache-read))))))
    found))

(defun magnus-coord--extract-number (pattern start end)
  "Extract a number matching PATTERN between START and END."
  (save-excursion
    (goto-char start)
    (when (re-search-forward pattern end t)
      (string-to-number (match-string 1)))))

;;; @mention watching

(defvar magnus-coord--watchers nil
  "Alist of (directory . watch-descriptor) for coordination file watchers.")

(defun magnus-coord-ensure-watchers ()
  "Start file watchers for all directories with active instances.
Call this on startup to ensure @mention detection works for
instances restored from persistence."
  (let ((dirs (delete-dups
               (mapcar #'magnus-instance-directory (magnus-instances-list)))))
    (dolist (dir dirs)
      (when (file-exists-p (magnus-coord-file-path dir))
        (unless (assoc dir magnus-coord--watchers)
          (magnus-coord-start-watching dir))))))

(defvar magnus-coord--processed-mentions nil
  "Alist of (directory . list-of-processed-mention-hashes) to avoid duplicates.")

(defun magnus-coord-start-watching (directory)
  "Start watching the coordination file in DIRECTORY for @mentions."
  (let ((file (magnus-coord-file-path directory)))
    ;; Remove existing watcher if any
    (magnus-coord-stop-watching directory)
    ;; Only watch if file exists
    (when (file-exists-p file)
      (let ((descriptor (file-notify-add-watch
                         file
                         '(change)
                         (lambda (event)
                           (magnus-coord--handle-file-change directory event)))))
        (push (cons directory descriptor) magnus-coord--watchers)
        ;; Initialize processed mentions from current content
        (magnus-coord--init-processed-mentions directory)))))

(defun magnus-coord-stop-watching (directory)
  "Stop watching the coordination file in DIRECTORY."
  (when-let ((entry (assoc directory magnus-coord--watchers)))
    (file-notify-rm-watch (cdr entry))
    (setq magnus-coord--watchers (assoc-delete-all directory magnus-coord--watchers))
    (setq magnus-coord--processed-mentions
          (assoc-delete-all directory magnus-coord--processed-mentions))))

(defun magnus-coord-stop-all-watchers ()
  "Stop all coordination file watchers."
  (dolist (entry magnus-coord--watchers)
    (ignore-errors (file-notify-rm-watch (cdr entry))))
  (setq magnus-coord--watchers nil)
  (setq magnus-coord--processed-mentions nil))

(defun magnus-coord--init-processed-mentions (directory)
  "Initialize processed mentions for DIRECTORY from current file content."
  (let ((file (magnus-coord-file-path directory)))
    (when (file-exists-p file)
      (let ((mentions (magnus-coord--extract-mentions
                       (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string)))))
        (setf (alist-get directory magnus-coord--processed-mentions nil nil #'equal)
              (mapcar #'magnus-coord--mention-hash mentions))))))

(defun magnus-coord--handle-file-change (directory event)
  "Handle a file change EVENT for DIRECTORY's coordination file."
  (when (and magnus-coord-mention-notify
             (eq (nth 1 event) 'changed))
    (condition-case err
        (magnus-coord--check-new-mentions directory)
      (error
       (message "Magnus: coord file change handler error: %s"
                (error-message-string err))))))

(defun magnus-coord--check-new-mentions (directory)
  "Check for new @mentions in DIRECTORY's coordination file."
  (let* ((file (magnus-coord-file-path directory))
         (content (when (file-exists-p file)
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))))
         (mentions (when content (magnus-coord--extract-mentions content)))
         (processed (alist-get directory magnus-coord--processed-mentions nil nil #'equal)))
    (dolist (mention mentions)
      (let ((hash (magnus-coord--mention-hash mention)))
        (unless (member hash processed)
          ;; New mention - notify the agent
          (magnus-coord--notify-mention directory mention)
          (push hash processed))))
    (setf (alist-get directory magnus-coord--processed-mentions nil nil #'equal)
          processed)))

(defun magnus-coord--extract-mentions (content)
  "Extract all @mentions from CONTENT.
Returns list of (agent-name . context-line) pairs."
  (let (mentions)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "@\\([a-zA-Z][-a-zA-Z0-9_]*\\)" nil t)
        (let ((agent (match-string 1))
              (line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (push (cons agent line) mentions))))
    (nreverse mentions)))

(defun magnus-coord--mention-hash (mention)
  "Create a hash for MENTION to track duplicates."
  (secure-hash 'md5 (format "%s:%s" (car mention) (cdr mention))))

(defun magnus-coord--notify-mention (directory mention)
  "Notify the agent named in MENTION within DIRECTORY."
  (let* ((agent-name (car mention))
         (context-line (cdr mention))
         (instance (magnus-coord--find-instance-by-name agent-name directory)))
    (when instance
      (magnus-coord--send-mention-notification instance context-line))))

(defun magnus-coord--find-instance-by-name (name directory)
  "Find an instance with NAME working in DIRECTORY."
  (cl-find-if (lambda (inst)
                (and (string= (magnus-instance-name inst) name)
                     (string= (magnus-instance-directory inst) directory)))
              (magnus-instances-list)))

(defun magnus-coord--extract-sender-and-message (context-line _target-name)
  "Extract sender name and message from CONTEXT-LINE.
Returns (sender . message) or nil."
  (when (string-match
         "\\[.*?\\] \\([^:]+\\): .*?@[^ ]+ \\(.*\\)"
         context-line)
    (let ((sender (match-string 1 context-line))
          (message (match-string 2 context-line)))
      (cons sender (string-trim message)))))

(defun magnus-coord--send-mention-notification (instance context-line)
  "Send a mention notification to INSTANCE with CONTEXT-LINE.
Formats the message as a direct user instruction so Claude acts on it."
  (let* ((name (magnus-instance-name instance))
         (parsed (magnus-coord--extract-sender-and-message context-line name))
         (msg (if parsed
                  (format "%s (another agent) says: %s — Do this now. Log your acknowledgment and progress in .magnus-coord.md"
                          (car parsed) (cdr parsed))
                (format "Another agent mentioned you in .magnus-coord.md: \"%s\" — Read the file, do what's asked, and log your progress there."
                        context-line))))
    (magnus-coord-nudge-agent instance msg)))

;;; Coordination file management

(defun magnus-coord-file-path (directory)
  "Get the coordination file path for DIRECTORY."
  (expand-file-name magnus-coord-file directory))

(defun magnus-coord-instructions-path (directory)
  "Get the instructions file path for DIRECTORY."
  (expand-file-name magnus-coord-instructions-file directory))

(defun magnus-coord-ensure-file (directory)
  "Ensure coordination file exists in DIRECTORY."
  (let ((file (magnus-coord-file-path directory)))
    (unless (file-exists-p file)
      (magnus-coord--create-file file))
    file))

(defun magnus-coord--create-file (file)
  "Create a new coordination FILE with initial template."
  (with-temp-file file
    (insert "# Agent Coordination\n\n")
    (insert "This file is used by Claude Code agents to coordinate their work.\n")
    (insert "Agents should check this file before starting and announce their plans.\n\n")
    (insert "## Active Work\n\n")
    (insert "<!-- Agents: Update this section when you start/finish work -->\n\n")
    (insert "| Agent | Area | Status | Files |\n")
    (insert "|-------|------|--------|-------|\n")
    (insert "\n## Discoveries\n\n")
    (insert "<!-- Share things you learned that other agents should know -->\n\n")
    (insert "## Log\n\n")
    (insert "<!-- Agents: Append messages here to communicate -->\n\n")
    (insert "## Decisions\n\n")
    (insert "<!-- Record agreed-upon decisions here -->\n\n")))

(defun magnus-coord-ensure-instructions (directory)
  "Ensure agent instructions file exists in DIRECTORY."
  (let ((file (magnus-coord-instructions-path directory)))
    (unless (file-exists-p file)
      (magnus-coord--create-instructions file directory))
    file))

(defun magnus-coord--create-instructions (file directory)
  "Create instructions FILE for agents in DIRECTORY."
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (with-temp-file file
    (insert (magnus-coord--instructions-content directory))))

(defun magnus-coord--instructions-content (_directory)
  "Generate instructions content."
  (format "# Magnus Coordination Protocol

You are one of multiple Claude Code agents working on this project.
To coordinate with other agents, follow this protocol:

## Before Starting Work

1. Read `%s` to see what other agents are working on
2. Check the Active Work table for potential conflicts
3. If your work might conflict, discuss in the Log section first

## Announcing Your Work

When you start a task, update the Active Work table:

```markdown
| your-name | area you're working on | in-progress | file1.ts, file2.ts |
```

## Communicating

To talk to other agents, append to the Log section:

```markdown
[HH:MM] your-name: Your message here. @other-agent if you need their attention.
```

**Note:** When you @mention another agent, they will automatically receive a
notification with your message. You don't need to wait for them to check the file.

## Sharing What You Learn

As you work, you will discover things — API quirks, gotchas, patterns, or
non-obvious behavior in the codebase. Add these to the **Discoveries** section
of the coordination file. Other agents read this, and your insight might save
them hours. Think of it as leaving breadcrumbs for your teammates.

## When You Finish

1. Update your row in Active Work (status: done, or remove it)
2. Log a message that you've finished
3. Add any non-obvious learnings to the Discoveries section
4. If you made decisions that affect others, add to the Decisions section
5. When you commit, include context in your commit message — what you learned,
   why you made the choices you did, any gotchas future developers should know.
   Commit messages are the permanent record; the coordination file is ephemeral.

## Conflict Resolution

If you need a file another agent is using:
1. Post in the Log asking if they can release it
2. Wait for their response before proceeding
3. If urgent, explain why in your message

## Requesting User Attention

When you need user input (permissions, confirmations, etc.):
1. BEFORE asking, announce in the Log: `[HH:MM] your-name: [ATTENTION] Need user input for <reason>`
2. Wait briefly for other agents to finish their current attention requests
3. Other agents should pause requests when they see another agent waiting
4. After receiving input, log: `[HH:MM] your-name: [ATTENTION] Done, input received`

This prevents multiple agents from asking for input simultaneously.

## Important Files

- Coordination: `%s`
- Shared context: `.magnus-context.md` (if exists)

Remember: Check the coordination file periodically, especially before major changes.
" magnus-coord-file magnus-coord-file))

;;; Coordination skill

(defun magnus-coord-skill-path (directory)
  "Get the coordination skill file path for DIRECTORY."
  (expand-file-name magnus-coord-skill-file directory))

(defun magnus-coord-ensure-skill (directory)
  "Ensure the coordinate skill file exists in DIRECTORY."
  (let ((file (magnus-coord-skill-path directory)))
    (unless (file-exists-p file)
      (magnus-coord--create-skill file))
    file))

(defun magnus-coord--create-skill (file)
  "Create the coordination skill FILE."
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (with-temp-file file
    (insert (magnus-coord--skill-content))))

(defun magnus-coord--skill-content ()
  "Generate the coordination skill content."
  (format "# Coordination Check-in

When you run /coordinate, perform these steps in order:

## Steps

1. **Read the coordination file**: Open and read `%s` completely.
2. **Review active work**: Check the Active Work table. Note which agents are working on what files.
3. **Identify conflicts**: Compare your planned work against the table. Flag any file overlaps.
4. **Announce your claims**: Update the Active Work table with your row:
   - Your agent name
   - The area you are working on
   - Status: `in-progress`
   - Files you will touch (comma-separated)
5. **Log your check-in**: Append a message to the Log section:
   ```
   [HH:MM] your-name: Checked in. Working on <area>. Files: <list>.
   ```
6. **Resolve conflicts**: If you found conflicts in step 3, @mention the conflicting agent in the Log section and wait for acknowledgment before proceeding.

## After Completing a Task

1. Update your Active Work row: change status to `done` or remove it.
2. Log completion: `[HH:MM] your-name: Completed <task>. Files released: <list>.`
3. **Debrief**: Add anything you learned to the Discoveries section — gotchas, patterns, API quirks, things that surprised you. Your teammates will thank you.
4. If you made architectural decisions, add them to the Decisions section.
5. **Commit with context**: When committing, write a message that captures the *why* — what you learned, trade-offs you considered, gotchas you hit. This is the permanent record of your work.

## Important

- Always use the current time (HH:MM format) in log entries.
- Never modify another agent's Active Work row.
- If you are blocked by another agent, @mention them — they will be notified automatically.
- Read the Discoveries section when you check in — other agents may have learned something that helps you.
" magnus-coord-file))

;;; Parsing coordination file

(defun magnus-coord-parse (directory)
  "Parse the coordination file in DIRECTORY.
Returns a plist with :active, :log, and :decisions."
  (let ((file (magnus-coord-file-path directory)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (magnus-coord--parse-buffer))
      (list :active nil :log nil :decisions nil))))

(defun magnus-coord--parse-buffer ()
  "Parse the current buffer as a coordination file."
  (let ((active (magnus-coord--parse-active-table))
        (log (magnus-coord--parse-log))
        (decisions (magnus-coord--parse-decisions)))
    (list :active active :log log :decisions decisions)))

(defun magnus-coord--parse-active-table ()
  "Parse the Active Work table from current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (entries)
      (when (re-search-forward "^## Active Work" nil t)
        ;; Skip to table content (past header rows)
        (when (re-search-forward "^|[-|]+" nil t)
          (forward-line 1)
          (while (looking-at "^| *\\([^|]+\\) *| *\\([^|]+\\) *| *\\([^|]+\\) *| *\\([^|]*\\) *|")
            (let ((agent (string-trim (match-string 1)))
                  (area (string-trim (match-string 2)))
                  (status (string-trim (match-string 3)))
                  (files (string-trim (match-string 4))))
              (unless (string-empty-p agent)
                (push (list :agent agent :area area :status status :files files)
                      entries)))
            (forward-line 1))))
      (nreverse entries))))

(defun magnus-coord--parse-log ()
  "Parse the Log section from current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (entries)
      (when (re-search-forward "^## Log" nil t)
        (let ((section-end (save-excursion
                            (if (re-search-forward "^## " nil t)
                                (match-beginning 0)
                              (point-max)))))
          (while (re-search-forward "^\\[\\([0-9:]+\\)\\] \\([^:]+\\): \\(.+\\)$" section-end t)
            (push (list :time (match-string 1)
                       :agent (match-string 2)
                       :message (match-string 3))
                  entries))))
      (nreverse entries))))

(defun magnus-coord--parse-decisions ()
  "Parse the Decisions section from current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (entries)
      (when (re-search-forward "^## Decisions" nil t)
        (while (re-search-forward "^- \\(.+\\)$" nil t)
          (push (match-string 1) entries)))
      (nreverse entries))))

;;; Writing to coordination file

(defun magnus-coord-add-log (directory agent message)
  "Add a log MESSAGE from AGENT to coordination file in DIRECTORY."
  (let ((file (magnus-coord-ensure-file directory))
        (time (format-time-string "%H:%M")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (re-search-forward "^## Log\n+" nil t)
          (progn
            ;; Skip HTML comments (single or multi-line) and blank lines
            (while (and (not (eobp))
                        (or (looking-at "^$")
                            (looking-at "^<!--")))
              (if (looking-at "^<!--")
                  ;; Jump past closing -->, whether same line or later
                  (if (re-search-forward "-->" nil t)
                      (forward-line 1)
                    (goto-char (point-max)))
                (forward-line 1)))
            (insert (format "[%s] %s: %s\n\n" time agent message)))
        ;; No Log section, append at end
        (goto-char (point-max))
        (insert (format "\n[%s] %s: %s\n" time agent message)))
      (magnus-coord--write-file-atomic file))))

(defun magnus-coord-update-active (directory agent area status files)
  "Update AGENT's entry in the Active Work table in DIRECTORY.
AREA is what they're working on, STATUS is their status,
FILES is a list of files they're touching."
  (let ((file (magnus-coord-ensure-file directory))
        (files-str (if (listp files) (string-join files ", ") files)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (re-search-forward "^## Active Work" nil t)
          (let ((table-start (save-excursion
                              (re-search-forward "^|[-|]+" nil t)
                              (forward-line 1)
                              (point)))
                (table-end (save-excursion
                            (if (re-search-forward "^## " nil t)
                                (match-beginning 0)
                              (point-max))))
                (found nil))
            ;; Look for existing entry
            (goto-char table-start)
            (while (and (< (point) table-end)
                        (not found)
                        (looking-at "^| *\\([^|]+\\) *|"))
              (if (string= (string-trim (match-string 1)) agent)
                  (progn
                    (setq found t)
                    (delete-region (line-beginning-position)
                                  (1+ (line-end-position)))
                    (unless (string= status "done")
                      (insert (format "| %s | %s | %s | %s |\n"
                                     agent area status files-str))))
                (forward-line 1)))
            ;; Add new entry if not found
            (unless (or found (string= status "done"))
              (goto-char table-start)
              (insert (format "| %s | %s | %s | %s |\n"
                             agent area status files-str))))
        ;; No Active Work section, create one
        (goto-char (point-min))
        (when (re-search-forward "^# " nil t)
          (end-of-line)
          (insert "\n\n## Active Work\n\n")
          (insert "| Agent | Area | Status | Files |\n")
          (insert "|-------|------|--------|-------|\n")
          (insert (format "| %s | %s | %s | %s |\n"
                         agent area status files-str))))
      (magnus-coord--write-file-atomic file))))

(defun magnus-coord-clear-agent (directory agent)
  "Remove AGENT from the Active Work table in DIRECTORY."
  (magnus-coord-update-active directory agent "" "done" ""))

(defun magnus-coord-mark-session-end (directory)
  "Mark the end of a session in DIRECTORY's coordination file.
Appends a log entry instead of clearing sections, preserving
Discoveries and Decisions for future agents."
  (magnus-coord-add-log directory "Magnus" "Session ended"))

(defun magnus-coord-reconcile (directory)
  "Reconcile the Active Work table in DIRECTORY with live instances.
Removes entries for agents not in the Magnus registry, and entries
with stale statuses like done, died, finished, completed, stopped."
  (let* ((file (magnus-coord-file-path directory))
         (live-names (mapcar #'magnus-instance-name
                             (cl-remove-if-not
                              (lambda (inst)
                                (string= (magnus-instance-directory inst) directory))
                              (magnus-instances-list)))))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "^## Active Work" nil t)
          (when (re-search-forward "^|[-|]+" nil t)
            (forward-line 1)
            (while (looking-at "^| *\\([^|]+\\) *| *\\([^|]+\\) *| *\\([^|]+\\) *|")
              (let ((agent (string-trim (match-string 1)))
                    (status (string-trim (match-string 3))))
                (if (or (not (member agent live-names))
                        (string-match-p "done\\|died\\|finished\\|completed\\|stopped"
                                        status))
                    (delete-region (line-beginning-position)
                                  (min (1+ (line-end-position)) (point-max)))
                  (forward-line 1))))))
        (magnus-coord--write-file-atomic file)))))

(defun magnus-coord-reconcile-all ()
  "Reconcile coordination files for all project directories."
  (let ((dirs (delete-dups
               (mapcar #'magnus-instance-directory (magnus-instances-list)))))
    (dolist (dir dirs)
      (magnus-coord-reconcile dir))))

;;; Agent registration

(defun magnus-coord-register-agent (directory instance)
  "Register INSTANCE as active in DIRECTORY's coordination file."
  (let ((name (magnus-instance-name instance)))
    (magnus-coord-ensure-file directory)
    (magnus-coord-ensure-instructions directory)
    (magnus-coord-ensure-skill directory)
    (magnus-coord-add-log directory name "Joined the session")
    ;; Start watching for @mentions if not already
    (unless (assoc directory magnus-coord--watchers)
      (magnus-coord-start-watching directory))))

(defun magnus-coord-unregister-agent (directory instance)
  "Unregister INSTANCE from DIRECTORY's coordination file."
  (let ((name (magnus-instance-name instance)))
    (magnus-coord-clear-agent directory name)
    (magnus-coord-add-log directory name "Left the session")
    ;; If no agents remain, clean up for next session
    (let ((remaining (cl-count-if
                      (lambda (inst)
                        (and (not (eq inst instance))
                             (string= (magnus-instance-directory inst) directory)))
                      (magnus-instances-list))))
      (when (zerop remaining)
        (magnus-coord-stop-watching directory)
        (magnus-coord-mark-session-end directory)))))

;;; Display

(defun magnus-coord-format-active (parsed)
  "Format the :active entries from PARSED for display."
  (let ((active (plist-get parsed :active)))
    (if active
        (mapconcat
         (lambda (entry)
           (format "  %s: %s [%s]"
                   (propertize (plist-get entry :agent) 'face 'magnus-status-instance-name)
                   (plist-get entry :area)
                   (propertize (plist-get entry :status)
                              'face (if (string= (plist-get entry :status) "in-progress")
                                       'magnus-status-running
                                     'magnus-status-instance-dir))))
         active
         "\n")
      (propertize "  No active work" 'face 'magnus-status-empty-hint))))

(defun magnus-coord-format-log (parsed &optional limit)
  "Format the :log entries from PARSED for display.
Show at most LIMIT entries (default 5)."
  (let* ((log (plist-get parsed :log))
         (entries (if limit (seq-take log limit) log)))
    (if entries
        (mapconcat
         (lambda (entry)
           (format "  [%s] %s: %s"
                   (propertize (plist-get entry :time) 'face 'magnus-status-instance-dir)
                   (propertize (plist-get entry :agent) 'face 'magnus-status-instance-name)
                   (plist-get entry :message)))
         entries
         "\n")
      (propertize "  No messages yet" 'face 'magnus-status-empty-hint))))

;;; Interactive commands

(defun magnus-coord-open (directory)
  "Open the coordination file for DIRECTORY."
  (interactive (list (magnus-coord--get-directory)))
  (find-file (magnus-coord-ensure-file directory)))

(defun magnus-coord-open-instructions (directory)
  "Open the instructions file for DIRECTORY."
  (interactive (list (magnus-coord--get-directory)))
  (find-file (magnus-coord-ensure-instructions directory)))

(defun magnus-coord--get-directory ()
  "Get directory for coordination, prompting if needed."
  (or (when (bound-and-true-p magnus-context--directory)
        magnus-context--directory)
      (when (fboundp 'project-current)
        (when-let ((project (project-current 'maybe)))
          (project-root project)))
      (read-directory-name "Project directory: ")))

(provide 'magnus-coord)
;;; magnus-coord.el ends here
