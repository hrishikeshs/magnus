;;; magnus-coord.el --- Agent coordination for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module connects Magnus's shared `.magnus-coord.md' file to agent
;; delivery and the status UI.  The file is both the human-readable state and
;; the communication channel for a project.
;; Agents use coordination to:
;; - Announce what they're working on
;; - Communicate with other agents
;; - Record decisions and agreements
;; - Avoid stepping on each other's work

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magnus-instances)
(require 'magnus-provider)
(require 'magnus-background)
(require 'magnus-terminal)

(declare-function project-root "project")
(defvar magnus-claude-executable)
(defvar magnus-headless-model)
(declare-function magnus--strip-thinking-markers "magnus")

;;; Vterm buffer activity tracking

(defvar magnus-coord--buffer-ticks (make-hash-table :test 'equal)
  "Last observed terminal activity keyed by instance ID.
Each value records the exact buffer, process, modified tick, and change time.
Runtime identity prevents a replacement TUI from inheriting an old terminal's
quiescent timestamp merely because both fresh buffers have the same tick.")

(defcustom magnus-coord-quiescence-threshold 30
  "Seconds of vterm buffer inactivity before an agent is considered idle.
Only idle agents receive periodic nudges and presence changes."
  :type 'integer
  :group 'magnus)

(defun magnus-coord--update-buffer-ticks ()
  "Update buffer activity timestamps for all running instances."
  (dolist (instance (magnus-instances-list))
    (when (eq (magnus-instance-status instance) 'running)
      (let* ((id (magnus-instance-id instance))
             (buffer (magnus-instance-buffer instance))
             (previous (gethash id magnus-coord--buffer-ticks))
             (valid-previous (eq (car-safe previous) :buffer)))
        (when (and buffer (buffer-live-p buffer))
          (let ((process (get-buffer-process buffer))
                (tick (buffer-modified-tick buffer)))
            (if (and valid-previous
                     (eq buffer (plist-get previous :buffer))
                     (eq process (plist-get previous :process))
                     (= tick (plist-get previous :tick)))
                ;; Tick unchanged — keep existing timestamp
                nil
              ;; Activity or runtime changed — begin a fresh quiet interval.
              (puthash id (list :buffer buffer :process process
                                :tick tick :changed-at (float-time))
                       magnus-coord--buffer-ticks))))))))

(defun magnus-coord-agent-quiescent-p (instance)
  "Return non-nil if INSTANCE's vterm buffer has been quiet.
Quiet means no output for `magnus-coord-quiescence-threshold' seconds."
  (let* ((id (magnus-instance-id instance))
         (entry (gethash id magnus-coord--buffer-ticks))
         (last-change (and (eq (car-safe entry) :buffer)
                           (plist-get entry :changed-at))))
    (and (numberp last-change)
         (> (- (float-time) last-change)
            magnus-coord-quiescence-threshold))))

;;; Customization

(defcustom magnus-coord-file ".magnus-coord.md"
  "Name of the coordination file in project directories."
  :type 'string
  :group 'magnus)

(defcustom magnus-coord-instructions-file ".claude/magnus-instructions.md"
  "Path to the instructions file for agents (relative to project)."
  :type 'string
  :group 'magnus)

(defconst magnus-coord--instructions-version 10
  "Version of the instructions template.
Bump this when the template content changes.  Files with an older
version marker are automatically regenerated.")

(defcustom magnus-coord-mention-notify t
  "If non-nil, automatically notify agents when they are @mentioned."
  :type 'boolean
  :group 'magnus)

(defcustom magnus-coord-skill-file ".claude/skills/coordinate/SKILL.md"
  "Legacy coordination skill path, relative to the project.
Magnus no longer creates or uses this file during normal coordination setup.
The option remains for callers of `magnus-coord-ensure-skill'."
  :type 'string
  :group 'magnus)

(defcustom magnus-coord-reminder-interval 600
  "Seconds between coordination file reminders to agents.
Set to nil to disable.  Default is 600 (10 minutes)."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'magnus)

(defcustom magnus-coord-nudge-debounce 300
  "Minimum seconds between system nudges to the same agent.
Prevents rapid-fire reminders when multiple nudge paths fire
close together.  Only applies to Magnus system messages, not
agent-to-agent DMs or @mentions."
  :type 'integer
  :group 'magnus)

(defcustom magnus-coord-log-max-entries 25
  "Maximum number of log entries to keep in the coordination file.
Older entries are trimmed automatically.  Set to nil to disable."
  :type '(choice (integer :tag "Entries")
                 (const :tag "Unlimited" nil))
  :group 'magnus)

(defcustom magnus-coord-tidy-size-threshold 20480
  "File size in bytes above which an agent is asked to tidy the coord file.
When the coordination file exceeds this size, a random quiescent agent
is asked to consolidate the Discoveries and Decisions sections.
Set to nil to disable."
  :type '(choice (integer :tag "Bytes")
                 (const :tag "Disabled" nil))
  :group 'magnus)

(put 'magnus-coord-log-max-entries 'safe-local-variable
     (lambda (v) (or (null v) (integerp v))))
(put 'magnus-coord-tidy-size-threshold 'safe-local-variable
     (lambda (v) (or (null v) (integerp v))))

(defun magnus-coord--dir-local-value (var dir)
  "Return VAR's dir-local value in DIR, falling back to its global value.
Timers run outside any buffer visiting DIR, so dir-local settings in
DIR's .dir-locals.el are invisible to them unless resolved explicitly."
  (with-temp-buffer
    (setq default-directory (file-name-as-directory (expand-file-name dir)))
    (let ((enable-local-variables :safe))
      (hack-dir-local-variables-non-file-buffer))
    (symbol-value var)))

(defcustom magnus-coord-idle-threshold 300
  "Seconds of inactivity before telling agents to sleep.
When the user is idle for this long, Magnus asks each quiescent interactive
agent to sleep and suppresses periodic nudges.  Contradictory away/back state
is coalesced until its terminal can accept it.  Set to nil to disable."
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

(defvar magnus-coord--last-nudge (make-hash-table :test 'equal)
  "Hash of instance-id → timestamp of last Magnus system nudge.")

(defun magnus-coord--log-undelivered-nudge (instance text reason)
  "Record that TEXT could not reach INSTANCE because of REASON."
  (let* ((name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance))
         (safe-text (replace-regexp-in-string
                     "@" "(at) "
                     (replace-regexp-in-string "[\n\r]+" " " text)))
         (entry (format "Undelivered nudge to %s (%s): %s"
                        name reason safe-text)))
    (condition-case err
        (magnus-coord-add-log directory "Magnus" entry)
      (error
       (message "Magnus: could not log undelivered nudge for %s: %s"
                name (error-message-string err))))
    (message "Magnus: %s" entry)
    nil))

(defun magnus-coord--instance-running-p (instance)
  "Return non-nil when INSTANCE can currently receive a nudge.
This intentionally avoids requiring `magnus-process', which itself requires
this module and would create a load-order cycle for standalone callers."
  (and
   (magnus-instance-interactive-p instance)
   (if (magnus-provider-external-p instance)
       (magnus-provider-call instance 'running-p)
     (when-let ((buffer (magnus-instance-buffer instance)))
       (and (buffer-live-p buffer)
            (get-buffer-process buffer)
            (process-live-p (get-buffer-process buffer)))))))

(defun magnus-coord-nudge-agent (instance message &optional source)
  "Nudge INSTANCE by sending MESSAGE through its provider.
When SOURCE is non-nil, prepend \"[From SOURCE]:\" to distinguish
system messages from user-typed input.  System messages from Magnus
are debounced per `magnus-coord-nudge-debounce'."
  (catch 'magnus-debounced
    (let ((id (magnus-instance-id instance)))
      (when (and (string= source "Magnus")
                 magnus-coord-nudge-debounce)
        (let ((last (gethash id magnus-coord--last-nudge 0)))
          (when (< (- (float-time) last) magnus-coord-nudge-debounce)
            (throw 'magnus-debounced nil))))
      (let ((text (if source
                      (format "[From %s]: %s" source message)
                    message)))
        (condition-case err
            (if (not (magnus-instance-interactive-p instance))
                (magnus-coord--log-undelivered-nudge
                 instance text "headless task has no interactive terminal")
              (if (not (magnus-coord--instance-running-p instance))
                  (magnus-coord--log-undelivered-nudge
                   instance text "not running")
                (if (magnus-provider-external-p instance)
                    (magnus-provider-call instance 'send text)
                  ;; Coordination shares the terminal FIFO with reviews,
                  ;; onboarding, and external transports.  It must never split
                  ;; text and Return across independent timer callbacks.
                  (magnus-terminal-submit
                   instance text nil :settle-delay 0.1 :scope 'magnus-coord))
                (when (string= source "Magnus")
                  (puthash id (float-time) magnus-coord--last-nudge))))
          (error
           (magnus-coord--log-undelivered-nudge
            instance text (error-message-string err))))))))


;;; Periodic reminders

(defvar magnus-coord--reminder-timer nil
  "Timer for periodic coordination reminders.")

(defvar magnus-coord--presence-states (make-hash-table :test 'equal)
  "Latest-state delivery records keyed by interactive instance ID.
Each record separates desired presence from receipt-confirmed presence and an
optional exact in-flight attempt.  This contradiction/deduplication policy is
only for replaceable lifecycle state; peer messages remain append-only.")

(defvar magnus-coord--presence-retry-timer nil
  "One-shot timer for retrying replaceable presence delivery.")

(defvar magnus-coord--presence-attempt-sequence 0
  "Monotonic sequence used to fence presence delivery receipts.")

(defun magnus-coord-start-reminders ()
  "Start periodic coordination reminders and AFK detection."
  (magnus-coord-stop-reminders)
  (when magnus-coord-reminder-interval
    (setq magnus-coord--reminder-timer
          (run-with-timer magnus-coord-reminder-interval
                         magnus-coord-reminder-interval
                         #'magnus-coord--send-reminders)))
  (add-hook 'magnus-instances-changed-hook
            #'magnus-coord--on-presence-instances-changed)
  (magnus-coord--start-idle-watch))

(defun magnus-coord-stop-reminders ()
  "Stop periodic coordination reminders and AFK detection."
  (when magnus-coord--reminder-timer
    (cancel-timer magnus-coord--reminder-timer)
    (setq magnus-coord--reminder-timer nil))
  (magnus-coord--stop-idle-watch)
  (remove-hook 'magnus-instances-changed-hook
               #'magnus-coord--on-presence-instances-changed)
  (magnus-coord--stop-presence-retries)
  (clrhash magnus-coord--presence-states))

(defvar magnus-coord--reminder-templates
  '("Hey %s — take a quick look at %S. Share useful status or discoveries when needed."
    "Coordination check, %s. Review %S for messages, decisions, and overlapping work."
    "%s, heads up: revisit %S. A teammate may have left context that saves you time."
    "Quick sync, %s. Read %S, then update your status or non-obvious discoveries when needed.")
  "Rotating reminder messages.
The first format argument is the agent name and the second is its configured
coordination-file path.")

(defvar magnus-coord--reminder-index 0
  "Index into the rotating reminder templates.")

;;; Attention pattern learning

(defvar magnus-coord--attention-data (make-hash-table :test 'equal)
  "Hash: agent-name -> plist (:visits (FLOAT-TIME...) :messages INTEGER).
Visit timestamps are kept newest-first, max 50 entries.
Persists across sessions for adaptive nudge intervals.")

(defvar magnus-coord--attention-save-timer nil
  "Debounced timer for saving attention data.")

(defvar magnus-attention-data-file)

(defun magnus-coord--record-visit (agent-name)
  "Record a user visit to AGENT-NAME.
Debounced: only records if last visit was more than 30 seconds ago."
  (let* ((data (or (gethash agent-name magnus-coord--attention-data)
                   (list :visits nil :messages 0)))
         (visits (plist-get data :visits))
         (last-visit (car visits)))
    (when (or (null last-visit) (> (- (float-time) last-visit) 30))
      (puthash agent-name
               (plist-put data :visits
                          (seq-take (cons (float-time) visits) 50))
               magnus-coord--attention-data)
      (magnus-coord--schedule-attention-save))))

(defun magnus-coord--adaptive-interval (agent-name)
  "Return adaptive nudge interval for AGENT-NAME in seconds.
Matches the user's visiting rhythm.  Agents visited frequently
get shorter intervals (matching the pace).  Agents rarely visited
get longer intervals (up to the default)."
  (let* ((data (gethash agent-name magnus-coord--attention-data))
         (visits (plist-get data :visits))
         (default (or magnus-coord-reminder-interval 600)))
    (if (and visits (>= (length visits) 3))
        (let* ((recent (seq-take visits 10))
               (intervals (cl-mapcar (lambda (a b) (- a b))
                                     recent (cdr recent)))
               (avg (/ (apply #'+ intervals) (float (length intervals)))))
          (min default (max 300 (round (* avg 1.5)))))
      default)))

(defun magnus-coord--neglected-p (instance)
  "Return non-nil if INSTANCE is overdue for user attention.
Compares time since last visit against twice the adaptive interval."
  (when (eq (magnus-instance-status instance) 'running)
    (let* ((name (magnus-instance-name instance))
           (data (gethash name magnus-coord--attention-data))
           (visits (plist-get data :visits))
           (last-visit (car visits))
           (interval (magnus-coord--adaptive-interval name)))
      (and last-visit
           (> (- (float-time) last-visit) (* interval 2))))))

(defun magnus-coord--on-buffer-focus (_frame)
  "Record when the user switches to an agent's buffer."
  (let ((buf (window-buffer (selected-window))))
    (dolist (instance (magnus-instances-list))
      (when (eq buf (magnus-instance-buffer instance))
        (magnus-coord--record-visit (magnus-instance-name instance))))))

(defun magnus-coord-setup-attention-tracking ()
  "Start tracking user attention patterns."
  (add-hook 'window-buffer-change-functions
            #'magnus-coord--on-buffer-focus))

(defun magnus-coord-stop-attention-tracking ()
  "Stop tracking user attention patterns."
  (remove-hook 'window-buffer-change-functions
               #'magnus-coord--on-buffer-focus))

;;; Attention persistence

(defun magnus-coord--schedule-attention-save ()
  "Schedule a debounced save of attention data."
  (when magnus-coord--attention-save-timer
    (cancel-timer magnus-coord--attention-save-timer))
  (setq magnus-coord--attention-save-timer
        (run-with-idle-timer 5 nil #'magnus-coord-attention-save)))

(defun magnus-coord-attention-save ()
  "Save attention data to disk."
  (setq magnus-coord--attention-save-timer nil)
  (when (and (bound-and-true-p magnus-attention-data-file)
             (hash-table-count magnus-coord--attention-data))
    (let ((alist nil))
      (maphash (lambda (k v) (push (cons k v) alist))
               magnus-coord--attention-data)
      (with-temp-file magnus-attention-data-file
        (insert ";; Magnus attention data - do not edit manually\n")
        (pp alist (current-buffer))))))

(defun magnus-coord-attention-load ()
  "Load attention data from disk."
  (when (and (bound-and-true-p magnus-attention-data-file)
             (file-exists-p magnus-attention-data-file))
    (condition-case nil
        (let ((alist (with-temp-buffer
                       (insert-file-contents magnus-attention-data-file)
                       (goto-char (point-min))
                       (read (current-buffer)))))
          (clrhash magnus-coord--attention-data)
          (dolist (entry alist)
            (puthash (car entry) (cdr entry) magnus-coord--attention-data)))
      (error
       (message "Magnus: failed to load attention data")))))

(defvar magnus-coord--user-idle-p nil
  "Non-nil when the user has been detected as AFK.
Set by `magnus-coord--on-user-idle', cleared by `magnus-coord--on-user-return'.")

(defvar magnus-coord--do-not-disturb nil
  "Non-nil when do-not-disturb mode is active.
All periodic nudges are suppressed.  Toggle with `magnus-coord-toggle-dnd'.")

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

(defun magnus-coord--send-reminders ()
  "Send a coordination reminder to idle instances.
Skips agents that are busy or whose vterm buffer is still active.
Suppressed entirely when AFK or DND is on."
  (unless (or magnus-coord--user-idle-p
              magnus-coord--do-not-disturb)
    (let ((template (nth (mod magnus-coord--reminder-index
                              (length magnus-coord--reminder-templates))
                         magnus-coord--reminder-templates)))
      (dolist (instance (magnus-instances-list))
        (when (and (eq (magnus-instance-status instance) 'running)
                   (not (magnus-coord-agent-busy-p instance))
                   (magnus-coord-agent-quiescent-p instance))
          (magnus-coord-nudge-agent
           instance
           (format template
                   (magnus-instance-name instance)
                   (magnus-coord-display-file
                    (magnus-instance-directory instance)))
           "Magnus")))
      (setq magnus-coord--reminder-index
            (1+ magnus-coord--reminder-index))))
  ;; Housekeeping still runs while reminders are suppressed.
  (magnus-coord-trim-all)
  ;; Check context utilization (runs even when idle/active)
  (magnus-coord-check-context-all)
  (magnus-coord--maybe-tidy))

;;; Log trimming

(defun magnus-coord--trim-log-content (content max-entries)
  "Return (CHANGED . CONTENT) after bounding Markdown log CONTENT.
MAX-ENTRIES is the number of newest timestamped Log entries to retain.
Invalid limits and content without a Log section are returned unchanged."
  (if (not (and (stringp content)
                (integerp max-entries)
                (>= max-entries 0)))
      (cons nil content)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (if (not (re-search-forward "^## Log[[:space:]]*$" nil t))
          (cons nil content)
        (let ((section-end
               (save-excursion
                 (if (re-search-forward "^## " nil t)
                     (match-beginning 0)
                   (point-max))))
              entries)
          (while (re-search-forward "^\\[[^]\n]+\\] .+$" section-end t)
            (let ((start (line-beginning-position))
                  (end (save-excursion
                         (forward-line 1)
                         (when (looking-at "^[[:space:]]*$")
                           (forward-line 1))
                         (point))))
              (push (cons start end) entries)))
          ;; Log entries are newest-first.  Delete older entries bottom-up so
          ;; saved positions remain valid.
          (let ((stale (nthcdr max-entries (nreverse entries))))
            (if (null stale)
                (cons nil content)
              (dolist (range (reverse stale))
                (delete-region (car range) (cdr range)))
              (cons t (buffer-string)))))))))

(defun magnus-coord-trim-log (directory)
  "Keep the newest Log entries in DIRECTORY's coordination file.
Keep at most `magnus-coord-log-max-entries' timestamped entries."
  (let* ((max-entries (magnus-coord--dir-local-value
                       'magnus-coord-log-max-entries directory))
         (file (magnus-coord-file-path directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (pcase-let ((`(,changed . ,content)
                     (magnus-coord--trim-log-content
                      (buffer-string) max-entries)))
          (when changed
            (erase-buffer)
            (insert content)
            (magnus-coord--write-file-atomic file)))))))

(defun magnus-coord-trim-all ()
  "Trim coordination file logs for all active project directories."
  (let ((dirs (delete-dups
               (mapcar
                (lambda (instance)
                  (magnus-coord--normalized-directory
                   (magnus-instance-directory instance)))
                (magnus-instances-list)))))
    (dolist (dir dirs)
      (magnus-coord-trim-log dir))))

;;; Intelligent coord file tidying

(defvar magnus-coord--last-tidy-time nil
  "Alist of (directory . timestamp) for last tidy request, for debouncing.")

(defun magnus-coord--maybe-tidy ()
  "Ask a random idle agent to tidy the coord file if it has grown too large.
Checks each project directory's coordination file against
`magnus-coord-tidy-size-threshold' and, when exceeded, picks a
random quiescent agent to consolidate the Discoveries and Decisions
sections.  Debounced to at most once per hour per directory."
  (let ((dirs (delete-dups
               (mapcar
                (lambda (instance)
                  (magnus-coord--normalized-directory
                   (magnus-instance-directory instance)))
                (magnus-instances-list)))))
    (dolist (dir dirs)
      (let ((file (magnus-coord-file-path dir))
            (threshold (magnus-coord--dir-local-value
                        'magnus-coord-tidy-size-threshold dir)))
        (when (and threshold
                   (file-exists-p file)
                   (> (file-attribute-size (file-attributes file))
                      threshold)
                     (let ((last (alist-get dir magnus-coord--last-tidy-time
                                            nil nil #'equal)))
                       (or (null last)
                           (> (- (float-time) last) 3600))))
            (let ((candidates
                   (cl-remove-if-not
                    (lambda (inst)
                      (and (magnus-coord--same-directory-p
                            (magnus-instance-directory inst) dir)
                           (eq (magnus-instance-status inst) 'running)
                           (not (magnus-coord-agent-busy-p inst))
                           (magnus-coord-agent-quiescent-p inst)))
                    (magnus-instances-list))))
              (when candidates
                (let ((chosen (nth (random (length candidates)) candidates)))
                  (setf (alist-get dir magnus-coord--last-tidy-time
                                   nil nil #'equal)
                        (float-time))
                  (magnus-coord-nudge-agent
                   chosen
                   (format
                    (concat
                     "The coordination file is getting large. Please tidy it up: "
                     "read %S, then in the Discoveries section "
                     "remove entries that are outdated or already captured in "
                     "code and commits, merge related entries, and keep only "
                     "what is still useful for agents working on this project. "
                     "In the Decisions section remove decisions that have been "
                     "fully implemented or are no longer relevant and keep "
                     "active architectural decisions. Do not touch the Active "
                     "Work or Log sections. Be aggressive about trimming — a "
                     "shorter file is more useful than a comprehensive one.")
                    (magnus-coord-display-file dir))
                   "Magnus")
                  (magnus-coord-add-log
                   dir "Magnus"
                   (format "Asked %s to tidy the coordination file"
                           (magnus-instance-name chosen)))))))))))

;;; AFK detection

(defvar magnus-coord--idle-timer nil
  "Idle timer that fires after `magnus-coord-idle-threshold' seconds.")

(defconst magnus-coord--presence-retry-delay 3
  "Seconds between attempts to deliver a pending presence change.")

(defun magnus-coord--presence-instances ()
  "Return running interactive instances that can have presence state."
  (cl-remove-if-not
   (lambda (instance)
     (and (eq (magnus-instance-status instance) 'running)
          (magnus-instance-interactive-p instance)))
   (magnus-instances-list)))

(defun magnus-coord--prune-presence-states (instances)
  "Forget presence state not owned by one of INSTANCES."
  (let ((live-ids (make-hash-table :test 'equal))
        stale-ids)
    (dolist (instance instances)
      (puthash (magnus-instance-id instance) t live-ids))
    (maphash
     (lambda (id _state)
       (unless (gethash id live-ids)
         (push id stale-ids)))
     magnus-coord--presence-states)
    (dolist (id stale-ids)
      (remhash id magnus-coord--presence-states))))

(defun magnus-coord--new-presence-record ()
  "Return a presence record for an agent assumed to be awake."
  (list :desired 'back :delivered 'back
        :attempt nil :attempt-process nil :parked nil))

(defun magnus-coord--presence-converged-p (record)
  "Return non-nil when RECORD has no outstanding presence transition."
  (and (null (plist-get record :attempt))
       (eq (plist-get record :desired)
           (plist-get record :delivered))))

(defun magnus-coord--store-presence-record (id record)
  "Store RECORD for ID, removing a fully awake converged record."
  (if (and (magnus-coord--presence-converged-p record)
           (eq (plist-get record :delivered) 'back))
      (remhash id magnus-coord--presence-states)
    (puthash id record magnus-coord--presence-states)))

(defun magnus-coord--mark-user-away (instances)
  "Set desired presence to away for INSTANCES without queuing input."
  (dolist (instance instances)
    (let* ((id (magnus-instance-id instance))
           (record (or (gethash id magnus-coord--presence-states)
                       (magnus-coord--new-presence-record)))
           (previous (plist-get record :desired)))
      (setq record (plist-put record :desired 'away))
      (unless (eq previous 'away)
        (setq record (plist-put record :parked nil)))
      (magnus-coord--store-presence-record id record))))

(defun magnus-coord--mark-user-back (instances)
  "Set desired presence to back for INSTANCES without queuing input."
  (dolist (instance instances)
    (let* ((id (magnus-instance-id instance))
           (record (gethash id magnus-coord--presence-states)))
      (when record
        (unless (eq (plist-get record :desired) 'back)
          (setq record (plist-put record :parked nil)))
        (setq record (plist-put record :desired 'back))
        (magnus-coord--store-presence-record id record)))))

(defun magnus-coord--presence-message (instance state)
  "Return the lifecycle prompt for INSTANCE in presence STATE."
  (pcase state
    ('away
     (let* ((name (magnus-instance-name instance))
            (memory-rel (format ".claude/agents/%s/memory.md" name)))
       (magnus-process--ensure-agent-dir instance)
       (format
        (concat
         "The user is away. Before you sleep, update your memory file at %s "
         "— write down what matters from this session: key decisions, things "
         "you learned, gotchas, unfinished work, your relationships with other "
         "agents. This file persists across restarts — it's how future-you "
         "remembers. Then go to sleep and wait quietly.")
        memory-rel)))
    ('back
     (format
      "The user is back! Resume normal operation — check %S for updates."
      (magnus-coord-display-file (magnus-instance-directory instance))))))

(defun magnus-coord--try-presence-submit (instance text accepted)
  "Try to submit ephemeral TEXT to INSTANCE and call ACCEPTED on receipt.
Return `submitted', `queued', or nil.  Nil guarantees that the terminal owns
no part of TEXT, so Magnus may replace the presence transition safely."
  (let ((message (format "[From Magnus]: %s" text))
        (external (magnus-provider-external-p instance)))
    (if (and external
             (magnus-provider-operation-p instance 'try-send))
        (magnus-provider-call instance 'try-send message accepted)
      ;; Interactive adapters have always owned a vterm buffer.  Falling back
      ;; to the shared non-prequeueing substrate preserves lifecycle support
      ;; for third-party adapters written before `try-send' was available.
      (magnus-terminal-try-submit
       instance message :settle-delay 0.1 :accepted accepted))))

(defun magnus-coord--accept-presence (id token state)
  "Accept exact presence attempt TOKEN for ID as delivered STATE."
  (when-let ((record (gethash id magnus-coord--presence-states)))
    (when (eq token (plist-get record :attempt))
      (setq record (plist-put record :delivered state))
      (setq record (plist-put record :attempt nil))
      (setq record (plist-put record :attempt-process nil))
      (setq record (plist-put record :parked nil))
      (magnus-coord--store-presence-record id record)
      ;; Desired state may have changed while a pasted message waited for its
      ;; Return.  Keep one retry alive to deliver the compensating transition.
      (magnus-coord--maintain-presence-retry))))

(defun magnus-coord--clear-presence-attempt (id token)
  "Clear ID's presence attempt when it still belongs to TOKEN."
  (when-let ((record (gethash id magnus-coord--presence-states)))
    (when (eq token (plist-get record :attempt))
      (setq record (plist-put record :attempt nil))
      (setq record (plist-put record :attempt-process nil))
      (magnus-coord--store-presence-record id record))))

(defun magnus-coord--park-presence-attempt (id token state)
  "Park failed presence STATE for ID when its attempt still owns TOKEN."
  (when-let ((record (gethash id magnus-coord--presence-states)))
    (when (eq token (plist-get record :attempt))
      (setq record (plist-put record :attempt nil))
      (setq record (plist-put record :attempt-process nil))
      (setq record (plist-put record :parked state))
      (magnus-coord--store-presence-record id record))))

(defun magnus-coord--discard-replaced-runtime-attempt (instance record)
  "Clear RECORD's attempt if INSTANCE no longer owns its exact process."
  (let* ((buffer (magnus-instance-buffer instance))
         (current (and (buffer-live-p buffer) (get-buffer-process buffer)))
         (owner (plist-get record :attempt-process)))
    (if (and owner
             (or (not (process-live-p owner))
                 (not (eq owner current))))
        (progn
          (setq record (plist-put record :attempt nil))
          (setq record (plist-put record :attempt-process nil)))
      record)))

(defun magnus-coord--stop-presence-retries ()
  "Cancel the outstanding presence retry timer, if any."
  (when magnus-coord--presence-retry-timer
    (cancel-timer magnus-coord--presence-retry-timer)
    (setq magnus-coord--presence-retry-timer nil)))

(defun magnus-coord--presence-work-p ()
  "Return non-nil when a presence record needs delivery or reconciliation."
  (let (work)
    (maphash
     (lambda (_id record)
       (when (or (plist-get record :attempt)
                 (and (not (eq (plist-get record :desired)
                               (plist-get record :delivered)))
                      (not (eq (plist-get record :parked)
                               (plist-get record :desired)))))
         (setq work t)))
     magnus-coord--presence-states)
    work))

(defun magnus-coord--maintain-presence-retry ()
  "Keep exactly one retry timer while presence work remains."
  (if (not (magnus-coord--presence-work-p))
      (magnus-coord--stop-presence-retries)
    (unless magnus-coord--presence-retry-timer
      (setq magnus-coord--presence-retry-timer
            (run-with-timer magnus-coord--presence-retry-delay nil
                            #'magnus-coord--presence-retry)))))

(defun magnus-coord--presence-retry ()
  "Retry replaceable presence delivery after refreshing quiescence."
  (setq magnus-coord--presence-retry-timer nil)
  (magnus-coord--update-buffer-ticks)
  (magnus-coord--flush-presence))

(defun magnus-coord--flush-presence ()
  "Deliver coalesced presence changes to agents that are safely quiescent."
  (let ((instances (magnus-coord--presence-instances)))
    (magnus-coord--prune-presence-states instances)
    ;; An instance can be created after the idle timer fired.  Reconciliation
    ;; here gives it the current state without needing another lifecycle hook.
    (when magnus-coord--user-idle-p
      (magnus-coord--mark-user-away instances))
    (dolist (instance instances)
      (let* ((id (magnus-instance-id instance))
             (record (gethash id magnus-coord--presence-states)))
        (when record
          (setq record
                (magnus-coord--discard-replaced-runtime-attempt
                 instance record))
          (magnus-coord--store-presence-record id record))
        (when (and record
                   (null (plist-get record :attempt))
                   (not (eq (plist-get record :desired)
                            (plist-get record :delivered)))
                   (not (eq (plist-get record :parked)
                            (plist-get record :desired)))
                   (not (magnus-coord-agent-busy-p instance))
                   (magnus-coord-agent-quiescent-p instance)
                   (magnus-coord--instance-running-p instance))
          (let* ((state (plist-get record :desired))
                 (token (cons id (cl-incf magnus-coord--presence-attempt-sequence)))
                 (buffer (magnus-instance-buffer instance))
                 (process (and (buffer-live-p buffer)
                               (get-buffer-process buffer))))
            (setq record (plist-put record :attempt token))
            (setq record (plist-put record :attempt-process process))
            (puthash id record magnus-coord--presence-states)
            (condition-case err
                (unless
                    (memq
                     (magnus-coord--try-presence-submit
                      instance
                      (magnus-coord--presence-message instance state)
                      (lambda ()
                        (magnus-coord--accept-presence id token state)))
                     '(submitted queued))
                  ;; Nil is the crucial non-ownership receipt: no terminal
                  ;; queue contains this message, so the next user transition
                  ;; remains free to replace it.
                  (magnus-coord--clear-presence-attempt id token))
              (error
               ;; Persistent local failures (for example an unwritable memory
               ;; directory) must not emit an error every three seconds.  Park
               ;; this desired state until the next lifecycle transition.
               (magnus-coord--park-presence-attempt id token state)
               (message "Magnus: presence update failed for %s: %s"
                        (magnus-instance-name instance)
                        (error-message-string err))))))))
    (magnus-coord--maintain-presence-retry)))

(defun magnus-coord--on-presence-instances-changed ()
  "Reconcile presence ownership after the instance registry changes."
  (let ((instances (magnus-coord--presence-instances)))
    (magnus-coord--prune-presence-states instances)
    ;; A replacement runtime may have repaired a provider- or filesystem-level
    ;; failure, so give previously parked state one fresh attempt.
    (dolist (instance instances)
      (let* ((id (magnus-instance-id instance))
             (record (gethash id magnus-coord--presence-states)))
        (when (and record (plist-get record :parked))
          (setq record (plist-put record :parked nil))
          (magnus-coord--store-presence-record id record))))
    (when magnus-coord--user-idle-p
      (magnus-coord--update-buffer-ticks)
      (magnus-coord--mark-user-away instances)))
  (magnus-coord--maintain-presence-retry))

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
(declare-function magnus-process-create "magnus-process")

(defun magnus-coord--on-user-idle ()
  "Called when the user has been idle for `magnus-coord-idle-threshold'.
Queues a coalesced sleep transition for each running interactive agent."
  (setq magnus-coord--user-idle-p t)
  ;; Seed current ticks before checking quiescence.  Without this first
  ;; observation, an active terminal absent from the table looks idle.
  (magnus-coord--update-buffer-ticks)
  (let ((instances (magnus-coord--presence-instances)))
    (magnus-coord--prune-presence-states instances)
    (magnus-coord--mark-user-away instances))
  (magnus-coord--flush-presence)
  (add-hook 'pre-command-hook #'magnus-coord--on-user-return))

(defun magnus-coord--on-user-return ()
  "Called when the user presses a key after being idle.
Coalesces pending lifecycle state and re-arms the idle timer."
  (remove-hook 'pre-command-hook #'magnus-coord--on-user-return)
  (setq magnus-coord--user-idle-p nil)
  (let ((instances (magnus-coord--presence-instances)))
    (magnus-coord--prune-presence-states instances)
    (magnus-coord--mark-user-back instances))
  ;; Do not submit terminal input from `pre-command-hook'.  The presence retry
  ;; will deliver any required wake-up after the user's command and after the
  ;; agent's terminal becomes quiet.
  (magnus-coord--maintain-presence-retry)
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
        (condition-case err
            (magnus-coord--check-context-one instance)
          (error (message "Magnus: context check error for %s: %s"
                          (magnus-instance-name instance)
                          (error-message-string err))))))))

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
                       pct memory-rel)
               "Magnus"))))))))

(defun magnus-coord--read-context-usage (instance)
  "Read the latest context token count from INSTANCE's session trace.
Returns total input tokens or nil if unavailable.  Only reads the
last 32KB of the file for efficiency."
  (when-let ((session-id (magnus-instance-session-id instance))
             (jsonl (magnus-process--session-jsonl-path
                     (magnus-instance-directory instance) session-id)))
    (let* ((attrs (file-attributes jsonl))
           (size (file-attribute-size attrs))
           (start (max 0 (- size 32768))))
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
                      "\"input_tokens\":[[:space:]]*\\([0-9]+\\)" line-start line-end))
              (cache-create (magnus-coord--extract-number
                             "\"cache_creation_input_tokens\":[[:space:]]*\\([0-9]+\\)" line-start line-end))
              (cache-read (magnus-coord--extract-number
                           "\"cache_read_input_tokens\":[[:space:]]*\\([0-9]+\\)" line-start line-end)))
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

(defvar magnus-coord--watched-dirs nil
  "List of directories being polled for coordination file changes.")

(defun magnus-coord-watched-directories ()
  "Return a copy of physical project roots with live coordination watchers."
  (copy-sequence magnus-coord--watched-dirs))

(defvar magnus-coord--file-mtimes nil
  "Alist of (directory . modification-time) for polling dedup.")

(defvar magnus-coord--states nil
  "Alist of parsed coordination state for watched project directories.")

(defun magnus-coord--normalized-directory (directory)
  "Return DIRECTORY's canonical physical project identity."
  (directory-file-name (file-truename (expand-file-name directory))))

(defun magnus-coord--same-directory-p (left right)
  "Return non-nil when LEFT and RIGHT have the same normalized spelling."
  (and (stringp left) (stringp right)
       (string= (magnus-coord--normalized-directory left)
                (magnus-coord--normalized-directory right))))

(defun magnus-coord--project-owned-p (directory)
  "Return non-nil when an active agent still owns DIRECTORY's watch."
  (cl-some
   (lambda (instance)
     (magnus-coord--same-directory-p
      (magnus-instance-directory instance) directory))
   (magnus-instances-active-list)))

(defun magnus-coord--maybe-stop-watching (directory)
  "Stop watching DIRECTORY after every active agent releases it."
  (setq directory (magnus-coord--normalized-directory directory))
  (when (and (member directory magnus-coord--watched-dirs)
             (not (magnus-coord--project-owned-p directory)))
    (magnus-coord-stop-watching directory)))

(defvar magnus-coord--processed-mentions nil
  "Alist of (directory . list-of-processed-mention-hashes) to avoid duplicates.")

(defvar magnus-coord--processed-dms nil
  "Alist of (directory . list-of-processed-dm-hashes) to avoid duplicates.")

(defvar magnus-coord--processed-summons nil
  "Alist of (directory . list-of-processed-summon-hashes) to avoid duplicates.")

(defvar magnus-coord--poll-timer nil
  "Timer for polling coordination files for messages.")

(defun magnus-coord-ensure-watchers ()
  "Watch coordination files for all active-instance directories.
Call this on startup for instances restored from persistence."
  (let ((dirs
         (delete-dups
          (mapcar (lambda (instance)
                    (magnus-coord--normalized-directory
                     (magnus-instance-directory instance)))
                  (magnus-instances-active-list)))))
    (dolist (dir dirs)
      (condition-case err
          (magnus-coord-start-watching dir)
        (error
         (message "Magnus: could not restore coordination watcher for %s: %s"
                  dir (error-message-string err))))))
  (magnus-coord--start-poll-timer))

(defun magnus-coord--seed-content (directory content)
  "Seed delivery deduplication for DIRECTORY from one CONTENT read."
  (magnus-coord--cache-content directory content)
  (magnus-coord--init-processed-mentions directory content)
  (magnus-coord--init-processed-dms directory content)
  (magnus-coord--init-processed-summons directory content))

(defun magnus-coord--consume-content (directory content)
  "Consume every coordination effect for DIRECTORY from one CONTENT read."
  (when magnus-coord-mention-notify
    (magnus-coord--check-new-mentions directory content))
  (magnus-coord--check-new-dms directory content)
  (magnus-coord--check-new-summons directory content))

(defun magnus-coord-start-watching (directory)
  "Start observing coordination in DIRECTORY.
Calling this again does not reseed delivery state, so an in-flight message
cannot be silently reclassified as history."
  (setq directory (magnus-coord--normalized-directory directory))
  ;; A restored agent can resume without registering again.  Refresh generated
  ;; guidance at watcher acquisition so deleted protocol generations cannot
  ;; survive an Emacs restart and misdirect that agent.
  (magnus-coord-ensure-file directory)
  (magnus-coord-ensure-instructions directory)
  (unless (member directory magnus-coord--watched-dirs)
    (let* ((file (magnus-coord-file-path directory))
           (content (magnus-coord--read-content directory)))
      (cl-pushnew directory magnus-coord--watched-dirs :test #'equal)
      (setf (alist-get directory magnus-coord--file-mtimes nil nil #'equal)
            (and (file-exists-p file)
                 (file-attribute-modification-time (file-attributes file))))
      (magnus-coord--seed-content directory content)))
  (magnus-coord--start-poll-timer))

(defun magnus-coord-stop-watching (directory)
  "Stop polling the coordination file in DIRECTORY."
  (setq directory (magnus-coord--normalized-directory directory))
  (setq magnus-coord--watched-dirs (delete directory magnus-coord--watched-dirs))
  (setq magnus-coord--file-mtimes
        (assoc-delete-all directory magnus-coord--file-mtimes))
  (setq magnus-coord--states
        (assoc-delete-all directory magnus-coord--states))
  (setq magnus-coord--processed-mentions
        (assoc-delete-all directory magnus-coord--processed-mentions))
  (setq magnus-coord--processed-dms
        (assoc-delete-all directory magnus-coord--processed-dms))
  (setq magnus-coord--processed-summons
        (assoc-delete-all directory magnus-coord--processed-summons))
  (when (and (null magnus-coord--watched-dirs)
             magnus-coord--poll-timer)
    (cancel-timer magnus-coord--poll-timer)
    (setq magnus-coord--poll-timer nil)))

(defun magnus-coord--start-poll-timer ()
  "Start the coordination file poll timer."
  (when (and magnus-coord--watched-dirs
             (null magnus-coord--poll-timer))
    (setq magnus-coord--poll-timer
          (run-with-timer 3 3 #'magnus-coord--poll-all))))

(defun magnus-coord--poll-all ()
  "Poll watched projects for coordination messages.
Also update vterm buffer activity ticks for quiescence tracking."
  (magnus-coord--update-buffer-ticks)
  (dolist (directory magnus-coord--watched-dirs)
    (condition-case err
        (let* ((file (magnus-coord-file-path directory))
               (mtime (and (file-exists-p file)
                           (file-attribute-modification-time
                            (file-attributes file))))
               (last-mtime
                (alist-get directory magnus-coord--file-mtimes
                           nil nil #'equal)))
          (unless (equal mtime last-mtime)
            (let* ((content (magnus-coord--read-content directory))
                   ;; Delivery sees the whole immutable ingress revision even
                   ;; when presentation history is trimmed before caching.
                   (ingress-content content)
                   (max-entries
                    (magnus-coord--dir-local-value
                     'magnus-coord-log-max-entries directory))
                   (trimmed
                    (magnus-coord--trim-log-content content max-entries)))
              (when (car trimmed)
                (setq content (cdr trimmed))
                (with-temp-buffer
                  (insert content)
                  (magnus-coord--write-file-atomic file))
                ;; The atomic replacement has its own identity and mtime.
                (setq mtime
                      (file-attribute-modification-time
                       (file-attributes file))))
              (magnus-coord--cache-content directory content)
              (magnus-coord--consume-content directory ingress-content)
              (setf (alist-get directory magnus-coord--file-mtimes
                               nil nil #'equal)
                    mtime))))
      (error
       (message "Magnus: coordination poll error for %s: %s"
                directory (error-message-string err))))))

(defun magnus-coord-stop-all-watchers ()
  "Stop all coordination file polling."
  (when magnus-coord--poll-timer
    (cancel-timer magnus-coord--poll-timer)
    (setq magnus-coord--poll-timer nil))
  (setq magnus-coord--watched-dirs nil)
  (setq magnus-coord--file-mtimes nil)
  (setq magnus-coord--states nil)
  (setq magnus-coord--processed-mentions nil)
  (setq magnus-coord--processed-dms nil)
  (setq magnus-coord--processed-summons nil))

(defun magnus-coord-shutdown ()
  "Stop every long-lived coordination resource.
Safe to call after partial setup and safe to call more than once."
  (magnus-coord-stop-reminders)
  (magnus-coord-stop-all-watchers)
  (magnus-coord-stop-attention-tracking)
  (remove-hook 'pre-command-hook #'magnus-coord--on-user-return)
  (when magnus-coord--attention-save-timer
    (cancel-timer magnus-coord--attention-save-timer)
    (setq magnus-coord--attention-save-timer nil)
    (condition-case err
        (magnus-coord-attention-save)
      (error
       (message "Magnus: could not flush attention data: %s"
                (error-message-string err))))))

(defun magnus-coord--read-content (directory)
  "Read DIRECTORY's coordination file once, or return nil."
  (let ((file (magnus-coord-file-path directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun magnus-coord--parse-content (content)
  "Parse already-read coordination CONTENT."
  (if content
      (with-temp-buffer
        (insert content)
        (magnus-coord--parse-buffer))
    (list :active nil :log nil :discoveries nil :decisions nil)))

(defun magnus-coord--cache-content (directory content)
  "Cache parsed CONTENT for watched DIRECTORY."
  (setf (alist-get directory magnus-coord--states nil nil #'equal)
        (magnus-coord--parse-content content)))

(cl-defun magnus-coord--init-processed-mentions
    (directory &optional (content nil content-supplied-p))
  "Initialize processed mentions for DIRECTORY from CONTENT.
Read the coordination file when CONTENT was not supplied."
  (unless content-supplied-p
    (setq content (magnus-coord--read-content directory)))
  (when content
    (let ((mentions (magnus-coord--extract-mentions content)))
      (setf (alist-get directory magnus-coord--processed-mentions nil nil #'equal)
            (mapcar #'magnus-coord--mention-hash mentions)))))

(cl-defun magnus-coord--check-new-mentions
    (directory &optional (content nil content-supplied-p))
  "Check CONTENT for new @mentions in DIRECTORY.
Read the coordination file when CONTENT was not supplied."
  (unless content-supplied-p
    (setq content (magnus-coord--read-content directory)))
  (let* ((mentions (when content (magnus-coord--extract-mentions content)))
         (processed
          (alist-get directory magnus-coord--processed-mentions
                     nil nil #'equal)))
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
      (while (re-search-forward
              "@\\(?:{\\([^}\n]+\\)}\\|\\([a-zA-Z][-a-zA-Z0-9_]*\\)\\)"
              nil t)
        (let* ((prefix
                (buffer-substring-no-properties
                 (line-beginning-position) (match-beginning 0)))
               (agent (string-trim (or (match-string 1) (match-string 2))))
               (line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
          ;; Direct-message and summon envelopes have their own delivery
          ;; paths; do not also turn their target token into a plain mention.
          (unless (string-match-p "\\[\\(?:DM\\|SUMMON\\) \\'" prefix)
            (push (cons agent line) mentions)))))
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
                     (magnus-coord--same-directory-p
                      (magnus-instance-directory inst) directory)))
              (magnus-instances-list)))

(defun magnus-coord--extract-sender-and-message (context-line)
  "Extract sender name and message from CONTEXT-LINE.
Returns (sender . message) or nil."
  (when (string-match
         (concat "\\[.*?\\] \\([^:]+\\): .*?"
                 "@\\(?:{[^}\n]+}\\|[^ ]+\\) \\(.*\\)")
         context-line)
    (let ((sender (match-string 1 context-line))
          (message (match-string 2 context-line)))
      (cons sender (string-trim message)))))

(defun magnus-coord--send-mention-notification (instance context-line)
  "Send a mention notification to INSTANCE with CONTEXT-LINE.
Delivers the message content without commanding the agent."
  (let* ((parsed (magnus-coord--extract-sender-and-message context-line))
         (msg (if parsed
                  (format "[From %s]: %s"
                          (car parsed) (cdr parsed))
                (format "[Mention in %s]: %s"
                        (magnus-coord-display-file
                         (magnus-instance-directory instance))
                        context-line))))
    (magnus-coord-nudge-agent instance msg)))

;;; Agent-to-agent direct messages

(cl-defun magnus-coord--init-processed-dms
    (directory &optional (content nil content-supplied-p))
  "Initialize processed DMs for DIRECTORY from CONTENT.
Read the coordination file when CONTENT was not supplied."
  (unless content-supplied-p
    (setq content (magnus-coord--read-content directory)))
  (when content
    (let ((dms (magnus-coord--extract-dms content)))
      (setf (alist-get directory magnus-coord--processed-dms nil nil #'equal)
            (mapcar #'magnus-coord--dm-hash dms)))))

(defun magnus-coord--extract-dms (content)
  "Extract all [DM @name] patterns from CONTENT.
Returns list of (target sender message) tuples."
  (let (dms)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward
              "\\[DM @\\([^]\n]+\\)\\][[:space:]]*\\(.*\\)"
              nil t)
        (let* ((target (string-trim (match-string 1)))
               (message (string-trim (match-string 2)))
               (line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
               (sender (when (string-match "\\] \\([^:]+\\):" line)
                         (match-string 1 line))))
          (push (list target (or sender "unknown") message) dms))))
    (nreverse dms)))

(defun magnus-coord--dm-hash (dm)
  "Create a hash for DM to track duplicates."
  (secure-hash 'md5 (format "%s:%s:%s" (nth 0 dm) (nth 1 dm) (nth 2 dm))))

(cl-defun magnus-coord--check-new-dms
    (directory &optional (content nil content-supplied-p))
  "Check CONTENT for new direct messages in DIRECTORY.
Read the coordination file when CONTENT was not supplied."
  (unless content-supplied-p
    (setq content (magnus-coord--read-content directory)))
  (let* ((dms (when content (magnus-coord--extract-dms content)))
         (processed
          (alist-get directory magnus-coord--processed-dms
                     nil nil #'equal)))
    (dolist (dm dms)
      (let ((hash (magnus-coord--dm-hash dm)))
        (unless (member hash processed)
          (magnus-coord--deliver-dm directory dm)
          (push hash processed))))
    (setf (alist-get directory magnus-coord--processed-dms nil nil #'equal)
          processed)))

(defun magnus-coord--deliver-dm (directory dm)
  "Deliver DM to the target agent in DIRECTORY.
DM is (target sender message)."
  (let* ((target (nth 0 dm))
         (sender (nth 1 dm))
         (message (nth 2 dm))
         (instance (magnus-coord--find-instance-by-name target directory)))
    (when instance
      (magnus-coord-nudge-agent
       instance
       (format "[DM from %s]: %s" sender message)))))

;;; Agent-initiated summoning

(defvar magnus--summon-context)

(cl-defun magnus-coord--init-processed-summons
    (directory &optional (content nil content-supplied-p))
  "Initialize processed summons for DIRECTORY from CONTENT.
Read the coordination file when CONTENT was not supplied."
  (unless content-supplied-p
    (setq content (magnus-coord--read-content directory)))
  (when content
    (let ((summons (magnus-coord--extract-summons content)))
      (setf (alist-get directory magnus-coord--processed-summons nil nil #'equal)
            (mapcar #'magnus-coord--summon-hash summons)))))

(defun magnus-coord--extract-summons (content)
  "Extract all [SUMMON @name] patterns from CONTENT.
Returns list of (target sender reason) tuples."
  (let (summons)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward
              "\\[SUMMON @\\([^]\n]+\\)\\][[:space:]]*\\(.*\\)"
              nil t)
        (let* ((target (string-trim (match-string 1)))
               (reason (string-trim (match-string 2)))
               (line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
               (sender (when (string-match "\\] \\([^:]+\\):" line)
                         (match-string 1 line))))
          (push (list target (or sender "unknown") reason) summons))))
    (nreverse summons)))

(defun magnus-coord--summon-hash (summon)
  "Create a hash for SUMMON to track duplicates."
  (secure-hash 'md5 (format "%s:%s:%s"
                             (nth 0 summon) (nth 1 summon) (nth 2 summon))))

(cl-defun magnus-coord--check-new-summons
    (directory &optional (content nil content-supplied-p))
  "Check CONTENT for new summon requests in DIRECTORY.
Read the coordination file when CONTENT was not supplied."
  (unless content-supplied-p
    (setq content (magnus-coord--read-content directory)))
  (let* ((summons (when content (magnus-coord--extract-summons content)))
         (processed (alist-get directory magnus-coord--processed-summons
                               nil nil #'equal)))
    (dolist (summon summons)
      (let ((hash (magnus-coord--summon-hash summon)))
        (unless (member hash processed)
          ;; Wait for user to be idle before prompting
          (run-with-idle-timer 5 nil
                               #'magnus-coord--handle-summon directory summon)
          (push hash processed))))
    (setf (alist-get directory magnus-coord--processed-summons nil nil #'equal)
          processed)))

(defun magnus-coord--handle-summon (directory summon)
  "Handle a SUMMON request in DIRECTORY.
SUMMON is (target-name sender reason)."
  (let* ((target (nth 0 summon))
         (sender (nth 1 summon))
         (reason (nth 2 summon))
         (agents-dir (expand-file-name ".claude/agents/" directory))
         (memory (expand-file-name (concat target "/memory.md") agents-dir))
         (existing (mapcar #'magnus-instance-name (magnus-instances-list)))
         (is-active (member target existing))
         (is-dormant (and (file-exists-p memory) (not is-active))))
    (cond
     (is-dormant
      (when (y-or-n-p
             (format "Agent %s requests: summon %s (%s)? "
                     sender target
                     (if (string-empty-p reason) "no reason given" reason)))
        (magnus-coord--execute-summon directory target sender reason)))
     (is-active
      ;; Target is already running — notify the requesting agent
      (when-let ((requester (magnus-coord--find-instance-by-name sender directory)))
        (magnus-coord-nudge-agent
         requester
         (format "%s is already online — publish an @mention through the coordination protocol instead."
                 target)
         "Magnus"))))))

(defun magnus-coord--execute-summon (directory target sender reason)
  "Execute a summon of TARGET in DIRECTORY, requested by SENDER for REASON."
  (setq magnus--summon-context (list :sender sender :reason reason))
  (unwind-protect
      (magnus-process-create directory target)
    (setq magnus--summon-context nil))
  (magnus-coord-add-log
   directory "Magnus"
   (format "Summoned %s (requested by %s: %s)" target sender reason))
  (message "Magnus: summoned %s for %s (%s)" target sender reason))

;;; Session retrospectives

(defvar magnus-coord--session-start-times (make-hash-table :test 'equal)
  "Hash: directory -> float-time when first agent joined this session.")

(defvar magnus-coord--latest-retro (make-hash-table :test 'equal)
  "Hash: directory -> path of the most recent retro file.")

(defun magnus-coord--git-log-since (directory since-time)
  "Get git log since SINCE-TIME in DIRECTORY."
  (let ((default-directory directory)
        (since (format-time-string "%Y-%m-%dT%H:%M:%S"
                                   (seconds-to-time since-time))))
    (condition-case err
        (with-temp-buffer
          (call-process "git" nil t nil
                        "log" "--oneline" "-n" "100"
                        (format "--since=%s" since))
          (let ((output (string-trim (buffer-string))))
            (if (string-empty-p output) "No commits" output)))
      (error
       (message "Magnus: git log failed: %s" (error-message-string err))
       "No git data"))))

(defun magnus-coord--collect-retro-data (directory)
  "Collect session data for a retrospective in DIRECTORY.
Returns a plist with :log, :discoveries, :decisions, :git, :start, :end."
  (let* ((parsed (magnus-coord-parse directory))
         (log-entries (plist-get parsed :log))
         (decisions (plist-get parsed :decisions))
         ;; Preserve prose and nested Markdown written by older Magnus agents;
         ;; the status parser's top-level bullet model is intentionally narrower.
         (discoveries (magnus-coord--section-text directory "Discoveries"))
         (start-time (gethash directory magnus-coord--session-start-times))
         (git-log (when start-time
                    (magnus-coord--git-log-since directory start-time))))
    (list :log log-entries
          :discoveries discoveries
          :decisions decisions
          :git (or git-log "No git data")
          :start start-time
          :end (float-time))))

(defun magnus-coord--section-text (directory heading)
  "Return complete Markdown section HEADING from DIRECTORY's journal."
  (with-temp-buffer
    (insert (or (magnus-coord--read-content directory) ""))
    (goto-char (point-min))
    (when (re-search-forward
           (format "^## %s[[:space:]]*$" (regexp-quote heading)) nil t)
      (forward-line 1)
      (let ((start (point))
            (end (if (re-search-forward "^## " nil t)
                     (match-beginning 0)
                   (point-max))))
        (let ((text (string-trim
                     (buffer-substring-no-properties start end))))
          (unless (string-empty-p text) text))))))

(defun magnus-coord--format-log-for-retro (entries)
  "Format log ENTRIES for the retro prompt."
  (if entries
      (mapconcat (lambda (e)
                   (format "[%s] %s: %s"
                           (plist-get e :time)
                           (plist-get e :agent)
                           (plist-get e :message)))
                 entries "\n")
    "No log entries"))

(defun magnus-coord--retro-prompt (data)
  "Build the Claude prompt for a session retro from DATA."
  (format "Summarize this multi-agent coding session. Be concise and useful.

## Coordination Log
%s

## Discoveries
%s

## Decisions
%s

## Git Commits This Session
%s

Write a session retrospective with these sections:
- **Accomplished**: What got done (2-4 bullets)
- **Decisions**: Key choices made and why
- **Discovered**: Important learnings or gotchas
- **Unfinished**: What's still pending
- **Next**: Suggested priorities for next session

Keep it under 250 words. No filler."
          (magnus-coord--format-log-for-retro (plist-get data :log))
          (or (plist-get data :discoveries) "None recorded")
          (if (plist-get data :decisions)
              (mapconcat #'identity (plist-get data :decisions) "\n")
            "None recorded")
          (plist-get data :git)))

(defun magnus-coord-generate-retro (directory)
  "Generate a session retrospective for DIRECTORY asynchronously."
  (setq directory (magnus-coord--normalized-directory directory))
  (when (bound-and-true-p magnus-claude-executable)
    (let* ((data (magnus-coord--collect-retro-data directory))
           (key (list 'coord-retro directory)))
      (magnus-background-submit
       key 'claude
       (list :purpose 'agent
             :directory directory
             :prompt (magnus-coord--retro-prompt data)
             :allowed-tools ""
             :model (and (boundp 'magnus-headless-model)
                         magnus-headless-model)
             :name "session-retro")
       (list
        :on-complete
        (lambda (result)
          (when (plist-get result :success-p)
            (magnus-coord--save-retro
             directory
             (magnus--strip-thinking-markers (plist-get result :output))
             data))))))))

(defun magnus-coord--save-retro (directory content data)
  "Save retro CONTENT for DIRECTORY with session DATA metadata."
  (let* ((retros-dir (expand-file-name ".claude/retros/" directory))
         (timestamp (format-time-string "%Y-%m-%d-%H%M%S"))
         (file (expand-file-name (concat timestamp ".md") retros-dir))
         (start (plist-get data :start))
         (end-time (plist-get data :end)))
    (unless (file-directory-p retros-dir)
      (make-directory retros-dir t))
    (with-temp-file file
      (insert (format "# Session Retrospective — %s\n\n" timestamp))
      (when start
        (insert (format "**Session**: %s to %s\n\n"
                        (format-time-string "%H:%M" (seconds-to-time start))
                        (format-time-string "%H:%M" (seconds-to-time end-time)))))
      (insert content)
      (insert "\n"))
    (message "Magnus: session retro saved. Press F in magnus to view.")
    ;; Store the path for quick retrieval
    (puthash directory file magnus-coord--latest-retro)))

(defun magnus-coord--display-retro (file)
  "Display retro FILE in a buffer."
  (when (and file (file-exists-p file))
    (let ((buf (get-buffer-create "*magnus-retro*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents file)
          (goto-char (point-min)))
        (special-mode)
        (setq-local truncate-lines nil)
        (setq-local word-wrap t))
      (switch-to-buffer buf))))

(defun magnus-coord--find-latest-retro (directory)
  "Find the most recent retro file in DIRECTORY."
  (or (gethash directory magnus-coord--latest-retro)
      (let ((retros-dir (expand-file-name ".claude/retros/" directory)))
        (when (file-directory-p retros-dir)
          (let ((files (directory-files retros-dir t "\\.md$")))
            (car (last (sort files #'string<))))))))

(defun magnus-retro ()
  "Show the latest session retrospective, or generate one.
If agents are running, generates a mid-session retro.
If no agents are running, shows the most recent saved retro."
  (interactive)
  (let* ((dir (or (when-let ((inst (car (magnus-instances-list))))
                    (magnus-instance-directory inst))
                  (magnus-coord--get-directory)))
         (has-agents (cl-some
                      (lambda (inst)
                        (and (magnus-coord--same-directory-p
                              (magnus-instance-directory inst) dir)
                             (eq (magnus-instance-status inst) 'running)))
                      (magnus-instances-list))))
    (if has-agents
        (progn
          (message "Generating mid-session retro...")
          (magnus-coord-generate-retro dir))
      ;; No agents — show latest saved retro
      (let ((file (magnus-coord--find-latest-retro dir)))
        (if file
            (magnus-coord--display-retro file)
          (user-error "No retrospectives found for this project"))))))

;;; Coordination file management

(defun magnus-coord-file-path (directory)
  "Get the coordination file path for DIRECTORY."
  (expand-file-name magnus-coord-file directory))

(defun magnus-coord-instructions-path (directory)
  "Get the instructions file path for DIRECTORY."
  (expand-file-name magnus-coord-instructions-file directory))

(defun magnus-coord--display-path (directory file)
  "Return FILE relative to DIRECTORY when it is inside that project."
  (let* ((root (file-name-as-directory (expand-file-name directory)))
         (absolute (expand-file-name file root))
         (relative (file-relative-name absolute root)))
    (if (or (file-name-absolute-p relative)
            (string-match-p "\\`\\.\\.?\\(?:/\\|\\'\\)" relative))
        absolute
      relative)))

(defun magnus-coord-display-file (directory)
  "Return DIRECTORY's configured coordination path for agent guidance."
  (magnus-coord--display-path directory magnus-coord-file))

(defun magnus-coord-display-instructions-file (directory)
  "Return DIRECTORY's configured instruction path for agent guidance."
  (magnus-coord--display-path directory magnus-coord-instructions-file))

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
    (insert "This file is used by Magnus agents to coordinate their work.\n")
    (insert "Agents should check this file before starting and announce their plans.\n\n")
    (insert "## Active Work\n\n")
    (insert "<!-- Agents: Update this section when you start/finish work -->\n\n")
    (insert "| Agent | Area | Status | Files |\n")
    (insert "|-------|------|--------|-------|\n")
    (insert "\n## Discoveries\n\n")
    (insert "<!-- Share things you learned that other agents should know -->\n\n")
    (insert "## Decisions\n\n")
    (insert "<!-- Record agreed-upon decisions here -->\n\n")
    (insert "## Log\n\n")
    (insert "<!-- Agents: Insert newest messages below this comment; do not append at the bottom. -->\n\n")))

(defun magnus-coord-ensure-instructions (directory)
  "Ensure agent instructions file exists and is up-to-date in DIRECTORY."
  (let ((file (magnus-coord-instructions-path directory)))
    (if (file-exists-p file)
        (when (magnus-coord--instructions-stale-p file)
          (magnus-coord--create-instructions file directory))
      (magnus-coord--create-instructions file directory))
    file))

(defun magnus-coord--instructions-stale-p (file)
  "Return non-nil if FILE has an outdated instructions version."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((content (buffer-string)))
      (if (string-match "magnus-instructions-version: \\([0-9]+\\)" content)
          (< (string-to-number (match-string 1 content))
             magnus-coord--instructions-version)
        ;; No version marker — file predates versioning, regenerate
        t))))

(defun magnus-coord--create-instructions (file directory)
  "Create instructions FILE for agents in DIRECTORY."
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (with-temp-file file
    (insert (magnus-coord--instructions-content directory))))

(defun magnus-coord--instructions-content (directory)
  "Generate instructions content."
  (let ((journal (magnus-coord-display-file directory))
        (instructions (magnus-coord-display-instructions-file directory)))
    (format "# Magnus Coordination Protocol

These generated instructions live at %S. Magnus may run Claude and Codex agents
together. They coordinate through the shared %S file.

## Read First

1. Read applicable project guidance and inspect existing work.
2. Read %S completely.
3. Check Active Work for overlap before editing.
4. Announce substantive work in your own Active Work row and add a short Log
   entry. Never modify another agent's row.

## Coordination Conventions

- Keep your Active Work row current and list the files you expect to touch.
- Put useful discoveries and architectural decisions in their named sections.
- The Log is stored newest-first. Insert each new entry immediately below the
  Log heading's comments and blank preamble; never append it at the bottom.
  Write ordinary entries as `[HH:MM] name: message`.
- Use `@name` or `@{display name}` for mentions, `[DM @name]` for a direct
  message, `[SUMMON @name]` to request a dormant teammate, and `[ATTENTION]`
  when the user should inspect something.
- Re-read the file before writing when teammates may have updated it. Preserve
  their entries and resolve overlaps in the Log.
- No plugin or skill is required.

## Finishing and Authority

Release your Active Work row, log completion, preserve useful
discoveries/decisions, and update your first-person memory. Coordination context
does not authorize commits, pushes, deployments, destructive actions, external
messages, or unrelated changes.

<!-- magnus-instructions-version: %d -->
" instructions journal journal magnus-coord--instructions-version)))

;;; Legacy explicit coordination skill API

(defun magnus-coord-skill-path (directory)
  "Return the legacy coordination skill path for DIRECTORY."
  (expand-file-name magnus-coord-skill-file directory))

(defun magnus-coord-ensure-skill (directory)
  "Explicitly ensure the legacy coordination skill exists in DIRECTORY.
This compatibility entry point is never called by Magnus's normal setup or
onboarding workflow."
  (let ((file (magnus-coord-skill-path directory)))
    (unless (file-exists-p file)
      (magnus-coord--create-skill file directory))
    file))

(defun magnus-coord--create-skill (file &optional directory)
  "Create legacy coordination skill FILE for DIRECTORY."
  (let ((parent (file-name-directory file)))
    (unless (file-exists-p parent)
      (make-directory parent t)))
  (with-temp-file file
    (insert (magnus-coord--skill-content directory))))

(defun magnus-coord--skill-content (&optional directory)
  "Return legacy coordination skill content for DIRECTORY."
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
5. **Log your check-in**: Insert a message at the top of the Log section,
   immediately below its comments and blank preamble:
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
- Keep the Log newest-first; never append new entries at the bottom.
- Never modify another agent's Active Work row.
- If you are blocked by another agent, @mention them — they will be notified automatically.
- Read the Discoveries section when you check in — other agents may have learned something that helps you.
" (magnus-coord-display-file (or directory default-directory))))

;;; Parsing coordination file

(defun magnus-coord--parsed-state (directory)
  "Parse DIRECTORY's coordination file without rereading a watched file."
  (setq directory (magnus-coord--normalized-directory directory))
  (let ((cached (assoc directory magnus-coord--states)))
    (if cached
        (cdr cached)
      (magnus-coord--parse-content
       (magnus-coord--read-content directory)))))

(defun magnus-coord-parse (directory)
  "Return Markdown coordination state for DIRECTORY.
Log entries are returned in chronological order."
  (let ((parsed (copy-tree (magnus-coord--parsed-state directory))))
    ;; `magnus-coord-add-log' inserts at the top of the Markdown section.
    ;; Normalize once at the public read boundary for status and retrospectives.
    (plist-put parsed :log (reverse (plist-get parsed :log)))
    parsed))

(defun magnus-coord--parse-buffer ()
  "Parse the current buffer as a coordination file."
  (let ((active (magnus-coord--parse-active-table))
        (log (magnus-coord--parse-log))
        (discoveries (magnus-coord--parse-list-section "Discoveries"))
        (decisions (magnus-coord--parse-decisions)))
    (list :active active :log log
          :discoveries discoveries :decisions decisions)))

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

(defun magnus-coord--parse-list-section (heading)
  "Parse Markdown list entries below section HEADING in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (entries)
      (when (re-search-forward
             (format "^## %s[[:space:]]*$" (regexp-quote heading)) nil t)
        (let ((section-end
               (save-excursion
                 (if (re-search-forward "^## " nil t)
                     (match-beginning 0)
                   (point-max)))))
          (while (re-search-forward "^- \\(.+\\)$" section-end t)
            (push (match-string 1) entries))))
      (nreverse entries))))

(defun magnus-coord--parse-decisions ()
  "Parse the Decisions section from current buffer."
  (magnus-coord--parse-list-section "Decisions"))

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
Inserts a log entry instead of clearing sections, preserving
Discoveries and Decisions for future agents."
  (magnus-coord-add-log directory "Magnus" "Session ended"))

(defun magnus-coord-reconcile (directory)
  "Reconcile the Active Work table in DIRECTORY with runtime-capable instances.
Keep rows owned by running or suspended instances.  Remove every other row,
including stale statuses like done, died, finished, completed, or stopped."
  (let* ((file (magnus-coord-file-path directory))
         (live-names (mapcar #'magnus-instance-name
                             (cl-remove-if-not
                              (lambda (inst)
                                (and
                                 (memq (magnus-instance-status inst)
                                       '(running suspended))
                                 (magnus-coord--same-directory-p
                                  (magnus-instance-directory inst) directory)))
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
               (append
                (mapcar
                 (lambda (instance)
                   (magnus-coord--normalized-directory
                    (magnus-instance-directory instance)))
                 (magnus-instances-list))
                (copy-sequence magnus-coord--watched-dirs)))))
    (dolist (dir dirs)
      (magnus-coord-reconcile dir))))

(defun magnus-coord-refresh (directory)
  "Refresh DIRECTORY from Markdown and deliver new coordination effects."
  (setq directory (magnus-coord--normalized-directory directory))
  (let* ((file (magnus-coord-file-path directory))
         (content (magnus-coord--read-content directory))
         (mtime (and (file-exists-p file)
                     (file-attribute-modification-time
                      (file-attributes file)))))
    (magnus-coord--cache-content directory content)
    (magnus-coord--consume-content directory content)
    (setf (alist-get directory magnus-coord--file-mtimes nil nil #'equal)
          mtime)
    (magnus-coord-parse directory)))

(defun magnus-coord-refresh-all ()
  "Refresh coordination state for every active or watched project.
Return an alist of project directories and refresh results.  A failure in one
project is reported without preventing healthy siblings from refreshing."
  (let ((directories
         (delete-dups
          (append (copy-sequence magnus-coord--watched-dirs)
                  (mapcar
                   (lambda (instance)
                     (magnus-coord--normalized-directory
                      (magnus-instance-directory instance)))
                   (magnus-instances-active-list)))))
        results)
    (dolist (directory directories)
      (condition-case err
          (push
           (cons
            directory
            (if (member directory magnus-coord--watched-dirs)
                (magnus-coord-refresh directory)
              ;; Acquiring a watcher already performs one shared read, caches
              ;; it, and consumes every effect.  Do not consume it again.
              (magnus-coord-start-watching directory)
              (magnus-coord-parse directory)))
           results)
        (error
         (message "Magnus: coordination refresh failed for %s: %s"
                  directory (error-message-string err)))))
    (nreverse results)))

(defun magnus-coord-has-state-p (directory)
  "Return non-nil when DIRECTORY has cached or on-disk coordination state."
  (setq directory (magnus-coord--normalized-directory directory))
  (or (assoc directory magnus-coord--states)
      (file-exists-p (magnus-coord-file-path directory))))

;;; Agent registration

(defun magnus-coord-register-agent (directory instance)
  "Register INSTANCE in DIRECTORY's coordination file."
  (setq directory (magnus-coord--normalized-directory directory))
  (let ((name (magnus-instance-name instance)))
    (magnus-coord-ensure-file directory)
    (magnus-coord-ensure-instructions directory)
    (magnus-coord-add-log directory name "Joined the session")
    (unless (member directory magnus-coord--watched-dirs)
      (magnus-coord-start-watching directory))
    (unless (gethash directory magnus-coord--session-start-times)
      (puthash directory (float-time) magnus-coord--session-start-times))))

(defun magnus-coord-unregister-agent (directory instance)
  "Unregister INSTANCE from DIRECTORY's coordination file."
  (setq directory (magnus-coord--normalized-directory directory))
  (let* ((name (magnus-instance-name instance))
         (remaining
          (cl-count-if
           (lambda (other)
             (and (not (eq other instance))
                  (magnus-coord--same-directory-p
                   (magnus-instance-directory other) directory)))
           (magnus-instances-active-list))))
    (magnus-coord-clear-agent directory name)
    (magnus-coord-add-log directory name "Left the session")
    (when (zerop remaining)
      (magnus-coord-refresh directory)
      (magnus-coord-generate-retro directory)
      (remhash directory magnus-coord--session-start-times)
      (magnus-coord-mark-session-end directory)
      (magnus-coord--maybe-stop-watching directory))))

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

(defun magnus-coord-recent-log (entries limit)
  "Return the most recent LIMIT chronological log ENTRIES.
The returned entries retain chronological order."
  (let ((drop (- (length entries) (max 0 limit))))
    (if (> drop 0) (nthcdr drop entries) entries)))

(defun magnus-coord-format-log (parsed &optional limit)
  "Format the :log entries from PARSED for display.
Show at most LIMIT entries (default 5)."
  (let* ((log (plist-get parsed :log))
         (entries (if limit (magnus-coord-recent-log log limit) log)))
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
  "Open DIRECTORY's coordination file, creating it if necessary."
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
