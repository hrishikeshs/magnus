;;; magnus.el --- Manage multiple AI coding agents -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S

;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Maintainer: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (vterm "0.0.2") (transient "0.4.0") (magit-section "3.3.0"))
;; Keywords: tools, processes, convenience
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Magnus is a Magit-inspired interface for hands-on management of Claude Code
;; and Codex agents in Emacs.  Interactive agents retain their native terminal
;; UIs while Magnus supplies durable identities, shared onboarding, lifecycle
;; management, attention and health monitoring, and a shared coordination
;; journal.
;; Committed work can be sent to a headless reviewer from either provider and
;; read as a structured, folding Git diff.
;;
;; Main entry point: M-x magnus
;; Installation diagnostics: M-x magnus-doctor
;;
;; Key bindings in magnus buffer:
;;   RET - Visit instance or review
;;   c   - Create a Claude Code instance
;;   X   - Create a Codex instance
;;   k   - Archive instance
;;   r   - Rename instance
;;   v   - Request an independent review
;;   g   - Refresh status
;;   ?   - Show help menu

;;; Code:

(require 'cl-lib)
(require 'magnus-environment)
(require 'magnus-instances)

(declare-function magnus-persistence-load "magnus-persistence")
(declare-function magnus-persistence--setup-autosave "magnus-persistence")
(declare-function magnus-attention-start "magnus-attention")
(declare-function magnus-coord-start-reminders "magnus-coord")
(declare-function magnus-coord-ensure-watchers "magnus-coord")
(declare-function magnus-status "magnus-status")
(declare-function magnus-process-create "magnus-process")
(declare-function magnus-process-create-codex "magnus-process")
(declare-function magnus-instances-list "magnus-instances")
(declare-function magnus-instance-name "magnus-instances")
(declare-function magnus-instance-directory "magnus-instances")
(declare-function magnus-context-setup-hooks "magnus-context")
(declare-function project-root "project")
(declare-function magnus-attention-setup-hooks "magnus-attention")
(declare-function magnus-process-create-headless "magnus-process")
(declare-function magnus-health-start "magnus-health")
(declare-function magnus-coord-setup-attention-tracking "magnus-coord")
(declare-function magnus-coord-attention-load "magnus-coord")
(declare-function magnus-coord-nudge-agent "magnus-coord")
(declare-function magnus-instances-active-list "magnus-instances")
(declare-function magnus-process-archive "magnus-process")
(declare-function magnus-process--agent-memory-path "magnus-process")
(declare-function magnus-persistence-save "magnus-persistence")
(declare-function magnus-review-load-all "magnus-review")
(declare-function magnus-review-controller-setup "magnus-review-controller")
(declare-function magnus-review-controller-shutdown "magnus-review-controller")
(declare-function magnus-status-shutdown "magnus-status")
(declare-function magnus-persistence-shutdown "magnus-persistence")
(declare-function magnus-context-shutdown "magnus-context")
(declare-function magnus-coord-shutdown "magnus-coord")
(declare-function magnus-attention-shutdown "magnus-attention")
(declare-function magnus-health-shutdown "magnus-health")
(declare-function magnus-trace-stop-timer "magnus-trace")
(declare-function magnus-background-submit "magnus-background")
(declare-function magnus-background-setup "magnus-background")
(declare-function magnus-background-shutdown "magnus-background")

(defvar magnus-context-directory)
(defvar magnus-context-cache-directory)

;;; Customization

(defgroup magnus nil
  "Manage multiple AI coding agents."
  :group 'tools
  :prefix "magnus-")

(defcustom magnus-claude-executable "claude"
  "Claude Code executable or command prefix.
The value may include established command-line arguments."
  :type 'string
  :group 'magnus)

(defcustom magnus-headless-model nil
  "Model to use for lightweight headless Claude calls.
These include fortune generation, session retrospectives, expertise
indexing, and smart resurrection matching.  When nil, no --model flag
is passed and Claude uses its default model."
  :type '(choice (const :tag "Default (no --model flag)" nil)
                 (string :tag "Model name"))
  :group 'magnus)

(defcustom magnus-expertise-match-timeout 15
  "Maximum seconds a synchronous dormant-expertise match may use.
The bounded matcher falls back cleanly so an unavailable Claude CLI cannot
freeze an interactive Magnus review request indefinitely."
  :type 'number
  :group 'magnus)

(defcustom magnus-default-directory nil
  "Default directory for new Claude Code instances.
If nil, uses the current project root or `default-directory'."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Fixed directory"))
  :group 'magnus)

(defcustom magnus-state-file
  (expand-file-name ".magnus/state.el" (getenv "HOME"))
  "File to persist instance state across Emacs sessions.
Stored in ~/.magnus/ so it survives Emacs config resets and
package reinstalls."
  :type 'file
  :group 'magnus)

(defcustom magnus-headless-allowed-tools "Read Write Edit Glob Grep Bash"
  "Space-separated list of tools allowed in headless mode.
Since headless agents cannot ask for permission interactively,
only tools listed here will be available."
  :type 'string
  :group 'magnus)

(defcustom magnus-buffer-name "*magnus*"
  "Name of the magnus status buffer."
  :type 'string
  :group 'magnus)

(defcustom magnus-attention-data-file
  (expand-file-name ".magnus/attention.el" (getenv "HOME"))
  "File to persist attention pattern data across Emacs sessions.
Stored in ~/.magnus/ so it survives Emacs config resets and
package reinstalls."
  :type 'file
  :group 'magnus)

(defcustom magnus-agents-index-file
  (expand-file-name ".magnus/agents-index.el" (getenv "HOME"))
  "File to store agent expertise index.
Maps agent names to expertise keywords extracted by AI.
Stored in ~/.magnus/ so it survives Emacs config resets."
  :type 'file
  :group 'magnus)

(defcustom magnus-agents-index-memory-limit (* 64 1024)
  "Maximum bytes read from an agent memory file for expertise indexing."
  :type 'integer
  :group 'magnus)

(defcustom magnus-instance-name-generator #'magnus-generate-instance-name
  "Function to generate names for new instances.
Takes the working directory as argument and returns a string."
  :type 'function
  :group 'magnus)

;;; Internal variables

(defvar magnus--initialized nil
  "Non-nil if magnus has been initialized.")

(defvar magnus--restart-required nil
  "Non-nil after an in-session Magnus reinstall requires an Emacs restart.")

(defvar magnus--agents-index nil
  "In-memory agent expertise index.
Alist of (DIRECTORY . ((NAME . TAGS-STRING) ...)).")

;;; Utilities

(defun magnus-project-root ()
  "Get the current project root if available.
Tries Projectile first (non-interactive), then `project.el' with
`maybe' to avoid prompting."
  (or
   (when (and (fboundp 'projectile-project-root)
              (bound-and-true-p projectile-mode))
     (ignore-errors (projectile-project-root)))
   (when (fboundp 'project-current)
     (when-let ((project (project-current 'maybe)))
       (project-root project)))))

;;; Name generation

(defvar magnus--creation-task nil
  "Task description for current agent creation.
Temporarily bound during name generation for smart resurrection.")

(defvar magnus--summon-context nil
  "Summon context for current agent creation.
Plist with :sender and :reason, bound during agent-initiated summoning.")

(defvar magnus--name-adjectives
  '("swift" "bright" "calm" "bold" "keen"
    "wise" "quick" "sharp" "cool" "warm")
  "Adjectives for generating instance names.")

(defvar magnus--name-nouns
  '("fox" "owl" "hawk" "wolf" "bear"
    "deer" "crow" "lynx" "hare" "wren")
  "Nouns for generating instance names.")

(defun magnus-generate-instance-name (directory)
  "Generate an instance name, preferring dormant identities.
When `magnus--creation-task' is set, uses AI to match the task
against dormant agent memories for smart resurrection.  Falls back
to most-recent resurrection, then random name generation."
  (let ((existing
         (append (mapcar #'magnus-instance-name (magnus-instances-list))
                 (magnus-instances-reserved-names directory))))
    (or (when magnus--creation-task
          (magnus--smart-resurrect directory existing magnus--creation-task))
        (magnus--resurrect-dormant-identity directory existing)
        (magnus--generate-random-name existing))))

(defun magnus--resurrect-dormant-identity (directory existing-names)
  "Find a dormant agent identity to resurrect in DIRECTORY.
EXISTING-NAMES is a list of names already in use.
Returns the name of the most recently active dormant identity, or nil."
  (let* ((agents-dir (expand-file-name ".claude/agents/" directory))
         (candidates nil))
    (when (file-directory-p agents-dir)
      (dolist (entry (directory-files agents-dir nil "\\`[^.]"))
        (let ((memory (expand-file-name (concat entry "/memory.md") agents-dir)))
          (when (and (file-exists-p memory)
                     (not (member entry existing-names)))
            (push (cons entry (file-attribute-modification-time
                               (file-attributes memory)))
                  candidates)))))
    ;; Return most recently modified (most experienced agent first)
    (when candidates
      (car (car (sort candidates
                      (lambda (a b)
                        (time-less-p (cdr b) (cdr a)))))))))

(defun magnus--generate-random-name (existing-names)
  "Generate a random name not in EXISTING-NAMES."
  (let ((attempts 0)
        name)
    (while (and (< attempts 100)
                (or (null name) (member name existing-names)))
      (setq name (format "%s-%s"
                         (nth (random (length magnus--name-adjectives))
                              magnus--name-adjectives)
                         (nth (random (length magnus--name-nouns))
                              magnus--name-nouns)))
      (setq attempts (1+ attempts)))
    name))

;;; Smart resurrection

(defun magnus--smart-resurrect (directory existing task)
  "Match dormant agents in DIRECTORY to TASK description.
EXISTING is the list of names currently in use.
Uses a synchronous `claude --print' call to find the best match.
Returns the chosen name, or nil if no match or user declines."
  (when-let ((match (magnus-expertise-match directory task existing)))
    (let ((name (plist-get match :name))
          (reason (plist-get match :reason)))
      (when (y-or-n-p (format "Resurrect %s? (%s) " name reason))
        name))))

(defun magnus-expertise-match (directory task &optional exclusions)
  "Return the dormant identity whose expertise best matches TASK.
DIRECTORY identifies the project and EXCLUSIONS is a list of identity names
that cannot be selected.  The result is a plist containing :name, :reason,
and :expertise, or nil when no suitable identity is available.

This function performs matching only.  Unlike `magnus--smart-resurrect', it
does not prompt, resume a provider session, or otherwise change lifecycle
state, so other workflows such as independent reviews can reuse it."
  (let ((candidates (magnus-expertise-candidates directory exclusions)))
    (when candidates
      (message "Matching task to dormant agents...")
      (when-let ((output (magnus--run-match-sync
                          (magnus--build-match-prompt candidates task))))
        (when-let ((match (magnus--parse-match-output output candidates)))
          (let ((name (car match)))
            (list :name name
                :reason (cdr match)
                :expertise (magnus--agents-index-get
                            directory name)
                :summary (cdr (assoc name candidates)))))))))

(defun magnus-expertise-candidates (directory &optional exclusions)
  "Return dormant expertise candidates for DIRECTORY.
EXCLUSIONS is a list of identity names to omit.  The return value is an alist
of (NAME . SUMMARY), using indexed expertise when available and a bounded
memory excerpt otherwise."
  (let* ((agents-dir (expand-file-name ".claude/agents/" directory))
         candidates)
    (when (file-directory-p agents-dir)
      (dolist (entry (directory-files agents-dir nil "\\`[^.]"))
        (let ((memory (expand-file-name (concat entry "/memory.md")
                                        agents-dir)))
          (when (and (file-exists-p memory)
                     (not (member entry exclusions)))
            (let ((tags (magnus--agents-index-get directory entry)))
              (push (cons entry
                          (if tags
                              (format "Expertise: %s" tags)
                            (magnus--read-memory-summary memory)))
                    candidates))))))
    (nreverse candidates)))

(defun magnus--dormant-candidates (directory existing)
  "Return dormant agent candidates in DIRECTORY.
EXISTING is the list of names to exclude.
Returns an alist of (NAME . SUMMARY) pairs, using expertise index
tags when available, falling back to memory excerpt."
  (magnus-expertise-candidates directory existing))

(defun magnus--read-memory-summary (file)
  "Read the first 500 characters of memory FILE as a summary."
  (with-temp-buffer
    (insert-file-contents file nil 0 500)
    (buffer-string)))

(defun magnus--build-match-prompt (candidates task)
  "Build a prompt to match CANDIDATES against TASK.
CANDIDATES is an alist of (NAME . MEMORY-SUMMARY)."
  (let ((memories (mapconcat
                   (lambda (c)
                     (format "--- %s ---\n%s" (car c) (cdr c)))
                   candidates "\n\n")))
    (format "Dormant agents with experience on this project:\n\n\
%s\n\n\
New task: %s\n\n\
Which agent's experience is most relevant to this task?\n\
Reply with ONLY: agent-name|brief-reason (5 words max)\n\
Or reply: none|no match"
            memories task)))

(defun magnus--headless-command (prompt &optional no-bare)
  "Build a command list for a headless Claude call with PROMPT.
Uses --bare to skip CLAUDE.md, hooks, and plugins — UNLESS NO-BARE is
non-nil.  (In current Claude CLI builds `--bare' fails auth with
\"Not logged in\"; callers that need a real reply pass NO-BARE and
sanitize the CLAUDE.md-flavored output themselves.)
Includes --model flag only when `magnus-headless-model' is set."
  (append (magnus-environment-command-prefix
           magnus-claude-executable "Claude")
          (unless no-bare (list "--bare"))
          (list "--print")
          (when magnus-headless-model (list "--model" magnus-headless-model))
          (list prompt)))

(defun magnus--run-match-sync (prompt)
  "Run `claude --print' synchronously with PROMPT.
Returns the trimmed output string, or nil on error."
  (condition-case err
      (when (bound-and-true-p magnus-claude-executable)
        (let ((process-environment
               (cl-remove-if
                (lambda (e) (string-prefix-p "CLAUDECODE=" e))
                process-environment)))
          (with-temp-buffer
            (let* ((cmd (magnus--headless-command prompt t))
                   (process
                    (make-process
                     :name "magnus-expertise-match"
                     :buffer (current-buffer)
                     :command cmd
                     :connection-type 'pipe
                     :noquery t))
                   (deadline (+ (float-time)
                                (max 0.1 magnus-expertise-match-timeout))))
              (while (and (process-live-p process)
                          (< (float-time) deadline))
                (accept-process-output process 0.05))
              (when (process-live-p process)
                (delete-process process)
                (message "Magnus: expertise match timed out after %.1fs"
                         magnus-expertise-match-timeout))
              (when (and (eq (process-status process) 'exit)
                         (zerop (process-exit-status process)))
                ;; no-bare output may carry CLAUDE.md thinking markers; the
                ;; answer line ("name|reason") is the last line with a pipe.
                (let* ((clean (magnus--strip-thinking-markers (buffer-string)))
                       (lines
                        (seq-filter
                         (lambda (line)
                           (not (string-empty-p (string-trim line))))
                         (split-string clean "\n")))
                       (answer
                        (or (seq-find
                             (lambda (line) (string-match-p "|" line))
                             (reverse lines))
                            (car lines))))
                  (string-trim (or answer ""))))))))
    (error (message "Magnus: match sync error: %s" (error-message-string err))
           nil)))

(defun magnus--parse-match-output (output candidates)
  "Parse OUTPUT from a smart match call.
CANDIDATES is the alist used in the prompt.
Returns (NAME . REASON) if a valid match was found, or nil."
  (when (and output (not (string-empty-p output)))
    (let* ((line (car (split-string output "\n" t)))
           (parts (split-string line "|"))
           (name (string-trim (car parts)))
           (reason (if (cdr parts) (string-trim (cadr parts)) "relevant experience")))
      (when (and (not (string-prefix-p "none" (downcase name)))
                 (assoc name candidates))
        (cons name reason)))))

;;; Agent expertise index

(defun magnus--agents-index-load ()
  "Load the agents index from disk."
  (when (file-exists-p magnus-agents-index-file)
    (condition-case nil
        (setq magnus--agents-index
              (with-temp-buffer
                (insert-file-contents magnus-agents-index-file)
                (goto-char (point-min))
                (read (current-buffer))))
      (error
       (message "Magnus: failed to load agents index")
       (setq magnus--agents-index nil)))))

(defun magnus--agents-index-save ()
  "Save the agents index to disk."
  (when magnus--agents-index
    (let ((dir (file-name-directory magnus-agents-index-file)))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    (with-temp-file magnus-agents-index-file
      (insert ";; Magnus agent expertise index - do not edit manually\n")
      (pp magnus--agents-index (current-buffer)))))

(defun magnus--agents-index-get (directory name)
  "Get expertise tags for agent NAME in DIRECTORY."
  (when-let ((dir-entry (assoc directory magnus--agents-index)))
    (cdr (assoc name (cdr dir-entry)))))

(defun magnus--agents-index-set (directory name tags)
  "Set expertise TAGS for agent NAME in DIRECTORY."
  (let ((dir-entry (assoc directory magnus--agents-index)))
    (if dir-entry
        (let ((agent-entry (assoc name (cdr dir-entry))))
          (if agent-entry
              (setcdr agent-entry tags)
            (push (cons name tags) (cdr dir-entry))))
      (push (cons directory (list (cons name tags)))
            magnus--agents-index)))
  (magnus--agents-index-save))

(defun magnus--agents-index-update (instance)
  "Update the expertise index for INSTANCE asynchronously.
Reads a bounded prefix of the agent's memory and queues one text-only model
call.  All Magnus background work is serialized, so archiving many agents does
not launch many provider processes at once."
  (let* ((name (magnus-instance-name instance))
         (instance-id (magnus-instance-id instance))
         (directory (magnus-instance-directory instance))
         (memory-path (magnus-process--agent-memory-path instance)))
    (when (and (stringp memory-path)
               (file-exists-p memory-path)
               (bound-and-true-p magnus-claude-executable))
      (let* ((memory (with-temp-buffer
                       (insert-file-contents
                        memory-path nil 0
                        (max 0 magnus-agents-index-memory-limit))
                       (buffer-string)))
             (prompt (format "Read this agent memory and extract 3-5 expertise \
keywords that describe what this agent knows. Reply with ONLY comma-separated \
lowercase keywords, nothing else.\n\nMemory:\n%s" memory))
             (key (list 'expertise instance-id)))
        (magnus-background-submit
         key 'claude
         (list :purpose 'agent
               :directory directory
               :prompt prompt
               :allowed-tools ""
               :model magnus-headless-model
               :name (format "index-%s" name))
         (list
          :on-complete
          (lambda (result)
            (when (plist-get result :success-p)
              (let ((current (magnus-instances-get instance-id))
                    (tags
                     (magnus--sanitize-expertise
                      (plist-get result :output))))
                ;; A queued archive job may finish after a transactional rename
                ;; or directory move.  Never publish metadata under its stale
                ;; identity; the next archive/reindex can submit fresh evidence.
                (when (and current
                           (equal name (magnus-instance-name current))
                           (equal directory
                                  (magnus-instance-directory current))
                           (not (string-empty-p tags)))
                  (magnus--agents-index-set directory name tags)
                  (message "Magnus: indexed %s → %s" name tags)))))))))))

(defun magnus--strip-thinking-markers (raw)
  "Strip [thinking]...[end-thinking] blocks and marker lines from RAW.
Agents emit these per project CLAUDE.md conventions; any caller that
passes NO-BARE to `magnus--headless-command' gets them in the output."
  (let ((s (replace-regexp-in-string
            "\\[thinking\\]\\(?:.\\|\n\\)*?\\[end-thinking\\]" "" raw)))
    (replace-regexp-in-string
     "\\[/?\\(?:end-\\)?\\(?:thinking\\|response\\)\\]" "" s)))

(defun magnus--sanitize-expertise (raw)
  "Reduce RAW model output to a clean comma-separated keyword line.
Agents emit [thinking]/[response] markers (per CLAUDE.md), so strip those,
then keep the last non-empty line — the keywords come last."
  (let* ((s (magnus--strip-thinking-markers raw))
         (lines (seq-filter (lambda (l) (not (string-empty-p (string-trim l))))
                            (split-string s "\n")))
         (last (string-trim (or (car (last lines)) ""))))
    ;; Only accept a keyword-ish line: no sentences, must contain a comma or be short.
    (if (and (not (string-empty-p last))
             (< (length last) 120)
             (not (string-match-p "[.!?]$" last)))
        last
      "")))

(defun magnus-reindex-agents ()
  "Re-extract expertise tags for every running agent with a memory file.
The index was previously only refreshed when an agent was ARCHIVED, so
never-archived agents showed no expertise label.  Run this to populate
them now; new agents populate once their memory file exists."
  (interactive)
  (let ((n 0))
    (dolist (instance (magnus-instances-list))
      (let ((memory-path (magnus-process--agent-memory-path instance)))
        (when (and memory-path (file-exists-p memory-path))
          (magnus--agents-index-update instance)
          (setq n (1+ n)))))
    (message "Magnus: reindexing expertise for %d agent(s)…" n)))

;;; Core functionality

(defun magnus--migrate-data-files ()
  "Migrate data files from old locations to ~/.magnus/."
  (let ((migrations
         (list
          (cons (expand-file-name "magnus-state.el" user-emacs-directory)
                magnus-state-file)
          (cons (expand-file-name "magnus-attention-data.el" user-emacs-directory)
                magnus-attention-data-file)
          ;; Also check ~/.claude/ (brief intermediate location)
          (cons (expand-file-name ".claude/magnus-state.el" (getenv "HOME"))
                magnus-state-file)
          (cons (expand-file-name ".claude/magnus-attention-data.el" (getenv "HOME"))
                magnus-attention-data-file))))
    (dolist (pair migrations)
      (let ((old (car pair))
            (new (cdr pair)))
        (when (and (file-exists-p old) (not (file-exists-p new)))
          (make-directory (file-name-directory new) t)
          (copy-file old new)
          (message "Magnus: migrated %s to %s"
                   (file-name-nondirectory old) new)))))
  ;; Migrate context directory
  (let ((old-context (expand-file-name "magnus-context" user-emacs-directory)))
    (when (and (file-directory-p old-context)
               (not (file-directory-p magnus-context-directory)))
      (make-directory (file-name-directory magnus-context-directory) t)
      (copy-directory old-context magnus-context-directory nil t t)
      (message "Magnus: migrated context to %s" magnus-context-directory)))
  ;; Migrate URL cache
  (let ((old-cache (expand-file-name "magnus-url-cache" user-emacs-directory)))
    (when (and (file-directory-p old-cache)
               (not (file-directory-p magnus-context-cache-directory)))
      (make-directory (file-name-directory magnus-context-cache-directory) t)
      (copy-directory old-cache magnus-context-cache-directory nil t t)
      (message "Magnus: migrated URL cache to %s"
               magnus-context-cache-directory))))

(defun magnus--ensure-initialized ()
  "Ensure magnus is initialized."
  (when magnus--restart-required
    (user-error
     "Magnus was upgraded in this Emacs session; restart Emacs before use"))
  (unless magnus--initialized
    (let ((complete nil))
      (unwind-protect
          (progn
            (require 'magnus-instances)
            (require 'magnus-persistence)
            (require 'magnus-process)
            (require 'magnus-background)
            (require 'magnus-context)
            (require 'magnus-coord)
            (require 'magnus-attention)
            (require 'magnus-health)
            (require 'magnus-review)
            (require 'magnus-review-controller)
            (require 'magnus-review-ui)
            (require 'magnus-status)
            (require 'magnus-transient)
            (magnus--migrate-data-files)
            (magnus-persistence-load)
            (magnus--agents-index-load)
            (magnus-background-setup)
            ;; Completed review lineages load before the controller attaches
            ;; its entirely in-memory scope and execution machinery.
            (magnus-review-load-all)
            (magnus-persistence--setup-autosave)
            (add-hook 'kill-emacs-hook #'magnus--shutdown)
            (magnus-review-controller-setup)
            (magnus-coord-ensure-watchers)
            (magnus-context-setup-hooks)
            (magnus-attention-setup-hooks)
            (magnus-attention-start)
            (magnus-coord-start-reminders)
            (magnus-health-start)
            (magnus-coord-attention-load)
            (magnus-coord-setup-attention-tracking)
            (setq magnus--initialized t
                  complete t))
        ;; Any setup step may have installed a hook, launched a reviewer, or
        ;; allocated a timer before failing.  Teardown functions are idempotent,
        ;; so unwind without guessing how far initialization progressed.
        (unless complete
          (magnus--shutdown))))))

;;;###autoload
(defun magnus ()
  "Open the magnus status buffer."
  (interactive)
  (magnus--ensure-initialized)
  (magnus-status))

;;;###autoload
(defun magnus-create-instance (&optional directory name)
  "Create a new Claude Code instance.
DIRECTORY is the working directory.  If nil, prompts for one.
NAME is the instance name.  If nil, auto-generates one."
  (interactive)
  (magnus--ensure-initialized)
  (magnus-process-create directory name))

;;;###autoload
(defun magnus-create-codex (&optional directory name initial-message)
  "Create an opt-in Codex instance in its native TUI.
DIRECTORY and NAME have the same meaning as in `magnus-create-instance'.
When INITIAL-MESSAGE is non-nil, include it in the first turn."
  (interactive
   (list nil nil
         (let ((message (read-string "Initial Codex task (RET to skip): ")))
           (unless (string-empty-p message) message))))
  (magnus--ensure-initialized)
  (magnus-process-create-codex directory name initial-message))

;;;###autoload
(defun magnus-create-headless (prompt &optional directory name)
  "Create a headless Claude Code instance with PROMPT.
DIRECTORY is the working directory.  If nil, prompts for one.
NAME is the instance name.  If nil, auto-generates one.
The agent runs to completion and exits."
  (interactive "sTask prompt: ")
  (magnus--ensure-initialized)
  (magnus-process-create-headless prompt directory name))

;;; Upgrade

(defun magnus--shutdown-subsystem (function)
  "Call teardown FUNCTION without preventing the remaining cleanup.
Return non-nil when FUNCTION completed successfully or is unavailable."
  (if (not (fboundp function))
      t
    (condition-case err
        (progn
          (funcall function)
          t)
      (error
       (message "Magnus: %s failed during shutdown: %s"
                function (error-message-string err))
       nil))))

(defun magnus--shutdown ()
  "Stop all Magnus timers, watchers, and hooks."
  (unwind-protect
      (dolist (function
               '(magnus-review-controller-shutdown
                 magnus-status-shutdown
                 magnus-trace-stop-timer
                 magnus-coord-shutdown
                 magnus-health-shutdown
                 magnus-background-shutdown
                 magnus-attention-shutdown
                 magnus-context-shutdown
                 magnus-persistence-shutdown))
        (magnus--shutdown-subsystem function))
    (remove-hook 'kill-emacs-hook #'magnus--shutdown)
    (setq magnus--initialized nil)))

(defun magnus--upgrade-execute ()
  "Archive all agents, save state, and reinstall Magnus.
The user must restart Emacs afterward so loaded struct and function definitions
cannot be mixed across package versions."
  (let ((active (magnus-instances-active-list)))
    (when active
      (dolist (instance active)
        (magnus-process-archive instance))
      (message "Magnus: archived %d agent%s"
               (length active)
               (if (= (length active) 1) "" "s"))))
  (magnus-persistence-save)
  (magnus--shutdown)
  (package-reinstall 'magnus)
  (setq magnus--restart-required t)
  (message
   "Magnus upgraded. Restart Emacs, then run M-x magnus to resurrect agents."))

;;;###autoload
(defun magnus-upgrade ()
  "Upgrade Magnus gracefully.
Warns active agents, gives them 120 seconds to save their work,
archives them, saves state, and reinstalls the package.  Restart Emacs after
the reinstall so live data structures are not mixed across package versions."
  (interactive)
  (magnus--ensure-initialized)
  (let ((active (magnus-instances-active-list)))
    (if (null active)
        (progn
          (message "No active agents. Upgrading now...")
          (magnus-persistence-save)
          (magnus--shutdown)
          (package-reinstall 'magnus)
          (setq magnus--restart-required t)
          (message "Magnus upgraded. Restart Emacs, then run M-x magnus."))
      ;; Warn agents
      (dolist (instance active)
        (magnus-coord-nudge-agent
         instance
         (concat "Magnus is upgrading in 2 minutes. Save your work and "
                 "update your memory file now — write in first person, as "
                 "yourself. This session will be archived and you can be "
                 "resurrected after the upgrade.")
         "Magnus"))
      (message "Magnus: warned %d agent%s. Upgrading in 120 seconds..."
               (length active)
               (if (= (length active) 1) "" "s"))
      (run-with-timer 120 nil #'magnus--upgrade-execute))))

(provide 'magnus)
;;; magnus.el ends here
