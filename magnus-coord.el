;;; magnus-coord.el --- Agent coordination for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
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

;;; @mention watching

(defvar magnus-coord--watchers nil
  "Alist of (directory . watch-descriptor) for coordination file watchers.")

(defvar magnus-coord--last-content nil
  "Alist of (directory . content-hash) to track changes.")

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
    (condition-case nil
        (magnus-coord--check-new-mentions directory)
      (error nil))))  ; Silently ignore errors

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

(defun magnus-coord--send-mention-notification (instance context-line)
  "Send a mention notification to INSTANCE with CONTEXT-LINE."
  (when-let ((buffer (magnus-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (let ((msg (format "You were @mentioned in .magnus-coord.md: \"%s\" — Read .magnus-coord.md now, respond in the Log section, and then act on what was asked. First, acknowledge by appending a line like: [HH:MM] %s: Acknowledged, reading coordination file."
                         context-line
                         (magnus-instance-name instance))))
        (with-current-buffer buffer
          (vterm-send-string msg)
          (vterm-send-return))))))

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
    (insert "\n## Log\n\n")
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

(defun magnus-coord--instructions-content (directory)
  "Generate instructions content for DIRECTORY."
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

## When You Finish

1. Update your row in Active Work (status: done, or remove it)
2. Log a message that you've finished
3. If you made decisions that affect others, add to the Decisions section

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
            ;; Find end of comments
            (while (looking-at "^<!--")
              (forward-line 1)
              (while (not (looking-at "-->"))
                (forward-line 1))
              (forward-line 1))
            ;; Skip blank lines after comments
            (while (looking-at "^$")
              (forward-line 1))
            (insert (format "[%s] %s: %s\n\n" time agent message)))
        ;; No Log section, append at end
        (goto-char (point-max))
        (insert (format "\n[%s] %s: %s\n" time agent message)))
      (write-region (point-min) (point-max) file nil 'quiet))))

(defun magnus-coord-update-active (directory agent area status files)
  "Update AGENT's entry in the Active Work table.
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
      (write-region (point-min) (point-max) file nil 'quiet))))

(defun magnus-coord-clear-agent (directory agent)
  "Remove AGENT from the Active Work table in DIRECTORY."
  (magnus-coord-update-active directory agent "" "done" ""))

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
        (write-region (point-min) (point-max) file nil 'quiet)))))

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
    (magnus-coord-add-log directory name "Joined the session")
    ;; Start watching for @mentions if not already
    (unless (assoc directory magnus-coord--watchers)
      (magnus-coord-start-watching directory))))

(defun magnus-coord-unregister-agent (directory instance)
  "Unregister INSTANCE from DIRECTORY's coordination file."
  (let ((name (magnus-instance-name instance)))
    (magnus-coord-clear-agent directory name)
    (magnus-coord-add-log directory name "Left the session")
    ;; Stop watching if no agents remain in this directory
    (let ((remaining (cl-count-if
                      (lambda (inst)
                        (and (not (eq inst instance))
                             (string= (magnus-instance-directory inst) directory)))
                      (magnus-instances-list))))
      (when (zerop remaining)
        (magnus-coord-stop-watching directory)))))

;;; Display

(defun magnus-coord-format-active (parsed)
  "Format the :active entries from PARSED for display."
  (let ((active (plist-get parsed :active)))
    (if active
        (mapconcat
         (lambda (entry)
           (format "  %s: %s [%s]"
                   (propertize (plist-get entry :agent) 'face 'magnus-instance-name)
                   (plist-get entry :area)
                   (propertize (plist-get entry :status)
                              'face (if (string= (plist-get entry :status) "in-progress")
                                       'magnus-status-running
                                     'magnus-instance-directory))))
         active
         "\n")
      (propertize "  No active work" 'face 'magnus-empty-hint))))

(defun magnus-coord-format-log (parsed &optional limit)
  "Format the :log entries from PARSED for display.
Show at most LIMIT entries (default 5)."
  (let* ((log (plist-get parsed :log))
         (entries (if limit (seq-take log limit) log)))
    (if entries
        (mapconcat
         (lambda (entry)
           (format "  [%s] %s: %s"
                   (propertize (plist-get entry :time) 'face 'magnus-instance-directory)
                   (propertize (plist-get entry :agent) 'face 'magnus-instance-name)
                   (plist-get entry :message)))
         entries
         "\n")
      (propertize "  No messages yet" 'face 'magnus-empty-hint))))

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
        (when-let ((project (project-current)))
          (if (fboundp 'project-root)
              (project-root project)
            (car (project-roots project)))))
      (read-directory-name "Project directory: ")))

(provide 'magnus-coord)
;;; magnus-coord.el ends here
