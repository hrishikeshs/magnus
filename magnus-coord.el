;;; magnus-coord.el --- Agent coordination for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrishikeshsathyian@gmail.com>
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

;;; Customization

(defcustom magnus-coord-file ".magnus-coord.md"
  "Name of the coordination file in project directories."
  :type 'string
  :group 'magnus)

(defcustom magnus-coord-instructions-file ".claude/magnus-instructions.md"
  "Path to the instructions file for agents (relative to project)."
  :type 'string
  :group 'magnus)

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

## When You Finish

1. Update your row in Active Work (status: done, or remove it)
2. Log a message that you've finished
3. If you made decisions that affect others, add to the Decisions section

## Conflict Resolution

If you need a file another agent is using:
1. Post in the Log asking if they can release it
2. Wait for their response before proceeding
3. If urgent, explain why in your message

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

;;; Agent registration

(defun magnus-coord-register-agent (directory instance)
  "Register INSTANCE as active in DIRECTORY's coordination file."
  (let ((name (magnus-instance-name instance)))
    (magnus-coord-ensure-file directory)
    (magnus-coord-ensure-instructions directory)
    (magnus-coord-add-log directory name "Joined the session")))

(defun magnus-coord-unregister-agent (directory instance)
  "Unregister INSTANCE from DIRECTORY's coordination file."
  (let ((name (magnus-instance-name instance)))
    (magnus-coord-clear-agent directory name)
    (magnus-coord-add-log directory name "Left the session")))

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
