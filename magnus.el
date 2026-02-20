;;; magnus.el --- Manage multiple Claude Code instances -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S

;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Maintainer: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (vterm "0.0.2") (transient "0.4.0"))
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

;; Magnus is a magit-inspired interface for managing multiple Claude Code
;; instances within Emacs.  It provides a status buffer showing all active
;; instances and transient menus for quick actions.
;;
;; Main entry point: M-x magnus
;;
;; Key bindings in magnus buffer:
;;   RET - Switch to instance
;;   c   - Create new instance
;;   k   - Kill instance
;;   r   - Rename instance
;;   g   - Refresh status
;;   ?   - Show help menu

;;; Code:

(require 'cl-lib)

(declare-function magnus-persistence-load "magnus-persistence")
(declare-function magnus-persistence--setup-autosave "magnus-persistence")
(declare-function magnus-attention-start "magnus-attention")
(declare-function magnus-coord-start-reminders "magnus-coord")
(declare-function magnus-coord-ensure-watchers "magnus-coord")
(declare-function magnus-status "magnus-status")
(declare-function magnus-process-create "magnus-process")
(declare-function magnus-instances-list "magnus-instances")
(declare-function magnus-instance-name "magnus-instances")
(declare-function magnus-context-setup-hooks "magnus-context")
(declare-function project-root "project")
(declare-function magnus-attention-setup-hooks "magnus-attention")
(declare-function magnus-process-create-headless "magnus-process")
(declare-function magnus-health-start "magnus-health")
(declare-function magnus-chat "magnus-chat")
(declare-function magnus-coord-setup-attention-tracking "magnus-coord")
(declare-function magnus-coord-attention-load "magnus-coord")
(declare-function magnus-retro "magnus-coord")

;;; Customization

(defgroup magnus nil
  "Manage multiple Claude Code instances."
  :group 'tools
  :prefix "magnus-")

(defcustom magnus-claude-executable "claude"
  "Path to the Claude Code executable."
  :type 'string
  :group 'magnus)

(defcustom magnus-default-directory nil
  "Default directory for new Claude Code instances.
If nil, uses the current project root or `default-directory'."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Fixed directory"))
  :group 'magnus)

(defcustom magnus-state-file
  (expand-file-name "magnus-state.el" user-emacs-directory)
  "File to persist instance state across Emacs sessions."
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
  (expand-file-name "magnus-attention-data.el" user-emacs-directory)
  "File to persist attention pattern data across Emacs sessions."
  :type 'file
  :group 'magnus)

(defcustom magnus-instance-name-generator #'magnus-generate-instance-name
  "Function to generate names for new instances.
Takes the working directory as argument and returns a string."
  :type 'function
  :group 'magnus)

;;; Internal variables

(defvar magnus--initialized nil
  "Non-nil if magnus has been initialized.")

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
  (let ((existing (mapcar #'magnus-instance-name (magnus-instances-list))))
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
  (let ((candidates (magnus--dormant-candidates directory existing)))
    (when candidates
      (message "Matching task to dormant agents...")
      (when-let ((output (magnus--run-match-sync
                          (magnus--build-match-prompt candidates task))))
        (when-let ((match (magnus--parse-match-output output candidates)))
          (let ((name (car match))
                (reason (cdr match)))
            (when (y-or-n-p (format "Resurrect %s? (%s) " name reason))
              name)))))))

(defun magnus--dormant-candidates (directory existing)
  "Return dormant agent candidates in DIRECTORY.
EXISTING is the list of names to exclude.
Returns an alist of (NAME . MEMORY-SUMMARY) pairs."
  (let* ((agents-dir (expand-file-name ".claude/agents/" directory))
         (candidates nil))
    (when (file-directory-p agents-dir)
      (dolist (entry (directory-files agents-dir nil "\\`[^.]"))
        (let ((memory (expand-file-name (concat entry "/memory.md") agents-dir)))
          (when (and (file-exists-p memory)
                     (not (member entry existing)))
            (push (cons entry (magnus--read-memory-summary memory))
                  candidates)))))
    (nreverse candidates)))

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

(defun magnus--run-match-sync (prompt)
  "Run `claude --print' synchronously with PROMPT.
Returns the trimmed output string, or nil on error."
  (condition-case nil
      (when (and (bound-and-true-p magnus-claude-executable)
                 (executable-find magnus-claude-executable))
        (with-temp-buffer
          (call-process magnus-claude-executable nil t nil "--print" prompt)
          (string-trim (buffer-string))))
    (error nil)))

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

;;; Core functionality

(defun magnus--ensure-initialized ()
  "Ensure magnus is initialized."
  (unless magnus--initialized
    (require 'magnus-instances)
    (require 'magnus-persistence)
    (require 'magnus-process)
    (require 'magnus-status)
    (require 'magnus-transient)
    (require 'magnus-context)
    (require 'magnus-coord)
    (require 'magnus-attention)
    (require 'magnus-health)
    (require 'magnus-chat)
    (magnus-persistence-load)
    (magnus-persistence--setup-autosave)
    (magnus-coord-ensure-watchers)
    (magnus-context-setup-hooks)
    (magnus-attention-setup-hooks)
    (magnus-attention-start)
    (magnus-coord-start-reminders)
    (magnus-health-start)
    (magnus-coord-attention-load)
    (magnus-coord-setup-attention-tracking)
    (setq magnus--initialized t)))

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
(defun magnus-create-headless (prompt &optional directory name)
  "Create a headless Claude Code instance with PROMPT.
DIRECTORY is the working directory.  If nil, prompts for one.
NAME is the instance name.  If nil, auto-generates one.
The agent runs to completion and exits."
  (interactive "sTask prompt: ")
  (magnus--ensure-initialized)
  (magnus-process-create-headless prompt directory name))

(provide 'magnus)
;;; magnus.el ends here
