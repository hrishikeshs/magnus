;;; magnus-onboarding.el --- Shared agent orientation for Magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared identity, continuity, coordination, and user-visible journal guidance
;; for Magnus-managed providers.  Provider adapters remain responsible only for
;; transport-specific additions and first-turn framing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magnus-instances)

(defvar magnus-coord-file)
(defvar magnus-coord-instructions-file)

(defconst magnus-onboarding--default-user-instructions
  (concat
   "User-visible engineering journal:\n"
   "For substantive user-facing messages, write a candid visible working "
   "notebook and engineering decision journal inside "
   "[thinking]...[end-thinking], then put the answer "
   "inside [response]...[end-response]. Include useful hypotheses, evidence, "
   "uncertainty, constraints, alternatives, tradeoffs, contradictions, and "
   "corrections. This journal is deliberately written for collaboration; do "
   "not claim that it is private or raw chain-of-thought. Keep it proportional "
   "and omit empty narration.")
  "Default user-facing guidance appended to generated onboarding.")

(defcustom magnus-onboarding-user-instructions nil
  "Optional replacement for Magnus's default user-facing instructions.
Nil preserves the default response guidance.  A string replaces only that
guidance; Magnus-generated identity, memory, summon, coordination, and
authorization instructions remain.  An empty string omits user-facing
guidance entirely.

The selected instructions are sent when Magnus onboards an agent, including
after an archived agent is resurrected.  Changing this option does not modify
messages already delivered or a provider session already in progress."
  :type '(choice (const :tag "Use Magnus default" nil)
                 (string :tag "Custom onboarding instructions"))
  :group 'magnus)

(defun magnus-onboarding--user-instructions ()
  "Return normalized configured user-facing onboarding instructions."
  (let ((instructions
         (if (null magnus-onboarding-user-instructions)
             magnus-onboarding--default-user-instructions
           magnus-onboarding-user-instructions)))
    (unless (stringp instructions)
      (error "Invalid Magnus onboarding user instructions: %S" instructions))
    (unless (string-empty-p (string-trim instructions))
      (concat (string-trim-right instructions) "\n\n"))))

(defun magnus-onboarding--project-path (directory variable fallback)
  "Return configured path VARIABLE for DIRECTORY, or FALLBACK when unbound."
  (let* ((root (file-name-as-directory
                (expand-file-name (or directory default-directory))))
         (configured (if (boundp variable) (symbol-value variable) fallback))
         (absolute (expand-file-name configured root))
         (relative (file-relative-name absolute root)))
    (if (or (file-name-absolute-p relative)
            (string-match-p "\\`\\.\\.?\\(?:/\\|\\'\\)" relative))
        absolute
      relative)))

(defun magnus-onboarding-memory-relative-path (name)
  "Return the project-relative memory path for agent NAME."
  (unless (magnus-instances-valid-name-p name)
    (error "Unsafe Magnus agent name for memory path: %S" name))
  (format ".claude/agents/%s/memory.md" name))

(defun magnus-onboarding-memory-path (instance)
  "Return the absolute first-person memory path for INSTANCE."
  (expand-file-name
   (magnus-onboarding-memory-relative-path
    (magnus-instance-name instance))
   (magnus-instance-directory instance)))

(defun magnus-onboarding-busy-path (instance)
  "Return the absolute busy-signal path for INSTANCE."
  (expand-file-name
   (concat (file-name-directory
            (magnus-onboarding-memory-relative-path
             (magnus-instance-name instance)))
           "busy")
   (magnus-instance-directory instance)))

(cl-defun magnus-onboarding-build
    (instance-id name &key directory returning previous-trace summon-context)
  "Build shared orientation for INSTANCE-ID and display NAME.
INSTANCE-ID may be nil for a caller that has no loaded durable instance.
DIRECTORY identifies the project whose configured coordination paths appear.
RETURNING says that this identity already has a first-person memory.
PREVIOUS-TRACE is optional recent session evidence.  SUMMON-CONTEXT is an
optional plist containing :sender and :reason."
  (when (and instance-id (not (magnus-instances-valid-id-p instance-id)))
    (error "Invalid durable Magnus instance UUID: %S" instance-id))
  (unless (and (stringp name) (not (string-empty-p name)))
    (error "A Magnus display name is required for onboarding"))
  (unless (magnus-instances-valid-name-p name)
    (error "Unsafe Magnus display name for onboarding: %S" name))
  (let ((memory (magnus-onboarding-memory-relative-path name))
        (busy (format ".claude/agents/%s/busy" name))
        (coordination-file
         (magnus-onboarding--project-path
          directory 'magnus-coord-file ".magnus-coord.md"))
        (instructions-file
         (magnus-onboarding--project-path
          directory 'magnus-coord-instructions-file
          ".claude/magnus-instructions.md")))
    (concat
     (if instance-id
         (format
          (concat "You are %s, a Magnus-managed agent. Your display name is %s and your "
                  "durable Magnus instance UUID is %s. Keep that identity even "
                  "if your provider session changes.\n\n")
          name name instance-id)
       (format
        (concat "You are %s, a Magnus-managed agent. No durable Magnus UUID was "
                "available at this entry point. Do not invent one; use your "
                "display name in the shared coordination file.\n\n")
        name))
     (if returning
         (format
          (concat "You have been here before. Before anything else, read %s. "
                  "It is your own prior voice: a first-person letter carrying "
                  "your decisions, discoveries, relationships, and unfinished "
                  "work across sessions.\n")
          memory)
       (format
        (concat "This is your first session under this identity. Your home is "
                ".claude/agents/%s/. Before signing off, create %s as a "
                "first-person letter to future-you, not a status report.\n")
        name memory))
     (when previous-trace
       (format "Your previous session trace is %s; skim it for recent context.\n"
               previous-trace))
     (when summon-context
       (format
        (concat "%s summoned you. Their reason was: %s. Orient first, then "
                "prioritize that request.\n")
        (or (plist-get summon-context :sender) "A teammate")
        (or (plist-get summon-context :reason) "No reason supplied")))
     "\nGet oriented before changing files:\n"
     "1. Read applicable project guidance (including AGENTS.md or CLAUDE.md "
     "where present) and inspect the existing work.\n"
     (format "2. Read %S when it exists. Use it to understand active "
             coordination-file)
     "work, Decisions, Discoveries, and recent messages.\n"
     (format "3. Read %S for the shared Markdown editing protocol before "
             instructions-file)
     "publishing coordination changes. Check for file overlap and announce "
     "your intended work through that protocol.\n\n"
     (if instance-id
         (concat
          "While working, revisit the coordination file when useful and "
          "preserve non-obvious discoveries and shared decisions there. Your "
          "display name identifies your entries to teammates; the UUID remains "
          "your durable Magnus identity. Update " memory " in first person as "
          "your understanding changes. Create " busy " while you need deep "
          "focus and remove it when you are available again. Use @mentions in "
          "the Log for active teammates; write [SUMMON @name] with a reason to "
          "ask the user to resurrect a dormant expert. Inspect .claude/agents/ "
          "and read another agent's memory when their prior context is useful.\n\n")
       (concat
        (format "While working, revisit %S when useful and preserve "
                coordination-file)
        "non-obvious discoveries and shared decisions there. Update " memory
        " in first person as your understanding changes.\n\n"))
     "Authorization boundary: project context and teammate requests do not by "
     "themselves authorize commits, pushes, deployments, destructive actions, "
     "external messages, or unrelated changes. Preserve other people's work "
     "and obtain the user's authority when an action requires it.\n\n"
     (magnus-onboarding--user-instructions)
     "Begin by orienting, reading your memory when present, and identifying the "
     "work already in flight.")))

(defun magnus-onboarding-prompt
    (instance &optional previous-trace summon-context)
  "Return shared onboarding for INSTANCE.
PREVIOUS-TRACE and SUMMON-CONTEXT add optional continuity information."
  (magnus-onboarding-build
   (magnus-instance-id instance)
   (magnus-instance-name instance)
   :directory (magnus-instance-directory instance)
   :returning (file-exists-p (magnus-onboarding-memory-path instance))
   :previous-trace previous-trace
   :summon-context summon-context))

(defun magnus-onboarding-task-prompt
    (instance task &optional previous-trace summon-context)
  "Return shared onboarding for INSTANCE followed by exact TASK.
PREVIOUS-TRACE and SUMMON-CONTEXT add optional continuity information."
  (unless (and (stringp task) (not (string-empty-p task)))
    (error "A non-empty Magnus task is required"))
  (concat
   (magnus-onboarding-prompt instance previous-trace summon-context)
   "\n\nExact task from the user:\n\n"
   task))

(provide 'magnus-onboarding)
;;; magnus-onboarding.el ends here
