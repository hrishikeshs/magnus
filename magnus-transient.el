;;; magnus-transient.el --- Transient menus for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides transient popup menus for magnus, inspired by
;; magit's interface.

;;; Code:

(require 'transient)
(require 'subr-x)
(require 'magnus-instances)
(require 'magnus-process)
(require 'magnus-review)
(require 'magnus-review-controller)
(require 'magnus-status)

(declare-function magnus-context "magnus-context")
(declare-function magnus-context-export-for-agent "magnus-context")
(declare-function magnus-context-copy-for-agent "magnus-context")
(declare-function magnus-coord-open "magnus-coord")
(declare-function magnus-coord-open-instructions "magnus-coord")
(declare-function magnus-attention-next "magnus-attention")
(declare-function magnus-attention-show-queue "magnus-attention")
(declare-function magnus-coord-toggle-dnd "magnus-coord")
(declare-function magnus-retro "magnus-coord")
(declare-function magnus-health-toggle "magnus-health")
(declare-function magnus-process-create-headless "magnus-process")
(declare-function magnus-project-root "magnus")
(declare-function magnus-status--get-review-at-point "magnus-status")
(declare-function magnus-status--selection-error "magnus-status"
                  (target action))
(declare-function magnus-review-ui-open "magnus-review-ui")

;; Source checkouts do not have package-generated autoloads.  Keep Doctor lazy
;; while ensuring Transient can validate the suffix before its first use.
(autoload 'magnus-doctor "magnus-doctor"
  "Open a read-only report of Magnus installation and runtime health." t)

(defvar magnus--creation-task)

(defvar magnus-transient--review-request-context nil
  "Cached task-scoped context for the visible review request transient.")

(defun magnus-transient--main-review-description ()
  "Describe the review request action for the current status row."
  (let ((review (magnus-status--get-review-at-point))
        (instance (magnus-status--get-instance-at-point)))
    (cond
     (review
      (format "Actions for %s's review…"
              (or (magnus-review-reviewer-name review)
                  "selected reviewer")))
     (instance
      (format "Review %s's committed work…"
              (magnus-instance-name instance)))
     (t "Review… (select an agent or review)"))))

(defun magnus-transient--current-review-request-context ()
  "Return the cached review request context, if any."
  magnus-transient--review-request-context)

(defun magnus-transient--review-request-new-p ()
  "Return non-nil when the request transient would create a reviewer."
  (eq (plist-get (magnus-transient--current-review-request-context) :action)
      'new))

(defun magnus-transient--review-request-busy-p ()
  "Return non-nil when the selected review already has pending work."
  (memq (plist-get (magnus-transient--current-review-request-context) :action)
        '(asking-scope running)))

(defun magnus-transient--review-request-heading ()
  "Describe the exact task-scoped operation in the request transient."
  (let* ((context (magnus-transient--current-review-request-context))
         (author (plist-get context :author))
         (review (plist-get context :review))
         (action (plist-get context :action))
         (author-name (and author (magnus-instance-name author)))
         (reviewer-name (and review (magnus-review-reviewer-name review))))
    (pcase action
      ('new (format "Independent review of %s" author-name))
      ('rereview (format "Re-review %s with %s (same reviewer session)"
                         author-name reviewer-name))
      ('failed (format "Retry %s's failed review of %s"
                       reviewer-name author-name))
      ('interrupted (format "Retry %s's interrupted review of %s"
                            reviewer-name author-name))
      ('asking-scope (format "Asking %s which commits belong to its work"
                             author-name))
      ('running (format "%s is reviewing %s now"
                        reviewer-name author-name))
      (_ "Independent review"))))

(defun magnus-transient--review-options-heading ()
  "Describe defaults for a newly created durable reviewer."
  (format "Optional settings (defaults: %s provider, default model, %s effort)"
          (or magnus-review-default-provider 'opposite)
          magnus-review-default-effort))

(defun magnus-transient--review-request-action-description ()
  "Describe what RET will do in the request transient."
  (let ((context (magnus-transient--current-review-request-context)))
    (pcase (plist-get context :action)
      ('new "Start independent review")
      ('rereview "Request the next review round")
      ('failed "Retry failed review")
      ('interrupted "Retry interrupted review")
      ('asking-scope
       "Waiting for the author to identify its committed range")
      ('running "Review is in progress")
      (_ "Start review"))))

;;;###autoload
(defun magnus-review-request-dispatch ()
  "Open the contextual review command for the status entity at point.
On an agent, open its review request transient.  On a durable review, open its
review actions."
  (interactive)
  (if-let ((review (magnus-status--get-review-at-point)))
      (magnus-review-actions review)
    (let ((author
           (or (magnus-status--get-instance-at-point)
               (magnus-status--selection-error
                "an agent or review row" "request or manage a review"))))
      (setq magnus-transient--review-request-context
            (magnus-review-request-context author))
      (transient-setup #'magnus-review-request-menu))))

;;; Main dispatch

;;;###autoload (autoload 'magnus-dispatch "magnus-transient" nil t)
(transient-define-prefix magnus-dispatch ()
  "Magnus command dispatcher."
  [1 "Instance Actions"
   (1 "c" "Create Claude agent" magnus-status-create)
   (1 "X" "Create Codex agent" magnus-transient-create-codex)
   (1 "h" "Create headless Claude task" magnus-transient-create-headless)
   (1 "k" "Archive instance" magnus-status-archive)
   (1 "R" "Resurrect purged" magnus-status-resurrect-purged)
   (1 "r" "Rename archived instance" magnus-status-rename)
   (1 "s" "Suspend instance" magnus-status-suspend)
   (1 "S" "Resume instance" magnus-status-resume)
   (1 "d" "Change directory" magnus-status-chdir)
   (1 "m" "Send message" magnus-status-send-message)
   (1 "t" "Thinking trace" magnus-status-trace)
   (1 "P" "Archive all instances" magnus-status-archive-all)]
  [1 "Independent Reviews"
   (1 "v" magnus-review-request-dispatch
    :description magnus-transient--main-review-description)
   (1 "o" "Open completed review at point" magnus-transient-review-open)
   (1 "V" "Actions for review at point" magnus-review-actions)]
  [1 "Context (shared notes)"
   (1 "x" "Open context buffer" magnus-context)
   (1 "e" "Export to file" magnus-context-export-for-agent)
   (1 "w" "Copy to clipboard" magnus-context-copy-for-agent)]
  [1 "Coordination (agent communication)"
   (1 "C" "Open coordination file" magnus-status-coordination)
   (1 "I" "Open agent instructions" magnus-transient-open-instructions)
   (1 "F" "Session retrospective" magnus-retro)]
  [1 "Attention (permission requests)"
   (1 "a" "Next in attention queue" magnus-attention-next)
   (1 "A" "Show attention queue" magnus-attention-show-queue)
   (1 "T" "Toggle attention monitoring" magnus-attention-toggle)
   (1 "H" "Toggle health monitoring" magnus-health-toggle)
   (1 "z" "Toggle Do Not Disturb" magnus-coord-toggle-dnd)]
  [1 "Navigation"
   (1 "RET" "Visit item" magnus-status-visit)
   (1 "n" "Next item" magnus-status-next)
   (1 "p" "Previous item" magnus-status-previous)]
  [1 "Buffer"
   (1 "g" "Refresh" magnus-status-refresh)
   (1 "D" "Diagnose installation" magnus-doctor)
   (1 "q" "Quit" quit-window)])

;;; Shipped create-menu compatibility

(transient-define-prefix magnus-create-dispatch ()
  "Create a new Claude Code instance."
  ["Create Instance"
   ("c" "In current directory" magnus-transient-create-current-dir)
   ("d" "Choose directory" magnus-transient-create-choose-dir)
   ("p" "In project root" magnus-transient-create-project-root)
   ("h" "Headless (fire-and-forget)" magnus-transient-create-headless)])

;;; Durable reviews

(transient-define-prefix magnus-review-request-menu ()
  "Request an independent review for the instance at point."
  [1 :description magnus-transient--review-request-heading
   (1 "RET" magnus-transient-request-review
    :description magnus-transient--review-request-action-description
    :inapt-if magnus-transient--review-request-busy-p)]
  [1 :description magnus-transient--review-options-heading
   :if magnus-transient--review-request-new-p
   (1 "p" "Provider" "--provider="
    :choices ("opposite" "claude" "codex"))
   (1 "m" "Model" "--model=")
   (1 "e" "Effort" "--effort="
    :choices ("low" "medium" "high" "xhigh" "max"))])

(defun magnus-transient-request-review ()
  "Request a review using the current review transient arguments."
  (interactive)
  (let* ((context (magnus-transient--current-review-request-context))
         (author (or (plist-get context :author)
                     (magnus-status--get-instance-at-point)
                     (user-error
                      "Put point on the agent whose work should be reviewed")))
         (arguments (transient-args 'magnus-review-request-menu))
         (provider-name (transient-arg-value "--provider=" arguments))
         (model (transient-arg-value "--model=" arguments))
         (effort-name (transient-arg-value "--effort=" arguments))
         ;; Preserve an explicit opposite choice so it overrides a customized
         ;; `magnus-review-default-provider' instead of collapsing to nil.
         (provider (and provider-name (intern provider-name)))
         (effort (and effort-name (intern effort-name))))
    (magnus-review-request author
                           :provider provider
                           :model (and model (not (string-empty-p model)) model)
                           :effort effort
                           :context context)
    (setq magnus-transient--review-request-context nil)
    (magnus-status-refresh)))

(defvar magnus-transient--review-action-context nil
  "Immutable identity snapshot for the visible review action transient.")

(defun magnus-transient--make-review-action-context (review &optional round)
  "Return a pinned action context for REVIEW and optional ROUND."
  (list :review-id (magnus-review-id review)
        :round-number (and round (magnus-review-scope-number round))
        :attempt (magnus-review-controller--attempt-token review)
        :reviewer-name (magnus-review-reviewer-name review)
        :author-name (magnus-review-author-name review)))

(defun magnus-transient--review-description ()
  "Return a heading for the current review action transient."
  (if magnus-transient--review-action-context
      (format "Review: %s → %s"
              (plist-get magnus-transient--review-action-context
                         :reviewer-name)
              (plist-get magnus-transient--review-action-context
                         :author-name))
    "No review selected"))

(defun magnus-transient--review-rereview-description ()
  "Describe the selected review's next-round action."
  "Ask author for the next committed round")

(defun magnus-transient--review-action-current-review ()
  "Return the review named by the current action popup, or nil."
  (when-let ((context magnus-transient--review-action-context))
    (magnus-review-get (plist-get context :review-id))))

(defun magnus-transient--review-action-state ()
  "Return the selected review's current execution state, or nil."
  (when-let ((review (magnus-transient--review-action-current-review)))
    (magnus-review-execution review)))

(defun magnus-transient--review-open-inapt-p ()
  "Return non-nil when the selected review has no completed report."
  (let ((review (magnus-transient--review-action-current-review)))
    (or (null review) (null (magnus-review-rounds review)))))

(defun magnus-transient--review-rereview-inapt-p ()
  "Return non-nil unless the selected lineage can start another round."
  (or (not (eq (magnus-transient--review-action-state) 'complete))
      (when-let ((review (magnus-transient--review-action-current-review)))
        (eq (magnus-review-lifecycle review) 'archived))))

(defun magnus-transient--review-retry-inapt-p ()
  "Return non-nil unless the selected review has disposable failed work."
  (not (memq (magnus-transient--review-action-state)
             '(failed interrupted))))

(defun magnus-transient--review-fresh-session-inapt-p ()
  "Return non-nil unless failed reviewer work can use a fresh session."
  (let ((review (magnus-transient--review-action-current-review)))
    (or (not (memq (magnus-transient--review-action-state)
                   '(failed interrupted)))
        (null review)
        (null (magnus-review-controller-candidate-round review)))))

(defun magnus-transient--review-interrupt-inapt-p ()
  "Return non-nil unless the selected review has disposable active work."
  (not (memq (magnus-transient--review-action-state)
             '(asking-scope running))))

(defun magnus-transient--review-archive-inapt-p ()
  "Return non-nil when the selected review is already archived."
  (let ((review (magnus-transient--review-action-current-review)))
    (or (null review) (eq (magnus-review-lifecycle review) 'archived))))

(transient-define-prefix magnus-review-actions-menu ()
  "Actions for one review lineage."
  [1 "Review"
   :description magnus-transient--review-description
   (1 "RET" "Open completed report" magnus-transient-review-open
    :inapt-if magnus-transient--review-open-inapt-p)
   (1 "r" magnus-transient-review-rereview
    :description magnus-transient--review-rereview-description
    :inapt-if magnus-transient--review-rereview-inapt-p)
   (1 "t" "Retry failed work" magnus-transient-review-retry
    :inapt-if magnus-transient--review-retry-inapt-p)
   (1 "f" "Retry with fresh reviewer session"
    magnus-transient-review-fresh-session
    :inapt-if magnus-transient--review-fresh-session-inapt-p)
   (1 "i" "Interrupt active work" magnus-transient-review-interrupt
    :inapt-if magnus-transient--review-interrupt-inapt-p)
   (1 "k" "Archive" magnus-transient-review-archive
    :inapt-if magnus-transient--review-archive-inapt-p)])

(defun magnus-review-actions (&optional review round)
  "Open actions for REVIEW and optional ROUND.
When called from the status buffer, use the review at point."
  (interactive)
  (setq review
        (or review (magnus-status--get-review-at-point)
            (magnus-status--selection-error
             "a review row" "open its actions")))
  (setq magnus-transient--review-action-context
        (magnus-transient--make-review-action-context review round))
  (transient-setup #'magnus-review-actions-menu))

(defun magnus-transient--review-actions-active-p ()
  "Return non-nil while invoking a suffix of the review action transient."
  (eq transient-current-command 'magnus-review-actions-menu))

(defun magnus-transient--review-action-review ()
  "Resolve the action transient's pinned review."
  (let* ((context magnus-transient--review-action-context)
         (id (plist-get context :review-id))
         (review (and id (magnus-review-get id))))
    (unless review
      (user-error "The selected review is no longer loaded"))
    review))

(defun magnus-transient--review-action-round (review)
  "Resolve the action transient's pinned round within fresh REVIEW."
  (when-let ((number
              (plist-get magnus-transient--review-action-context
                         :round-number)))
    (let ((round (nth (1- number) (magnus-review-rounds review))))
      (unless (and round (= (magnus-review-scope-number round) number))
        (user-error "The selected review round is no longer available"))
      round)))

(defun magnus-transient--selected-review ()
  "Return the review selected by transient context or status point."
  (if (derived-mode-p 'magnus-status-mode)
      (or (magnus-status--get-review-at-point)
          (magnus-status--selection-error
           "a review row" "open or modify it"))
    (magnus-transient--review-action-review)))

(defun magnus-transient--review-for-mutation ()
  "Return the currently selected review for a popup mutation."
  (if (magnus-transient--review-actions-active-p)
      (magnus-transient--review-action-review)
    (magnus-transient--selected-review)))

(defun magnus-transient-review-open ()
  "Open the selected review in its Magit-style reader."
  (interactive)
  (if (magnus-transient--review-actions-active-p)
      (let ((review (magnus-transient--review-action-review)))
        (magnus-review-ui-open
         review (magnus-transient--review-action-round review)))
    (if (derived-mode-p 'magnus-status-mode)
        (progn
          ;; The main dispatch also appears on instance rows; keep its labeled
          ;; review action honest instead of accidentally visiting that instance.
          (magnus-transient--selected-review)
          (magnus-status-visit))
      (magnus-review-ui-open (magnus-transient--selected-review) nil))))

(defun magnus-transient-review-rereview ()
  "Ask the author to identify the selected review's next committed round."
  (interactive)
  (magnus-review-rereview (magnus-transient--review-for-mutation))
  (magnus-status-refresh))

(defun magnus-transient-review-retry ()
  "Retry the selected review's latest failed or interrupted round."
  (interactive)
  (magnus-review-retry (magnus-transient--review-for-mutation))
  (magnus-status-refresh))

(defun magnus-transient-review-fresh-session ()
  "Retry prepared failed work in a fresh reviewer provider session."
  (interactive)
  (magnus-review-restart-session (magnus-transient--review-for-mutation))
  (magnus-status-refresh))

(defun magnus-transient-review-interrupt ()
  "Interrupt the selected review's ephemeral query or provider run."
  (interactive)
  (let ((review (magnus-transient--review-for-mutation))
        (attempt
         (and (magnus-transient--review-actions-active-p)
              (plist-get magnus-transient--review-action-context :attempt))))
    (when (yes-or-no-p
           (format "Interrupt the review by %s? "
                   (magnus-review-reviewer-name review)))
      (magnus-review-interrupt review attempt)
      (magnus-status-refresh))))

(defun magnus-transient-review-archive ()
  "Archive the selected review without deleting completed reports."
  (interactive)
  (let ((review (magnus-transient--review-for-mutation)))
    (if (eq (magnus-review-lifecycle review) 'archived)
        (user-error "Review is already archived")
      (when (yes-or-no-p
             (format "Archive review of %s? "
                     (magnus-review-author-name review)))
        (magnus-review-controller-archive review)
        (magnus-status-refresh)
        (message "Archived review by %s"
                 (magnus-review-reviewer-name review))))))

;;; Create instance commands

(defun magnus-transient-create-current-dir ()
  "Create a Claude agent in `default-directory'."
  (interactive)
  (magnus-process-create default-directory)
  (magnus-status-refresh))

(defun magnus-transient-create-choose-dir ()
  "Create a Claude agent in a chosen directory."
  (interactive)
  (let ((directory (read-directory-name "Directory: " nil nil t)))
    (magnus-process-create directory)
    (magnus-status-refresh)))

(defun magnus-transient-create-project-root ()
  "Create a Claude agent in the current project root."
  (interactive)
  (if-let ((root (magnus-project-root)))
      (progn
        (magnus-process-create root)
        (magnus-status-refresh))
    (user-error "Not in a project")))

(defun magnus-transient--creation-directory ()
  "Return the most relevant directory for a status-buffer creation action."
  (or (when-let ((instance (ignore-errors
                             (magnus-status--get-instance-at-point))))
        (magnus-instance-directory instance))
      (when-let ((instance (car (magnus-instances-list))))
        (magnus-instance-directory instance))
      default-directory))

(defun magnus-transient-create-codex ()
  "Create a Codex agent in the selected status row's project."
  (interactive)
  (let* ((task (read-string "Initial Codex task (RET to skip): "))
         (initial-message (unless (string-empty-p task) task))
         (magnus--creation-task initial-message))
    (magnus-process-create
     (magnus-transient--creation-directory) nil 'codex initial-message)
    (magnus-status-refresh)))

(defun magnus-transient-create-headless ()
  "Create a headless (fire-and-forget) Claude Code instance.
Prompts for a task description, uses directory from instance at point
or the best status-buffer project directory."
  (interactive)
  (let* ((prompt (read-string "Task prompt: "))
         (dir (magnus-transient--creation-directory)))
    (magnus-process-create-headless prompt dir)
    (magnus-status-refresh)))

;;; Instance actions

(transient-define-prefix magnus-instance-dispatch ()
  "Actions for the instance at point."
  [1 "Instance"
   :description magnus-transient--instance-description
   (1 "RET" "Visit" magnus-status-visit)
   (1 "k" "Archive" magnus-status-archive)
   (1 "R" "Resurrect" magnus-status-resurrect-purged)
   (1 "r" "Rename archived" magnus-status-rename)
   (1 "s" "Suspend" magnus-status-suspend)
   (1 "S" "Resume" magnus-status-resume)
   (1 "d" "Change directory" magnus-status-chdir)
   (1 "m" "Send message" magnus-status-send-message)
   (1 "t" "Thinking trace" magnus-status-trace)])

(defun magnus-transient--instance-description ()
  "Return description for current instance."
  (if-let ((instance (magnus-status--get-instance-at-point)))
      (format "Instance: %s" (magnus-instance-name instance))
    "No instance at point"))

(defun magnus-transient-open-instructions ()
  "Open the agent instructions file."
  (interactive)
  (if-let ((instance (car (magnus-instances-list))))
      (magnus-coord-open-instructions (magnus-instance-directory instance))
    (user-error "No instances to get project directory from")))

;; The review reader calls this dispatcher from its `?' binding.  Controller
;; setup installs the same value during Magnus startup; assigning it here also
;; makes direct loading of the UI and transient modules behave consistently.
(provide 'magnus-transient)
;;; magnus-transient.el ends here
