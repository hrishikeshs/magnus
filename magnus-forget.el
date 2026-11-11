;;; magnus-forget.el --- Permanent archived-agent cleanup for Magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Forgetting is deliberately narrower than deleting history.  It removes an
;; archived agent from Magnus and its project identity home, while preserving a
;; private graveyard copy and leaving provider transcripts, reviews, Git, and
;; shared coordination history untouched.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magnus)
(require 'magnus-attention)
(require 'magnus-background)
(require 'magnus-coord)
(require 'magnus-health)
(require 'magnus-onboarding)
(require 'magnus-persistence)
(require 'magnus-review)
(require 'magnus-terminal)

(defvar magnus-process--headless-instance nil)
(defvar magnus-process--instance-id nil)
(defvar magnus-trace--instance nil)

(declare-function magnus--agents-index-remove "magnus" (directory name))
(declare-function magnus-attention-forget-instance
                  "magnus-attention" (instance))
(declare-function magnus-background-cancel "magnus-background" (key))
(declare-function magnus-coord-forget-instance-state
                  "magnus-coord" (instance))
(declare-function magnus-health-forget-instance "magnus-health" (instance))
(declare-function magnus-terminal--discard-buffer "magnus-terminal" (buffer))
(declare-function magnus-terminal-release-process
                  "magnus-terminal" (process))

(defcustom magnus-forgotten-agents-directory
  (expand-file-name ".magnus/forgotten-agents" (or (getenv "HOME") "~"))
  "Private root containing permanently forgotten Magnus agent records."
  :type 'directory
  :group 'magnus)

(defconst magnus-forget--record-schema-version 1
  "Current schema version for a forgotten-agent `record.el'.")

(cl-defstruct (magnus-forget--plan
               (:constructor magnus-forget--make-plan))
  "One fully preflighted archived-agent forgetting plan."
  instance source-home staged-home record-directory project-directory
  home-present-p record-created-p home-moved-p)

(defun magnus-forget-record-directory (instance)
  "Return INSTANCE's permanent graveyard directory without creating it."
  (unless (magnus-instance-p instance)
    (signal 'wrong-type-argument (list 'magnus-instance-p instance)))
  (let ((id (magnus-instance-id instance)))
    (unless (magnus-instances-valid-id-p id)
      (user-error "Invalid Magnus instance ID for graveyard path: %S" id))
    (expand-file-name
     id
     (expand-file-name
      (magnus-review-compute-project-hash
       (magnus-instance-directory instance))
      (directory-file-name
       (expand-file-name magnus-forgotten-agents-directory))))))

(defun magnus-forget--path-exists-p (path)
  "Return non-nil when PATH exists or is a dangling symbolic link."
  (or (file-exists-p path) (file-symlink-p path)))

(defun magnus-forget--require-real-directory (path description)
  "Require existing PATH to be a local, non-symlink directory.
Missing paths are accepted for later creation.  DESCRIPTION names PATH in
diagnostics."
  (when (file-remote-p path)
    (user-error "%s may not be remote: %s" description path))
  (when (file-symlink-p path)
    (user-error "Refusing symlinked %s: %s" description path))
  (when (and (file-exists-p path) (not (file-directory-p path)))
    (user-error "%s is not a directory: %s" description path)))

(defun magnus-forget--source-home (instance)
  "Return INSTANCE's project identity home."
  (directory-file-name
   (file-name-directory (magnus-onboarding-memory-path instance))))

(defun magnus-forget--open-review (instance)
  "Return a loaded open review whose author is INSTANCE, if any."
  (let ((id (magnus-instance-id instance)))
    (cl-find-if
     (lambda (review)
       (and (eq (magnus-review-lifecycle review) 'open)
            (equal (magnus-review-author-instance-id review) id)))
     (magnus-review-list))))

(defun magnus-forget--preflight-instance (instance seen-ids)
  "Return a forgetting plan for exact current INSTANCE.
SEEN-IDS rejects duplicate batch members before any mutation."
  (unless (magnus-instance-p instance)
    (signal 'wrong-type-argument (list 'magnus-instance-p instance)))
  (let* ((id (magnus-instance-id instance))
         (name (magnus-instance-name instance))
         (directory (magnus-instance-directory instance)))
    (unless (and (magnus-instances-valid-id-p id)
                 (magnus-instances-valid-name-p name))
      (user-error "Archived agent has an unsafe identity: %S / %S" id name))
    (when (gethash id seen-ids)
      (user-error "Archived agent %s appears more than once" name))
    (puthash id t seen-ids)
    (unless (eq (magnus-instances-get id) instance)
      (user-error "Archived agent %s is stale; refresh Magnus and retry" name))
    (unless (eq (magnus-instance-status instance) 'purged)
      (user-error "Archive %s before forgetting it" name))
    (when-let ((review (magnus-forget--open-review instance)))
      (user-error "Archive review by %s before forgetting %s"
                  (or (magnus-review-reviewer-name review) "its reviewer")
                  name))
    (unless (and (stringp directory) (file-name-absolute-p directory))
      (user-error "Agent %s has an invalid project directory: %S"
                  name directory))
    (when (file-remote-p directory)
      (user-error "Cannot forget an agent in a remote project: %s" directory))
    (let* ((source-home (magnus-forget--source-home instance))
           (claude-directory
            (directory-file-name (expand-file-name ".claude" directory)))
           (agents-directory
            (directory-file-name
             (expand-file-name ".claude/agents" directory)))
           (expected-home
            (directory-file-name (expand-file-name name agents-directory)))
           (staged-home
            (expand-file-name (format ".magnus-forget-%s" id)
                              agents-directory))
           (record-directory (magnus-forget-record-directory instance))
           (project-directory
            (directory-file-name (file-name-directory record-directory)))
           (graveyard-root
            (directory-file-name
             (expand-file-name magnus-forgotten-agents-directory))))
      (unless (string= source-home expected-home)
        (user-error "Agent home escapes its managed project directory: %s"
                    source-home))
      (when (file-remote-p source-home)
        (user-error "Agent home may not be remote: %s" source-home))
      (dolist (managed-directory (list claude-directory agents-directory))
        (when (file-symlink-p managed-directory)
          (user-error "Refusing symlinked managed agent directory: %s"
                      managed-directory)))
      (when (file-symlink-p source-home)
        (user-error "Refusing symlinked agent home: %s" source-home))
      (when (and (file-exists-p source-home)
                 (not (file-directory-p source-home)))
        (user-error "Agent home is not a directory: %s" source-home))
      (when (and (file-directory-p agents-directory)
                 (not (file-in-directory-p
                       (file-truename agents-directory)
                       (file-name-as-directory (file-truename directory)))))
        (user-error "Managed agent directory escapes its project: %s"
                    agents-directory))
      (when (magnus-forget--path-exists-p staged-home)
        (user-error "Agent-home staging path already exists: %s" staged-home))
      (magnus-forget--require-real-directory
       graveyard-root "forgotten-agent graveyard")
      (magnus-forget--require-real-directory
       project-directory "forgotten-agent project directory")
      (when (magnus-forget--path-exists-p record-directory)
        (user-error "Forgotten-agent record already exists: %s"
                    record-directory))
      (when (and (file-directory-p source-home)
                 (file-in-directory-p graveyard-root source-home))
        (user-error "Forgotten-agent graveyard may not be inside agent home: %s"
                    graveyard-root))
      (magnus-forget--make-plan
       :instance instance
       :source-home source-home
       :staged-home staged-home
       :record-directory record-directory
       :project-directory project-directory
       :home-present-p (file-directory-p source-home)))))

(defun magnus-forget--preflight (instances)
  "Preflight every member of INSTANCES and return transaction plans."
  (unless (proper-list-p instances)
    (signal 'wrong-type-argument (list 'proper-list-p instances)))
  (let ((seen-ids (make-hash-table :test #'equal)) plans)
    (dolist (instance instances (nreverse plans))
      (push (magnus-forget--preflight-instance instance seen-ids) plans))))

(defun magnus-forget--validate-current (plans)
  "Revalidate exact registry and review ownership for PLANS."
  (dolist (plan plans)
    (let* ((instance (magnus-forget--plan-instance plan))
           (id (magnus-instance-id instance)))
      (unless (and (eq (magnus-instances-get id) instance)
                   (eq (magnus-instance-status instance) 'purged))
        (user-error "Archived agent %s changed during forgetting"
                    (magnus-instance-name instance)))
      (when-let ((review (magnus-forget--open-review instance)))
        (user-error "Archive review by %s before forgetting %s"
                    (or (magnus-review-reviewer-name review) "its reviewer")
                    (magnus-instance-name instance))))))

(defun magnus-forget--ensure-private-directory (directory)
  "Create real local DIRECTORY if needed and make it private."
  (magnus-forget--require-real-directory directory "graveyard directory")
  (unless (file-directory-p directory)
    (make-directory directory t))
  (set-file-modes directory #o700)
  directory)

(defun magnus-forget--write-record (plan file forgotten-at)
  "Write PLAN's readable recovery record to FILE at FORGOTTEN-AT."
  (let ((instance (magnus-forget--plan-instance plan)))
    (with-temp-file file
      (insert ";; Magnus forgotten-agent record - do not edit manually\n")
      (pp (list :schema-version magnus-forget--record-schema-version
                :forgotten-at forgotten-at
                :project-hash
                (file-name-nondirectory
                 (magnus-forget--plan-project-directory plan))
                :source-home (magnus-forget--plan-source-home plan)
                :home-present
                (and (magnus-forget--plan-home-present-p plan) t)
                :instance (magnus-instances-serialize instance))
          (current-buffer)))
    (set-file-modes file #o600)))

(defun magnus-forget--prepare-record (plan forgotten-at)
  "Copy PLAN's home and atomically publish its graveyard record."
  (let* ((project (magnus-forget--plan-project-directory plan))
         (root (directory-file-name (file-name-directory project)))
         (record (magnus-forget--plan-record-directory plan))
         temporary)
    (magnus-forget--ensure-private-directory root)
    (magnus-forget--ensure-private-directory project)
    (setq temporary
          (make-temp-file
           (expand-file-name
            (format ".%s-" (magnus-instance-id
                              (magnus-forget--plan-instance plan)))
            project)
           t))
    (set-file-modes temporary #o700)
    (unwind-protect
        (progn
          (if (magnus-forget--plan-home-present-p plan)
              (let ((source (magnus-forget--plan-source-home plan)))
                (when (file-symlink-p source)
                  (user-error "Agent home became a symlink: %s" source))
                (unless (file-directory-p source)
                  (user-error "Agent home disappeared while forgetting: %s"
                              source))
                (let ((copy-directory-create-symlink t))
                  (copy-directory
                   source (expand-file-name "home" temporary) t t t)))
            (when (magnus-forget--path-exists-p
                   (magnus-forget--plan-source-home plan))
              (user-error "Agent home appeared while forgetting: %s"
                          (magnus-forget--plan-source-home plan))))
          (magnus-forget--write-record
           plan (expand-file-name "record.el" temporary) forgotten-at)
          ;; TEMPORARY and RECORD share a parent, so this publication is an
          ;; atomic same-filesystem rename even when the project lives elsewhere.
          (let ((inhibit-quit t))
            (rename-file temporary record nil)
            (setq temporary nil)
            (setf (magnus-forget--plan-record-created-p plan) t)))
      (when (and temporary (file-exists-p temporary))
        (delete-directory temporary t)))))

(defun magnus-forget--move-home (plan)
  "Atomically hide PLAN's copied source home beside its original path."
  (let ((source (magnus-forget--plan-source-home plan))
        (staged (magnus-forget--plan-staged-home plan)))
    (when (magnus-forget--path-exists-p staged)
      (user-error "Agent-home staging path appeared during forgetting: %s"
                  staged))
    (if (magnus-forget--plan-home-present-p plan)
        (progn
          (when (file-symlink-p source)
            (user-error "Agent home became a symlink: %s" source))
          (unless (file-directory-p source)
            (user-error "Agent home disappeared while forgetting: %s" source))
          ;; SOURCE and STAGED are siblings, so this is atomic and cannot cross
          ;; a filesystem boundary.
          (let ((inhibit-quit t))
            (rename-file source staged nil)
            (setf (magnus-forget--plan-home-moved-p plan) t)))
      (when (magnus-forget--path-exists-p source)
        (user-error "Agent home appeared during forgetting: %s" source)))))

(defun magnus-forget--restore-homes (plans)
  "Restore moved homes in PLANS, returning non-nil on complete success."
  (let ((complete t))
    (dolist (plan (reverse plans))
      (when (magnus-forget--plan-home-moved-p plan)
        (let ((source (magnus-forget--plan-source-home plan))
              (staged (magnus-forget--plan-staged-home plan)))
          (condition-case err
              (progn
                (when (magnus-forget--path-exists-p source)
                  (error "Original home path was recreated: %s" source))
                (rename-file staged source nil)
                (setf (magnus-forget--plan-home-moved-p plan) nil))
            (error
             (setq complete nil)
             (message "Magnus: could not restore agent home %s: %s"
                      source (error-message-string err)))))))
    complete))

(defun magnus-forget--requarantine-homes (plans)
  "Re-hide every restored home in PLANS after an incomplete rollback.
Return non-nil only when every original home is back in its same-parent staging
path and every canonical project-home path is absent."
  (let ((complete t))
    (dolist (plan plans)
      (let ((source (magnus-forget--plan-source-home plan))
            (staged (magnus-forget--plan-staged-home plan)))
        (if (magnus-forget--plan-home-present-p plan)
            (cond
             ((magnus-forget--plan-home-moved-p plan)
              (unless (and (file-directory-p staged)
                           (not (magnus-forget--path-exists-p source)))
                (setq complete nil)))
             ((and (file-directory-p source)
                   (not (magnus-forget--path-exists-p staged)))
              (condition-case err
                  (progn
                    (rename-file source staged nil)
                    (setf (magnus-forget--plan-home-moved-p plan) t))
                (error
                 (setq complete nil)
                 (message "Magnus: could not re-hide agent home %s: %s"
                          source (error-message-string err)))))
             (t
              (setq complete nil)))
          (when (magnus-forget--path-exists-p source)
            (setq complete nil)))))
    complete))

(defun magnus-forget--remove-records (plans)
  "Remove graveyard records created for rolled-back PLANS."
  (dolist (plan plans)
    (when (magnus-forget--plan-record-created-p plan)
      (let ((record (magnus-forget--plan-record-directory plan)))
        (condition-case err
            (progn
              (if (file-symlink-p record)
                  (delete-file record)
                (when (file-directory-p record)
                  (delete-directory record t)))
              (setf (magnus-forget--plan-record-created-p plan) nil))
          (error
           (message "Magnus: could not remove rolled-back record %s: %s"
                    record (error-message-string err))))))))

(defun magnus-forget--buffer-instance-id (variable)
  "Return durable instance ID held by buffer-local VARIABLE."
  (when (and (boundp variable) (local-variable-p variable))
    (let ((value (symbol-value variable)))
      (cond
       ((stringp value) value)
       ((magnus-instance-p value) (magnus-instance-id value))))))

(defun magnus-forget--close-instance-buffers (instance)
  "Close terminal and trace buffers owned by exact INSTANCE identity."
  (let ((id (magnus-instance-id instance)))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((terminal-owned
                 (or (equal id
                            (magnus-forget--buffer-instance-id
                             'magnus-process--instance-id))
                     (equal id
                            (magnus-forget--buffer-instance-id
                             'magnus-process--headless-instance))))
                (trace-owned
                 (equal id
                        (magnus-forget--buffer-instance-id
                         'magnus-trace--instance))))
            (cond
             (terminal-owned
              (when-let ((process (get-buffer-process buffer)))
                (magnus-terminal-release-process process))
              (magnus-terminal--discard-buffer buffer))
             (trace-owned
              (kill-buffer buffer)))))))))

(defun magnus-forget--post-commit (plans)
  "Release live resources and stale metadata for committed PLANS."
  (dolist (plan plans)
    (let* ((instance (magnus-forget--plan-instance plan))
           (id (magnus-instance-id instance))
           (name (magnus-instance-name instance))
           (directory (magnus-instance-directory instance)))
      (dolist
          (cleanup
           `(("staged project home"
              . ,(lambda ()
                   (let ((staged (magnus-forget--plan-staged-home plan)))
                     (when (file-symlink-p staged)
                       (error "Staged agent home became a symlink: %s" staged))
                     (when (file-directory-p staged)
                       (delete-directory staged t)))))
             ("background expertise job"
              . ,(lambda ()
                   (magnus-background-cancel (list 'expertise id))))
             ("attention state"
              . ,(lambda () (magnus-attention-forget-instance instance)))
             ("health state"
              . ,(lambda () (magnus-health-forget-instance instance)))
             ("coordination cache"
              . ,(lambda () (magnus-coord-forget-instance-state instance)))
             ("expertise index"
              . ,(lambda () (magnus--agents-index-remove directory name)))
             ("terminal and trace buffers"
              . ,(lambda () (magnus-forget--close-instance-buffers instance)))))
        (condition-case err
            (funcall (cdr cleanup))
          (error
           (message "Magnus: forgot %s but could not clean %s: %s"
                    name (car cleanup) (error-message-string err)))))))
  (dolist (directory
           (delete-dups
            (mapcar (lambda (plan)
                      (magnus-instance-directory
                       (magnus-forget--plan-instance plan)))
                    plans)))
    (condition-case err
        (magnus-coord-reconcile directory)
      (error
       (message "Magnus: could not reconcile coordination in %s: %s"
                directory (error-message-string err))))))

(defun magnus-forget--run-changed-hook ()
  "Notify instance observers without letting one observer abort cleanup."
  (run-hook-wrapped
   'magnus-instances-changed-hook
   (lambda (function)
     (condition-case err
         (funcall function)
       (error
        (message "Magnus: instance observer %s failed after forgetting: %s"
                 function (error-message-string err))))
     nil)))

(defun magnus-forget--registry-without (plans)
  "Return the current instance registry without identities in PLANS."
  (let ((forgotten-ids (make-hash-table :test #'equal)))
    (dolist (plan plans)
      (puthash (magnus-instance-id (magnus-forget--plan-instance plan))
               t forgotten-ids))
    (cl-remove-if
     (lambda (instance)
       (gethash (magnus-instance-id instance) forgotten-ids))
     magnus-instances)))

(defun magnus-forget-archived-instances (instances)
  "Permanently forget exact current archived INSTANCES and return their count.
Every candidate is preflighted before any mutation.  Each project identity
home is copied to `magnus-forgotten-agents-directory' before its original is
hidden with an atomic same-parent rename.  The registry is replaced and saved
once for the whole batch; a failed save restores both registry and homes."
  (let* ((plans (magnus-forget--preflight instances))
         (count (length plans))
         (forgotten-at (float-time))
         old-registry remaining-registry registry-mutated committed)
    (when plans
      (condition-case err
          (progn
            ;; Purged runtimes should already be exiting.  Close any exact-ID
            ;; survivor before taking the graveyard snapshot.
            (dolist (plan plans)
              (magnus-forget--close-instance-buffers
               (magnus-forget--plan-instance plan)))
            (dolist (plan plans)
              (magnus-forget--prepare-record plan forgotten-at))
            (magnus-forget--validate-current plans)
            (dolist (plan plans)
              (magnus-forget--move-home plan))
            (magnus-forget--validate-current plans)
            ;; Buffer teardown above can run sentinels and public hooks.  Fence
            ;; the complete registry only now so unrelated lifecycle changes
            ;; cannot be overwritten by an earlier whole-list snapshot.
            (setq old-registry magnus-instances
                  remaining-registry (magnus-forget--registry-without plans)
                  magnus-instances remaining-registry
                  registry-mutated t)
            (magnus-persistence-save)
            (setq committed t))
        ((error quit)
         (let* ((inhibit-quit t)
                (homes-restored (magnus-forget--restore-homes plans))
                (registry-restored (not registry-mutated)))
           (if homes-restored
               (when registry-mutated
                 (setq magnus-instances old-registry)
                 (condition-case restore-error
                     (progn
                       (magnus-persistence-save)
                       (setq registry-restored t))
                   (error
                    (message "Magnus: could not persist registry rollback: %s"
                             (error-message-string restore-error)))))
             ;; Rollback can fail after restoring an earlier batch member.
             ;; Re-hide those successful restores before publishing the
             ;; terminal forget, or their homes would become dormant agents.
             (unless remaining-registry
               (setq remaining-registry
                     (magnus-forget--registry-without plans)))
             (setq magnus-instances remaining-registry)
             (if (magnus-forget--requarantine-homes plans)
                 (condition-case fallback-error
                     (progn
                       (magnus-persistence-save)
                       (magnus-forget--post-commit plans)
                       (magnus-forget--run-changed-hook))
                   (error
                    (message "Magnus: could not persist recovery state: %s"
                             (error-message-string fallback-error))))
               (message
                "Magnus: incomplete home rollback needs manual recovery; graveyard records retained")))
           (when (and homes-restored registry-restored)
             (magnus-forget--remove-records plans)))
         (signal (car err) (cdr err))))
      (when committed
        ;; Remove the quarantined project copy before observers run, minimizing
        ;; the crash window in which the forgotten home still exists locally.
        (magnus-forget--post-commit plans)
        (magnus-forget--run-changed-hook)))
    count))

(provide 'magnus-forget)
;;; magnus-forget.el ends here
