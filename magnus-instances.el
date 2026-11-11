;;; magnus-instances.el --- Instance registry for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides the data structures and functions for managing
;; the registry of agent instances.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Instance structure

(cl-defstruct (magnus-instance (:constructor magnus-instance--create)
                               (:copier nil))
  "An agent instance managed by Magnus."
  (id nil :documentation "Unique identifier (UUID string).")
  (name nil :documentation "User-friendly name.")
  (directory nil :documentation "Working directory.")
  (buffer nil :documentation "The display buffer running the agent.")
  (created-at nil :documentation "Creation timestamp.")
  (provider 'claude :documentation "Agent provider symbol (defaults to `claude').")
  (status 'stopped :documentation "Status: running, stopped, suspended, purged.")
  (session-id nil :documentation "Provider session ID for this instance.")
  (previous-session-id nil :documentation "Session ID before last directory change.")
  (purged-at nil :documentation "Timestamp when instance was archived (purged).")
  ;; Keep newly persisted slots at the end so existing accessor offsets remain
  ;; stable when users reload Magnus in a long-lived Emacs.
  (kind 'interactive :documentation "Instance kind: interactive or headless."))

;;; Registry

(defvar magnus-instances nil
  "List of all Claude Code instances.")

(defvar magnus-instances-name-reservation-functions nil
  "Functions that return additional reserved names for a project.
Each function receives a physical project directory and returns a list of
names.  This lets adjacent durable registries, such as independent reviews,
reserve identities without making the instance registry depend on them.")

(defun magnus-instances-list ()
  "Return a copy of the instances list."
  (copy-sequence magnus-instances))

(defun magnus-instances-count ()
  "Return the number of instances."
  (length magnus-instances))

(defun magnus-instances-get (id)
  "Get instance by ID."
  (cl-find id magnus-instances :key #'magnus-instance-id :test #'string=))

(defun magnus-instances-get-by-name (name)
  "Get instance by NAME."
  (cl-find name magnus-instances :key #'magnus-instance-name :test #'string=))

(defun magnus-instances-get-by-buffer (buffer)
  "Get instance by BUFFER."
  (cl-find buffer magnus-instances :key #'magnus-instance-buffer))

(defun magnus-instances-active-list ()
  "Return active (non-purged) instances."
  (cl-remove-if (lambda (i) (eq (magnus-instance-status i) 'purged))
                (magnus-instances-list)))

(defun magnus-instances-purged-list ()
  "Return purged instances, sorted by purged-at descending."
  (sort (cl-remove-if-not (lambda (i) (eq (magnus-instance-status i) 'purged))
                          (magnus-instances-list))
        (lambda (a b)
          (> (or (magnus-instance-purged-at a) 0)
             (or (magnus-instance-purged-at b) 0)))))

;;; Instance creation and management

(defun magnus-instances--generate-id ()
  "Generate a unique ID for an instance."
  (format "%s-%s-%s-%s-%s"
          (magnus-instances--random-hex 8)
          (magnus-instances--random-hex 4)
          (magnus-instances--random-hex 4)
          (magnus-instances--random-hex 4)
          (magnus-instances--random-hex 12)))

(defun magnus-instances--random-hex (length)
  "Generate a random hex string of LENGTH characters."
  (let ((chars "0123456789abcdef")
        (result ""))
    (dotimes (_ length result)
      (setq result (concat result (string (aref chars (random 16))))))))

(defun magnus-instances-valid-id-p (id)
  "Return non-nil when ID is a bounded path-safe durable identity."
  (and (stringp id)
       (<= 1 (string-bytes id) 160)
       (string-match-p "\\`[A-Za-z0-9_][A-Za-z0-9_.-]*\\'" id)))

(defun magnus-instances-valid-name-p (name)
  "Return non-nil when display NAME is safe as one agent-home segment.
Spaces and non-ASCII display characters remain supported.  Directory
separators, control characters, and dot-directory aliases are rejected."
  (and (stringp name)
       (<= 1 (string-bytes name) 256)
       (not (member name '("." "..")))
       (not
        (cl-some
         (lambda (character)
           (or (< character 32) (= character 127)
               (= character ?/) (= character ?\\)))
         (string-to-list name)))))

(defun magnus-instances-valid-kind-p (kind)
  "Return non-nil when KIND is a supported instance interaction kind."
  (memq kind '(interactive headless)))

(defun magnus-instance-effective-kind (instance)
  "Return INSTANCE's interaction kind, including hot-reload compatibility.
Instances created before the durable kind slot existed are interactive.  The
slot was appended to the struct, so every older accessor remains stable while
this accessor alone can fall beyond the old vector boundary."
  (unless (magnus-instance-p instance)
    (signal 'wrong-type-argument (list 'magnus-instance-p instance)))
  (condition-case nil
      (or (magnus-instance-kind instance) 'interactive)
    (args-out-of-range 'interactive)))

(defun magnus-instance-interactive-p (instance)
  "Return non-nil when INSTANCE owns an interactive terminal transport."
  (eq (magnus-instance-effective-kind instance) 'interactive))

(defun magnus-instances--validate-name (name)
  "Return display NAME, or signal when it cannot name an agent safely."
  (unless (magnus-instances-valid-name-p name)
    (user-error "Unsafe Magnus agent name: %S" name))
  name)

(defun magnus-instances--canonical-directory (directory)
  "Return DIRECTORY's physical identity without a trailing separator."
  (directory-file-name (file-truename (expand-file-name directory))))

(defun magnus-instances-name-conflict (directory name)
  "Return an instance already using NAME.
DIRECTORY is accepted for symmetry with adjacent project-scoped reservations.
Instance display names stay globally unique because terminal buffers and
legacy name-based compatibility entry points are workspace-global."
  (ignore directory)
  (cl-find-if
   (lambda (instance)
     (string= name (magnus-instance-name instance)))
   magnus-instances))

(defun magnus-instances-reserved-names (directory)
  "Return names reserved by adjacent registries for DIRECTORY."
  (let ((project (magnus-instances--canonical-directory directory)) names)
    (dolist (function magnus-instances-name-reservation-functions)
      (setq names (append (funcall function project) names)))
    (delete-dups names)))

(defun magnus-instances--ensure-name-available (directory name)
  "Reject NAME when another instance already owns it in DIRECTORY."
  (when (magnus-instances-name-conflict directory name)
    (user-error "Magnus instance name %S is already in use" name))
  (when (member name (magnus-instances-reserved-names directory))
    (user-error "Agent name %S is reserved by active project work" name))
  name)

(defun magnus-instances-add (instance)
  "Add INSTANCE to the registry."
  (push instance magnus-instances)
  (run-hooks 'magnus-instances-changed-hook)
  instance)

(defun magnus-instances-remove (instance)
  "Remove INSTANCE from the registry."
  (setq magnus-instances (delq instance magnus-instances))
  (run-hooks 'magnus-instances-changed-hook))

(defun magnus-instances-remove-by-id (id)
  "Remove instance with ID from the registry."
  (when-let ((instance (magnus-instances-get id)))
    (magnus-instances-remove instance)))

(defun magnus-instances-update (instance &rest properties)
  "Update INSTANCE with PROPERTIES.
PROPERTIES is a plist of slot names and values."
  (while properties
    (let ((slot (pop properties))
          (value (pop properties)))
      (cl-case slot
        (:buffer (setf (magnus-instance-buffer instance) value))
        (:status (setf (magnus-instance-status instance) value))
        (:directory (setf (magnus-instance-directory instance) value))
        (:provider (setf (magnus-instance-provider instance) value))
        (:session-id (setf (magnus-instance-session-id instance) value))
        (:previous-session-id (setf (magnus-instance-previous-session-id instance) value))
        (:purged-at (setf (magnus-instance-purged-at instance) value)))))
  (run-hooks 'magnus-instances-changed-hook)
  instance)

(defun magnus-instances-create (directory name &optional provider kind)
  "Create a new instance for DIRECTORY with NAME, PROVIDER, and KIND.
PROVIDER defaults to `claude' and KIND defaults to `interactive'.
Returns the new instance (not yet added to registry)."
  (magnus-instances--validate-name name)
  (magnus-instances--ensure-name-available directory name)
  (setq kind (or kind 'interactive))
  (unless (magnus-instances-valid-kind-p kind)
    (user-error "Unsupported Magnus instance kind: %S" kind))
  (magnus-instance--create
   :id (magnus-instances--generate-id)
   :name name
   :directory (directory-file-name (expand-file-name directory))
   :buffer nil
   :created-at (current-time)
   :provider (or provider 'claude)
   :kind kind
   :status 'stopped))

(defun magnus-instances-clear ()
  "Clear all instances from the registry."
  (setq magnus-instances nil)
  (run-hooks 'magnus-instances-changed-hook))

;;; Hooks

(defvar magnus-instances-changed-hook nil
  "Hook run when the instances list changes.")

;;; Serialization

(defun magnus-instances-serialize (instance)
  "Serialize INSTANCE to a plist for persistence."
  (list :id (magnus-instance-id instance)
        :name (magnus-instance-name instance)
        :directory (magnus-instance-directory instance)
        :created-at (magnus-instance-created-at instance)
        :provider (or (magnus-instance-provider instance) 'claude)
        :kind (magnus-instance-effective-kind instance)
        :session-id (magnus-instance-session-id instance)
        :previous-session-id (magnus-instance-previous-session-id instance)
        :status (magnus-instance-status instance)
        :purged-at (magnus-instance-purged-at instance)))

(defun magnus-instances-deserialize (plist)
  "Deserialize PLIST to an instance."
  (magnus-instance--create
   :id (plist-get plist :id)
   :name (plist-get plist :name)
   :directory (plist-get plist :directory)
   :buffer nil
   :created-at (plist-get plist :created-at)
   ;; State written before provider support has no :provider key and must
   ;; retain Magnus's original Claude Code behavior.
   :provider (or (plist-get plist :provider) 'claude)
   ;; State written before headless tasks has no :kind key.  Such instances
   ;; were interactive agents, so preserve that lifecycle behavior.
   :kind (if (plist-member plist :kind)
             (plist-get plist :kind)
           'interactive)
   :status (or (plist-get plist :status) 'stopped)
   :session-id (plist-get plist :session-id)
   :previous-session-id (plist-get plist :previous-session-id)
   :purged-at (plist-get plist :purged-at)))

(provide 'magnus-instances)
;;; magnus-instances.el ends here
