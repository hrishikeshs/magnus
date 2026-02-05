;;; magnus-instances.el --- Instance registry for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrishikeshsathyian@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides the data structures and functions for managing
;; the registry of Claude Code instances.

;;; Code:

(require 'cl-lib)

;;; Instance structure

(cl-defstruct (magnus-instance (:constructor magnus-instance--create)
                               (:copier nil))
  "A Claude Code instance."
  (id nil :documentation "Unique identifier (UUID string).")
  (name nil :documentation "User-friendly name.")
  (directory nil :documentation "Working directory.")
  (buffer nil :documentation "The vterm buffer running claude.")
  (created-at nil :documentation "Creation timestamp.")
  (status 'stopped :documentation "Status: running, stopped."))

;;; Registry

(defvar magnus-instances nil
  "List of all Claude Code instances.")

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
        (:name (setf (magnus-instance-name instance) value))
        (:buffer (setf (magnus-instance-buffer instance) value))
        (:status (setf (magnus-instance-status instance) value))
        (:directory (setf (magnus-instance-directory instance) value)))))
  (run-hooks 'magnus-instances-changed-hook)
  instance)

(defun magnus-instances-create (directory name)
  "Create a new instance for DIRECTORY with NAME.
Returns the new instance (not yet added to registry)."
  (magnus-instance--create
   :id (magnus-instances--generate-id)
   :name name
   :directory (expand-file-name directory)
   :buffer nil
   :created-at (current-time)
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
        :created-at (magnus-instance-created-at instance)))

(defun magnus-instances-deserialize (plist)
  "Deserialize PLIST to an instance."
  (magnus-instance--create
   :id (plist-get plist :id)
   :name (plist-get plist :name)
   :directory (plist-get plist :directory)
   :buffer nil
   :created-at (plist-get plist :created-at)
   :status 'stopped))

(provide 'magnus-instances)
;;; magnus-instances.el ends here
