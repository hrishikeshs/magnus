;;; magnus.el --- Manage multiple Claude Code instances -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S

;; Author: Hrishikesh S
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (vterm "0.0.1") (transient "0.4.0"))
;; Keywords: tools, processes
;; URL: https://github.com/hrishikeshs/magnus

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the MIT License.

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

(defcustom magnus-buffer-name "*magnus*"
  "Name of the magnus status buffer."
  :type 'string
  :group 'magnus)

(defcustom magnus-instance-name-generator #'magnus-generate-instance-name
  "Function to generate names for new instances.
Takes the working directory as argument and returns a string."
  :type 'function
  :group 'magnus)

;;; Internal variables

(defvar magnus--initialized nil
  "Non-nil if magnus has been initialized.")

;;; Name generation

(defvar magnus--name-adjectives
  '("swift" "bright" "calm" "bold" "keen"
    "wise" "quick" "sharp" "cool" "warm")
  "Adjectives for generating instance names.")

(defvar magnus--name-nouns
  '("fox" "owl" "hawk" "wolf" "bear"
    "deer" "crow" "lynx" "hare" "wren")
  "Nouns for generating instance names.")

(defun magnus-generate-instance-name (_directory)
  "Generate a random instance name.
DIRECTORY is ignored but accepted for API consistency."
  (format "%s-%s"
          (nth (random (length magnus--name-adjectives))
               magnus--name-adjectives)
          (nth (random (length magnus--name-nouns))
               magnus--name-nouns)))

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
    (magnus-persistence-load)
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

(provide 'magnus)
;;; magnus.el ends here
