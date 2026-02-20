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
(require 'magnus-instances)
(require 'magnus-process)
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
(declare-function magnus-chat "magnus-chat")
(declare-function magnus-project-root "magnus")

;;; Main dispatch

;;;###autoload (autoload 'magnus-dispatch "magnus-transient" nil t)
(transient-define-prefix magnus-dispatch ()
  "Magnus command dispatcher."
  ["Instance Actions"
   ("c" "Create instance" magnus-status-create)
   ("k" "Kill instance" magnus-status-kill)
   ("K" "Force kill instance" magnus-status-kill-force)
   ("r" "Rename instance" magnus-status-rename)
   ("R" "Restart instance" magnus-status-restart)
   ("s" "Suspend instance" magnus-status-suspend)
   ("S" "Resume instance" magnus-status-resume)
   ("d" "Change directory" magnus-status-chdir)
   ("m" "Send message" magnus-status-send-message)
   ("t" "Thinking trace" magnus-status-trace)
   ("P" "Purge all instances" magnus-status-purge)]
  ["Context (shared notes)"
   ("x" "Open context buffer" magnus-context)
   ("e" "Export to file" magnus-context-export-for-agent)
   ("w" "Copy to clipboard" magnus-context-copy-for-agent)]
  ["Coordination (agent communication)"
   ("C" "Open coordination file" magnus-status-coordination)
   ("I" "Open agent instructions" magnus-transient-open-instructions)
   ("F" "Session retrospective" magnus-retro)]
  ["Attention (permission requests)"
   ("a" "Next in attention queue" magnus-attention-next)
   ("A" "Show attention queue" magnus-attention-show-queue)
   ("T" "Toggle attention monitoring" magnus-attention-toggle)
   ("H" "Toggle health monitoring" magnus-health-toggle)
   ("z" "Toggle Do Not Disturb" magnus-coord-toggle-dnd)]
  ["Chat"
   ("M" "Open chat center" magnus-chat)]
  ["Navigation"
   ("RET" "Visit instance" magnus-status-visit)
   ("n" "Next instance" magnus-status-next)
   ("p" "Previous instance" magnus-status-previous)]
  ["Buffer"
   ("g" "Refresh" magnus-status-refresh)
   ("q" "Quit" quit-window)])

;;; Create instance menu

(transient-define-prefix magnus-create-dispatch ()
  "Create a new Claude Code instance."
  ["Create Instance"
   ("c" "In current directory" magnus-transient-create-current-dir)
   ("d" "Choose directory" magnus-transient-create-choose-dir)
   ("p" "In project root" magnus-transient-create-project-root)
   ("h" "Headless (fire-and-forget)" magnus-transient-create-headless)])

(defun magnus-transient-create-current-dir ()
  "Create instance in current directory."
  (interactive)
  (magnus-process-create default-directory)
  (magnus-status-refresh))

(defun magnus-transient-create-choose-dir ()
  "Create instance in a chosen directory."
  (interactive)
  (let ((dir (read-directory-name "Directory: " nil nil t)))
    (magnus-process-create dir)
    (magnus-status-refresh)))

(defun magnus-transient-create-project-root ()
  "Create instance in the current project root."
  (interactive)
  (let ((root (magnus-project-root)))
    (if root
        (progn
          (magnus-process-create root)
          (magnus-status-refresh))
      (user-error "Not in a project"))))

(defun magnus-transient-create-headless ()
  "Create a headless (fire-and-forget) Claude Code instance.
Prompts for a task description, uses directory from instance at point
or `default-directory'."
  (interactive)
  (let* ((prompt (read-string "Task prompt: "))
         (dir (if-let ((instance (ignore-errors
                                   (magnus-status--get-instance-at-point))))
                  (magnus-instance-directory instance)
                default-directory)))
    (magnus-process-create-headless prompt dir)
    (magnus-status-refresh)))

;;; Instance actions

(transient-define-prefix magnus-instance-dispatch ()
  "Actions for the instance at point."
  ["Instance"
   :description magnus-transient--instance-description
   ("RET" "Visit" magnus-status-visit)
   ("k" "Kill" magnus-status-kill)
   ("K" "Force kill" magnus-status-kill-force)
   ("r" "Rename" magnus-status-rename)
   ("R" "Restart" magnus-status-restart)
   ("s" "Suspend" magnus-status-suspend)
   ("S" "Resume" magnus-status-resume)
   ("d" "Change directory" magnus-status-chdir)
   ("m" "Send message" magnus-status-send-message)
   ("t" "Thinking trace" magnus-status-trace)])

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

(provide 'magnus-transient)
;;; magnus-transient.el ends here
