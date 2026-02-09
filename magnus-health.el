;;; magnus-health.el --- Health monitoring for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/hrishikeshs/magnus
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module monitors the health of Claude Code instances by hashing
;; buffer content and detecting stale, stuck, or dead agents.  Health
;; status is displayed in the magnus status buffer.

;;; Code:

(require 'magnus-instances)

(declare-function magnus-process-running-p "magnus-process")

;;; Customization

(defcustom magnus-health-check-interval 30
  "Seconds between health checks."
  :type 'number
  :group 'magnus)

(defcustom magnus-health-stale-threshold 120
  "Seconds with no buffer output change before marking agent as stale."
  :type 'number
  :group 'magnus)

(defcustom magnus-health-stuck-threshold 3
  "Number of consecutive stale checks before marking agent as stuck."
  :type 'integer
  :group 'magnus)

(defcustom magnus-health-hash-chars 1000
  "Number of characters from end of buffer to hash for change detection."
  :type 'integer
  :group 'magnus)

;;; Faces

(defface magnus-health-ok
  '((t :inherit success))
  "Face for healthy agents."
  :group 'magnus)

(defface magnus-health-stale
  '((t :inherit warning))
  "Face for stale agents (no output change)."
  :group 'magnus)

(defface magnus-health-stuck
  '((t :inherit error :weight bold))
  "Face for stuck agents (repeated stale checks)."
  :group 'magnus)

(defface magnus-health-dead
  '((t :inherit shadow :strike-through t))
  "Face for dead agents (process exited)."
  :group 'magnus)

;;; State

(defvar magnus-health--state (make-hash-table :test 'equal)
  "Health state per instance-id.
Values are plists: (:hash STRING :last-change-time FLOAT
                    :stuck-count INTEGER :health SYMBOL)")

(defvar magnus-health--timer nil
  "Timer for periodic health checks.")

;;; Core logic

(defun magnus-health-check-all ()
  "Check health of all running instances."
  (dolist (instance (magnus-instances-list))
    (when (eq (magnus-instance-status instance) 'running)
      (magnus-health--check-instance instance)))
  ;; Clean up state for removed instances
  (let ((live-ids (mapcar #'magnus-instance-id (magnus-instances-list))))
    (maphash (lambda (id _state)
               (unless (member id live-ids)
                 (remhash id magnus-health--state)))
             magnus-health--state)))

(defun magnus-health--check-instance (instance)
  "Check and update health for INSTANCE."
  (let* ((id (magnus-instance-id instance))
         (buffer (magnus-instance-buffer instance))
         (prev (gethash id magnus-health--state))
         (prev-hash (plist-get prev :hash))
         (prev-time (or (plist-get prev :last-change-time) (float-time)))
         (prev-stuck (or (plist-get prev :stuck-count) 0))
         (prev-health (plist-get prev :health))
         (now (float-time)))
    (cond
     ;; No buffer or dead buffer
     ((or (null buffer) (not (buffer-live-p buffer)))
      (puthash id (list :hash nil :last-change-time now
                        :stuck-count 0 :health 'dead)
               magnus-health--state)
      (when (eq (magnus-instance-status instance) 'running)
        (magnus-instances-update instance :status 'stopped)))
     ;; Process not live
     ((not (magnus-process-running-p instance))
      (puthash id (list :hash nil :last-change-time now
                        :stuck-count 0 :health 'dead)
               magnus-health--state))
     ;; Process is live — check buffer content
     (t
      (let ((new-hash (magnus-health--compute-hash buffer)))
        (if (and prev-hash (string= new-hash prev-hash))
            ;; Content unchanged
            (let* ((stale-secs (- now prev-time))
                   (new-stuck (1+ prev-stuck))
                   (health (cond
                            ((>= new-stuck magnus-health-stuck-threshold) 'stuck)
                            ((>= stale-secs magnus-health-stale-threshold) 'stale)
                            (t 'ok))))
              (puthash id (list :hash new-hash
                                :last-change-time prev-time
                                :stuck-count new-stuck
                                :health health)
                       magnus-health--state)
              (when (and (not (eq prev-health health))
                         (memq health '(stale stuck)))
                (message "Magnus: agent '%s' is %s"
                         (magnus-instance-name instance) health)))
          ;; Content changed
          (puthash id (list :hash new-hash
                            :last-change-time now
                            :stuck-count 0
                            :health 'ok)
                   magnus-health--state)))))))

(defun magnus-health--compute-hash (buffer)
  "Compute MD5 hash of last N chars of BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((end (point-max))
             (start (max (point-min) (- end magnus-health-hash-chars)))
             (text (buffer-substring-no-properties start end)))
        (secure-hash 'md5 text)))))

;;; Accessors

(defun magnus-health-get (instance)
  "Get the health status symbol for INSTANCE.
Returns one of: ok, stale, stuck, dead, or nil if unknown."
  (let ((state (gethash (magnus-instance-id instance) magnus-health--state)))
    (plist-get state :health)))

(defun magnus-health-indicator (instance)
  "Return a propertized health indicator string for INSTANCE."
  (let ((health (magnus-health-get instance)))
    (pcase health
      ('ok (propertize "+" 'face 'magnus-health-ok))
      ('stale (propertize "~" 'face 'magnus-health-stale))
      ('stuck (propertize "!" 'face 'magnus-health-stuck))
      ('dead (propertize "x" 'face 'magnus-health-dead))
      (_ " "))))

;;; Timer management

(defun magnus-health-start ()
  "Start health monitoring."
  (interactive)
  (magnus-health-stop)
  (setq magnus-health--timer
        (run-with-timer magnus-health-check-interval
                        magnus-health-check-interval
                        #'magnus-health-check-all))
  (message "Magnus health monitoring started"))

(defun magnus-health-stop ()
  "Stop health monitoring."
  (interactive)
  (when magnus-health--timer
    (cancel-timer magnus-health--timer)
    (setq magnus-health--timer nil))
  (message "Magnus health monitoring stopped"))

(defun magnus-health-toggle ()
  "Toggle health monitoring."
  (interactive)
  (if magnus-health--timer
      (magnus-health-stop)
    (magnus-health-start)))

(provide 'magnus-health)
;;; magnus-health.el ends here
