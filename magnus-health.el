;;; magnus-health.el --- Health monitoring for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; Version: 0.1.0

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
                   (is-stale (>= stale-secs magnus-health-stale-threshold))
                   (new-stuck (if is-stale (1+ prev-stuck) prev-stuck))
                   (health (cond
                            ((>= new-stuck magnus-health-stuck-threshold) 'stuck)
                            (is-stale 'stale)
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

;;; Dashboard display

(defface magnus-health-dashboard-green
  '((t :foreground "#00ff41" :background "#0a0a0a" :weight bold))
  "Face for positive dashboard indicators."
  :group 'magnus)

(defface magnus-health-dashboard-red
  '((t :foreground "#ff3333" :background "#0a0a0a" :weight bold))
  "Face for negative dashboard indicators."
  :group 'magnus)

(defface magnus-health-dashboard-yellow
  '((t :foreground "#ffcc00" :background "#0a0a0a"))
  "Face for highlighted dashboard labels."
  :group 'magnus)

(defface magnus-health-dashboard-dim
  '((t :foreground "#555555" :background "#0a0a0a"))
  "Face for dim dashboard separators."
  :group 'magnus)

(defface magnus-health-dashboard-info
  '((t :foreground "#00aaff" :background "#0a0a0a" :slant italic))
  "Face for dashboard informational text."
  :group 'magnus)

(defface magnus-health-dashboard-bg
  '((t :background "#0a0a0a" :extend t))
  "Face for dashboard background."
  :group 'magnus)

(defvar magnus-health-dashboard--tickers
  '(;; Index & blue chips
    ("SPY"  . 487.23) ("QQQ"  . 432.10) ("DIA"  . 389.45)
    ("AAPL" . 198.45) ("MSFT" . 415.67) ("GOOG" . 176.89)
    ("AMZN" . 185.34) ("META" . 523.78) ("NVDA" . 876.12)
    ("BRK.B" . 412.33) ("JPM" . 198.12) ("V"   . 278.90)
    ("COST" . 734.56) ("DIS"  . 112.45) ("NFLX" . 628.45)
    ;; Semiconductors
    ("AMD"  . 178.56) ("INTC" . 31.20) ("AVGO" . 168.90)
    ("ARM"  . 152.34) ("SMCI" . 89.67) ("MU"   . 92.45)
    ("QCOM" . 167.80) ("TSM"  . 142.56)
    ;; Cloud & SaaS
    ("CRM"  . 278.90) ("SNOW" . 167.23) ("NET"  . 89.45)
    ("CRWD" . 312.67) ("PLTR" . 0.04) ("DDOG" . 124.56)
    ;; Consumer & meme adjacent
    ("TSLA" . 248.90) ("COIN" . 178.34) ("SHOP" . 68.90)
    ("UBER" . 72.34) ("ABNB" . 148.90) ("RIVN" . 12.34)
    ("RBLX" . 42.56) ("SNAP" . 11.23) ("ZM"   . 67.89)
    ;; Languages & tools — the real index
    ("EMCS" . 29.10)    ; Emacs version, rock solid
    ("LISP" . 1958.00)  ; Birth year, timeless
    ("VIM"  . 0.01)     ; Penny stock energy
    ("NVIM" . 0.02)     ; Slightly better fork
    ("VSCD" . 14.50)    ; Mass market, thin margins
    ("CRSR" . 8.75)     ; AI wrapper, no moat
    ("RUST" . 445.00)   ; Memory safe, mass adoption
    ("PTHN" . 312.50)   ; Runs everything, slowly
    ("JAVA" . 23.40)    ; Enterprise legacy, declining
    ("COBL" . 0.50)     ; Undead, runs your bank
    ("PERL" . 0.03)     ; In hospice
    ("PHP"  . 4.20)     ; Still powers Facebook somehow
    ("HTMX" . 67.89)    ; Hypermedia darling
    ("VTRM" . 42.00)    ; Answer to everything
    ("MACS" . 1984.01)  ; Think different
    ("GNU"  . 1983.09)  ; Free as in freedom
    ("MAGN" . 888.88)   ; This project, to the moon
    ("NTPD" . 0.001))   ; Notepad, technically still trading
  "Dashboard data points with base values.")

(defvar magnus-health-dashboard--messages
  '("Any sufficiently advanced Emacs config is indistinguishable from an operating system."
    "There are two kinds of people: those who use Emacs, and those who don't know they need it yet."
    "It's not a bug, it's an undocumented feature of the attention system."
    "In a world of VS Code, be an Emacs."
    "The best code is the code you convinced an agent to write for you."
    "Nine agents coding, one human vibing."
    "Premature optimization is the root of all evil. But have you tried M-x butterfly?"
    "Ask not what your editor can do for you, ask what Lisp you can write for your editor."
    "The real treasure was the parentheses we balanced along the way."
    "First they ignore your Emacs config, then they laugh at it, then they ask for a copy."
    "I don't always test my code, but when I do, I do it in production with headless agents."
    "Life is too short for modal editing. Unless you're in evil-mode."
    "The cloud is just someone else's Emacs running on a server somewhere."
    "Behind every successful project is a .magnus-coord.md file nobody read."
    "Talk is cheap. Show me the defun."
    "sudo make me a sandwich. M-x make-me-a-sandwich."
    "There is no place like 127.0.0.1 and no editor like GNU Emacs."
    "To mass-assign or not to mass-assign, that is the let-binding."
    "Weeks of coding can save you hours of reading the documentation."
    "A monad is just a monoid in the category of endofunctors. What's the problem?"
    "rm -rf / -- just kidding. Always (setq delete-by-moving-to-trash t)."
    "The universe is written in Lisp. We just haven't found the REPL yet."
    "If debugging is removing bugs, then programming must be putting them in."
    "99 little bugs in the code, 99 little bugs. Take one down, patch it around, 127 little bugs in the code."
    "I have not failed. I've just found 10,000 ways that don't byte-compile."
    "Keep calm and M-x doctor.")
  "Rotating dashboard status messages.")

(defvar magnus-health-dashboard--timer nil "Dashboard refresh timer.")
(defvar magnus-health-dashboard--scroll 0 "Dashboard scroll position.")
(defvar magnus-health-dashboard--prices nil "Current data points.")
(defvar magnus-health-dashboard--changes nil "Current deltas.")
(defvar magnus-health-dashboard--msg-countdown 0 "Message rotation counter.")
(defvar magnus-health-dashboard--current-msg nil "Current status message.")
(defvar magnus-health-dashboard--buffer " *magnus-dashboard*" "Buffer name.")

(defun magnus-health-dashboard--init ()
  "Initialize dashboard data from base values."
  (setq magnus-health-dashboard--prices
        (mapcar (lambda (pair) (cons (car pair) (cdr pair)))
                magnus-health-dashboard--tickers))
  (setq magnus-health-dashboard--changes
        (mapcar (lambda (pair) (cons (car pair) 0.0))
                magnus-health-dashboard--tickers)))

(defun magnus-health-dashboard--tick ()
  "Advance data simulation."
  (dolist (entry magnus-health-dashboard--prices)
    (let* ((sym (car entry))
           (price (cdr entry))
           (pct-change (/ (- (random 200) 100) 5000.0))
           (new-price (* price (+ 1.0 pct-change))))
      (setcdr entry new-price)
      (setcdr (assoc sym magnus-health-dashboard--changes) (* pct-change 100.0)))))

(defun magnus-health-dashboard--agent-metrics ()
  "Collect agent metrics for display."
  (let (stats)
    (dolist (inst (magnus-instances-list))
      (when (eq (magnus-instance-status inst) 'running)
        (push (list :name (upcase (magnus-instance-name inst))
                    :tokens (+ 10.0 (/ (random 900) 10.0))
                    :tools (+ 5 (random 95))
                    :cost (/ (random 500) 10000.0))
              stats)))
    (nreverse stats)))

(defun magnus-health-dashboard--render-ticker ()
  "Render scrolling data line."
  (let ((parts nil)
        (sep (propertize "  |  " 'face 'magnus-health-dashboard-dim)))
    (dolist (entry magnus-health-dashboard--prices)
      (let* ((sym (car entry))
             (price (cdr entry))
             (change (cdr (assoc sym magnus-health-dashboard--changes)))
             (up (>= change 0))
             (arrow (if up "\u25b2" "\u25bc"))
             (face (if up 'magnus-health-dashboard-green 'magnus-health-dashboard-red)))
        (push (concat (propertize sym 'face 'magnus-health-dashboard-yellow)
                      " "
                      (propertize (format "%.2f" price) 'face face)
                      " "
                      (propertize (format "%s%.1f%%" arrow (abs change)) 'face face))
              parts)))
    (dolist (stat (magnus-health-dashboard--agent-metrics))
      (push (concat (propertize (plist-get stat :name) 'face 'magnus-health-dashboard-yellow)
                    " "
                    (propertize (format "%.1fk tok" (plist-get stat :tokens))
                                'face 'magnus-health-dashboard-green)
                    " "
                    (propertize (format "%d tools" (plist-get stat :tools))
                                'face 'magnus-health-dashboard-dim)
                    " "
                    (propertize (format "$%.2f" (plist-get stat :cost))
                                'face 'magnus-health-dashboard-green))
            parts))
    (setq parts (nreverse parts))
    (let* ((full (mapconcat #'identity parts sep))
           (doubled (concat full sep full))
           (len (length doubled))
           (width (max 80 (- (frame-width) 2)))
           (offset (mod magnus-health-dashboard--scroll (/ len 2))))
      (substring doubled offset (min (+ offset width) len)))))

(defun magnus-health-dashboard--render-msg ()
  "Render status message line."
  (if magnus-health-dashboard--current-msg
      (concat (propertize "  >> " 'face 'magnus-health-dashboard-dim)
              (propertize magnus-health-dashboard--current-msg
                          'face 'magnus-health-dashboard-info))
    (propertize "  >> MAGNUS TERMINAL v1.0 -- Live Market Data & Agent Metrics"
                'face 'magnus-health-dashboard-dim)))

(defun magnus-health-dashboard--render-header ()
  "Render dashboard header bar."
  (let* ((time (propertize (format-time-string " %H:%M:%S ")
                            'face 'magnus-health-dashboard-yellow))
         (label (propertize " MAGNUS TERMINAL " 'face
                            '(:foreground "#0a0a0a" :background "#00ff41" :weight bold)))
         (agents (length (cl-remove-if-not
                          (lambda (i) (eq (magnus-instance-status i) 'running))
                          (magnus-instances-list))))
         (status (propertize (format " %d AGENTS LIVE " agents)
                             'face '(:foreground "#0a0a0a" :background "#ffcc00" :weight bold)))
         (pad (propertize " " 'face 'magnus-health-dashboard-bg)))
    (concat label pad time pad status)))

(defun magnus-health-dashboard--render ()
  "Render the full dashboard display."
  (when-let ((buf (get-buffer magnus-health-dashboard--buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (win (get-buffer-window buf)))
          (erase-buffer)
          (insert (magnus-health-dashboard--render-header))
          (insert (propertize "\n" 'face 'magnus-health-dashboard-bg))
          (insert (propertize " " 'face 'magnus-health-dashboard-bg))
          (insert (magnus-health-dashboard--render-ticker))
          (insert (propertize "\n" 'face 'magnus-health-dashboard-bg))
          (insert (magnus-health-dashboard--render-msg))
          (insert (propertize "\n" 'face 'magnus-health-dashboard-bg))
          (when win
            (set-window-point win (point-min))))))))

(defun magnus-health-dashboard--update ()
  "Timer callback to advance dashboard state."
  (magnus-health-dashboard--tick)
  (setq magnus-health-dashboard--scroll
        (+ magnus-health-dashboard--scroll 3))
  (setq magnus-health-dashboard--msg-countdown
        (1+ magnus-health-dashboard--msg-countdown))
  (when (>= magnus-health-dashboard--msg-countdown 60)
    (setq magnus-health-dashboard--msg-countdown 0)
    (setq magnus-health-dashboard--current-msg
          (nth (random (length magnus-health-dashboard--messages))
               magnus-health-dashboard--messages)))
  (magnus-health-dashboard--render))

;;;###autoload
(defun magnus-bloomberg ()
  "Toggle the real-time dashboard."
  (interactive)
  (if magnus-health-dashboard--timer
      (magnus-health-dashboard--stop)
    (magnus-health-dashboard--start)))

(defun magnus-health-dashboard--start ()
  "Start the dashboard."
  (magnus-health-dashboard--init)
  (setq magnus-health-dashboard--scroll 0)
  (setq magnus-health-dashboard--msg-countdown 55)
  (setq magnus-health-dashboard--current-msg nil)
  (let ((buf (get-buffer-create magnus-health-dashboard--buffer)))
    (with-current-buffer buf
      (special-mode)
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil)
      (setq-local cursor-type nil)
      (buffer-face-set 'magnus-health-dashboard-bg))
    (display-buffer buf
                    '(display-buffer-in-side-window
                      (side . bottom)
                      (window-height . 4)
                      (dedicated . t)
                      (preserve-size . (nil . t))))
    (when-let ((win (get-buffer-window buf)))
      (set-window-dedicated-p win t)
      (set-window-parameter win 'no-other-window t)))
  (setq magnus-health-dashboard--timer
        (run-with-timer 0 0.5 #'magnus-health-dashboard--update))
  (message "MAGNUS TERMINAL ONLINE"))

(defun magnus-health-dashboard--stop ()
  "Stop the dashboard."
  (when magnus-health-dashboard--timer
    (cancel-timer magnus-health-dashboard--timer)
    (setq magnus-health-dashboard--timer nil))
  (when-let ((buf (get-buffer magnus-health-dashboard--buffer)))
    (when-let ((win (get-buffer-window buf)))
      (delete-window win))
    (kill-buffer buf))
  (message "MAGNUS TERMINAL OFFLINE"))

(provide 'magnus-health)
;;; magnus-health.el ends here
