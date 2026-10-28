;;; magnus-terminal-tests.el --- Shared terminal substrate tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)

;; CI does not install vterm.  The unit tests replace its entry points.
(unless (featurep 'vterm)
  (provide 'vterm))

(require 'magnus-terminal)

(ert-deftest magnus-terminal-init-failure-does-not-leak-buffer ()
  (let ((name " *magnus-failed-vterm*"))
    (when-let ((old (get-buffer name)))
      (kill-buffer old))
    (cl-letf (((symbol-function 'vterm-mode)
               (lambda () (error "vterm initialization failed"))))
      (should-error (magnus-terminal-create-buffer name)))
    (should-not (get-buffer name))))

(ert-deftest magnus-terminal-setup-failure-discards-partial-process ()
  (let ((name " *magnus-partial-vterm*")
        process)
    (when-let ((old (get-buffer name)))
      (kill-buffer old))
    (cl-letf (((symbol-function 'vterm-mode)
               (lambda ()
                 (setq process
                       (make-pipe-process
                        :name (generate-new-buffer-name
                               "magnus-partial-vterm")
                        :buffer (current-buffer)))))
              ((symbol-function 'magnus-terminal-setup-keys)
               (lambda () (error "terminal key setup failed"))))
      (should-error (magnus-terminal-create-buffer name)))
    (should process)
    (should-not (process-live-p process))
    (should-not (process-query-on-exit-flag process))
    (should-not (get-buffer name))))

(ert-deftest magnus-terminal-setup-keys-binds-quit-to-escape ()
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (magnus-terminal-setup-keys)
    (should (eq (local-key-binding (kbd "C-g"))
                #'magnus-terminal-send-escape))))

(ert-deftest magnus-terminal-send-escape-forwards-to-vterm ()
  (let (sent)
    (cl-letf (((symbol-function 'vterm-send-key)
               (lambda (&rest arguments) (setq sent arguments))))
      (magnus-terminal-send-escape))
    (should (equal sent '("<escape>")))))

(ert-deftest magnus-terminal-applies-isolated-process-environment ()
  (let ((process-environment '("KEEP=yes" "MAGNUS_TEST_ID=old"))
        observed
        buffer)
    (cl-letf (((symbol-function 'vterm-mode)
               (lambda () (setq observed process-environment)))
              ((symbol-function 'magnus-terminal-setup-keys) #'ignore))
      (setq buffer
            (magnus-terminal-create-buffer
             " *magnus-environment*"
             '("MAGNUS_TEST_ID=new"
               "MAGNUS_TEST_NAME=fixture"))))
    (unwind-protect
        (progn
          (should (member "KEEP=yes" observed))
          (should (member "MAGNUS_TEST_ID=new" observed))
          (should (member "MAGNUS_TEST_NAME=fixture" observed))
          (should-not (member "MAGNUS_TEST_ID=old" observed))
          (should (equal process-environment
                         '("KEEP=yes" "MAGNUS_TEST_ID=old"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magnus-terminal-command-buffer-binds-only-one-direct-command ()
  (let ((vterm-shell "/bin/original-shell")
        (command "claude --session-id fixture 'first prompt'")
        observed
        buffer)
    (cl-letf (((symbol-function 'vterm-mode)
               (lambda () (setq observed (symbol-value 'vterm-shell))))
              ((symbol-function 'magnus-terminal-setup-keys) #'ignore))
      (setq buffer
            (magnus-terminal-create-command-buffer
             " *magnus-direct-command*" command)))
    (unwind-protect
        (progn
          (should (equal observed command))
          (should (equal vterm-shell "/bin/original-shell")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  (should-error
   (magnus-terminal-create-command-buffer " *magnus-empty-command*" "")))

(ert-deftest magnus-terminal-cancels-only-the-requested-delivery-scope ()
  (let* ((buffer (generate-new-buffer " *magnus-terminal-scopes*"))
         (process
          (make-pipe-process :name "magnus-terminal-scopes"
                             :buffer buffer :noquery t))
         (instance
          (magnus-instance--create
           :id "scoped" :name "scoped" :buffer buffer :status 'running))
         (magnus-terminal--delivery-processes
          (make-hash-table :test #'eq))
         timer events)
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (text &optional _paste-p) (push text events)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (push 'return events)))
                  ((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat function &rest arguments)
                     (setq timer (cons function arguments))
                     'settle-timer)))
          (should (eq (magnus-terminal-submit
                       instance "review" nil :settle-delay 1
                       :scope 'review-controller)
                      'submitted))
          (should (eq (magnus-terminal-submit
                       instance "codex" nil :scope 'codex)
                      'queued))
          (magnus-terminal-cancel-scope 'review-controller)
          ;; The review entry was already submitted and is only settling;
          ;; cancelling its scope must retain both its timer and later Codex
          ;; work on the same exact process.
          (should timer)
          (should
           (equal (mapcar
                   (lambda (entry) (plist-get entry :text))
                   (process-get process 'magnus-terminal-delivery-queue))
                  '("codex")))
          (apply (car timer) (cdr timer))
          (should (equal (nreverse events)
                         '("review" return "codex" return)))
          (should (magnus-terminal-delivery-idle-p process)))
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(ert-deftest magnus-terminal-scope-cancellation-retains-pending-return ()
  "Cancellation cannot abandon text already pasted into a live composer."
  (let* ((buffer (generate-new-buffer " *magnus-terminal-cancel-pasted*"))
         (process
          (make-pipe-process :name "magnus-terminal-cancel-pasted"
                             :buffer buffer :noquery t))
         (instance
          (magnus-instance--create
           :id "cancel-pasted" :name "cancel-pasted"
           :buffer buffer :status 'running))
         (magnus-terminal--delivery-processes
          (make-hash-table :test #'eq))
         (pasted
          (list :instance instance :buffer buffer :process process
                :text "already pasted" :scope 'cancel-me :phase 'pasted))
         (pending
          (list :instance instance :buffer buffer :process process
                :text "not pasted" :scope 'cancel-me :phase 'pending)))
    (unwind-protect
        (progn
          (process-put process 'magnus-terminal-delivery-queue
                       (list pasted pending))
          (puthash process t magnus-terminal--delivery-processes)
          (magnus-terminal-cancel-scope 'cancel-me)
          (should (equal
                   (process-get process 'magnus-terminal-delivery-queue)
                   (list pasted))))
      (magnus-terminal-release-process process)
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(ert-deftest magnus-terminal-deduplication-is-scoped ()
  (let* ((buffer (generate-new-buffer " *magnus-terminal-dedup-scope*"))
         (process
          (make-pipe-process :name "magnus-terminal-dedup-scope"
                             :buffer buffer :noquery t))
         (instance
          (magnus-instance--create
           :id "dedup" :name "dedup" :buffer buffer :status 'running))
         (magnus-terminal--delivery-processes
          (make-hash-table :test #'eq))
         (not-ready (lambda (_process) nil)))
    (unwind-protect
        (progn
          (magnus-terminal-submit
           instance "same text" nil :ready-p not-ready
           :scope 'review-controller :deduplicate t)
          (magnus-terminal-submit
           instance "same text" nil :scope 'codex :deduplicate t)
          (should
           (equal (mapcar
                   (lambda (entry) (plist-get entry :scope))
                   (process-get process 'magnus-terminal-delivery-queue))
                  '(review-controller codex))))
      (magnus-terminal-release-process process)
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(ert-deftest magnus-terminal-try-submit-owns-text-and-return-atomically ()
  "Immediate ingress cannot interleave with durable queued delivery."
  (let* ((buffer (generate-new-buffer " *magnus-terminal-immediate*"))
         (process
          (make-pipe-process :name "magnus-terminal-immediate"
                             :buffer buffer :noquery t))
         (instance
          (magnus-instance--create
           :id "immediate" :name "immediate" :buffer buffer :status 'running))
         (magnus-terminal--delivery-processes
          (make-hash-table :test #'eq))
         timer events accepted)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-terminal--selected-buffer-p)
                   (lambda (_buffer) nil))
                  ((symbol-function 'vterm-send-string)
                   (lambda (text &optional _paste-p) (push text events)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (push 'return events)))
                  ((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat function &rest arguments)
                     (setq timer (cons function arguments))
                     'settle-timer)))
          (should
           (eq (magnus-terminal-try-submit
                instance "mailbox" :settle-delay 1
                :accepted
                (lambda ()
                  (setq accepted t)
                  (should
                   (eq (magnus-terminal-submit
                        instance "durable" nil :scope 'coord)
                       'queued))))
               'submitted))
          (should accepted)
          (should (equal (nreverse (copy-sequence events))
                         '("mailbox" return)))
          (should (process-get process 'magnus-terminal-delivery-busy))
          (should
           (equal (mapcar
                   (lambda (entry) (plist-get entry :text))
                   (process-get process 'magnus-terminal-delivery-queue))
                  '("durable")))
          (apply (car timer) (cdr timer))
          (should (equal (nreverse events)
                         '("mailbox" return "durable" return)))
          (should (magnus-terminal-delivery-idle-p process)))
      (magnus-terminal-release-process process)
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(ert-deftest magnus-terminal-fifo-retries-only-return-after-partial-submit ()
  "A durable FIFO entry never pastes its text twice after Return fails."
  (let* ((buffer (generate-new-buffer " *magnus-terminal-partial-fifo*"))
         (process
          (make-pipe-process :name "magnus-terminal-partial-fifo"
                             :buffer buffer :noquery t))
         (instance
          (magnus-instance--create
           :id "partial-fifo" :name "partial-fifo"
           :buffer buffer :status 'running))
         (magnus-terminal--delivery-processes
          (make-hash-table :test #'eq))
         retry events accepted selected (return-attempts 0))
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-terminal--selected-buffer-p)
                   (lambda (_buffer) selected))
                  ((symbol-function 'vterm-send-string)
                   (lambda (text &optional _paste-p)
                     (push (list 'paste text) events)))
                  ((symbol-function 'vterm-send-return)
                   (lambda ()
                     (cl-incf return-attempts)
                     (push 'return events)
                     (when (= return-attempts 1)
                       (error "temporary Return failure"))))
                  ((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat function &rest arguments)
                     (setq retry (cons function arguments))
                     'retry-timer)))
          (should
           (eq (magnus-terminal-submit
                instance "durable text" (lambda () (setq accepted t))
                :scope 'partial-fifo)
               'queued))
          (let ((entry (car (process-get
                             process 'magnus-terminal-delivery-queue))))
            (should (eq (plist-get entry :process) process))
            (should (eq (plist-get entry :phase) 'pasted)))
          (should-not accepted)
          (should retry)
          ;; Ownership is already committed after paste.  Selecting the buffer
          ;; cannot turn the pending Return into user-owned composer text.
          (setq selected t)
          (apply (car retry) (cdr retry))
          (should accepted)
          (should-not (process-get process 'magnus-terminal-delivery-queue))
          (should (= return-attempts 2))
          (should (= (cl-count-if (lambda (event) (eq (car-safe event) 'paste))
                                  events)
                     1)))
      (magnus-terminal-release-process process)
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(ert-deftest magnus-terminal-try-submit-owns-partial-paste-until-return ()
  "Immediate ingress queues only Return after its text reached exact PROCESS."
  (let* ((buffer (generate-new-buffer " *magnus-terminal-partial-immediate*"))
         (process
          (make-pipe-process :name "magnus-terminal-partial-immediate"
                             :buffer buffer :noquery t))
         (instance
          (magnus-instance--create
           :id "partial-immediate" :name "partial-immediate"
           :buffer buffer :status 'running))
         (magnus-terminal--delivery-processes
          (make-hash-table :test #'eq))
         retry events accepted (return-attempts 0) (ready-checks 0))
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-terminal--selected-buffer-p)
                   (lambda (_buffer) nil))
                  ((symbol-function 'vterm-send-string)
                   (lambda (text &optional _paste-p)
                     (push (list 'paste text) events)))
                  ((symbol-function 'vterm-send-return)
                   (lambda ()
                     (cl-incf return-attempts)
                     (push 'return events)
                     (when (= return-attempts 1)
                       (error "temporary Return failure"))))
                  ((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat function &rest arguments)
                     (setq retry (cons function arguments))
                     'retry-timer)))
          (should
           (eq (magnus-terminal-try-submit
                instance "mailbox text"
                :ready-p
                (lambda (_process)
                  (cl-incf ready-checks)
                  (if (= ready-checks 1)
                      t
                    (error "readiness must not be rechecked after paste")))
                :accepted (lambda () (setq accepted t)))
               'queued))
          (let ((entry (car (process-get
                             process 'magnus-terminal-delivery-queue))))
            (should (eq (plist-get entry :process) process))
            (should (eq (plist-get entry :phase) 'pasted)))
          (should-not accepted)
          ;; The durable caller may poll again, but Magnus already owns this
          ;; exact paste and must neither retain nor paste a duplicate.
          (should-not (magnus-terminal-try-submit instance "mailbox text"))
          (should (= (cl-count-if (lambda (event) (eq (car-safe event) 'paste))
                                  events)
                     1))
          (apply (car retry) (cdr retry))
          (should accepted)
          (should-not (process-get process 'magnus-terminal-delivery-queue))
          (should (= return-attempts 2))
          (should (= ready-checks 1))
          (should (= (cl-count-if (lambda (event) (eq (car-safe event) 'paste))
                                  events)
                     1)))
      (magnus-terminal-release-process process)
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(ert-deftest magnus-terminal-partial-paste-rejects-replacement-process ()
  "A pending Return cannot cross from its pasted PROCESS to a replacement."
  (let* ((old-buffer (generate-new-buffer " *magnus-terminal-partial-old*"))
         (old-process
          (make-pipe-process :name "magnus-terminal-partial-old"
                             :buffer old-buffer :noquery t))
         (new-buffer (generate-new-buffer " *magnus-terminal-partial-new*"))
         (new-process
          (make-pipe-process :name "magnus-terminal-partial-new"
                             :buffer new-buffer :noquery t))
         (instance
          (magnus-instance--create
           :id "partial-owner" :name "partial-owner"
           :buffer old-buffer :status 'running))
         (magnus-terminal--delivery-processes
          (make-hash-table :test #'eq))
         retry accepted (return-attempts 0))
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-terminal--selected-buffer-p)
                   (lambda (_buffer) nil))
                  ((symbol-function 'vterm-send-string) #'ignore)
                  ((symbol-function 'vterm-send-return)
                   (lambda ()
                     (cl-incf return-attempts)
                     (error "temporary Return failure")))
                  ((symbol-function 'run-with-timer)
                   (lambda (_delay _repeat function &rest arguments)
                     (setq retry (cons function arguments))
                     'retry-timer)))
          (should
           (eq (magnus-terminal-try-submit
                instance "owned by old process"
                :accepted (lambda () (setq accepted t)))
               'queued))
          (setf (magnus-instance-buffer instance) new-buffer)
          (apply (car retry) (cdr retry))
          (should-not accepted)
          (should (= return-attempts 1))
          (should-not (process-get old-process
                                   'magnus-terminal-delivery-queue))
          (should (magnus-terminal-delivery-idle-p old-process))
          (should (process-live-p new-process)))
      (magnus-terminal-release-process old-process)
      (magnus-terminal-release-process new-process)
      (when (process-live-p old-process) (delete-process old-process))
      (when (process-live-p new-process) (delete-process new-process))
      (when (buffer-live-p old-buffer) (kill-buffer old-buffer))
      (when (buffer-live-p new-buffer) (kill-buffer new-buffer)))))

(ert-deftest magnus-terminal-try-submit-declines-without-retaining-text ()
  "Immediate ingress fails closed while the composer is unavailable."
  (let* ((buffer (generate-new-buffer " *magnus-terminal-decline*"))
         (process
          (make-pipe-process :name "magnus-terminal-decline"
                             :buffer buffer :noquery t))
         (instance
          (magnus-instance--create
           :id "decline" :name "decline" :buffer buffer :status 'running))
         (magnus-terminal--delivery-processes
          (make-hash-table :test #'eq))
         selected events)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-terminal--selected-buffer-p)
                   (lambda (_buffer) selected))
                  ((symbol-function 'vterm-send-string)
                   (lambda (&rest arguments) (push arguments events)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (push 'return events))))
          (setq selected t)
          (should-not (magnus-terminal-try-submit instance "selected"))
          (setq selected nil)
          (should-not
           (magnus-terminal-try-submit
            instance "not-ready" :ready-p (lambda (_process) nil)))
          (should-not
           (magnus-terminal-try-submit
            instance "ready-error"
            :ready-p (lambda (_process) (error "readiness unavailable"))))
          (process-put process 'magnus-terminal-delivery-busy t)
          (should-not (magnus-terminal-try-submit instance "busy"))
          (should-not events)
          (should-not (process-get process 'magnus-terminal-delivery-queue)))
      (magnus-terminal-release-process process)
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(provide 'magnus-terminal-tests)
;;; magnus-terminal-tests.el ends here
