;;; magnus-process-lifecycle-tests.el --- Creation transaction tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)

;; CI does not install vterm.  These tests exercise lifecycle boundaries and
;; replace terminal creation before it can call into the package.
(unless (featurep 'vterm)
  (provide 'vterm))

(require 'magnus-process)

;; `magnus-process.el' declares these core-owned variables without defaults.
;; Bind them here so isolated loading (without magnus.el) can exercise spawn.
(defvar magnus-claude-executable nil)
(defvar magnus--summon-context nil)
(defvar magnus-headless-allowed-tools nil)

(defmacro magnus-test-process-lifecycle--isolated (&rest body)
  "Run BODY with isolated registry and coordination ownership state."
  (declare (indent 0) (debug t))
  `(let ((magnus-instances nil)
         (magnus-instances-changed-hook nil)
         (magnus-coord--watched-dirs nil)
         (magnus-coord--file-mtimes nil)
         (magnus-coord--processed-mentions nil)
         (magnus-coord--processed-dms nil)
         (magnus-coord--processed-summons nil)
         (magnus-coord--states nil)
         (magnus-process--legacy-session-launches
          (make-hash-table :test #'equal))
         (magnus-coord--session-start-times
          (make-hash-table :test #'equal)))
     ,@body))

(defun magnus-test-process-lifecycle--coord-file (directory &optional agent)
  "Create and return a minimal coordination file in DIRECTORY.
When AGENT is non-nil, include its pre-existing Active Work row."
  (let ((file (magnus-coord-file-path directory)))
    (with-temp-file file
      (insert "# Agent Coordination\n\n## Active Work\n\n")
      (insert "| Agent | Area | Status | Files |\n")
      (insert "|-------|------|--------|-------|\n")
      (when agent
        (insert (format "| %s | existing | in-progress | old.el |\n"
                        agent)))
      (insert "\n## Log\n\n"
              "<!-- Agents insert newest messages below this comment. -->\n\n"))
    file))

(defun magnus-test-process-lifecycle--coord-state-from-file (directory)
  "Return coordination state read directly from DIRECTORY's shared file."
  (magnus-coord--parse-content (magnus-coord--read-content directory)))

(defun magnus-test-process-lifecycle--prepare-row-rollback
    (directory name pre-existing started-at)
  "Prepare watched coordination state for NAME's rollback test.
When PRE-EXISTING is non-nil, seed NAME's Active Work row."
  (magnus-test-process-lifecycle--coord-file
   directory (and pre-existing name))
  (magnus-coord-add-log directory "peer" "Shared history")
  (let ((key
         (magnus-test-process-lifecycle--claim-coord directory started-at)))
    (magnus-coord--cache-content key (magnus-coord--read-content key))
    key))

(defun magnus-test-process-lifecycle--publish-attempt-row
    (directory instance started-at)
  "Publish INSTANCE's attempted row and log, then refresh its watched cache."
  (let ((name (magnus-instance-name instance))
        (key (magnus-test-process-lifecycle--coord-key directory)))
    (magnus-coord-update-active
     directory name "attempt" "in-progress" '("attempt.el"))
    (magnus-coord-add-log directory name "Attempt joined")
    (magnus-test-process-lifecycle--claim-coord directory started-at)
    (magnus-coord--cache-content key (magnus-coord--read-content key))))

(defun magnus-test-process-lifecycle--assert-row-rollback
    (directory name pre-existing started-at)
  "Assert NAME rollback in DIRECTORY preserved only pre-existing ownership."
  (let* ((key (magnus-test-process-lifecycle--coord-key directory))
         (direct
          (magnus-test-process-lifecycle--coord-state-from-file directory))
         (cached (alist-get key magnus-coord--states nil nil #'equal))
         (direct-names
          (mapcar (lambda (entry) (plist-get entry :agent))
                  (plist-get direct :active)))
         (cached-names
          (mapcar (lambda (entry) (plist-get entry :agent))
                  (plist-get cached :active)))
         (messages
          (mapcar (lambda (entry) (plist-get entry :message))
                  (plist-get direct :log))))
    (should (eq (not (null (member name direct-names))) pre-existing))
    (should (equal cached-names direct-names))
    (should (member "Shared history" messages))
    (should (member "Attempt joined" messages))
    (should (member key magnus-coord--watched-dirs))
    (should (= (gethash key magnus-coord--session-start-times) started-at))))

(defun magnus-test-process-lifecycle--coord-key (directory)
  "Return DIRECTORY's canonical coordination ownership key."
  (magnus-coord--normalized-directory directory))

(defun magnus-test-process-lifecycle--claim-coord (directory started-at)
  "Model coordination ownership of DIRECTORY since STARTED-AT."
  (let ((key (magnus-test-process-lifecycle--coord-key directory)))
    (cl-pushnew key magnus-coord--watched-dirs :test #'equal)
    (puthash key started-at magnus-coord--session-start-times)
    key))

(ert-deftest magnus-process-transaction-runtime-owner-is-first-writer-wins ()
  (let* ((first-buffer (generate-new-buffer " *magnus-first-runtime*"))
         (second-buffer (generate-new-buffer " *magnus-second-runtime*"))
         (first-process
          (make-pipe-process
           :name (generate-new-buffer-name "magnus-first-runtime")
           :buffer first-buffer :noquery t))
         (second-process
          (make-pipe-process
           :name (generate-new-buffer-name "magnus-second-runtime")
           :buffer second-buffer :noquery t))
         (magnus-process--transaction-runtime-buffer (list nil)))
    (unwind-protect
        (progn
          (magnus-process--record-transaction-runtime
           first-buffer first-process)
          (magnus-process--record-transaction-runtime
           second-buffer second-process)
          (should (eq (caar magnus-process--transaction-runtime-buffer)
                      first-buffer))
          (should (eq (cdar magnus-process--transaction-runtime-buffer)
                      first-process)))
      (dolist (process (list first-process second-process))
        (when (process-live-p process)
          (delete-process process)))
      (dolist (buffer (list first-buffer second-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magnus-process-create-rolls-back-registry-add-failure ()
  (magnus-test-process-lifecycle--isolated
    (let ((directory (make-temp-file "magnus-create-registry-" t))
          coord-called
          spawn-called)
      (unwind-protect
          (cl-letf
              (((symbol-function 'magnus-instances-add)
                (lambda (instance)
                  ;; Model a changed-hook failure after the registry mutation.
                  (push instance magnus-instances)
                  (error "registry hook failed")))
               ((symbol-function 'magnus-coord-register-agent)
                (lambda (&rest _arguments) (setq coord-called t)))
               ((symbol-function 'magnus-process--spawn)
                (lambda (&rest _arguments) (setq spawn-called t))))
            (let ((err (should-error
                        (magnus-process-create directory "broken-registry"))))
              (should (string-match-p "registry hook failed"
                                      (error-message-string err))))
            (should-not magnus-instances)
            (should-not coord-called)
            (should-not spawn-called))
        (delete-directory directory t)))))

(ert-deftest magnus-process-create-rolls-back-coordination-registration-failure ()
  (magnus-test-process-lifecycle--isolated
    (let ((directory (make-temp-file "magnus-create-coord-" t))
          cleared spawn-called)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--coord-file directory)
            (cl-letf
                (((symbol-function 'magnus-coord-register-agent)
                 (lambda (dir _instance)
                    (let ((key (magnus-coord--normalized-directory dir)))
                      (push key magnus-coord--watched-dirs)
                      (puthash key 42 magnus-coord--session-start-times))
                    (error "coord registration failed")))
                 ((symbol-function 'magnus-coord-clear-agent)
                  (lambda (dir name) (setq cleared (list dir name))))
                 ((symbol-function 'magnus-process--spawn)
                  (lambda (&rest _arguments) (setq spawn-called t))))
              (let ((err (should-error
                          (magnus-process-create directory "broken-coord"))))
                (should (string-match-p "coord registration failed"
                                        (error-message-string err))))
              (should-not magnus-instances)
              (should-not magnus-coord--watched-dirs)
              (should-not
               (gethash directory magnus-coord--session-start-times))
              (should (magnus-coord--same-directory-p
                       (car cleared) directory))
              (should (equal (cadr cleared) "broken-coord"))
              (should-not spawn-called)))
        (delete-directory directory t)))))

(ert-deftest magnus-process-create-rolls-back-claude-spawn-failure ()
  (magnus-test-process-lifecycle--isolated
    (let ((directory (make-temp-file "magnus-create-claude-" t))
          terminal)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--coord-file directory)
            (cl-letf
                (((symbol-function 'magnus-coord-register-agent)
                  (lambda (dir _instance)
                    (let ((key (magnus-coord--normalized-directory dir)))
                      (push key magnus-coord--watched-dirs)
                      (puthash key 42 magnus-coord--session-start-times))))
                 ((symbol-function 'magnus-process--spawn)
                  (lambda (instance)
                    (setq terminal (generate-new-buffer " *failed-claude*"))
                    (magnus-instances-update
                     instance :buffer terminal :status 'running)
                    (error "claude spawn failed"))))
              (let ((err (should-error
                          (magnus-process-create directory "broken-claude"))))
                (should (string-match-p "claude spawn failed"
                                        (error-message-string err))))
              (should-not magnus-instances)
              (should-not (buffer-live-p terminal))
              (should-not magnus-coord--watched-dirs)
              (should-not
               (gethash directory magnus-coord--session-start-times))))
        (when (buffer-live-p terminal)
          (kill-buffer terminal))
        (delete-directory directory t)))))

(ert-deftest magnus-process-create-external-failure-preserves-project-owners ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-create-external-" t))
           (existing (magnus-instances-create
                      directory "existing-agent" 'claude))
           terminal
           provider-calls)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--coord-file
             directory "broken-external")
            (magnus-instances-add existing)
            (let ((key (magnus-coord--normalized-directory directory)))
              (setq magnus-coord--watched-dirs (list key))
              (puthash key 17 magnus-coord--session-start-times))
            (cl-letf
                (((symbol-function 'magnus-coord-register-agent) #'ignore)
                 ((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) t))
                 ((symbol-function 'magnus-provider-call)
                  (lambda (instance operation &rest arguments)
                    (push (cons operation arguments) provider-calls)
                    (pcase operation
                      ('start
                       (setq terminal
                             (generate-new-buffer " *failed-provider*"))
                       (magnus-instances-update
                        instance :buffer terminal :status 'running)
                       (error "external start failed"))
                      ('stop nil)
                      (_ (error "unexpected provider operation"))))))
              (let ((err
                     (should-error
                      (magnus-process-create
                       directory "broken-external" 'fake "initial task"))))
                (should (string-match-p "external start failed"
                                        (error-message-string err))))
              (should (equal (magnus-instances-list) (list existing)))
              (should
               (equal magnus-coord--watched-dirs
                      (list (magnus-coord--normalized-directory directory))))
              (should (= (gethash (magnus-coord--normalized-directory directory)
                                  magnus-coord--session-start-times)
                         17))
              (should-not (buffer-live-p terminal))
              (should (member '(start "initial task") provider-calls))
              (should (member '(stop t) provider-calls))))
        (when (buffer-live-p terminal)
          (kill-buffer terminal))
        (delete-directory directory t)))))

(ert-deftest magnus-process-create-rolls-back-only-its-active-row ()
  "Failed creation removes its new row but preserves a row it inherited."
  (dolist (pre-existing '(nil t))
    (magnus-test-process-lifecycle--isolated
      (let* ((directory (make-temp-file "magnus-create-row-rollback-" t))
             (name "create-row-owner")
             (started-at 101))
        (unwind-protect
            (progn
              (magnus-test-process-lifecycle--prepare-row-rollback
               directory name pre-existing started-at)
              (cl-letf
                  (((symbol-function 'magnus-coord-register-agent)
                    (lambda (project candidate)
                      (magnus-test-process-lifecycle--publish-attempt-row
                       project candidate 999)))
                   ((symbol-function 'magnus-process--spawn)
                    (lambda (_candidate) (error "create starter failed"))))
                (should-error
                 (magnus-process-create directory name)
                 :type 'error))
              (magnus-test-process-lifecycle--assert-row-rollback
               directory name pre-existing started-at)
              (should-not magnus-instances))
          (delete-directory directory t))))))

(ert-deftest magnus-process-resurrect-rolls-back-only-its-active-row ()
  "Failed resurrection preserves only a pre-existing same-name row."
  (dolist (pre-existing '(nil t))
    (magnus-test-process-lifecycle--isolated
      (let* ((directory (make-temp-file "magnus-resurrect-row-rollback-" t))
             (name "resurrect-row-owner")
             (started-at 202)
             (instance (magnus-instances-create directory name 'codex)))
        (unwind-protect
            (progn
              (setq magnus-instances (list instance))
              (magnus-instances-update
               instance :status 'purged :session-id "resume-session"
               :purged-at 123.0)
              (magnus-test-process-lifecycle--prepare-row-rollback
               directory name pre-existing started-at)
              (cl-letf
                  (((symbol-function 'magnus-provider-external-p)
                    (lambda (_candidate) t))
                   ((symbol-function 'magnus-coord-register-agent)
                    (lambda (project candidate)
                      (magnus-test-process-lifecycle--publish-attempt-row
                       project candidate 999)))
                   ((symbol-function 'magnus-provider-call)
                    (lambda (candidate operation &rest _arguments)
                      (pcase operation
                        ('resume (error "resurrection starter failed"))
                        ('stop
                         (magnus-instances-update
                          candidate :status 'stopped :buffer nil))))))
                (should-error
                 (magnus-process-resurrect-purged instance)
                 :type 'error))
              (magnus-test-process-lifecycle--assert-row-rollback
               directory name pre-existing started-at)
              (should (eq (magnus-instance-status instance) 'purged))
              (should (equal (magnus-instance-session-id instance)
                             "resume-session")))
          (delete-directory directory t))))))

(ert-deftest magnus-process-chdir-rolls-back-only-its-destination-row ()
  "Failed moves preserve destination history and only inherited same-name rows."
  (dolist (pre-existing '(nil t))
    (magnus-test-process-lifecycle--isolated
      (let* ((old-directory
              (make-temp-file "magnus-chdir-row-source-" t))
             (new-directory
              (make-temp-file "magnus-chdir-row-destination-" t))
             (name "chdir-row-owner")
             (started-at 303)
             (instance (magnus-instances-create old-directory name 'codex)))
        (unwind-protect
            (progn
              (setq magnus-instances (list instance))
              (magnus-instances-update
               instance :status 'running :session-id "source-session")
              (magnus-test-process-lifecycle--claim-coord old-directory 11)
              (magnus-test-process-lifecycle--prepare-row-rollback
               new-directory name pre-existing started-at)
              (cl-letf
                  (((symbol-function 'magnus-provider-external-p)
                    (lambda (_candidate) t))
                   ((symbol-function 'magnus-process--ensure-agent-dir)
                    #'ignore)
                   ((symbol-function 'magnus-coord-register-agent)
                    (lambda (project candidate)
                      (magnus-test-process-lifecycle--publish-attempt-row
                       project candidate 999)))
                   ((symbol-function 'magnus-provider-call)
                    (lambda (candidate operation &rest _arguments)
                      (pcase operation
                        ('stop
                         (magnus-instances-update
                          candidate :status 'stopped :buffer nil))
                        ('start (error "destination starter failed"))))))
                (should-error
                 (magnus-process-chdir instance new-directory)
                 :type 'error))
              (magnus-test-process-lifecycle--assert-row-rollback
               new-directory name pre-existing started-at)
              (should (equal (magnus-instance-directory instance)
                             old-directory))
              (should (eq (magnus-instance-status instance) 'stopped))
              (should (equal (magnus-instance-session-id instance)
                             "source-session")))
          (delete-directory old-directory t)
          (delete-directory new-directory t))))))

(ert-deftest magnus-process-create-keeps-successful-acquisition-order ()
  (magnus-test-process-lifecycle--isolated
    (let ((directory (make-temp-file "magnus-create-success-" t))
          calls)
      (unwind-protect
          (cl-letf
              (((symbol-function 'magnus-instances-add)
                (lambda (instance)
                  (push 'registry calls)
                  (push instance magnus-instances)
                  instance))
               ((symbol-function 'magnus-coord-register-agent)
                (lambda (&rest _arguments) (push 'coordination calls)))
               ((symbol-function 'magnus-provider-external-p)
                (lambda (_instance) t))
               ((symbol-function 'magnus-provider-call)
                (lambda (_instance operation &rest _arguments)
                  (push operation calls))))
            (let ((instance
                   (magnus-process-create directory "healthy" 'fake)))
              (should (eq instance (car magnus-instances)))
              (should (equal (nreverse calls)
                             '(registry coordination start)))))
        (delete-directory directory t)))))

(ert-deftest magnus-process-create-headless-rolls-back-launch-failure ()
  (magnus-test-process-lifecycle--isolated
    (let ((directory (make-temp-file "magnus-create-headless-" t))
          (magnus-claude-executable "claude")
          (magnus-headless-allowed-tools "Read"))
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--coord-file directory)
            (cl-letf
                (((symbol-function 'magnus-coord-register-agent)
                  (lambda (dir _instance)
                    (let ((key (magnus-coord--normalized-directory dir)))
                      (push key magnus-coord--watched-dirs)
                      (puthash key 42 magnus-coord--session-start-times))))
                 ((symbol-function 'magnus-headless-start)
                  (lambda (&rest _arguments)
                    (error "headless launch failed"))))
              (let ((err
                     (should-error
                      (magnus-process-create-headless
                       "Review this" directory "broken-headless"))))
                (should (string-match-p "headless launch failed"
                                        (error-message-string err))))
              (should-not magnus-instances)
              (should-not magnus-coord--watched-dirs)
              (should-not
               (gethash directory magnus-coord--session-start-times))
              (should-not (get-buffer "*claude-headless:broken-headless*"))))
        (when-let ((buffer (get-buffer "*claude-headless:broken-headless*")))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest magnus-process-create-headless-assigns-durable-kind ()
  "Headless identity exists before its runtime and buffer are created."
  (magnus-test-process-lifecycle--isolated
    (cl-letf (((symbol-function 'magnus-process--create-transaction)
               (lambda (instance _starter) instance)))
      (let ((instance
             (magnus-process-create-headless
              "Review this" default-directory "durable-headless")))
        (should (eq (magnus-instance-kind instance) 'headless))
        (should (magnus-process--headless-p instance))
        (should-not (magnus-instance-buffer instance))))))

(ert-deftest magnus-process-headless-spawn-uses-shared-runner ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-headless-runner-" t))
           (instance (magnus-instances-create directory "runner-agent"))
           (magnus-headless-allowed-tools "Read Write Edit")
           (magnus-buffer-name " *magnus-status-not-present*")
           provider request callbacks process buffer logged)
      (unwind-protect
          (let ((magnus-instances-changed-hook nil))
            (cl-letf
                (((symbol-function 'magnus-headless-start)
                  (lambda (selected-provider selected-request
                           selected-callbacks)
                    (setq provider selected-provider
                          request selected-request
                          callbacks selected-callbacks
                          process
                          (make-pipe-process
                           :name (generate-new-buffer-name
                                  "magnus-headless-runner")
                           :buffer (plist-get selected-request :buffer)
                           :noquery t))
                    process))
                 ((symbol-function 'magnus-coord-add-log)
                  (lambda (&rest arguments) (setq logged arguments))))
              (setq buffer
                    (magnus-process--spawn-headless
                     instance "Implement it"))
              (should (eq provider 'claude))
              (should (eq (plist-get request :purpose) 'agent))
              (should (equal (plist-get request :allowed-tools)
                             "Read Write Edit"))
              (should-not (plist-member request :environment-bindings))
              (should (string-match-p "Implement it"
                                      (plist-get request :prompt)))
              (should (eq (plist-get request :buffer) buffer))
              (should (eq (get-buffer-process buffer) process))
              (funcall (plist-get callbacks :on-event)
                       process '(:type "assistant" :text "Hello"))
              (funcall (plist-get callbacks :on-event)
                       process '(:type "result" :cost-usd 0.25))
              (with-current-buffer buffer
                (should (string-match-p "Hello" (buffer-string)))
                (should (string-match-p "Cost: \\$0\\.2500"
                                        (buffer-string))))
              (funcall (plist-get callbacks :on-complete)
                       process
                       '(:success-p t :status exit
                         :process-event "finished"))
              (should (eq (magnus-instance-status instance) 'finished))
              (should
               (equal logged
                      (list directory "runner-agent"
                            "Headless task finished")))
              (with-current-buffer buffer
                (should (string-match-p "Process finished"
                                        (buffer-string))))))
        (when (and process (process-live-p process))
          (delete-process process))
        (when (and buffer (buffer-live-p buffer))
          (kill-buffer buffer))
        (delete-directory directory t)))))

(ert-deftest magnus-process-headless-completion-preserves-purged-status ()
  (let* ((directory (make-temp-file "magnus-headless-purged-" t))
         (instance (magnus-instances-create directory "archived-agent"))
         (buffer (generate-new-buffer " *magnus-headless-purged*"))
         (process (make-pipe-process
                   :name (generate-new-buffer-name "magnus-headless-purged")
                   :buffer buffer :noquery t))
         (magnus-buffer-name " *magnus-status-not-present*")
         logged)
    (unwind-protect
        (let ((magnus-instances-changed-hook nil))
          (magnus-instances-update instance :buffer buffer :status 'purged)
          (cl-letf (((symbol-function 'magnus-coord-add-log)
                     (lambda (&rest arguments) (setq logged arguments))))
            (magnus-process--headless-complete
             instance process
             '(:success-p t :status exit :process-event "finished")))
          (should (eq (magnus-instance-status instance) 'purged))
          (should-not logged))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest magnus-process-claude-spawn-cancels-prewatch-resources ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-spawn-cleanup-" t))
           (instance (magnus-instances-create directory "failed-spawn"))
           (terminal (generate-new-buffer " *failed-spawn-terminal*"))
           (first-timer (timer-create))
           (second-timer (timer-create))
           (timers (list first-timer second-timer))
           cancelled
           sent-command
           terminal-environment)
      (unwind-protect
          (cl-letf
              (((symbol-value 'magnus-claude-executable) "claude")
               ((symbol-value 'magnus--summon-context) nil)
               ((symbol-function
                 'magnus-process--claude-session-id-supported-p)
                (lambda () t))
               ((symbol-function 'magnus-process--fresh-claude-session-id)
                (lambda () "11111111-1111-4111-a111-111111111111"))
               ((symbol-function 'magnus-process--list-sessions)
                (lambda (_directory)
                  (error "explicit session launch must not scan")))
               ((symbol-function 'magnus-terminal-create-buffer)
                (lambda (_name &optional environment)
                  (setq terminal-environment environment)
                  terminal))
               ((symbol-function 'vterm-send-string)
                (lambda (command &optional _paste)
                  (setq sent-command command)))
               ((symbol-function 'magnus-process--setup-sentinel) #'ignore)
               ((symbol-function 'run-with-timer)
                (lambda (&rest _arguments) (pop timers)))
               ((symbol-function 'cancel-timer)
                (lambda (timer) (push timer cancelled)))
               ((symbol-function 'magnus-process--watch-for-session)
                (lambda (&rest _arguments) (error "watch setup failed"))))
            (let ((err (should-error (magnus-process--spawn instance))))
            (should (string-match-p "watch setup failed"
                                      (error-message-string err))))
            (should-not terminal-environment)
            (should-not (buffer-live-p terminal))
            (should-not (magnus-instance-buffer instance))
            (should (eq (magnus-instance-status instance) 'stopped))
            (should-not (magnus-instance-session-id instance))
            (should
             (equal sent-command
                    "claude --session-id 11111111-1111-4111-a111-111111111111"))
            (should (memq first-timer cancelled))
            (should (memq second-timer cancelled)))
        (when (buffer-live-p terminal)
          (kill-buffer terminal))
        (delete-directory directory t)))))

(ert-deftest magnus-process-runtime-rollback-preserves-same-buffer-replacement ()
  (magnus-test-process-lifecycle--isolated
    (let* ((instance
            (magnus-instances-create default-directory "rollback-owner"))
           (buffer (generate-new-buffer " *magnus-rollback-owner*"))
           (old-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-old-rollback-owner")
             :buffer buffer :noquery t))
           (replacement-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-new-rollback-owner")
             :buffer buffer :noquery t)))
      (unwind-protect
          (progn
            (should (eq (get-buffer-process buffer) replacement-process))
            (magnus-instances-update
             instance :status 'running :buffer buffer)
            (should-error
             (magnus-process--discard-created-runtime instance t buffer))
            (cl-letf (((symbol-function 'magnus-provider-call)
                       (lambda (&rest _arguments)
                         (ert-fail "stale owner must not stop the provider"))))
              (magnus-process--discard-created-runtime
               instance t (cons buffer old-process)))
            (should-not (process-live-p old-process))
            (should (process-live-p replacement-process))
            (should (buffer-live-p buffer))
            (should (eq (magnus-instance-buffer instance) buffer))
            (should (eq (magnus-instance-status instance) 'running)))
        (when (process-live-p old-process)
          (delete-process old-process))
        (when (process-live-p replacement-process)
          (delete-process replacement-process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magnus-process-terminal-compatibility-wrappers-delegate ()
  (let (calls)
    (cl-letf (((symbol-function 'magnus-terminal-create-buffer)
               (lambda (name)
                 (push (list 'create name) calls)
                 'terminal-buffer))
              ((symbol-function 'magnus-terminal-send-escape)
               (lambda () (push '(escape) calls)))
              ((symbol-function 'magnus-terminal-setup-keys)
               (lambda () (push '(setup) calls))))
      (should (eq (magnus-process--create-vterm-buffer "compat")
                  'terminal-buffer))
      (magnus-process-send-escape)
      (magnus-process--setup-keys))
    (should (equal (nreverse calls)
                   '((create "compat") (escape) (setup))))))

(ert-deftest magnus-process-session-watch-setup-is-transactional ()
  (let* ((directory (make-temp-file "magnus-session-watch-" t))
         (instance (magnus-instances-create directory "failed-watch"))
         (poll-timer (timer-create))
         removed
         cancelled
         (timer-calls 0))
    (unwind-protect
        (cl-letf
            (((symbol-function 'magnus-process--project-hash)
              (lambda (_directory) "test-project"))
             ((symbol-function 'file-notify-add-watch)
              (lambda (&rest _arguments) 'watch-descriptor))
             ((symbol-function 'file-notify-rm-watch)
              (lambda (descriptor) (setq removed descriptor)))
             ((symbol-function 'run-with-timer)
              (lambda (&rest _arguments)
                (cl-incf timer-calls)
                (if (= timer-calls 1)
                    poll-timer
                  (error "cleanup timer setup failed"))))
             ((symbol-function 'cancel-timer)
              (lambda (timer) (setq cancelled timer))))
          (should-error
           (magnus-process--watch-for-session instance directory nil))
          (should (eq removed 'watch-descriptor))
          (should (eq cancelled poll-timer)))
      (delete-directory directory t))))

(ert-deftest magnus-process-claude-session-support-probe-is-cached ()
  (let ((magnus-claude-executable "/opt/Claude Code/claude")
        (magnus-process--claude-session-id-support-cache
         (make-hash-table :test #'equal))
        (calls 0))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_executable) "/opt/Claude Code/claude"))
              ((symbol-function 'call-process)
               (lambda (&rest _arguments)
                 (cl-incf calls)
                 (insert "  --session-id <uuid>\n")
                 0)))
      (should (magnus-process--claude-session-id-supported-p))
      (should (magnus-process--claude-session-id-supported-p))
      (should (= calls 1)))))

(ert-deftest magnus-process-claude-command-preserves-executable-prefix ()
  "Executable paths and shipped embedded flags become safe argv prefixes."
  (let ((magnus-claude-executable "/opt/Claude Code/claude"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (command)
                 (and (equal command magnus-claude-executable) command))))
      (should (equal (magnus-process--claude-command-prefix)
                     '("/opt/Claude Code/claude")))))
  (let ((magnus-claude-executable
         "claude --dangerously-skip-permissions"))
    (cl-letf (((symbol-function 'executable-find) (lambda (_command) nil)))
      (should (equal (magnus-process--claude-command-prefix)
                     '("claude" "--dangerously-skip-permissions"))))))

(ert-deftest magnus-process-claude-session-probe-includes-prefix-flags ()
  (let ((magnus-claude-executable "claude --fixture-flag")
        (magnus-process--claude-session-id-support-cache
         (make-hash-table :test #'equal))
        arguments)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (command)
                 (and (equal command "claude") "/bin/claude")))
              ((symbol-function 'call-process)
               (lambda (&rest given)
                 (setq arguments given)
                 (insert "  --session-id <uuid>\n")
                 0)))
      (should (magnus-process--claude-session-id-supported-p))
      (should (equal arguments
                     '("/bin/claude" nil t nil
                       "--fixture-flag" "--help"))))))

(ert-deftest magnus-process-concurrent-claude-launches-capture-exact-sessions ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-concurrent-claude-" t))
           (home (make-temp-file "magnus-concurrent-home-" t))
           (process-environment (copy-sequence process-environment))
           (magnus-claude-executable "/opt/Claude Code/claude")
           (first (magnus-instances-create directory "first-claude"))
           (second (magnus-instances-create directory "second-claude"))
           (candidates
            '("11111111-1111-4111-a111-111111111111"
              "22222222-2222-4222-a222-222222222222"))
           (commands (make-hash-table :test #'eq))
           watches
           buffers)
      (setenv "HOME" home)
      (unwind-protect
          (cl-letf
              (((symbol-function
                 'magnus-process--claude-session-id-supported-p)
                (lambda () t))
               ((symbol-function 'magnus-process--fresh-claude-session-id)
                (lambda ()
                  (prog1 (car candidates)
                    (setq candidates (cdr candidates)))))
               ((symbol-function 'magnus-process--list-sessions)
                (lambda (_directory)
                  (error "exact session launches must not scan")))
               ((symbol-function 'magnus-terminal-create-buffer)
                (lambda (name &optional _environment)
                  (let ((buffer (generate-new-buffer (concat " *" name))))
                    (push buffer buffers)
                    buffer)))
               ((symbol-function 'vterm-send-string)
                (lambda (command &optional _paste)
                  (puthash (current-buffer) command commands)))
               ((symbol-function 'vterm-send-return) #'ignore)
               ((symbol-function 'magnus-process--setup-sentinel) #'ignore)
               ((symbol-function 'run-with-timer)
                (lambda (&rest _arguments) nil))
               ((symbol-function 'magnus-process--watch-for-session)
                (lambda (instance project before &optional candidate owner
                                  _legacy-token owner-process)
                  (push (list :instance instance :directory project
                              :before before :candidate candidate
                              :owner owner :owner-process owner-process)
                        watches))))
            (magnus-process--spawn first)
            (magnus-process--spawn second)
            (should-not candidates)
            (should-not (magnus-instance-session-id first))
            (should-not (magnus-instance-session-id second))
            (dolist (watch watches)
              (let* ((instance (plist-get watch :instance))
                     (candidate (plist-get watch :candidate))
                     (owner (plist-get watch :owner))
                     (path
                      (magnus-process--session-candidate-path
                       directory candidate)))
                (should-not (plist-get watch :before))
                (should (eq owner (magnus-instance-buffer instance)))
                (should (eq (plist-get watch :owner-process)
                            (get-buffer-process owner)))
                (should
                 (equal
                  (gethash owner commands)
                  (magnus-process--claude-command
                   "--session-id" candidate)))
                (make-directory (file-name-directory path) t)
                (with-temp-file path (insert "{}\n"))
                (should
                 (eq (magnus-process--detect-new-session
                      instance directory nil '(nil nil nil)
                      candidate owner)
                     'captured))))
            (should
             (equal (magnus-instance-session-id first)
                    "11111111-1111-4111-a111-111111111111"))
            (should
             (equal (magnus-instance-session-id second)
                    "22222222-2222-4222-a222-222222222222")))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
        (delete-directory directory t)
        (delete-directory home t)))))

(ert-deftest magnus-process-legacy-claude-launches-serialize-session-capture ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-legacy-serialize-" t))
           (magnus-claude-executable "claude")
           (first (magnus-instances-create directory "legacy-first"))
           (second (magnus-instances-create directory "legacy-second"))
           (buffers nil)
           first-token)
      (unwind-protect
          (cl-letf
              (((symbol-function
                 'magnus-process--claude-session-id-supported-p)
                (lambda () nil))
               ((symbol-function 'magnus-process--list-sessions)
                (lambda (_directory) nil))
               ((symbol-function 'magnus-terminal-create-buffer)
                (lambda (name &optional _environment)
                  (let ((buffer (generate-new-buffer (concat " *" name))))
                    (push buffer buffers)
                    buffer)))
               ((symbol-function 'vterm-send-string) #'ignore)
               ((symbol-function 'magnus-process--setup-sentinel) #'ignore)
               ((symbol-function 'run-with-timer)
                (lambda (&rest _arguments) nil))
               ((symbol-function 'magnus-process--watch-for-session)
                (lambda (_instance _project _before &optional _candidate
                                  _owner legacy-token _owner-process)
                  (setq first-token legacy-token))))
            (magnus-process--spawn first)
            (let ((err (should-error (magnus-process--spawn second)
                                     :type 'user-error)))
              (should (string-match-p "already resolving"
                                      (error-message-string err))))
            (should first-token)
            (should (= (length buffers) 1))
            (should-not (magnus-instance-buffer second))
            (should-not (magnus-instance-session-id second))
            (magnus-process--release-legacy-session-launch
             directory first-token)
            (should-not
             (gethash (magnus-coord--normalized-directory directory)
                      magnus-process--legacy-session-launches)))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
        (delete-directory directory t)))))

(ert-deftest magnus-process-legacy-session-capture-rejects-ambiguous-delta ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-ambiguous-session-" t))
           (instance (magnus-instances-create directory "legacy-claude"))
           (owner (generate-new-buffer " *legacy-claude-owner*"))
           (token (cons instance 'legacy-launch))
           cleaned)
      (unwind-protect
          (progn
            (magnus-instances-update instance :buffer owner :status 'running)
            (magnus-process--reserve-legacy-session-launch directory token)
            (cl-letf (((symbol-function 'magnus-process--list-sessions)
                       (lambda (_directory) '("old" "fresh-a" "fresh-b")))
                      ((symbol-function
                        'magnus-process--cleanup-session-watch)
                       (lambda (resources) (setq cleaned resources))))
              (should
               (eq (magnus-process--detect-new-session
                    instance directory '("old") '(watch poll cleanup)
                    nil owner token)
                   'ambiguous)))
            (should-not (magnus-instance-session-id instance))
            (should (equal cleaned '(watch poll cleanup)))
            (should-not
             (gethash (magnus-coord--normalized-directory directory)
                      magnus-process--legacy-session-launches)))
        (when (buffer-live-p owner)
          (kill-buffer owner))
        (delete-directory directory t)))))

(ert-deftest magnus-process-exact-session-watcher-rejects-stale-owner ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-stale-session-" t))
           (home (make-temp-file "magnus-stale-home-" t))
           (process-environment (copy-sequence process-environment))
           (instance (magnus-instances-create directory "relaunched-claude"))
           (stale-owner (generate-new-buffer " *stale-claude-owner*"))
           (current-owner (generate-new-buffer " *current-claude-owner*"))
           (candidate "33333333-3333-4333-a333-333333333333")
           cleaned)
      (setenv "HOME" home)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :buffer current-owner :status 'running)
            (let ((path
                   (magnus-process--session-candidate-path
                    directory candidate)))
              (make-directory (file-name-directory path) t)
              (with-temp-file path (insert "{}\n")))
            (cl-letf (((symbol-function
                        'magnus-process--cleanup-session-watch)
                       (lambda (resources) (setq cleaned resources))))
              (should
               (eq (magnus-process--detect-new-session
                    instance directory nil '(watch poll cleanup)
                    candidate stale-owner)
                   'stale)))
            (should-not (magnus-instance-session-id instance))
            (should (equal cleaned '(watch poll cleanup))))
        (when (buffer-live-p stale-owner)
          (kill-buffer stale-owner))
        (when (buffer-live-p current-owner)
          (kill-buffer current-owner))
        (delete-directory directory t)
        (delete-directory home t)))))

(ert-deftest magnus-process-session-watcher-rejects-same-buffer-replacement ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-replaced-session-" t))
           (home (make-temp-file "magnus-replaced-home-" t))
           (process-environment (copy-sequence process-environment))
           (instance (magnus-instances-create directory "reused-claude-buffer"))
           (buffer (generate-new-buffer " *shared-claude-session-owner*"))
           (old-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-old-session-owner")
             :buffer buffer :noquery t))
           replacement-process
           (candidate "44444444-4444-4444-a444-444444444444")
           cleaned)
      (setenv "HOME" home)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :buffer buffer :status 'running)
            (let ((path
                   (magnus-process--session-candidate-path
                    directory candidate)))
              (make-directory (file-name-directory path) t)
              (with-temp-file path (insert "{}\n")))
            (setq replacement-process
                  (make-pipe-process
                   :name (generate-new-buffer-name
                          "magnus-new-session-owner")
                   :buffer buffer :noquery t))
            (should (eq (get-buffer-process buffer) replacement-process))
            (cl-letf (((symbol-function
                        'magnus-process--cleanup-session-watch)
                       (lambda (resources) (setq cleaned resources))))
              (should
               (eq (magnus-process--detect-new-session
                    instance directory nil '(watch poll cleanup)
                    candidate buffer nil old-process)
                   'stale)))
            (should-not (magnus-instance-session-id instance))
            (should (equal cleaned '(watch poll cleanup))))
        (when (process-live-p old-process)
          (delete-process old-process))
        (when (process-live-p replacement-process)
          (delete-process replacement-process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (delete-directory directory t)
        (delete-directory home t)))))

(ert-deftest magnus-process-session-watcher-rehomes-to-restored-instance ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-restored-session-" t))
           (home (make-temp-file "magnus-restored-home-" t))
           (process-environment (copy-sequence process-environment))
           (old-instance (magnus-instances-create directory "restored-claude"))
           (restored
            (magnus-instances-deserialize
             (magnus-instances-serialize old-instance)))
           (buffer (generate-new-buffer " *restored-claude-session-owner*"))
           (process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-restored-session-owner")
             :buffer buffer :noquery t))
           (candidate "55555555-5555-4555-a555-555555555555")
           cleaned)
      (setenv "HOME" home)
      (unwind-protect
          (progn
            (magnus-instances-update
             old-instance :buffer buffer :status 'running)
            (magnus-instances-update
             restored :buffer buffer :status 'running)
            (setq magnus-instances (list restored))
            (let ((path
                   (magnus-process--session-candidate-path
                    directory candidate)))
              (make-directory (file-name-directory path) t)
              (with-temp-file path (insert "{}\n")))
            (cl-letf (((symbol-function
                        'magnus-process--cleanup-session-watch)
                       (lambda (resources) (setq cleaned resources))))
              (should
               (eq (magnus-process--detect-new-session
                    old-instance directory nil '(watch poll cleanup)
                    candidate buffer nil process)
                   'captured)))
            (should-not (magnus-instance-session-id old-instance))
            (should (equal (magnus-instance-session-id restored) candidate))
            (should (equal cleaned '(watch poll cleanup))))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (delete-directory directory t)
        (delete-directory home t)))))

(ert-deftest magnus-process-chdir-transfers-external-coordination-ownership ()
  (magnus-test-process-lifecycle--isolated
    (let* ((old-directory (make-temp-file "magnus-move-old-" t))
           (new-directory (make-temp-file "magnus-move-new-" t))
           (instance
            (magnus-instances-create old-directory "moving-codex" 'codex))
           calls)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'running :session-id "old-session"
             :previous-session-id "older-session")
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) t))
                 ((symbol-function 'magnus-provider-call)
                  (lambda (candidate operation &rest arguments)
                    (push (list operation
                                (magnus-instance-directory candidate)
                                arguments)
                          calls)
                    (pcase operation
                      ('stop
                       (magnus-instances-update
                        candidate :status 'stopped :buffer nil))
                      ('start
                       (magnus-instances-update candidate :status 'running)))))
                 ((symbol-function 'magnus-process--ensure-agent-dir)
                  (lambda (candidate)
                    (push (list 'onboarding
                                (magnus-instance-directory candidate))
                          calls)))
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (directory candidate)
                    (should (string= directory
                                     (magnus-instance-directory candidate)))
                    (push (list 'register directory) calls)))
                 ((symbol-function 'magnus-coord-unregister-agent)
                  (lambda (directory candidate)
                    (should (eq (magnus-instance-status candidate) 'running))
                    (push (list 'unregister directory) calls))))
              (should (eq (magnus-process-chdir instance new-directory)
                          instance)))
            (should
             (equal
              (nreverse calls)
              (list (list 'stop old-directory '(t))
                    (list 'onboarding new-directory)
                    (list 'register new-directory)
                    (list 'start new-directory nil)
                    (list 'unregister old-directory))))
            (should (string= (magnus-instance-directory instance)
                             new-directory))
            (should (eq (magnus-instance-status instance) 'running))
            (should-not (magnus-instance-session-id instance))
            (should (equal (magnus-instance-previous-session-id instance)
                           "old-session")))
        (delete-directory old-directory t)
        (delete-directory new-directory t)))))

(ert-deftest magnus-process-chdir-transfers-local-coordination-ownership ()
  (magnus-test-process-lifecycle--isolated
    (let* ((old-directory (make-temp-file "magnus-move-local-old-" t))
           (new-directory (make-temp-file "magnus-move-local-new-" t))
           (instance (magnus-instances-create old-directory "moving-claude"))
           calls)
      (unwind-protect
          (cl-letf
              (((symbol-function 'magnus-provider-external-p)
                (lambda (_instance) nil))
               ((symbol-function 'magnus-process--stop-local-for-chdir)
                (lambda (candidate)
                  (push (list 'stop
                              (magnus-instance-directory candidate))
                        calls)
                  (magnus-instances-update
                   candidate :status 'stopped :buffer nil)))
               ((symbol-function 'magnus-process--ensure-agent-dir)
                (lambda (candidate)
                  (push (list 'onboarding
                              (magnus-instance-directory candidate))
                        calls)))
               ((symbol-function 'magnus-coord-register-agent)
                (lambda (directory _candidate)
                  (push (list 'register directory) calls)))
               ((symbol-function 'magnus-process--spawn)
                (lambda (candidate)
                  (push (list 'spawn
                              (magnus-instance-directory candidate))
                        calls)
                  (magnus-instances-update candidate :status 'running)))
               ((symbol-function 'magnus-coord-unregister-agent)
                (lambda (directory _candidate)
                  (push (list 'unregister directory) calls)))
               ((symbol-function 'run-with-timer)
                (lambda (&rest _arguments)
                  (error "directory move must not defer startup"))))
            (should (eq (magnus-process-chdir instance new-directory)
                        instance))
            (should
             (equal
              (nreverse calls)
              (list (list 'stop old-directory)
                    (list 'onboarding new-directory)
                    (list 'register new-directory)
                    (list 'spawn new-directory)
                    (list 'unregister old-directory))))
            (should (eq (magnus-instance-status instance) 'running)))
        (delete-directory old-directory t)
        (delete-directory new-directory t)))))

(ert-deftest magnus-process-chdir-symlink-alias-is-physical-no-op ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-move-real-" t))
           (alias (concat directory "-alias"))
           instance
           calls)
      (unwind-protect
          (progn
            (make-symbolic-link directory alias)
            (setq instance (magnus-instances-create alias "same-project"))
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) nil))
                 ((symbol-function 'magnus-process--stop-local-for-chdir)
                  (lambda (&rest _arguments) (push 'stop calls)))
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (&rest _arguments) (push 'register calls)))
                 ((symbol-function 'magnus-process--spawn)
                  (lambda (&rest _arguments) (push 'spawn calls)))
                 ((symbol-function 'magnus-coord-unregister-agent)
                  (lambda (&rest _arguments) (push 'unregister calls))))
              (should (eq (magnus-process-chdir instance directory)
                          instance)))
            (should-not calls)
            ;; Keep the user's working-path spelling; only ownership identity
            ;; is canonicalized to the physical project root.
            (should (string= (magnus-instance-directory instance) alias)))
        (when (file-symlink-p alias)
          (delete-file alias))
        (delete-directory directory t)))))

(ert-deftest magnus-process-chdir-rolls-back-external-start-failure ()
  (magnus-test-process-lifecycle--isolated
    (let* ((old-directory (make-temp-file "magnus-move-fail-old-" t))
           (new-directory (make-temp-file "magnus-move-fail-new-" t))
           (instance
            (magnus-instances-create old-directory "broken-codex" 'codex))
           failed-buffer
           registrations
           stopped-watchers)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--claim-coord old-directory 17)
            (magnus-instances-update
             instance :status 'running :session-id "old-session"
             :previous-session-id "older-session")
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) t))
                 ((symbol-function 'magnus-provider-call)
                  (lambda (candidate operation &rest _arguments)
                    (pcase operation
                      ('stop
                       (magnus-instances-update
                        candidate :status 'stopped :buffer nil))
                      ('start
                       (setq failed-buffer
                             (generate-new-buffer " *failed-move-codex*"))
                       (magnus-instances-update
                        candidate :status 'running :buffer failed-buffer)
                       (error "destination start failed")))))
                 ((symbol-function 'magnus-process--ensure-agent-dir) #'ignore)
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (directory _candidate)
                    (push directory registrations)
                    (magnus-test-process-lifecycle--claim-coord
                     directory 23)))
                 ((symbol-function 'magnus-coord-stop-watching)
                  (lambda (directory)
                    (push directory stopped-watchers)
                    (setq magnus-coord--watched-dirs
                          (delete directory magnus-coord--watched-dirs)))))
              (let ((err (should-error
                          (magnus-process-chdir instance new-directory))))
                (should (string-match-p "destination start failed"
                                        (error-message-string err)))))
            (should (equal registrations (list new-directory)))
            (should
             (equal stopped-watchers
                    (list (magnus-test-process-lifecycle--coord-key
                           new-directory))))
            (should
             (equal magnus-coord--watched-dirs
                    (list (magnus-test-process-lifecycle--coord-key
                           old-directory))))
            (should (= (gethash
                        (magnus-test-process-lifecycle--coord-key old-directory)
                                magnus-coord--session-start-times)
                       17))
            (should-not
             (gethash (magnus-test-process-lifecycle--coord-key new-directory)
                      magnus-coord--session-start-times))
            (should (string= (magnus-instance-directory instance)
                             old-directory))
            (should (eq (magnus-instance-status instance) 'stopped))
            (should (equal (magnus-instance-session-id instance)
                           "old-session"))
            (should (equal (magnus-instance-previous-session-id instance)
                           "older-session"))
            (should-not (buffer-live-p failed-buffer)))
        (when (buffer-live-p failed-buffer)
          (kill-buffer failed-buffer))
        (delete-directory old-directory t)
        (delete-directory new-directory t)))))

(ert-deftest magnus-process-chdir-rolls-back-local-spawn-failure ()
  (magnus-test-process-lifecycle--isolated
    (let* ((old-directory (make-temp-file "magnus-move-local-fail-old-" t))
           (new-directory (make-temp-file "magnus-move-local-fail-new-" t))
           (instance (magnus-instances-create old-directory "broken-claude"))
           failed-buffer)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--claim-coord old-directory 31)
            (magnus-instances-update
             instance :status 'running :session-id "old-local-session")
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) nil))
                 ((symbol-function 'magnus-process--stop-local-for-chdir)
                  (lambda (candidate)
                    (magnus-instances-update
                     candidate :status 'stopped :buffer nil)))
                 ((symbol-function 'magnus-process--ensure-agent-dir) #'ignore)
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (directory _candidate)
                    (magnus-test-process-lifecycle--claim-coord
                     directory 37)))
                 ((symbol-function 'magnus-process--spawn)
                  (lambda (candidate)
                    (setq failed-buffer
                          (generate-new-buffer " *failed-move-claude*"))
                    (magnus-instances-update
                     candidate :status 'running :buffer failed-buffer)
                    (error "destination spawn failed")))
                 ((symbol-function 'magnus-coord-stop-watching)
                  (lambda (directory)
                    (setq magnus-coord--watched-dirs
                          (delete directory magnus-coord--watched-dirs)))))
              (let ((err (should-error
                          (magnus-process-chdir instance new-directory))))
                (should (string-match-p "destination spawn failed"
                                        (error-message-string err)))))
            (should
             (equal magnus-coord--watched-dirs
                    (list (magnus-test-process-lifecycle--coord-key
                           old-directory))))
            (should (= (gethash
                        (magnus-test-process-lifecycle--coord-key old-directory)
                                magnus-coord--session-start-times)
                       31))
            (should-not
             (gethash (magnus-test-process-lifecycle--coord-key new-directory)
                      magnus-coord--session-start-times))
            (should (string= (magnus-instance-directory instance)
                             old-directory))
            (should (eq (magnus-instance-status instance) 'stopped))
            (should (equal (magnus-instance-session-id instance)
                           "old-local-session"))
            (should-not (buffer-live-p failed-buffer)))
        (when (buffer-live-p failed-buffer)
          (kill-buffer failed-buffer))
        (delete-directory old-directory t)
        (delete-directory new-directory t)))))

(ert-deftest magnus-process-chdir-restores-source-after-release-failure ()
  (magnus-test-process-lifecycle--isolated
    (let* ((old-directory (make-temp-file "magnus-move-release-old-" t))
           (new-directory (make-temp-file "magnus-move-release-new-" t))
           (instance (magnus-instances-create old-directory "release-fail"))
           new-buffer
           registrations)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--claim-coord old-directory 41)
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) nil))
                 ((symbol-function 'magnus-process--stop-local-for-chdir)
                  (lambda (candidate)
                    (magnus-instances-update
                     candidate :status 'stopped :buffer nil)))
                 ((symbol-function 'magnus-process--ensure-agent-dir) #'ignore)
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (directory _candidate)
                    (push directory registrations)
                    (magnus-test-process-lifecycle--claim-coord
                     directory 43)))
                 ((symbol-function 'magnus-process--spawn)
                  (lambda (candidate)
                    (setq new-buffer
                          (generate-new-buffer " *release-fail-runtime*"))
                    (magnus-instances-update
                     candidate :status 'running :buffer new-buffer)))
                 ((symbol-function 'magnus-coord-unregister-agent)
                  (lambda (directory _candidate)
                    (let ((key
                           (magnus-test-process-lifecycle--coord-key
                            directory)))
                      (setq magnus-coord--watched-dirs
                            (delete key magnus-coord--watched-dirs))
                      (remhash key magnus-coord--session-start-times))
                    (error "source release failed")))
                 ((symbol-function 'magnus-coord-stop-watching)
                  (lambda (directory)
                    (setq magnus-coord--watched-dirs
                          (delete directory magnus-coord--watched-dirs)))))
              (let ((err (should-error
                          (magnus-process-chdir instance new-directory))))
                (should (string-match-p "source release failed"
                                        (error-message-string err)))))
            (should (equal registrations
                           (list old-directory new-directory)))
            (should
             (equal magnus-coord--watched-dirs
                    (list (magnus-test-process-lifecycle--coord-key
                           old-directory))))
            (should (= (gethash
                        (magnus-test-process-lifecycle--coord-key old-directory)
                                magnus-coord--session-start-times)
                       43))
            (should-not
             (gethash (magnus-test-process-lifecycle--coord-key new-directory)
                      magnus-coord--session-start-times))
            (should (string= (magnus-instance-directory instance)
                             old-directory))
            (should (eq (magnus-instance-status instance) 'stopped))
            (should-not (buffer-live-p new-buffer)))
        (when (buffer-live-p new-buffer)
          (kill-buffer new-buffer))
        (delete-directory old-directory t)
        (delete-directory new-directory t)))))

(ert-deftest magnus-process-resurrect-rolls-back-new-external-ownership ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-resurrect-codex-" t))
           (instance
            (magnus-instances-create directory "archived-codex" 'codex))
           failed-buffer
           stopped-watchers)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'purged :session-id "codex-session"
             :purged-at 123.0)
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) t))
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (project candidate)
                    (should (eq (magnus-instance-status candidate) 'purged))
                    (magnus-test-process-lifecycle--claim-coord project 51)))
                 ((symbol-function 'magnus-provider-call)
                  (lambda (candidate operation &rest _arguments)
                    (pcase operation
                      ('resume
                       (setq failed-buffer
                             (generate-new-buffer
                              " *failed-resurrect-codex*"))
                       (magnus-instances-update
                        candidate :status 'running :buffer failed-buffer)
                       (error "resume failed"))
                      ('stop
                       (magnus-instances-update
                        candidate :status 'stopped :buffer nil)))))
                 ((symbol-function 'magnus-coord-stop-watching)
                  (lambda (project)
                    (push project stopped-watchers)
                    (setq magnus-coord--watched-dirs
                          (delete project magnus-coord--watched-dirs)))))
              (let ((err
                     (should-error
                      (magnus-process-resurrect-purged instance))))
                (should (string-match-p "resume failed"
                                        (error-message-string err)))))
            (should
             (equal stopped-watchers
                    (list (magnus-test-process-lifecycle--coord-key
                           directory))))
            (should-not magnus-coord--watched-dirs)
            (should-not
             (gethash (magnus-test-process-lifecycle--coord-key directory)
                      magnus-coord--session-start-times))
            (should (eq (magnus-instance-status instance) 'purged))
            (should-not (magnus-instance-buffer instance))
            (should (equal (magnus-instance-session-id instance)
                           "codex-session"))
            (should (= (magnus-instance-purged-at instance) 123.0))
            (should-not (buffer-live-p failed-buffer)))
        (when (buffer-live-p failed-buffer)
          (kill-buffer failed-buffer))
        (delete-directory directory t)))))

(ert-deftest magnus-process-resurrect-rolls-back-external-onboarding-failure ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-resurrect-send-failure-" t))
           (instance
            (magnus-instances-create directory "archived-codex" 'codex))
           runtime
           process
           stopped-watchers
           operations)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'purged :session-id "codex-session"
             :purged-at 321.0)
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_candidate) t))
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (project _candidate)
                    (magnus-test-process-lifecycle--claim-coord project 52)))
                 ((symbol-function 'magnus-provider-call)
                  (lambda (candidate operation &rest _arguments)
                    (push operation operations)
                    (pcase operation
                      ('resume
                       (setq runtime
                             (generate-new-buffer
                              " *failed-resurrect-send*"))
                       (setq process
                             (make-pipe-process
                              :name (generate-new-buffer-name
                                     "failed-resurrect-send")
                              :buffer runtime :noquery t))
                       (magnus-process--record-transaction-runtime
                        runtime process)
                       (magnus-instances-update
                        candidate :status 'running :buffer runtime))
                      ('send (error "onboarding enqueue failed"))
                      ('stop
                       (magnus-instances-update
                        candidate :status 'stopped :buffer nil)))))
                 ((symbol-function 'magnus-coord-stop-watching)
                  (lambda (project)
                    (push project stopped-watchers)
                    (setq magnus-coord--watched-dirs
                          (delete project magnus-coord--watched-dirs)))))
              (let ((err
                     (should-error
                      (magnus-process-resurrect-purged instance))))
                (should
                 (string-match-p "onboarding enqueue failed"
                                 (error-message-string err)))))
            (should (equal (nreverse operations) '(resume send stop)))
            (should
             (equal stopped-watchers
                    (list (magnus-test-process-lifecycle--coord-key
                           directory))))
            (should (eq (magnus-instance-status instance) 'purged))
            (should-not (magnus-instance-buffer instance))
            (should (equal (magnus-instance-session-id instance)
                           "codex-session"))
            (should (= (magnus-instance-purged-at instance) 321.0))
            (should-not (process-live-p process))
            (should-not (buffer-live-p runtime)))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p runtime)
          (kill-buffer runtime))
        (delete-directory directory t)))))

(ert-deftest magnus-process-resurrect-preserves-shared-ownership-on-failure ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-resurrect-claude-" t))
           (instance
            (magnus-instances-create directory "archived-claude"))
           failed-buffer
           stopped-watcher)
      (unwind-protect
          (progn
            (magnus-test-process-lifecycle--claim-coord directory 59)
            (magnus-instances-update
             instance :status 'purged :session-id "claude-session"
             :purged-at 456.0)
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_instance) nil))
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (_project candidate)
                    (should (eq (magnus-instance-status candidate) 'purged))))
                 ((symbol-function 'magnus-process--spawn-with-session)
                  (lambda (candidate session-id)
                    (should (equal session-id "claude-session"))
                    (setq failed-buffer
                          (generate-new-buffer
                           " *failed-resurrect-claude*"))
                    (magnus-instances-update
                     candidate :status 'running :buffer failed-buffer)
                    (error "spawn resume failed")))
                 ((symbol-function 'magnus-coord-stop-watching)
                  (lambda (_project) (setq stopped-watcher t))))
              (let ((err
                     (should-error
                      (magnus-process-resurrect-purged instance))))
                (should (string-match-p "spawn resume failed"
                                        (error-message-string err)))))
            (should-not stopped-watcher)
            (should
             (equal magnus-coord--watched-dirs
                    (list (magnus-test-process-lifecycle--coord-key
                           directory))))
            (should (= (gethash
                        (magnus-test-process-lifecycle--coord-key directory)
                                magnus-coord--session-start-times)
                       59))
            (should (eq (magnus-instance-status instance) 'purged))
            (should-not (magnus-instance-buffer instance))
            (should (equal (magnus-instance-session-id instance)
                           "claude-session"))
            (should (= (magnus-instance-purged-at instance) 456.0))
            (should-not (buffer-live-p failed-buffer)))
        (when (buffer-live-p failed-buffer)
          (kill-buffer failed-buffer))
        (delete-directory directory t)))))

(ert-deftest magnus-process-resurrection-external-sends-complete-onboarding ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-resurrect-onboarding-" t))
           (magnus-onboarding-user-instructions
            "Custom restored response guidance.")
           (instance
            (magnus-instances-create directory "resurrected-codex" 'codex))
           runtime
           process
           operations
           onboarding)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'purged :session-id "codex-resume-me"
             :purged-at 123.0)
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_candidate) t))
                 ((symbol-function 'magnus-coord-register-agent) #'ignore)
                 ((symbol-function 'magnus-provider-call)
                  (lambda (candidate operation &rest arguments)
                    (push operation operations)
                    (pcase operation
                      ('resume
                       (setq runtime
                             (generate-new-buffer
                              " *magnus-resurrected-codex*"))
                       (setq process
                             (make-pipe-process
                              :name (generate-new-buffer-name
                                     "magnus-resurrected-codex")
                              :buffer runtime :noquery t))
                       (magnus-instances-update
                        candidate :status 'running :buffer runtime)
                       ;; Adapters may return the acquired terminal instead of
                       ;; calling Magnus's transaction recorder themselves.
                       runtime)
                      ('send
                       (should (eq (magnus-instance-buffer candidate) runtime))
                       (setq onboarding (car arguments)))
                      (_ (ert-fail (format "Unexpected provider operation: %S"
                                           operation)))))))
              (magnus-process-resurrect-purged instance))
            (should (equal (nreverse operations) '(resume send)))
            (dolist (required
                     (list "You are resurrected-codex"
                           (magnus-instance-id instance)
                           ".claude/agents/resurrected-codex/memory.md"
                           ".magnus-coord.md"
                           ".claude/magnus-instructions.md"
                           "Authorization boundary"
                           "Custom restored response guidance."))
              (should (string-match-p (regexp-quote required) onboarding)))
            (should-not (string-match-p "visible working notebook" onboarding))
            (should-not (string-match-p (regexp-quote "[thinking]") onboarding))
            (should (eq (magnus-instance-status instance) 'running))
            (should-not (magnus-instance-purged-at instance)))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p runtime)
          (kill-buffer runtime))
        (delete-directory directory t)))))

(ert-deftest magnus-process-resurrection-never-onboards-reentrant-replacement ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-resurrect-reentrant-" t))
           (instance
            (magnus-instances-create directory "reentrant-codex" 'codex))
           runtime
           old-process
           replacement-process
           send-called
           stop-called
           stopped-watcher)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'purged :session-id "reentrant-session"
             :purged-at 456.0)
            (cl-letf
                (((symbol-function 'magnus-provider-external-p)
                  (lambda (_candidate) t))
                 ((symbol-function 'magnus-coord-register-agent)
                  (lambda (project _candidate)
                    (magnus-test-process-lifecycle--claim-coord project 53)))
                 ((symbol-function 'magnus-provider-call)
                  (lambda (candidate operation &rest _arguments)
                    (pcase operation
                      ('resume
                       (setq runtime
                             (generate-new-buffer
                              " *magnus-reentrant-resurrection*"))
                       (setq old-process
                             (make-pipe-process
                              :name (generate-new-buffer-name
                                     "magnus-old-reentrant-resurrection")
                              :buffer runtime :noquery t))
                       (magnus-process--record-transaction-runtime
                        runtime old-process)
                       (magnus-instances-update
                        candidate :status 'running :buffer runtime)
                       ;; Model a public update hook installing a replacement
                       ;; process in the same buffer before RESUME returns.
                       (setq replacement-process
                             (make-pipe-process
                              :name (generate-new-buffer-name
                                     "magnus-new-reentrant-resurrection")
                              :buffer runtime :noquery t))
                       runtime)
                      ('send (setq send-called t))
                      ('stop (setq stop-called t)))))
                 ((symbol-function 'magnus-coord-stop-watching)
                  (lambda (_project) (setq stopped-watcher t))))
              (let ((err
                     (should-error
                      (magnus-process-resurrect-purged instance))))
                (should
                 (string-match-p "lost exact runtime ownership"
                                 (error-message-string err)))))
            (should-not send-called)
            (should-not stop-called)
            (should-not stopped-watcher)
            (should-not (process-live-p old-process))
            (should (process-live-p replacement-process))
            (should (buffer-live-p runtime))
            (should (eq (get-buffer-process runtime) replacement-process))
            (should (eq (magnus-instance-buffer instance) runtime))
            (should (eq (magnus-instance-status instance) 'running))
            (should-not (magnus-instance-purged-at instance))
            (should
             (member (magnus-test-process-lifecycle--coord-key directory)
                     magnus-coord--watched-dirs)))
        (when (process-live-p old-process)
          (delete-process old-process))
        (when (process-live-p replacement-process)
          (delete-process replacement-process))
        (when (buffer-live-p runtime)
          (kill-buffer runtime))
        (delete-directory directory t)))))

(ert-deftest magnus-process-project-hash-uses-the-physical-root ()
  (let* ((directory (make-temp-file "magnus-project-hash-" t))
         (link (concat directory "-link")))
    (unwind-protect
        (progn
          (make-symbolic-link directory link)
          (should
           (equal (magnus-process--project-hash directory)
                  (magnus-process--project-hash link))))
      (when (file-symlink-p link)
        (delete-file link))
      (delete-directory directory t))))

(ert-deftest magnus-process-reconnect-normalizes-stale-runtime-state ()
  (magnus-test-process-lifecycle--isolated
    (dolist (status '(running suspended))
      (let ((instance
             (magnus-instances-create
              default-directory
              (format "stale-%s-%s" status (random most-positive-fixnum)))))
        (magnus-instances-update instance :status status)
        (cl-letf (((symbol-function 'magnus-provider-external-p)
                   (lambda (_candidate) nil)))
          (magnus-process-reconnect instance))
        (should (eq (magnus-instance-status instance) 'stopped))
        (should-not (magnus-instance-buffer instance))))
    ;; Durable terminal states are evidence, not runtimes to revive.
    (dolist (status '(purged finished errored))
      (let ((instance
             (magnus-instances-create
              default-directory
              (format "terminal-%s-%s" status
                      (random most-positive-fixnum)))))
        (magnus-instances-update instance :status status)
        (cl-letf (((symbol-function 'magnus-provider-external-p)
                   (lambda (_candidate) nil)))
          (magnus-process-reconnect instance))
        (should (eq (magnus-instance-status instance) status))))
    ;; External terminals also cannot survive Emacs, including any stale
    ;; suspended state loaded from older or hand-edited storage.
    (dolist (status '(running suspended))
      (let ((instance
             (magnus-instances-create
              default-directory
              (format "external-%s-%s" status
                      (random most-positive-fixnum))
              'codex)))
        (magnus-instances-update instance :status status)
        (cl-letf (((symbol-function 'magnus-provider-external-p)
                   (lambda (_candidate) t)))
          (magnus-process-reconnect instance))
        (should (eq (magnus-instance-status instance) 'stopped))
        (should-not (magnus-instance-buffer instance))))))

(ert-deftest magnus-process-reconnect-requires-a-live-local-process ()
  (magnus-test-process-lifecycle--isolated
    (let* ((name (format "reconnect-%s" (random most-positive-fixnum)))
           (instance (magnus-instances-create default-directory name))
           (buffer (generate-new-buffer (format "*claude:%s*" name))))
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'suspended :buffer buffer)
            (cl-letf (((symbol-function 'magnus-provider-external-p)
                       (lambda (_candidate) nil)))
              (magnus-process-reconnect instance))
            ;; An untagged display-name collision is not ours to destroy.
            (should (buffer-live-p buffer))
            (should-not (magnus-instance-buffer instance))
            (should (eq (magnus-instance-status instance) 'stopped)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magnus-process-reconnect-preserves-a-live-suspension ()
  (magnus-test-process-lifecycle--isolated
    (let* ((name (format "live-reconnect-%s"
                         (random most-positive-fixnum)))
           (instance (magnus-instances-create default-directory name))
           (buffer (generate-new-buffer (format "*claude:%s*" name)))
           (process (make-pipe-process
                     :name (generate-new-buffer-name "magnus-live-reconnect")
                     :buffer buffer :noquery t)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local magnus-process--instance-id
                          (magnus-instance-id instance)))
            (magnus-instances-update instance :status 'suspended :buffer nil)
            (cl-letf (((symbol-function 'magnus-provider-external-p)
                       (lambda (_candidate) nil)))
              (magnus-process-reconnect instance))
            (should (eq (magnus-instance-buffer instance) buffer))
            (should (eq (magnus-instance-status instance) 'suspended)))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magnus-process-reconnect-ignores-an-untagged-name-collision ()
  (magnus-test-process-lifecycle--isolated
    (let* ((name (format "reconnect-collision-%s"
                         (random most-positive-fixnum)))
           (instance (magnus-instances-create default-directory name))
           (collision (generate-new-buffer (format "*claude:%s*" name)))
           (collision-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-reconnect-collision")
             :buffer collision :noquery t)))
      (unwind-protect
          (progn
            (magnus-instances-update instance :status 'running :buffer nil)
            (cl-letf (((symbol-function 'magnus-provider-external-p)
                       (lambda (_candidate) nil)))
              (magnus-process-reconnect instance))
            (should (eq (magnus-instance-status instance) 'stopped))
            (should-not (magnus-instance-buffer instance))
            (should (buffer-live-p collision))
            (should (process-live-p collision-process)))
        (when (process-live-p collision-process)
          (delete-process collision-process))
        (when (buffer-live-p collision)
          (kill-buffer collision))))))

(ert-deftest magnus-process-reconnect-finds-a-tagged-suffixed-terminal ()
  (magnus-test-process-lifecycle--isolated
    (let* ((name (format "reconnect-suffix-%s"
                         (random most-positive-fixnum)))
           (instance (magnus-instances-create default-directory name))
           (collision (generate-new-buffer (format "*claude:%s*" name)))
           (collision-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-reconnect-decoy")
             :buffer collision :noquery t))
           (owned (generate-new-buffer (format "*claude:%s*" name)))
           (owned-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-reconnect-owned")
             :buffer owned :noquery t)))
      (unwind-protect
          (progn
            (should-not (string= (buffer-name owned)
                                (format "*claude:%s*" name)))
            (with-current-buffer owned
              (setq-local magnus-process--instance-id
                          (magnus-instance-id instance)))
            (magnus-instances-update instance :status 'running :buffer nil)
            (cl-letf (((symbol-function 'magnus-provider-external-p)
                       (lambda (_candidate) nil)))
              (magnus-process-reconnect instance))
            (should (eq (magnus-instance-buffer instance) owned))
            (should (eq (magnus-instance-status instance) 'running))
            (should (process-live-p owned-process))
            (should (process-live-p collision-process)))
        (when (process-live-p collision-process)
          (delete-process collision-process))
        (when (process-live-p owned-process)
          (delete-process owned-process))
        (when (buffer-live-p collision)
          (kill-buffer collision))
        (when (buffer-live-p owned)
          (kill-buffer owned))))))

(ert-deftest magnus-process-reconnect-rehomes-exit-callback-to-restored-instance ()
  (magnus-test-process-lifecycle--isolated
    (let* ((old-instance
            (magnus-instances-create default-directory "restored-terminal"))
           (restored
            (magnus-instances-deserialize
             (magnus-instances-serialize old-instance)))
           (buffer (generate-new-buffer " *magnus-restored-terminal*"))
           (process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-restored-terminal")
             :buffer buffer :noquery t))
           sentinel
           (magnus-buffer-name " *magnus-status-not-present*"))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local magnus-process--instance-id
                          (magnus-instance-id old-instance)))
            (magnus-instances-update
             old-instance :status 'running :buffer buffer)
            (magnus-process--setup-sentinel old-instance buffer)
            ;; Model persistence atomically replacing the registry object.
            (setq magnus-instances (list restored))
            (cl-letf (((symbol-function 'magnus-provider-external-p)
                       (lambda (_candidate) nil)))
              (magnus-process-reconnect restored))
            (should (eq (magnus-instance-buffer restored) buffer))
            (setq sentinel (process-sentinel process))
            (set-process-sentinel process nil)
            (delete-process process)
            (funcall sentinel process "finished")
            (should (eq (magnus-instance-status restored) 'stopped))
            ;; The orphan is no longer a callback target.
            (should (eq (magnus-instance-status old-instance) 'running)))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magnus-process-sentinel-updates-only-its-current-buffer-owner ()
  (magnus-test-process-lifecycle--isolated
    (let* ((instance
            (magnus-instances-create default-directory "sentinel-owner"))
           (old-buffer (generate-new-buffer " *magnus-old-terminal*"))
           (old-process (make-pipe-process
                         :name (generate-new-buffer-name "magnus-old-terminal")
                         :buffer old-buffer :noquery t))
           (replacement
            (generate-new-buffer " *magnus-new-terminal*"))
           sentinel
           (magnus-buffer-name " *magnus-status-not-present*"))
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'running :buffer old-buffer)
            (magnus-process--setup-sentinel instance old-buffer)
            (setq sentinel (process-sentinel old-process))
            (set-process-sentinel old-process nil)
            (magnus-instances-update
             instance :status 'running :buffer replacement)
            (delete-process old-process)
            (funcall sentinel old-process "finished")
            (should (eq (magnus-instance-buffer instance) replacement))
            (should (eq (magnus-instance-status instance) 'running)))
        (when (process-live-p old-process)
          (delete-process old-process))
        (when (buffer-live-p old-buffer)
          (kill-buffer old-buffer))
        (when (buffer-live-p replacement)
          (kill-buffer replacement))))))

(ert-deftest magnus-process-sentinel-stops-its-current-buffer-owner ()
  (magnus-test-process-lifecycle--isolated
    (let* ((instance
            (magnus-instances-create default-directory "sentinel-current"))
           (buffer (generate-new-buffer " *magnus-current-terminal*"))
           (process (make-pipe-process
                     :name (generate-new-buffer-name "magnus-current-terminal")
                     :buffer buffer :noquery t))
           sentinel
           (magnus-buffer-name " *magnus-status-not-present*"))
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'running :buffer buffer)
            (magnus-process--setup-sentinel instance buffer)
            (setq sentinel (process-sentinel process))
            (set-process-sentinel process nil)
            (delete-process process)
            (funcall sentinel process "finished")
            (should (eq (magnus-instance-buffer instance) buffer))
            (should (eq (magnus-instance-status instance) 'stopped)))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magnus-process-sentinel-rejects-a-same-buffer-replacement ()
  (magnus-test-process-lifecycle--isolated
    (let* ((instance
            (magnus-instances-create default-directory "same-buffer-owner"))
           (buffer (generate-new-buffer " *magnus-shared-terminal*"))
           (old-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-old-shared-terminal")
             :buffer buffer :noquery t))
           replacement-process
           sentinel
           (magnus-buffer-name " *magnus-status-not-present*"))
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'running :buffer buffer)
            (magnus-process--setup-sentinel instance buffer)
            (setq sentinel (process-sentinel old-process))
            (set-process-sentinel old-process nil)
            (setq replacement-process
                  (make-pipe-process
                   :name (generate-new-buffer-name
                          "magnus-new-shared-terminal")
                   :buffer buffer :noquery t))
            (should (eq (get-buffer-process buffer) replacement-process))
            (delete-process old-process)
            (funcall sentinel old-process "finished")
            (should (eq (magnus-instance-buffer instance) buffer))
            (should (eq (magnus-instance-status instance) 'running)))
        (when (process-live-p old-process)
          (delete-process old-process))
        (when (process-live-p replacement-process)
          (delete-process replacement-process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magnus-process-delayed-stop-preserves-same-buffer-replacement ()
  (magnus-test-process-lifecycle--isolated
    (let* ((instance
            (magnus-instances-create default-directory "local-stop-owner"))
           (buffer (generate-new-buffer " *magnus-shared-stop-terminal*"))
           (old-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-old-stop-terminal")
             :buffer buffer :noquery t))
           replacement-process
           delayed-stop
           sent-key)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'running :buffer buffer)
            (cl-letf (((symbol-function 'vterm-send-key)
                       (lambda (key &rest _arguments) (setq sent-key key)))
                      ((symbol-function 'run-with-timer)
                       (lambda (_seconds _repeat function &rest arguments)
                         (setq delayed-stop (cons function arguments))
                         'test-timer)))
              (magnus-process-kill instance))
            (should (equal sent-key "C-c"))
            (should delayed-stop)
            (setq replacement-process
                  (make-pipe-process
                   :name (generate-new-buffer-name
                          "magnus-new-stop-terminal")
                   :buffer buffer :noquery t))
            (apply (car delayed-stop) (cdr delayed-stop))
            (should-not (process-live-p old-process))
            (should (process-live-p replacement-process))
            (should (buffer-live-p buffer))
            (should (eq (get-buffer-process buffer) replacement-process)))
        (when (process-live-p old-process)
          (delete-process old-process))
        (when (process-live-p replacement-process)
          (delete-process replacement-process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magnus-process-switch-rejects-archived-and-missing-terminal-output ()
  (magnus-test-process-lifecycle--isolated
    (dolist (status '(purged finished errored))
      (let ((instance
             (magnus-instances-create
              default-directory
              (format "unvisitable-%s-%s" status
                      (random most-positive-fixnum)))))
        (magnus-instances-update instance :status status)
        (cl-letf (((symbol-function 'magnus-provider-external-p)
                   (lambda (_candidate)
                     (ert-fail "terminal state must fail before dispatch")))
                  ((symbol-function 'magnus-process--spawn)
                   (lambda (&rest _arguments)
                     (ert-fail "terminal state must not spawn"))))
          (should-error (magnus-process-switch-to instance)
                        :type 'user-error))
        (should (eq (magnus-instance-status instance) status))))))

(ert-deftest magnus-process-switch-opens-retained-headless-output ()
  (magnus-test-process-lifecycle--isolated
    (dolist (status '(finished errored))
      (let* ((instance
              (magnus-instances-create
               default-directory
               (format "output-%s-%s" status
                       (random most-positive-fixnum))
               nil 'headless))
             (buffer (generate-new-buffer " *magnus-retained-output*"))
             switched)
        (unwind-protect
            (progn
              (magnus-instances-update
               instance :status status :buffer buffer)
              (cl-letf (((symbol-function 'switch-to-buffer)
                         (lambda (candidate) (setq switched candidate)))
                        ((symbol-function 'magnus-provider-external-p)
                         (lambda (_candidate)
                           (ert-fail "retained output must not dispatch")))
                        ((symbol-function 'magnus-process--spawn)
                         (lambda (&rest _arguments)
                           (ert-fail "retained output must not spawn"))))
                (magnus-process-switch-to instance))
              (should (eq switched buffer))
              (should (eq (magnus-instance-status instance) status)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest magnus-process-persisted-headless-task-never-becomes-interactive ()
  "A restored task without its output cannot spawn or resurrect a TUI."
  (magnus-test-process-lifecycle--isolated
    (let* ((created
            (magnus-instances-create
             default-directory "restored-headless" nil 'headless))
           (_ (magnus-instances-update
               created :status 'running :session-id "headless-session"))
           (instance
            (magnus-instances-deserialize
             (magnus-instances-serialize created))))
      ;; Persistence reloads a task that was running when Emacs exited.  Its
      ;; non-surviving local process normalizes to stopped without losing kind.
      (magnus-process-reconnect instance)
      (should (eq (magnus-instance-status instance) 'stopped))
      (cl-letf (((symbol-function 'magnus-provider-external-p)
                 (lambda (_candidate)
                   (ert-fail "headless guard must run before dispatch")))
                ((symbol-function 'magnus-process--spawn)
                 (lambda (&rest _arguments)
                   (ert-fail "headless task must not spawn a TUI")))
                ((symbol-function 'magnus-process--spawn-with-session)
                 (lambda (&rest _arguments)
                   (ert-fail "headless task must not resume a TUI"))))
        (should-error (magnus-process-switch-to instance) :type 'user-error)
        (magnus-instances-update instance :status 'purged :purged-at 10.0)
        (should-error (magnus-process-resurrect-purged instance)
                      :type 'user-error))
      (should (eq (magnus-instance-kind instance) 'headless))
      (should-not (magnus-instance-buffer instance)))))

(ert-deftest magnus-process-switch-replaces-a-processless-terminal ()
  (magnus-test-process-lifecycle--isolated
    (let* ((name (format "replace-terminal-%s"
                         (random most-positive-fixnum)))
           (instance (magnus-instances-create default-directory name))
           (stale (generate-new-buffer (format "*claude:%s*" name)))
           (replacement (generate-new-buffer " *magnus-replacement*"))
           resumed-session
           switched)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'stopped :buffer stale
             :session-id "resume-this-session")
            (cl-letf (((symbol-function 'magnus-provider-external-p)
                       (lambda (_candidate) nil))
                      ((symbol-function 'magnus-process--spawn-with-session)
                       (lambda (candidate session-id)
                         (setq resumed-session session-id)
                         (magnus-instances-update
                          candidate :status 'running :buffer replacement)))
                      ((symbol-function 'switch-to-buffer)
                       (lambda (candidate) (setq switched candidate))))
              (magnus-process-switch-to instance))
            (should-not (buffer-live-p stale))
            (should (equal resumed-session "resume-this-session"))
            (should (eq switched replacement)))
        (when (buffer-live-p stale)
          (kill-buffer stale))
        (when (buffer-live-p replacement)
          (kill-buffer replacement))))))

(ert-deftest magnus-process-resumed-claude-schedules-complete-onboarding ()
  (magnus-test-process-lifecycle--isolated
    (let* ((directory (make-temp-file "magnus-resumed-claude-onboarding-" t))
           (magnus-onboarding-user-instructions
            "Custom Claude restore guidance.")
           (instance
            (magnus-instances-create directory "restored-claude"))
           (terminal
            (generate-new-buffer " *magnus-restored-claude*"))
           (process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-restored-claude")
             :buffer terminal :noquery t))
           scheduled
           command
           submission)
      (unwind-protect
          (progn
            (cl-letf
                (((symbol-function 'magnus-terminal-create-buffer)
                  (lambda (&rest _arguments) terminal))
                 ((symbol-function 'magnus-process--claude-command)
                  (lambda (&rest arguments)
                    (setq command arguments)
                    "claude --resume claude-session"))
                 ((symbol-function 'vterm-send-string) #'ignore)
                 ((symbol-function 'magnus-process--setup-sentinel) #'ignore)
                 ((symbol-function 'run-with-timer)
                  (lambda (_seconds _repeat function &rest arguments)
                    (push (cons function arguments) scheduled)
                    nil)))
              (should
               (eq (magnus-process--spawn-with-session
                    instance "claude-session")
                   terminal)))
            (should (equal command '("--resume" "claude-session")))
            (let ((callback
                   (assq #'magnus-process--send-onboarding scheduled)))
              (should callback)
              (should (equal (cdr callback)
                             (list instance nil terminal process)))
              (cl-letf
                  (((symbol-function 'magnus-terminal-submit)
                    (lambda (candidate text accepted &rest arguments)
                      (setq submission
                            (list candidate text accepted arguments))
                      'submitted)))
                (apply (car callback) (cdr callback))))
            (should (eq (car submission) instance))
            (dolist (required
                     (list "You are restored-claude"
                           (magnus-instance-id instance)
                           ".claude/agents/restored-claude/memory.md"
                           ".magnus-coord.md"
                           "Authorization boundary"
                           "Custom Claude restore guidance."))
              (should
               (string-match-p (regexp-quote required) (cadr submission))))
            (should-not
             (string-match-p "visible working notebook" (cadr submission))))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p terminal)
          (kill-buffer terminal))
        (delete-directory directory t)))))

(ert-deftest magnus-process-resume-spawn-failure-discards-partial-terminal ()
  (magnus-test-process-lifecycle--isolated
    (let ((instance
           (magnus-instances-create default-directory "failed-resume"))
          terminal)
      (cl-letf (((symbol-value 'magnus-claude-executable) "claude")
                ((symbol-function 'magnus-terminal-create-buffer)
                 (lambda (&rest _arguments)
                   (setq terminal
                         (generate-new-buffer " *magnus-failed-resume*"))))
                ((symbol-function 'vterm-send-string)
                 (lambda (&rest _arguments)
                   (error "simulated resume send failure"))))
        (should-error
         (magnus-process--spawn-with-session instance "session-to-resume")))
      (should-not (buffer-live-p terminal))
      (should-not (magnus-instance-buffer instance))
      (should (eq (magnus-instance-status instance) 'stopped)))))

(ert-deftest magnus-process-stale-onboarding-cannot-steer-a-replacement ()
  (magnus-test-process-lifecycle--isolated
    (let* ((instance
            (magnus-instances-create default-directory "late-onboarding"))
           (old-buffer (generate-new-buffer " *magnus-old-onboarding*"))
           (old-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-old-onboarding")
             :buffer old-buffer :noquery t))
           (replacement
            (generate-new-buffer " *magnus-new-onboarding*"))
           (replacement-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-new-onboarding")
             :buffer replacement :noquery t))
           submission
           ready)
      (unwind-protect
          (cl-letf (((symbol-function 'magnus-process--onboarding-message)
                     (lambda (&rest _arguments) "hello\nreplacement"))
                    ((symbol-function 'magnus-terminal-submit)
                     (lambda (candidate text accepted &rest arguments)
                       (setq submission
                             (list candidate text accepted arguments))
                       'submitted)))
            ;; The five-second launch timer must become a no-op if this launch
            ;; lost ownership before it even tried to submit onboarding.
            (magnus-instances-update
             instance :status 'running :buffer replacement)
            (magnus-process--send-onboarding
             instance nil old-buffer old-process)
            (should-not submission)

            ;; The terminal arbiter owns text plus Return atomically.  Its
            ;; acceptance callback still rechecks runtime ownership before
            ;; publishing readiness.
            (magnus-instances-update
             instance :status 'running :buffer old-buffer)
            (let ((magnus-process-ready-hook
                   (list (lambda (_candidate) (setq ready t)))))
              (magnus-process--send-onboarding
               instance nil old-buffer old-process)
              (should (eq (car submission) instance))
              (should (equal (cadr submission) "hello replacement"))
              (should (equal (nth 3 submission)
                             '(:settle-delay 0.5 :scope magnus-onboarding
                               :deduplicate t)))
              (magnus-instances-update
               instance :status 'running :buffer replacement)
              (funcall (nth 2 submission)))
            (should-not ready))
        (when (process-live-p old-process)
          (delete-process old-process))
        (when (process-live-p replacement-process)
          (delete-process replacement-process))
        (when (buffer-live-p old-buffer)
          (kill-buffer old-buffer))
        (when (buffer-live-p replacement)
          (kill-buffer replacement))))))

(ert-deftest magnus-process-stale-resume-ready-timer-cannot-release-work ()
  (magnus-test-process-lifecycle--isolated
    (let* ((instance
            (magnus-instances-create default-directory "late-resume-ready"))
           (old-buffer (generate-new-buffer " *magnus-old-resume-ready*"))
           (old-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-old-resume-ready")
             :buffer old-buffer :noquery t))
           (replacement
            (generate-new-buffer " *magnus-new-resume-ready*"))
           (replacement-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-new-resume-ready")
             :buffer replacement :noquery t))
           ready
           returns)
      (unwind-protect
          (let ((magnus-process-ready-hook
                 (list (lambda (_candidate) (push 'ready ready)))))
            (magnus-instances-update
             instance :status 'running :buffer replacement)
            ;; Model both delayed callbacks installed by the old resume.
            (cl-letf (((symbol-function 'vterm-send-return)
                       (lambda () (push (current-buffer) returns))))
              (magnus-process--send-return-if-owner
               instance old-buffer old-process))
            (magnus-process--run-ready-hook
             instance old-buffer old-process)
            (should-not returns)
            (should-not ready)

            ;; The same callbacks still work for the exact current owner.
            (cl-letf (((symbol-function 'vterm-send-return)
                       (lambda () (push (current-buffer) returns))))
              (magnus-process--send-return-if-owner
               instance replacement replacement-process))
            (magnus-process--run-ready-hook
             instance replacement replacement-process)
            (should (equal returns (list replacement)))
            (should (equal ready '(ready))))
        (when (process-live-p old-process)
          (delete-process old-process))
        (when (process-live-p replacement-process)
          (delete-process replacement-process))
        (when (buffer-live-p old-buffer)
          (kill-buffer old-buffer))
        (when (buffer-live-p replacement)
          (kill-buffer replacement))))))

(ert-deftest magnus-process-headless-completion-cannot-overwrite-replacement ()
  (magnus-test-process-lifecycle--isolated
    (let* ((instance
            (magnus-instances-create default-directory "late-headless"))
           (old-buffer (generate-new-buffer " *magnus-old-headless*"))
           (old-process (make-pipe-process
                         :name (generate-new-buffer-name "magnus-old-headless")
                         :buffer old-buffer :noquery t))
           (replacement
            (generate-new-buffer " *magnus-headless-replacement*"))
           (magnus-buffer-name " *magnus-status-not-present*")
           logged)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'running :buffer old-buffer)
            (set-process-sentinel old-process nil)
            (delete-process old-process)
            (magnus-instances-update
             instance :status 'running :buffer replacement)
            (cl-letf (((symbol-function 'magnus-coord-add-log)
                       (lambda (&rest arguments) (setq logged arguments))))
              (magnus-process--headless-complete
               instance old-process
               '(:success-p t :status exit :process-event "finished")
               old-buffer))
            (should (eq (magnus-instance-buffer instance) replacement))
            (should (eq (magnus-instance-status instance) 'running))
            (should-not logged)
            (with-current-buffer replacement
              (should-not (string-match-p "Process finished"
                                          (buffer-string)))))
        (when (process-live-p old-process)
          (delete-process old-process))
        (when (buffer-live-p old-buffer)
          (kill-buffer old-buffer))
        (when (buffer-live-p replacement)
          (kill-buffer replacement))))))

(ert-deftest magnus-process-headless-completion-rejects-same-buffer-replacement ()
  (magnus-test-process-lifecycle--isolated
    (let* ((instance
            (magnus-instances-create default-directory "shared-headless"))
           (buffer (generate-new-buffer " *magnus-shared-headless*"))
           (old-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-old-headless-owner")
             :buffer buffer :noquery t))
           replacement-process
           (magnus-buffer-name " *magnus-status-not-present*")
           logged)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'running :buffer buffer)
            (setq replacement-process
                  (make-pipe-process
                   :name (generate-new-buffer-name
                          "magnus-new-headless-owner")
                   :buffer buffer :noquery t))
            (should (eq (get-buffer-process buffer) replacement-process))
            (cl-letf (((symbol-function 'magnus-coord-add-log)
                       (lambda (&rest arguments) (setq logged arguments))))
              (magnus-process--headless-complete
               instance old-process
               '(:success-p t :status exit :process-event "finished")
               buffer))
            (should (eq (magnus-instance-status instance) 'running))
            (should-not logged)
            (with-current-buffer buffer
              (should-not (string-match-p "Process finished"
                                          (buffer-string)))))
        (when (process-live-p old-process)
          (delete-process old-process))
        (when (process-live-p replacement-process)
          (delete-process replacement-process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magnus-process-headless-render-rejects-same-buffer-replacement ()
  (magnus-test-process-lifecycle--isolated
    (let* ((instance
            (magnus-instances-create default-directory "shared-headless-output"))
           (buffer (generate-new-buffer " *magnus-shared-headless-output*"))
           (old-process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-old-headless-output")
             :buffer buffer :noquery t))
           replacement-process)
      (unwind-protect
          (progn
            (with-current-buffer buffer (insert "replacement output\n"))
            (magnus-instances-update
             instance :status 'running :buffer buffer)
            (setq replacement-process
                  (make-pipe-process
                   :name (generate-new-buffer-name
                          "magnus-new-headless-output")
                   :buffer buffer :noquery t))
            (magnus-process--headless-render-event
             instance old-process '(:type "assistant" :text "stale\n") buffer)
            (with-current-buffer buffer
              (should (equal (buffer-string) "replacement output\n")))
            ;; The exact current owner remains able to render normally.
            (magnus-process--headless-render-event
             instance replacement-process
             '(:type "assistant" :text "current\n") buffer)
            (with-current-buffer buffer
              (should (equal (buffer-string)
                             "replacement output\ncurrent\n"))))
        (when (process-live-p old-process)
          (delete-process old-process))
        (when (process-live-p replacement-process)
          (delete-process replacement-process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magnus-process-headless-completes-after-output-buffer-is-killed ()
  (magnus-test-process-lifecycle--isolated
    (let* ((instance
            (magnus-instances-create
             default-directory "killed-output" nil 'headless))
           (buffer (generate-new-buffer " *magnus-killed-headless-output*"))
           (process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-killed-headless-output")
             :buffer buffer :noquery t))
           (magnus-buffer-name " *magnus-status-not-present*")
           logged)
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'running :buffer buffer)
            (kill-buffer buffer)
            (should (magnus-process--headless-p instance))
            (cl-letf (((symbol-function 'magnus-coord-add-log)
                       (lambda (&rest arguments) (setq logged arguments))))
              (magnus-process--headless-complete
               instance process
               '(:success-p t :status exit :process-event "finished")
               buffer))
            (should-not (buffer-live-p buffer))
            (should (eq (magnus-instance-buffer instance) buffer))
            (should (eq (magnus-instance-status instance) 'finished))
            (should
             (equal logged
                    (list (magnus-instance-directory instance)
                          "killed-output" "Headless task finished"))))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magnus-process-chdir-rejects-terminal-lifecycle-states ()
  (magnus-test-process-lifecycle--isolated
    (let ((old-directory (make-temp-file "magnus-chdir-terminal-old-" t))
          (new-directory (make-temp-file "magnus-chdir-terminal-new-" t)))
      (unwind-protect
          (dolist (status '(purged finished errored))
            (let* ((instance
                    (magnus-instances-create
                     old-directory
                     (format "immovable-%s-%s" status
                             (random most-positive-fixnum))))
                   (_ (magnus-instances-update
                       instance :status status :purged-at 123.0))
                   (before (magnus-instances-serialize instance)))
              (cl-letf (((symbol-function 'magnus-provider-external-p)
                         (lambda (_candidate)
                           (ert-fail "guard must run before provider dispatch")))
                        ((symbol-function 'magnus-coord--normalized-directory)
                         (lambda (_directory)
                           (ert-fail "guard must run before path work"))))
                (should-error (magnus-process-chdir instance new-directory)
                              :type 'user-error))
              (should (equal (magnus-instances-serialize instance) before))))
        (delete-directory old-directory t)
        (delete-directory new-directory t)))))

(ert-deftest magnus-process-live-headless-task-rejects-interactive-lifecycle ()
  (magnus-test-process-lifecycle--isolated
    (let* ((old-directory (make-temp-file "magnus-headless-live-old-" t))
           (new-directory (make-temp-file "magnus-headless-live-new-" t))
           (buffer (generate-new-buffer " *magnus-headless-live*"))
           (process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-headless-live")
             :buffer buffer :noquery t))
           (instance
            (magnus-instances-create
             old-directory "live-headless" nil 'headless)))
      (unwind-protect
          (progn
            (magnus-instances-update
             instance :status 'running :buffer buffer)
            (cl-letf (((symbol-function 'magnus-provider-external-p)
                       (lambda (_candidate) nil))
                      ((symbol-function 'signal-process)
                       (lambda (&rest _arguments)
                         (ert-fail "headless guard must run before signaling"))))
              (should-error (magnus-process-suspend instance)
                            :type 'user-error)
              (should-error (magnus-process-resume instance)
                            :type 'user-error)
              (should-error (magnus-process-chdir instance new-directory)
                            :type 'user-error))
            (should (process-live-p process))
            (should (eq (magnus-instance-status instance) 'running))
            (should (equal (magnus-instance-directory instance)
                           old-directory)))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (delete-directory old-directory t)
        (delete-directory new-directory t)))))

(ert-deftest magnus-process-resume-ignores-dead-local-process ()
  (magnus-test-process-lifecycle--isolated
    (let* ((buffer (generate-new-buffer " *magnus-dead-resume*"))
           (process
            (make-pipe-process
             :name (generate-new-buffer-name "magnus-dead-resume")
             :buffer buffer :noquery t))
           (instance
            (magnus-instances-create default-directory "dead-resume")))
      (unwind-protect
          (progn
            (delete-process process)
            (magnus-instances-update
             instance :status 'suspended :buffer buffer)
            (cl-letf (((symbol-function 'magnus-provider-external-p)
                       (lambda (_candidate) nil))
                      ((symbol-function 'get-buffer-process)
                       (lambda (_buffer) process))
                      ((symbol-function 'signal-process)
                       (lambda (&rest _arguments)
                         (ert-fail "dead process must not be signaled"))))
              (magnus-process-resume instance))
            (should (eq (magnus-instance-status instance) 'suspended)))
        (when (process-live-p process)
          (delete-process process))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest magnus-process-archive-purges-before-unregistering ()
  (dolist (provider '(claude codex))
    (magnus-test-process-lifecycle--isolated
      (let* ((directory (make-temp-file "magnus-archive-order-" t))
             (instance
              (magnus-instances-create directory "archive-me" provider))
             unregister-status)
        (unwind-protect
            (progn
              (magnus-instances-update instance :status 'running)
              (cl-letf
                  (((symbol-function 'magnus-provider-external-p)
                    (lambda (_instance) (eq provider 'codex)))
                   ((symbol-function 'magnus-provider-call)
                    (lambda (candidate operation &rest _arguments)
                      (should (eq operation 'stop))
                      (should-not
                       (eq (magnus-instance-status candidate) 'purged))
                      (magnus-instances-update candidate :status 'stopped)))
                   ((symbol-function 'magnus-coord-unregister-agent)
                    (lambda (_directory candidate)
                      (setq unregister-status
                            (magnus-instance-status candidate))))
                   ((symbol-function 'magnus--agents-index-update) #'ignore))
                (magnus-process-archive instance))
              (should (eq unregister-status 'purged))
              (should (eq (magnus-instance-status instance) 'purged)))
          (delete-directory directory t))))))

(provide 'magnus-process-lifecycle-tests)
;;; magnus-process-lifecycle-tests.el ends here
