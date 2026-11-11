;;; magnus-forget-tests.el --- Archived-agent forgetting tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-forget)

(defmacro magnus-test-forget--isolated (&rest body)
  "Run BODY with isolated registries and per-agent caches."
  (declare (indent 0) (debug t))
  `(let ((magnus-instances nil)
         (magnus-instances-changed-hook nil)
         (magnus-reviews nil)
         (magnus--agents-index nil)
         (magnus-attention-queue nil)
         (magnus-attention-current nil)
         (magnus-attention--focus-timer nil)
         (magnus-health--state (make-hash-table :test #'equal))
         (magnus-coord--buffer-ticks (make-hash-table :test #'equal))
         (magnus-coord--last-nudge (make-hash-table :test #'equal))
         (magnus-coord--presence-states (make-hash-table :test #'equal))
         (magnus-coord--context-warned (make-hash-table :test #'equal))
         (magnus-coord--attention-data (make-hash-table :test #'equal))
         (magnus-coord--attention-save-timer nil)
         (magnus-coord--presence-retry-timer nil)
         (magnus-background--queue nil)
         (magnus-background--active nil)
         (magnus-background--dispatching nil))
     ,@body))

(defun magnus-test-forget--instance (directory id name &optional status)
  "Return a fixture instance in DIRECTORY with ID, NAME, and STATUS."
  (magnus-instance--create
   :id id :name name :directory directory :provider 'claude
   :kind 'interactive :status (or status 'purged)
   :session-id (format "session-%s" id)
   :previous-session-id (format "previous-%s" id)
   :created-at 10.0 :purged-at 20.0))

(defun magnus-test-forget--home (instance)
  "Return INSTANCE's identity home."
  (directory-file-name
   (file-name-directory (magnus-onboarding-memory-path instance))))

(defun magnus-test-forget--read-form (file)
  "Read the sole Lisp form stored in FILE without reader evaluation."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((read-eval nil))
      (read (current-buffer)))))

(ert-deftest magnus-forget-archives-home-and-cleans-exact-owned-state ()
  (magnus-test-forget--isolated
    (let* ((root (make-temp-file "magnus-forget-success-" t))
           (project (expand-file-name "project" root))
           (magnus-forgotten-agents-directory
            (expand-file-name "magnus/forgotten-agents" root))
           (magnus-state-file (expand-file-name "magnus/state.el" root))
           (magnus-agents-index-file
            (expand-file-name "magnus/agents-index.el" root))
           (magnus-attention-data-file
            (expand-file-name "magnus/attention.el" root))
           (instance
            (magnus-test-forget--instance project "agent-1" "calm-owl"))
           (home (magnus-test-forget--home instance))
           (record (magnus-forget-record-directory instance))
           (terminal (generate-new-buffer " *magnus-forget-terminal*"))
           (trace (generate-new-buffer " *magnus-forget-trace*"))
           (changed 0)
           cancelled reconciled)
      (unwind-protect
          (progn
            (make-directory home t)
            (with-temp-file (expand-file-name "memory.md" home)
              (insert "I remember the whole project.\n"))
            (with-temp-file (expand-file-name "notes.txt" home)
              (insert "private notes\n"))
            (make-symbolic-link "memory.md"
                                (expand-file-name "memory-link" home))
            (setq magnus-instances (list instance)
                  magnus-attention-queue (list (magnus-instance-id instance))
                  magnus-attention-current (magnus-instance-id instance)
                  magnus--agents-index
                  (list (cons project (list (cons "calm-owl" "elisp")))))
            (puthash "agent-1" 'health magnus-health--state)
            (puthash "agent-1" 'tick magnus-coord--buffer-ticks)
            (puthash "agent-1" 12 magnus-coord--last-nudge)
            (puthash "agent-1" 'presence magnus-coord--presence-states)
            (puthash "agent-1" t magnus-coord--context-warned)
            (puthash "calm-owl" '(:visits (1.0) :messages 2)
                     magnus-coord--attention-data)
            (with-current-buffer terminal
              (setq-local magnus-process--instance-id "agent-1"))
            (with-current-buffer trace
              (setq-local magnus-trace--instance instance))
            (setq magnus-instances-changed-hook
                  (list (lambda () (cl-incf changed))))
            (cl-letf (((symbol-function 'magnus-background-cancel)
                       (lambda (key) (setq cancelled key) 1))
                      ((symbol-function 'magnus-coord-reconcile)
                       (lambda (directory) (push directory reconciled))))
              (should (= (magnus-forget-archived-instances (list instance)) 1)))
            (should-not magnus-instances)
            (should (= changed 1))
            (should-not (file-exists-p home))
            (should-not
             (file-exists-p
              (expand-file-name ".magnus-forget-agent-1"
                                (file-name-directory home))))
            (should (file-directory-p record))
            (should (equal
                     (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name "home/memory.md" record))
                       (buffer-string))
                     "I remember the whole project.\n"))
            (should (equal (file-symlink-p
                            (expand-file-name "home/memory-link" record))
                           "memory.md"))
            (let* ((saved
                    (magnus-test-forget--read-form
                     (expand-file-name "record.el" record)))
                   (serialized (plist-get saved :instance)))
              (should (= (plist-get saved :schema-version) 1))
              (should (equal (plist-get saved :source-home) home))
              (should (plist-get saved :home-present))
              (should (equal serialized
                             (magnus-instances-serialize instance)))
              (should (equal (plist-get serialized :id) "agent-1"))
              (should (equal (plist-get serialized :session-id)
                             "session-agent-1"))
              (should (equal (plist-get serialized :previous-session-id)
                             "previous-agent-1")))
            (should (equal (magnus-test-forget--read-form magnus-state-file)
                           nil))
            (should (equal cancelled '(expertise "agent-1")))
            (should (equal reconciled (list project)))
            (should-not magnus-attention-queue)
            (should-not magnus-attention-current)
            (should-not (gethash "agent-1" magnus-health--state))
            (dolist (table (list magnus-coord--buffer-ticks
                                 magnus-coord--last-nudge
                                 magnus-coord--presence-states
                                 magnus-coord--context-warned))
              (should-not (gethash "agent-1" table)))
            (should-not (gethash "calm-owl" magnus-coord--attention-data))
            (should-not magnus--agents-index)
            (should-not (buffer-live-p terminal))
            (should-not (buffer-live-p trace))
            ;; Empty persistent caches must stay empty after a restart.
            (should (equal
                     (magnus-test-forget--read-form magnus-agents-index-file)
                     nil))
            (should (equal
                     (magnus-test-forget--read-form magnus-attention-data-file)
                     nil)))
        (when (buffer-live-p terminal) (kill-buffer terminal))
        (when (buffer-live-p trace) (kill-buffer trace))
        (delete-directory root t)))))

(ert-deftest magnus-forget-changed-hook-continues-after-observer-error ()
  (let* ((events nil)
         (magnus-instances-changed-hook
          (list (lambda ()
                  (push 'failed events)
                  (error "observer failed"))
                (lambda () (push 'continued events)))))
    (cl-letf (((symbol-function 'message) #'ignore))
      (magnus-forget--run-changed-hook))
    (should (equal events '(continued failed)))))

(ert-deftest magnus-forget-batch-commits-once-and-records-missing-home ()
  (magnus-test-forget--isolated
    (let* ((root (make-temp-file "magnus-forget-batch-" t))
           (project (expand-file-name "project" root))
           (magnus-forgotten-agents-directory
            (expand-file-name "graveyard" root))
           (magnus-state-file (expand-file-name "magnus/state.el" root))
           (magnus-agents-index-file
            (expand-file-name "magnus/agents-index.el" root))
           (magnus-attention-data-file
            (expand-file-name "magnus/attention.el" root))
           (first (magnus-test-forget--instance project "agent-1" "calm-owl"))
           (second (magnus-test-forget--instance project "agent-2" "swift-hare"))
           (changed 0))
      (unwind-protect
          (progn
            (make-directory (magnus-test-forget--home first) t)
            (with-temp-file
                (expand-file-name "memory.md"
                                  (magnus-test-forget--home first))
              (insert "present"))
            ;; SECOND deliberately has no project home.
            (setq magnus-instances (list first second)
                  magnus-instances-changed-hook
                  (list (lambda () (cl-incf changed))))
            (should (= (magnus-forget-archived-instances
                        (list first second))
                       2))
            (should (= changed 1))
            (should-not magnus-instances)
            (should
             (file-exists-p
              (expand-file-name "home/memory.md"
                                (magnus-forget-record-directory first))))
            (let ((missing
                   (magnus-test-forget--read-form
                    (expand-file-name
                     "record.el" (magnus-forget-record-directory second)))))
              (should-not (plist-get missing :home-present))
              (should-not
               (file-exists-p
                (expand-file-name
                 "home" (magnus-forget-record-directory second))))))
        (delete-directory root t)))))

(ert-deftest magnus-forget-preserves-unrelated-teardown-registry-change ()
  (magnus-test-forget--isolated
    (let* ((root (make-temp-file "magnus-forget-registry-fence-" t))
           (project (expand-file-name "project" root))
           (magnus-forgotten-agents-directory
            (expand-file-name "graveyard" root))
           (magnus-state-file (expand-file-name "magnus/state.el" root))
           (magnus-agents-index-file
            (expand-file-name "magnus/agents-index.el" root))
           (magnus-attention-data-file
            (expand-file-name "magnus/attention.el" root))
           (forgotten
            (magnus-test-forget--instance project "agent-1" "calm-owl"))
           (existing
            (magnus-test-forget--instance
             project "agent-2" "swift-hare" 'stopped))
           (newcomer
            (magnus-test-forget--instance
             project "agent-3" "keen-owl" 'stopped))
           mutated)
      (unwind-protect
          (progn
            (make-directory (magnus-test-forget--home forgotten) t)
            (setq magnus-instances (list forgotten existing))
            (cl-letf (((symbol-function 'magnus-forget--close-instance-buffers)
                       (lambda (_instance)
                         (unless mutated
                           (setq mutated t
                                 magnus-instances
                                 (append magnus-instances (list newcomer))))))
                      ((symbol-function 'magnus-coord-reconcile) #'ignore))
              (should (= (magnus-forget-archived-instances
                          (list forgotten))
                         1)))
            (should (equal magnus-instances (list existing newcomer)))
            (let ((saved (magnus-test-forget--read-form magnus-state-file)))
              (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                                     saved)
                             '("agent-2" "agent-3")))))
        (delete-directory root t)))))

(ert-deftest magnus-forget-blocks-open-review-before-mutating-batch ()
  (magnus-test-forget--isolated
    (let* ((root (make-temp-file "magnus-forget-review-" t))
           (project (expand-file-name "project" root))
           (magnus-forgotten-agents-directory
            (expand-file-name "graveyard" root))
           (first (magnus-test-forget--instance project "agent-1" "calm-owl"))
           (second (magnus-test-forget--instance project "agent-2" "swift-hare"))
           (review (magnus-review--create
                    :id "review-1" :author-instance-id "agent-2"
                    :author-name "swift-hare" :reviewer-name "keen-owl"
                    :lifecycle 'open)))
      (unwind-protect
          (progn
            (dolist (instance (list first second))
              (make-directory (magnus-test-forget--home instance) t))
            (setq magnus-instances (list first second)
                  magnus-reviews (list review))
            (let ((error
                   (should-error
                    (magnus-forget-archived-instances (list first second))
                    :type 'user-error)))
              (should (string-match-p "Archive review by keen-owl"
                                      (error-message-string error))))
            (should (equal magnus-instances (list first second)))
            (should (file-directory-p (magnus-test-forget--home first)))
            (should (file-directory-p (magnus-test-forget--home second)))
            (should-not (file-exists-p magnus-forgotten-agents-directory)))
        (delete-directory root t)))))

(ert-deftest magnus-forget-save-failure-restores-registry-home-and-record ()
  (magnus-test-forget--isolated
    (let* ((root (make-temp-file "magnus-forget-rollback-" t))
           (project (expand-file-name "project" root))
           (magnus-forgotten-agents-directory
            (expand-file-name "graveyard" root))
           (instance
            (magnus-test-forget--instance project "agent-1" "calm-owl"))
           (home (magnus-test-forget--home instance))
           (record (magnus-forget-record-directory instance))
           (changed 0)
           (saves 0))
      (unwind-protect
          (progn
            (make-directory home t)
            (with-temp-file (expand-file-name "memory.md" home)
              (insert "restore me"))
            (setq magnus-instances (list instance)
                  magnus-instances-changed-hook
                  (list (lambda () (cl-incf changed))))
            (cl-letf (((symbol-function 'magnus-persistence-save)
                       (lambda ()
                         (cl-incf saves)
                         (when (= saves 1)
                           (error "simulated state write failure")))))
              (should-error
               (magnus-forget-archived-instances (list instance))
               :type 'error))
            (should (= saves 2))
            (should (equal magnus-instances (list instance)))
            (should (= changed 0))
            (should (file-directory-p home))
            (should (file-exists-p (expand-file-name "memory.md" home)))
            (should-not (file-exists-p record))
            (should-not
             (file-exists-p
              (expand-file-name ".magnus-forget-agent-1"
                                (file-name-directory home)))))
        (delete-directory root t)))))

(ert-deftest magnus-forget-double-save-failure-retains-recovery-record ()
  (magnus-test-forget--isolated
    (let* ((root (make-temp-file "magnus-forget-double-save-" t))
           (project (expand-file-name "project" root))
           (magnus-forgotten-agents-directory
            (expand-file-name "graveyard" root))
           (instance
            (magnus-test-forget--instance project "agent-1" "calm-owl"))
           (home (magnus-test-forget--home instance))
           (record (magnus-forget-record-directory instance))
           (saves 0))
      (unwind-protect
          (progn
            (make-directory home t)
            (with-temp-file (expand-file-name "memory.md" home)
              (insert "preserve me"))
            (setq magnus-instances (list instance))
            (cl-letf (((symbol-function 'magnus-persistence-save)
                       (lambda ()
                         (cl-incf saves)
                         (error "simulated persistent write failure"))))
              (should-error
               (magnus-forget-archived-instances (list instance))
               :type 'error))
            (should (= saves 2))
            (should (equal magnus-instances (list instance)))
            (should (file-directory-p home))
            (should (file-directory-p record))
            (should
             (file-exists-p (expand-file-name "home/memory.md" record))))
        (delete-directory root t)))))

(ert-deftest magnus-forget-partial-home-rollback-requarantines-whole-batch ()
  (magnus-test-forget--isolated
    (let* ((root (make-temp-file "magnus-forget-home-rollback-" t))
           (project (expand-file-name "project" root))
           (magnus-forgotten-agents-directory
            (expand-file-name "graveyard" root))
           (magnus-state-file (expand-file-name "magnus/state.el" root))
           (magnus-agents-index-file
            (expand-file-name "magnus/agents-index.el" root))
           (magnus-attention-data-file
            (expand-file-name "magnus/attention.el" root))
           (first
            (magnus-test-forget--instance project "agent-1" "calm-owl"))
           (second
            (magnus-test-forget--instance project "agent-2" "swift-hare"))
           (first-home (magnus-test-forget--home first))
           (second-home (magnus-test-forget--home second))
           (first-staged
            (expand-file-name ".magnus-forget-agent-1"
                              (file-name-directory first-home)))
           (second-staged
            (expand-file-name ".magnus-forget-agent-2"
                              (file-name-directory second-home)))
           (real-rename (symbol-function 'rename-file))
           (real-save (symbol-function 'magnus-persistence-save))
           (saves 0)
           (second-quarantines 0))
      (unwind-protect
          (progn
            (dolist (instance (list first second))
              (make-directory (magnus-test-forget--home instance) t)
              (with-temp-file
                  (expand-file-name "memory.md"
                                    (magnus-test-forget--home instance))
                (insert "recoverable")))
            (setq magnus-instances (list first second))
            (cl-letf
                (((symbol-function 'rename-file)
                  (lambda (file newname &optional ok-if-already-exists)
                    (when (and (string= (directory-file-name file) second-home)
                               (string= (directory-file-name newname)
                                        second-staged))
                      (cl-incf second-quarantines))
                    (if (and (string= (directory-file-name file) first-staged)
                             (string= (directory-file-name newname) first-home))
                        (error "simulated home restore failure")
                      (funcall real-rename file newname
                               ok-if-already-exists))))
                 ((symbol-function 'magnus-persistence-save)
                  (lambda ()
                    (cl-incf saves)
                    (if (= saves 1)
                        (error "simulated commit write failure")
                      (funcall real-save)))))
              (should-error
               (magnus-forget-archived-instances (list first second))
               :type 'error))
            (should (= saves 2))
            ;; SECOND moved once on the happy path and once more after its
            ;; successful rollback had to be reversed.
            (should (= second-quarantines 2))
            (should-not magnus-instances)
            (should (equal (magnus-test-forget--read-form magnus-state-file)
                           nil))
            (dolist (home (list first-home second-home))
              (should-not (file-exists-p home)))
            (dolist (staged (list first-staged second-staged))
              (should-not (file-exists-p staged)))
            (dolist (instance (list first second))
              (should (file-directory-p
                       (magnus-forget-record-directory instance)))))
        (delete-directory root t)))))

(ert-deftest magnus-forget-preflights-entire-batch-before-copying ()
  (magnus-test-forget--isolated
    (let* ((root (make-temp-file "magnus-forget-collision-" t))
           (project (expand-file-name "project" root))
           (magnus-forgotten-agents-directory
            (expand-file-name "graveyard" root))
           (first (magnus-test-forget--instance project "agent-1" "calm-owl"))
           (second (magnus-test-forget--instance project "agent-2" "swift-hare"))
           (first-record (magnus-forget-record-directory first))
           (second-record (magnus-forget-record-directory second)))
      (unwind-protect
          (progn
            (dolist (instance (list first second))
              (make-directory (magnus-test-forget--home instance) t))
            (make-directory second-record t)
            (setq magnus-instances (list first second))
            (should-error
             (magnus-forget-archived-instances (list first second))
             :type 'user-error)
            (should-not (file-exists-p first-record))
            (should (file-directory-p second-record))
            (should (file-directory-p (magnus-test-forget--home first)))
            (should (file-directory-p (magnus-test-forget--home second))))
        (delete-directory root t)))))

(ert-deftest magnus-forget-rejects-symlinked-home-without-following-it ()
  (magnus-test-forget--isolated
    (let* ((root (make-temp-file "magnus-forget-symlink-" t))
           (project (expand-file-name "project" root))
           (target (expand-file-name "external-home" root))
           (magnus-forgotten-agents-directory
            (expand-file-name "graveyard" root))
           (instance
            (magnus-test-forget--instance project "agent-1" "calm-owl"))
           (home (magnus-test-forget--home instance)))
      (unwind-protect
          (progn
            (make-directory (file-name-directory home) t)
            (make-directory target t)
            (with-temp-file (expand-file-name "memory.md" target)
              (insert "outside"))
            (make-symbolic-link target home)
            (setq magnus-instances (list instance))
            (should-error
             (magnus-forget-archived-instances (list instance))
             :type 'user-error)
            (should (file-symlink-p home))
            (should (file-exists-p (expand-file-name "memory.md" target)))
            (should-not (file-exists-p magnus-forgotten-agents-directory)))
        (delete-directory root t)))))

(ert-deftest magnus-forget-rejects-symlinked-managed-ancestor ()
  (magnus-test-forget--isolated
    (let* ((root (make-temp-file "magnus-forget-parent-symlink-" t))
           (project (expand-file-name "project" root))
           (outside (expand-file-name "outside-claude" root))
           (magnus-forgotten-agents-directory
            (expand-file-name "graveyard" root))
           (instance
            (magnus-test-forget--instance project "agent-1" "calm-owl"))
           (home (magnus-test-forget--home instance)))
      (unwind-protect
          (progn
            (make-directory project t)
            (make-directory (expand-file-name "agents/calm-owl" outside) t)
            (with-temp-file
                (expand-file-name "agents/calm-owl/memory.md" outside)
              (insert "outside"))
            (make-symbolic-link outside (expand-file-name ".claude" project))
            (setq magnus-instances (list instance))
            (should-error
             (magnus-forget-archived-instances (list instance))
             :type 'user-error)
            (should (file-symlink-p (expand-file-name ".claude" project)))
            (should (file-exists-p (expand-file-name "memory.md" home)))
            (should-not (file-exists-p magnus-forgotten-agents-directory)))
        (delete-directory root t)))))

(ert-deftest magnus-forget-rejects-stale-or-non-archived-instances ()
  (magnus-test-forget--isolated
    (let* ((root (make-temp-file "magnus-forget-identity-" t))
           (project (expand-file-name "project" root))
           (magnus-forgotten-agents-directory
            (expand-file-name "graveyard" root))
           (current
            (magnus-test-forget--instance project "agent-1" "calm-owl"))
           (stale
            (magnus-test-forget--instance project "agent-1" "calm-owl")))
      (unwind-protect
          (progn
            (setq magnus-instances (list current))
            (should-error
             (magnus-forget-archived-instances (list stale))
             :type 'user-error)
            (setf (magnus-instance-status current) 'stopped)
            (should-error
             (magnus-forget-archived-instances (list current))
             :type 'user-error)
            (should (equal magnus-instances (list current)))
            (should-not (file-exists-p magnus-forgotten-agents-directory)))
        (delete-directory root t)))))

(provide 'magnus-forget-tests)
;;; magnus-forget-tests.el ends here
