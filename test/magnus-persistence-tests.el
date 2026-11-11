;;; magnus-persistence-tests.el --- Persistence hardening tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus-persistence)

(defvar magnus-state-file)
(defvar magnus-persistence-tests--read-evaluated nil)

(defun magnus-persistence-tests--instance (id &optional provider)
  "Return a stable test instance named by ID for PROVIDER."
  (magnus-instance--create
   :id id
   :name (format "agent-%s" id)
   :directory (format "/tmp/project-%s" id)
   :created-at '(26781 4242 0 0)
   :provider (or provider 'claude)
   :status 'stopped))

(defun magnus-persistence-tests--write-state (file state)
  "Write STATE to FILE in Magnus's established Lisp format."
  (with-temp-file file
    (insert ";; Magnus state file - do not edit manually\n\n")
    (pp state (current-buffer))))

(ert-deftest magnus-persistence-first-save-creates-private-parent ()
  "A first-run save creates its directory and preserves the Lisp format."
  (let* ((root (make-temp-file "magnus-persistence-" t))
         (parent (expand-file-name ".magnus" root))
         (magnus-state-file (expand-file-name "state.el" parent))
         (instance (magnus-persistence-tests--instance "first"))
         (magnus-instances (list instance)))
    (unwind-protect
        (progn
          (magnus-persistence-save)
          (should (file-directory-p parent))
          (should (file-regular-p magnus-state-file))
          (should (= (logand (file-modes parent) #o777) #o700))
          (should (= (logand (file-modes magnus-state-file) #o777) #o600))
          (should
           (equal
            (with-temp-buffer
              (insert-file-contents magnus-state-file)
              (goto-char (point-min))
              (read (current-buffer)))
            (list (magnus-instances-serialize instance)))))
      (delete-directory root t))))

(ert-deftest magnus-persistence-save-preserves-custom-parent-mode ()
  "Saving does not privatize an existing custom parent directory."
  (let* ((root (make-temp-file "magnus-persistence-" t))
         (magnus-state-file (expand-file-name "state.el" root))
         (magnus-instances
          (list (magnus-persistence-tests--instance "custom"))))
    (unwind-protect
        (progn
          (set-file-modes root #o755)
          (magnus-persistence-save)
          (should (= (logand (file-modes root) #o777) #o755))
          (should (= (logand (file-modes magnus-state-file) #o777) #o600)))
      (delete-directory root t))))

(ert-deftest magnus-persistence-malformed-later-record-keeps-live-registry ()
  "No record becomes live when a later serialized record is malformed."
  (let* ((root (make-temp-file "magnus-persistence-" t))
         (magnus-state-file (expand-file-name "state.el" root))
         (sentinel (magnus-persistence-tests--instance "live"))
         (valid (magnus-instances-serialize
                 (magnus-persistence-tests--instance "valid")))
         (malformed '(:id "bad" :name "agent-bad" :directory 17
                          :status stopped))
         (magnus-instances (list sentinel))
         (magnus-instances-changed-hook nil)
         (reconnects 0))
    (unwind-protect
        (progn
          (magnus-persistence-tests--write-state
           magnus-state-file (list valid malformed))
          (cl-letf (((symbol-function 'magnus-process-reconnect)
                     (lambda (_instance) (cl-incf reconnects)))
                    ((symbol-function 'message) (lambda (&rest _arguments))))
            (magnus-persistence-load))
          (should (equal magnus-instances (list sentinel)))
          (should (= reconnects 0)))
      (delete-directory root t))))

(ert-deftest magnus-persistence-rejects-unsafe-agent-identities-transactionally ()
  "A persisted path escape or unsafe UUID never becomes live state."
  (let* ((valid (magnus-instances-serialize
                 (magnus-persistence-tests--instance "valid")))
         (unsafe-name (copy-sequence valid))
         (unsafe-id (copy-sequence valid)))
    (setq unsafe-name (plist-put unsafe-name :name "../outside"))
    (setq unsafe-id (plist-put unsafe-id :id "../../writer"))
    (dolist (state (list (list unsafe-name) (list unsafe-id)))
      (should-error (magnus-persistence--deserialize-state state)))))

(ert-deftest magnus-persistence-round-trips-and-validates-instance-kind ()
  "Headless identity is durable while old records remain interactive."
  (let* ((headless
          (magnus-instances-create "/tmp/headless" "headless" nil 'headless))
         (serialized (magnus-instances-serialize headless))
         (legacy (copy-sequence serialized))
         (invalid (copy-sequence serialized)))
    (should (eq (plist-get serialized :kind) 'headless))
    (should (eq (magnus-instance-kind
                 (magnus-instances-deserialize serialized))
                'headless))
    (cl-remf legacy :kind)
    (should (eq (magnus-instance-kind
                 (magnus-persistence--deserialize-record legacy 1))
                'interactive))
    (plist-put invalid :kind 'background)
    (should-error (magnus-persistence--deserialize-state (list invalid)))))

(ert-deftest magnus-persistence-repairs-obsolete-creation-metadata ()
  "Nonessential creation timestamps cannot discard otherwise valid agents."
  (let* ((first
          (magnus-instances-serialize
           (magnus-persistence-tests--instance "missing-created")))
         (second
          (magnus-instances-serialize
           (magnus-persistence-tests--instance "obsolete-created" 'codex)))
         (replacement '(30000 123 0 0))
         repaired)
    (plist-put first :created-at nil)
    (plist-put second :created-at "historical-format")
    (cl-letf (((symbol-function 'current-time) (lambda () replacement))
              ((symbol-function 'message) #'ignore))
      (setq repaired
            (magnus-persistence--deserialize-state (list first second))))
    (should (equal (mapcar #'magnus-instance-id repaired)
                   '("missing-created" "obsolete-created")))
    (dolist (instance repaired)
      (should (equal (magnus-instance-created-at instance) replacement)))
    (should (eq (magnus-instance-provider (cadr repaired)) 'codex))
    (should (equal (magnus-instance-directory (car repaired))
                   "/tmp/project-missing-created"))))

(ert-deftest magnus-persistence-load-disables-reader-evaluation ()
  "Reader evaluation in a state file is rejected without side effects."
  (let* ((root (make-temp-file "magnus-persistence-" t))
         (magnus-state-file (expand-file-name "state.el" root))
         (sentinel (magnus-persistence-tests--instance "live"))
         (magnus-instances (list sentinel))
         (magnus-persistence-tests--read-evaluated nil))
    (unwind-protect
        (progn
          (with-temp-file magnus-state-file
            (insert "(#.(progn (setq magnus-persistence-tests--read-evaluated t) nil))"))
          (cl-letf (((symbol-function 'message) (lambda (&rest _arguments))))
            (magnus-persistence-load))
          (should-not magnus-persistence-tests--read-evaluated)
          (should (equal magnus-instances (list sentinel))))
      (delete-directory root t))))

(ert-deftest magnus-persistence-failed-replacement-preserves-old-file ()
  "A failed atomic replacement leaves the previous state file untouched."
  (let* ((root (make-temp-file "magnus-persistence-" t))
         (magnus-state-file (expand-file-name "state.el" root))
         (magnus-instances
          (list (magnus-persistence-tests--instance "replacement")))
         (original "ORIGINAL STATE\n"))
    (unwind-protect
        (progn
          (with-temp-file magnus-state-file
            (insert original))
          (cl-letf (((symbol-function 'rename-file)
                     (lambda (&rest _arguments)
                       (error "simulated interrupted replacement"))))
            (should-error (magnus-persistence-save)))
          (should
           (equal (with-temp-buffer
                    (insert-file-contents magnus-state-file)
                    (buffer-string))
                  original))
          (should-not
           (directory-files root nil "\\`\\.magnus-state-tmp-")))
      (delete-directory root t))))

(ert-deftest magnus-persistence-load-swaps-once-before-provider-reconnects ()
  "Reconnect through the provider seam only after the full registry swap."
  (let* ((root (make-temp-file "magnus-persistence-" t))
         (magnus-state-file (expand-file-name "state.el" root))
         (first (magnus-persistence-tests--instance "claude" 'claude))
         (second (magnus-persistence-tests--instance "codex" 'codex))
         (magnus-instances nil)
         (notifications 0)
         (magnus-instances-changed-hook
          (list (lambda () (cl-incf notifications))))
         calls)
    (unwind-protect
        (progn
          (magnus-persistence-tests--write-state
           magnus-state-file
           (mapcar #'magnus-instances-serialize (list first second)))
          (cl-letf (((symbol-function 'magnus-process-reconnect)
                     (lambda (instance)
                       (push (list (magnus-instance-provider instance)
                                   (length magnus-instances))
                             calls)))
                    ((symbol-function 'message) (lambda (&rest _arguments))))
            (magnus-persistence-load))
          (should (equal (mapcar #'magnus-instance-id magnus-instances)
                         '("claude" "codex")))
          (should (equal (nreverse calls) '((claude 2) (codex 2))))
          (should (= notifications 1)))
      (delete-directory root t))))

(ert-deftest magnus-persistence-shutdown-flushes-once-and-releases-hooks ()
  "Persistence shutdown is complete and idempotent after setup."
  (let ((magnus-instances-changed-hook nil)
        (kill-emacs-hook nil)
        (magnus-persistence--autosave-active nil)
        (magnus-persistence--save-timer nil)
        timer
        (saves 0))
    (unwind-protect
        (progn
          (magnus-persistence--setup-autosave)
          (setq timer (run-at-time 3600 nil #'ignore)
                magnus-persistence--save-timer timer)
          (cl-letf (((symbol-function 'magnus-persistence-save)
                     (lambda () (cl-incf saves))))
            (magnus-persistence-shutdown)
            (magnus-persistence-shutdown))
          (should (= saves 1))
          (should-not magnus-persistence--autosave-active)
          (should-not magnus-persistence--save-timer)
          (should-not
           (memq #'magnus-persistence--schedule-save
                 magnus-instances-changed-hook))
          (should-not (memq #'magnus-persistence-save kill-emacs-hook)))
      (when (timerp timer)
        (cancel-timer timer))
      (remove-hook 'magnus-instances-changed-hook
                   #'magnus-persistence--schedule-save)
      (remove-hook 'kill-emacs-hook #'magnus-persistence-save))))

(provide 'magnus-persistence-tests)
;;; magnus-persistence-tests.el ends here
