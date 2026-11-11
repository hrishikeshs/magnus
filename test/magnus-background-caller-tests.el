;;; magnus-background-caller-tests.el --- Background caller tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magnus)
(require 'magnus-coord)
(require 'magnus-health)
(require 'magnus-provider-claude)

(ert-deftest magnus-claude-text-only-agent-honors-model-with-zero-tools ()
  (let* ((magnus-claude-executable "claude")
         (spec
          (magnus-claude-headless-agent-spec
           '(:purpose agent
             :prompt "Summarize"
             :name "summary"
             :model "claude-test-model"
             :allowed-tools "")))
         (command (plist-get spec :command)))
    (should (equal command
                   (list magnus-claude-executable
                         "--print" "Summarize"
                         "--verbose"
                         "--output-format" "stream-json"
                         "--model" "claude-test-model"
                         "--tools" "")))
    (should-not (member "--allowedTools" command))))

(ert-deftest magnus-headless-command-preserves-configured-prefix-flags ()
  (let ((magnus-claude-executable "claude --fixture-flag")
        (magnus-headless-model "small-model"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) nil)))
      (should
       (equal (magnus--headless-command "Match this" t)
              '("claude" "--fixture-flag" "--print"
                "--model" "small-model" "Match this"))))))

(ert-deftest magnus-agent-indexing-bounds-memory-and-queues-text-only-work ()
  (let* ((memory-file (make-temp-file "magnus-index-memory-"))
         (instance
          (magnus-instance--create
           :id "agent-uuid" :name "calm-owl"
           :directory default-directory :provider 'claude))
         (magnus-agents-index-memory-limit 8)
         (magnus-claude-executable "claude --fixture-flag")
         (magnus-headless-model "small-model")
         submission indexed)
    (unwind-protect
        (progn
          (with-temp-file memory-file
            (insert "1234567890SHOULD-NOT-BE-READ"))
          (cl-letf (((symbol-function 'magnus-process--agent-memory-path)
                     (lambda (_instance) memory-file))
                    ((symbol-function 'magnus-instances-get)
                     (lambda (_id) instance))
                    ((symbol-function 'executable-find)
                     (lambda (&rest _arguments)
                       (ert-fail "caller must not preflight the whole command")))
                    ((symbol-function 'magnus-background-submit)
                     (lambda (key provider request callbacks)
                       (setq submission
                             (list key provider request callbacks))
                       'queued-job))
                    ((symbol-function 'magnus--agents-index-set)
                     (lambda (directory name tags)
                       (setq indexed (list directory name tags))))
                    ((symbol-function 'message) (lambda (&rest _arguments))))
            (magnus--agents-index-update instance)
            (pcase-let ((`(,key ,provider ,request ,callbacks) submission))
              (should (equal key '(expertise "agent-uuid")))
              (should (eq provider 'claude))
              (should (equal (plist-get request :allowed-tools) ""))
              (should (equal (plist-get request :model) "small-model"))
              (should (string-match-p "12345678" (plist-get request :prompt)))
              (should-not
               (string-match-p "90SHOULD" (plist-get request :prompt)))
              (funcall (plist-get callbacks :on-complete)
                       '(:success-p t
                         :output "[response]\nlisp, emacs\n[end-response]")))
            (should (equal indexed
                           (list default-directory
                                 "calm-owl" "lisp, emacs")))
            (setq indexed 'unchanged)
            (setf (magnus-instance-directory instance)
                  (expand-file-name "moved-project" default-directory))
            (funcall (plist-get (nth 3 submission) :on-complete)
                     '(:success-p t :output "stale, metadata"))
            (should (eq indexed 'unchanged))))
      (delete-file memory-file))))

(ert-deftest magnus-coord-retro-uses-serialized-text-only-job ()
  (let ((directory (make-temp-file "magnus-retro-project-" t))
        (magnus-claude-executable "claude --fixture-flag")
        (magnus-headless-model "small-model")
        submission saved)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (&rest _arguments)
                     (ert-fail "caller must not preflight the whole command")))
                  ((symbol-function 'magnus-coord--collect-retro-data)
                   (lambda (_directory)
                     '(:log nil :discoveries nil :decisions nil
                       :git "No commits" :start nil :end 1.0)))
                  ((symbol-function 'magnus-background-submit)
                   (lambda (key provider request callbacks)
                     (setq submission (list key provider request callbacks))
                     'queued-job))
                  ((symbol-function 'magnus-coord--save-retro)
                   (lambda (root output data)
                     (setq saved (list root output data)))))
          (magnus-coord-generate-retro directory)
          (pcase-let ((`(,key ,provider ,request ,callbacks) submission))
            (should (equal key
                           (list 'coord-retro
                                 (directory-file-name
                                  (file-truename directory)))))
            (should (eq provider 'claude))
            (should (equal (plist-get request :allowed-tools) ""))
            (should (equal (plist-get request :model) "small-model"))
            (funcall (plist-get callbacks :on-complete)
                     '(:success-p t :output "# Retrospective")))
          (should (equal (cadr saved) "# Retrospective")))
      (delete-directory directory t))))

(ert-deftest magnus-dashboard-ai-messages-are-opt-in-and-cancel-scoped ()
  (let ((magnus-health-dashboard-ai-messages nil)
        (magnus-claude-executable "claude --fixture-flag")
        (magnus-health-dashboard--generating nil)
        submission cancelled)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _arguments)
                 (ert-fail "caller must not preflight the whole command")))
              ((symbol-function 'magnus-background-submit)
               (lambda (&rest arguments)
                 (setq submission arguments)
                 'queued-job))
              ((symbol-function 'magnus-background-cancel)
               (lambda (key) (setq cancelled key) 1))
              ((symbol-function 'message) (lambda (&rest _arguments))))
      (magnus-health-dashboard--generate-messages)
      (should-not submission)
      (setq magnus-health-dashboard-ai-messages t)
      (magnus-health-dashboard--generate-messages)
      (should (equal (car submission)
                     magnus-health-dashboard--job-key))
      (should (equal (plist-get (nth 2 submission) :allowed-tools) ""))
      (should magnus-health-dashboard--generating)
      (magnus-health-dashboard--stop)
      (should (eq cancelled magnus-health-dashboard--job-key))
      (should-not magnus-health-dashboard--generating))))

(provide 'magnus-background-caller-tests)
;;; magnus-background-caller-tests.el ends here
