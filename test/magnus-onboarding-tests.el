;;; magnus-onboarding-tests.el --- Shared onboarding tests -*- lexical-binding: t -*-

(require 'ert)
(require 'magnus-instances)
(require 'magnus-onboarding)
(require 'magnus-process)
(require 'magnus-provider-codex)

(defun magnus-test-onboarding--instance (id name directory provider)
  "Return a deterministic test instance with ID, NAME, DIRECTORY, and PROVIDER."
  (magnus-instance--create
   :id id :name name :directory directory :provider provider :status 'stopped))

(ert-deftest magnus-onboarding-provider-wrappers-share-core-guidance ()
  (let* ((directory (make-temp-file "magnus-onboarding-" t))
         (claude
          (magnus-test-onboarding--instance
           "claude-writer-uuid" "swift-hare" directory 'claude))
         (codex
          (magnus-test-onboarding--instance
           "codex-writer-uuid" "keen-owl" directory 'codex))
         (claude-prompt (magnus-process--onboarding-message claude))
         (codex-prompt (magnus-codex--instructions codex)))
    (unwind-protect
        (progn
          (dolist (entry
                   (list (cons claude-prompt
                               '("swift-hare" "claude-writer-uuid"))
                         (cons codex-prompt
                               '("keen-owl" "codex-writer-uuid"))))
            (let ((prompt (car entry)))
              (dolist (identity (cdr entry))
                (should (string-match-p (regexp-quote identity) prompt)))
              (dolist (guidance
                       '(".magnus-coord.md"
                         ".claude/magnus-instructions.md"
                         "AGENTS.md"
                         "[thinking]...[end-thinking]"
                         "[response]...[end-response]"
                         "not claim that it is private or raw chain-of-thought"
                         "Authorization boundary"
                         "first-person letter"))
                (should (string-match-p (regexp-quote guidance) prompt)))
              (should-not (string-match-p "encrypted and hidden" prompt))
              (should-not (string-match-p "raw and unfiltered" prompt)))))
      (delete-directory directory t))))

(ert-deftest magnus-onboarding-provider-wrappers-use-custom-project-paths ()
  "Both providers receive safely quoted paths configured for the instance root."
  (let* ((directory (make-temp-file "magnus-onboarding-custom-paths-" t))
         (magnus-coord-file "control/team journal.md")
         (magnus-coord-instructions-file
          ".agents/magnus shared instructions.md")
         (claude
          (magnus-test-onboarding--instance
           "claude-custom-uuid" "swift-hare" directory 'claude))
         (codex
          (magnus-test-onboarding--instance
           "codex-custom-uuid" "keen-owl" directory 'codex))
         (quoted-journal (prin1-to-string magnus-coord-file))
         (quoted-instructions
          (prin1-to-string magnus-coord-instructions-file)))
    (unwind-protect
        (dolist (prompt (list (magnus-process--onboarding-message claude)
                              (magnus-codex--instructions codex)))
          (should (string-match-p (regexp-quote quoted-journal) prompt))
          (should (string-match-p (regexp-quote quoted-instructions) prompt))
          (should-not (string-match-p (regexp-quote ".magnus-coord.md")
                                      prompt))
          (should-not
           (string-match-p
            (regexp-quote ".claude/magnus-instructions.md") prompt)))
      (delete-directory directory t))))

(ert-deftest magnus-onboarding-custom-user-instructions-replace-only-guidance ()
  "Custom guidance reaches both providers without replacing Magnus identity."
  (let* ((directory (make-temp-file "magnus-onboarding-custom-user-" t))
         (magnus-onboarding-user-instructions
          (concat "When responding, treat the user as an examiner.\n"
                  "Put context first and the relevant answer last."))
         (claude
          (magnus-test-onboarding--instance
           "claude-custom-user-uuid" "swift-wolf" directory 'claude))
         (codex
          (magnus-test-onboarding--instance
           "codex-custom-user-uuid" "wise-deer" directory 'codex)))
    (unwind-protect
        (dolist (entry
                 (list (cons (magnus-process--onboarding-message claude)
                             '("swift-wolf" "claude-custom-user-uuid"))
                       (cons (magnus-codex--instructions codex)
                             '("wise-deer" "codex-custom-user-uuid"))))
          (let ((prompt (car entry)))
            (dolist (identity (cdr entry))
              (should (string-match-p (regexp-quote identity) prompt)))
            (should (string-match-p "treat the user as an examiner" prompt))
            (should (string-match-p "relevant answer last" prompt))
            (should (string-match-p "Authorization boundary" prompt))
            (should (string-match-p "first-person letter" prompt))
            (should (string-match-p (regexp-quote ".magnus-coord.md") prompt))
            (should-not (string-match-p "visible working notebook" prompt))
            (should-not
             (string-match-p (regexp-quote "[thinking]") prompt))))
      (delete-directory directory t))))

(ert-deftest magnus-onboarding-empty-user-instructions-omit-only-guidance ()
  (let ((magnus-onboarding-user-instructions ""))
    (let ((prompt
           (magnus-onboarding-build
            "quiet-agent-uuid" "quiet-agent" :returning nil)))
      (should (string-match-p "You are quiet-agent" prompt))
      (should (string-match-p "Authorization boundary" prompt))
      (should (string-match-p "Begin by orienting" prompt))
      (should-not (string-match-p "User-visible engineering journal" prompt))
      (should-not
       (string-match-p (regexp-quote "[thinking]") prompt)))))

(ert-deftest magnus-onboarding-invalid-user-instructions-fail-clearly ()
  (let ((magnus-onboarding-user-instructions 42))
    (should-error
     (magnus-onboarding-build "invalid-user-uuid" "invalid-user")
     :type 'error)))

(ert-deftest magnus-onboarding-preserves-returning-and-summon-context ()
  (let ((prompt
         (magnus-onboarding-build
          "durable-writer" "bright-crow"
          :returning t
          :previous-trace "/tmp/previous.jsonl"
          :summon-context '(:sender "quick-wolf" :reason "Review the fix"))))
    (should (string-match-p "own prior voice" prompt))
    (should (string-match-p "/tmp/previous\\.jsonl" prompt))
    (should (string-match-p "quick-wolf summoned you" prompt))
    (should (string-match-p "Review the fix" prompt))))

(ert-deftest magnus-onboarding-headless-prompt-appends-exact-task ()
  (let* ((directory (make-temp-file "magnus-onboarding-headless-" t))
         (instance
          (magnus-test-onboarding--instance
           "headless-writer" "plain-ibis" directory 'claude))
         (task "Fix exactly this.\nPreserve this second line.")
         (magnus-onboarding-user-instructions "Custom response guidance.")
         (prompt (magnus-process--headless-prompt instance task)))
    (unwind-protect
        (progn
          (should (string-match-p "headless-writer" prompt))
          (should (string-match-p "Custom response guidance" prompt))
          (should (string-match-p "Exact task from the user:" prompt))
          (should (string-suffix-p task prompt)))
      (delete-directory directory t))))

(ert-deftest magnus-onboarding-rejects-agent-home-path-escapes ()
  (dolist (name '("../escape" "nested/agent" "nested\\agent" "." ".."))
    (should-not (magnus-instances-valid-name-p name))
    (should-error (magnus-onboarding-memory-relative-path name)))
  ;; Display names need not be restricted to the generated adjective-animal
  ;; convention as long as they remain one safe path segment.
  (should (magnus-instances-valid-name-p "Wise Deer ☃")))

(ert-deftest magnus-onboarding-legacy-wrapper-never-invents-an-instance-id ()
  (let ((prompt (magnus-process--onboarding-new "legacy-agent" nil)))
    (should (string-match-p "No durable Magnus UUID was available" prompt))
    (should (string-match-p "Do not invent one" prompt))
    (should (string-match-p "display name in the shared coordination file"
                            prompt))))

(provide 'magnus-onboarding-tests)
;;; magnus-onboarding-tests.el ends here
