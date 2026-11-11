;;; magnus-provider-tests.el --- Provider compatibility tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'magnus)
(require 'magnus-instances)
(require 'magnus-provider)
(require 'magnus-provider-codex)
(require 'magnus-trace)
(require 'magnus-coord)
(require 'magnus-health)

(defun magnus-test--codex-tui (instance)
  "Create a live terminal-process stand-in for INSTANCE."
  (let* ((buffer (generate-new-buffer
                  (format " *magnus-test-tui:%s*"
                          (magnus-instance-id instance))))
         (process (make-pipe-process
                   :name (generate-new-buffer-name "magnus-test-tui")
                   :buffer buffer
                   :noquery t)))
    (process-put process 'magnus-codex-instance instance)
    (magnus-instances-update instance :buffer buffer :status 'running)
    (cons buffer process)))

(defun magnus-test--delete-terminal (terminal)
  "Delete TERMINAL's process and buffer."
  (when terminal
    (when (process-live-p (cdr terminal))
      (delete-process (cdr terminal)))
    (when (buffer-live-p (car terminal))
      (kill-buffer (car terminal)))))

(defun magnus-test--write-codex-rollout
    (root id directory prompt &optional parent-thread-id padding session-id date)
  "Write a test Codex rollout under ROOT for ID, DIRECTORY, and PROMPT.
PARENT-THREAD-ID marks the record as a subagent when non-nil.
PADDING adds an unrelated record before the first user message.
SESSION-ID defaults to ID; a different value models a resumed rollout.
DATE defaults to the original deterministic test fixture date."
  (let* ((date-directory (expand-file-name (or date "2026/07/11") root))
         (file (expand-file-name (format "rollout-test-%s.jsonl" id)
                                 date-directory)))
    (make-directory date-directory t)
    (with-temp-file file
      (insert
       (json-encode
        `((type . "session_meta")
          (payload . ((id . ,id)
                      (session_id . ,(or session-id id))
                      (parent_thread_id . ,parent-thread-id)
                      (cwd . ,directory)))))
       "\n"
       (json-encode
        '((type . "event_msg") (payload . ((type . "task_started")))))
       "\n"
       (if padding
           (concat (json-encode `((type . "response_item")
                                  (payload . ((text . ,padding)))))
                   "\n")
         "")
       (json-encode
        `((type . "event_msg")
          (payload . ((type . "user_message") (message . ,prompt)))))
       "\n"))
    file))

(defun magnus-test--codex-event-line (event-type message)
  "Encode one Codex EVENT-TYPE carrying MESSAGE as JSONL."
  (json-encode
   `((timestamp . "2026-07-11T08:00:00.000Z")
     (type . "event_msg")
     (payload . ((type . ,event-type) (message . ,message))))))

(ert-deftest magnus-provider-legacy-state-defaults-to-claude ()
  (let ((instance (magnus-instances-deserialize
                   '(:id "legacy" :name "old-user"
                     :directory "/tmp" :status stopped))))
    (should (eq (magnus-instance-provider instance) 'claude))
    (should-not (magnus-provider-external-p instance))))

(ert-deftest magnus-upgrade-requires-restart-before-reinitialization ()
  (let ((magnus--restart-required t)
        (magnus--initialized nil))
    (should-error (magnus--ensure-initialized) :type 'user-error)))

(ert-deftest magnus-provider-round-trips-codex-state ()
  (let* ((instance (magnus-instances-create "/tmp" "codex-user" 'codex))
         (restored (magnus-instances-deserialize
                    (magnus-instances-serialize instance))))
    (should (eq (magnus-instance-provider restored) 'codex))
    (should (magnus-provider-external-p restored))))

(ert-deftest magnus-provider-unknown-never-falls-back-to-claude ()
  (let ((instance (magnus-instances-create "/tmp" "unknown" 'not-a-provider)))
    (should (magnus-provider-external-p instance))
    (should-error (magnus-provider-call instance 'start) :type 'user-error)))

(ert-deftest magnus-codex-onboarding-keeps-identity-and-journal ()
  (let* ((instance (magnus-instances-create "/tmp" "solar-fox" 'codex))
         (prompt (magnus-codex--onboarding-prompt
                  instance "Fix the parser" "marker-1")))
    (should (string-match-p "You are solar-fox" prompt))
    (should (string-match-p "Fix the parser" prompt))
    (should (string-match-p "marker-1" prompt))
    (should (string-match-p "\\[thinking\\]" prompt))
    (should (string-match-p "visible working notebook" prompt))
    (should (string-match-p "Discoveries" prompt))
    (should (string-match-p "future-you" prompt))))

(ert-deftest magnus-codex-onboards-returning-identity-from-memory ()
  (let* ((directory (make-temp-file "magnus-codex-memory-" t))
         (memory-directory
          (expand-file-name ".claude/agents/solar-fox" directory))
         (memory-file (expand-file-name "memory.md" memory-directory)))
    (unwind-protect
        (progn
          (make-directory memory-directory t)
          (with-temp-file memory-file
            (insert "# Solar Fox\n\nI remember this project.\n"))
          (let* ((instance (magnus-instances-create
                            directory "solar-fox" 'codex))
                 (instructions (magnus-codex--instructions instance)))
            (should (string-match-p "been here before" instructions))
            (should (string-match-p "own prior voice" instructions))
            (should (string-match-p
                     (regexp-quote
                      ".claude/agents/solar-fox/memory.md")
                     instructions))))
      (delete-directory directory t))))

(ert-deftest magnus-codex-tui-command-is-standalone-and-resumable ()
  (let ((instance (magnus-instances-create
                   "/tmp/project with spaces" "tui-codex" 'codex)))
    (let ((fresh (magnus-codex--tui-command instance "First prompt")))
      (should (string-match-p "exec codex" fresh))
      (should-not (string-match-p "app-server\\|--remote\\|resume" fresh)))
    (magnus-instances-update instance :session-id "thread-1")
    (let ((resumed (magnus-codex--tui-command instance "read this next")))
      (should (string-match-p "exec codex resume" resumed))
      (should (string-match-p "thread-1" resumed))
      (should (string-match-p
               (regexp-quote (shell-quote-argument "read this next"))
               resumed)))))

(ert-deftest magnus-codex-tui-command-preserves-configured-prefix-flags ()
  (let ((instance (magnus-instances-create "/tmp" "flagged-codex" 'codex))
        (magnus-codex-executable "codex --fixture-flag"))
    (cl-letf (((symbol-function 'executable-find) (lambda (_command) nil)))
      (should (string-prefix-p
               "exec codex --fixture-flag -C "
               (magnus-codex--tui-command instance))))))

(ert-deftest magnus-codex-spawn-uses-shared-terminal-substrate ()
  (let* ((directory (make-temp-file "magnus-codex-terminal-" t))
         (instance (magnus-instances-create
                    directory "shared-terminal" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (buffer (car terminal))
         requested-name
         requested-environment)
    (unwind-protect
        (let ((magnus-instances-changed-hook nil))
          (cl-letf
              (((symbol-function 'magnus-terminal-create-buffer)
                (lambda (name &optional environment)
                  (setq requested-name name
                        requested-environment environment)
                  buffer))
               ((symbol-function 'magnus-process--create-vterm-buffer)
                (lambda (&rest _arguments)
                  (error "Codex used the private process helper")))
               ((symbol-function 'magnus-codex--setup-tui-sentinel) #'ignore)
               ((symbol-function 'run-with-timer)
                (lambda (&rest _arguments) nil)))
            (should (eq (magnus-codex--spawn-tui instance "Inspect this")
                        buffer))
            (should (equal requested-name "*codex:shared-terminal*"))
            (should-not requested-environment)
            (should (eq (magnus-instance-buffer instance) buffer))
            (should (eq (magnus-instance-status instance) 'running))))
      (magnus-test--delete-terminal terminal)
      (delete-directory directory t))))

(ert-deftest magnus-codex-direct-start-rolls-back-failed-tui-replacement ()
  (let* ((directory (make-temp-file "magnus-codex-start-rollback-" t))
         (instance (magnus-instances-create
                    directory "failed-replacement" 'codex))
         (old-buffer (generate-new-buffer " *magnus-old-codex-tui*"))
         (new-buffer (generate-new-buffer " *magnus-new-codex-tui*"))
         (new-process
          (make-pipe-process
           :name (generate-new-buffer-name "magnus-new-codex-tui")
           :buffer new-buffer
           :noquery t))
         (unrelated-buffer
          (generate-new-buffer " *magnus-unrelated-codex-tui*"))
         (magnus-process--transaction-runtime-buffer (list nil))
         (magnus-instances-changed-hook nil)
         (magnus-buffer-name " *magnus-status-not-present*"))
    (unwind-protect
        (progn
          (magnus-instances-update
           instance :buffer old-buffer :status 'stopped
           :session-id "resume-after-failure")
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_executable) "/usr/bin/codex"))
                    ((symbol-function 'magnus-terminal-create-buffer)
                     (lambda (&rest _arguments) new-buffer))
                    ((symbol-function 'magnus-codex--setup-tui-sentinel)
                     (lambda (&rest _arguments)
                       (error "simulated sentinel setup failure"))))
            (let ((err (should-error (magnus-codex-start instance))))
              (should (string-match-p
                       "simulated sentinel setup failure"
                       (error-message-string err)))))
          (should-not (buffer-live-p old-buffer))
          (should-not (buffer-live-p new-buffer))
          (should-not (process-live-p new-process))
          (should (buffer-live-p unrelated-buffer))
          (should (eq (caar magnus-process--transaction-runtime-buffer)
                      new-buffer))
          (should (eq (cdar magnus-process--transaction-runtime-buffer)
                      new-process))
          (should-not (magnus-instance-buffer instance))
          (should (eq (magnus-instance-status instance) 'stopped)))
      (when (process-live-p new-process)
        (delete-process new-process))
      (dolist (buffer (list old-buffer new-buffer unrelated-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest magnus-codex-session-capture-matches-exact-first-prompt ()
  (let* ((root (make-temp-file "magnus-codex-sessions-" t))
         (directory (make-temp-file "magnus-codex-project-" t))
         (prompt "Unique Magnus onboarding\nmarker:alpha")
         (file (magnus-test--write-codex-rollout
                root "session-alpha" directory prompt)))
    (unwind-protect
        (should (equal (magnus-codex--session-id-from-file
                        file directory prompt)
                       "session-alpha"))
      (delete-directory root t)
      (delete-directory directory t))))

(ert-deftest magnus-codex-session-capture-is-concurrent-launch-safe ()
  (let* ((root (make-temp-file "magnus-codex-sessions-" t))
         (directory (make-temp-file "magnus-codex-project-" t))
         (first (magnus-instances-create directory "first-codex" 'codex))
         (second (magnus-instances-create directory "second-codex" 'codex))
         (first-file (magnus-test--write-codex-rollout
                      root "session-first" directory "prompt:first"))
         (second-file (magnus-test--write-codex-rollout
                       root "session-second" directory "prompt:second")))
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-codex--session-files)
                   (lambda () (list second-file first-file))))
          (should (equal (magnus-codex--find-session-id
                          first "prompt:first" nil)
                         "session-first"))
          (should (equal (magnus-codex--find-session-id
                          second "prompt:second" nil)
                         "session-second")))
      (delete-directory root t)
      (delete-directory directory t))))

(ert-deftest magnus-codex-session-capture-rejects-subagent-rollout ()
  (let* ((root (make-temp-file "magnus-codex-sessions-" t))
         (directory (make-temp-file "magnus-codex-project-" t))
         (prompt "Inherited parent prompt")
         (file (magnus-test--write-codex-rollout
                root "session-child" directory prompt "session-parent")))
    (unwind-protect
        (should-not
         (magnus-codex--session-id-from-file file directory prompt))
      (delete-directory root t)
      (delete-directory directory t))))

(ert-deftest magnus-codex-session-capture-tolerates-large-preamble ()
  (let* ((root (make-temp-file "magnus-codex-sessions-" t))
         (directory (make-temp-file "magnus-codex-project-" t))
         (prompt "Onboarding after a large preamble")
         (file (magnus-test--write-codex-rollout
                root "session-large" directory prompt nil
                (make-string 150000 ?x))))
    (unwind-protect
        (should (equal (magnus-codex--session-id-from-file
                        file directory prompt)
                       "session-large"))
      (delete-directory root t)
      (delete-directory directory t))))

(ert-deftest magnus-codex-trace-finds-rollouts-from-old-dates ()
  (let* ((home (make-temp-file "magnus-codex-home-" t))
         (other-home (make-temp-file "magnus-codex-other-home-" t))
         (process-environment (copy-sequence process-environment))
         (magnus-codex--trace-file-cache (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "old-codex" 'codex))
         (session-id "019f-old-session")
         (directory (expand-file-name "sessions/2024/01/02" home))
         (file (expand-file-name
                (format "rollout-2024-01-02T00-00-00-%s.jsonl" session-id)
                directory))
         (other-directory (expand-file-name "sessions/2023/12/31" other-home))
         (other-file (expand-file-name
                      (format "rollout-other-%s.jsonl" session-id)
                      other-directory)))
    (unwind-protect
        (progn
          (setenv "CODEX_HOME" home)
          (make-directory directory t)
          (with-temp-file file (insert "{}\n"))
          (magnus-instances-update instance :session-id session-id)
          (should (equal (magnus-codex-trace-file instance) file))
          ;; An identical session ID under another CODEX_HOME must not reuse
          ;; the first root's still-live cached path.
          (setenv "CODEX_HOME" other-home)
          (make-directory other-directory t)
          (with-temp-file other-file (insert "{}\n"))
          (should (equal (magnus-codex-trace-file instance) other-file)))
      (delete-directory home t)
      (delete-directory other-home t))))

(ert-deftest magnus-codex-trace-follows-resumed-rollout ()
  (let* ((home (make-temp-file "magnus-codex-resumed-trace-" t))
         (sessions-root (expand-file-name "sessions" home))
         (process-environment (copy-sequence process-environment))
         (magnus-codex--trace-file-cache (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp/project" "winter-codex"
                                            'codex))
         (session-id "019f-stable-thread")
         root-file continuation-file)
    (unwind-protect
        (progn
          (setenv "CODEX_HOME" home)
          (setq root-file
                (magnus-test--write-codex-rollout
                 sessions-root session-id "/tmp/project" "root turn"
                 nil nil nil "2024/01/02"))
          (magnus-instances-update instance :session-id session-id)
          (with-temp-buffer
            (magnus-trace-mode)
            (setq magnus-trace--instance instance)
            (magnus-trace-refresh)
            (should (equal magnus-trace--jsonl-file root-file))
            ;; A resume keeps SESSION-ID in metadata but gives the rollout a
            ;; new filename ID.  The still-live root cache must not conceal it.
            (let* ((recent-directory
                    (car (magnus-codex--session-directories)))
                   (recent-date
                    (file-relative-name recent-directory sessions-root)))
              (setq continuation-file
                    (magnus-test--write-codex-rollout
                     sessions-root "019f-continuation" "/tmp/project"
                     "resumed turn" nil nil session-id recent-date)))
            (set-file-times continuation-file
                            (time-add (current-time) (seconds-to-time 5)))
            (magnus-trace-refresh)
            (should (equal magnus-trace--jsonl-file continuation-file))
            (should (string-match-p "resumed turn" (buffer-string)))
            (should-not (string-match-p "root turn" (buffer-string)))))
      (delete-directory home t))))

(ert-deftest magnus-codex-trace-normalizes-only-visible-events ()
  (let* ((instance (magnus-instances-create "/tmp" "trace-codex" 'codex))
         (user
          (json-parse-string
           (magnus-test--codex-event-line "user_message" "Question")
           :object-type 'alist))
         (agent
          (json-parse-string
           (magnus-test--codex-event-line "agent_message" "Answer")
           :object-type 'alist))
         (duplicate
          '((type . "response_item")
            (payload . ((type . "message") (role . "assistant")))))
         (reasoning
          '((type . "response_item")
            (payload . ((type . "reasoning")
                        (encrypted_content . "private-ciphertext"))))))
    (should (equal
             (alist-get 'content
                        (alist-get 'message
                                   (magnus-codex-trace-entry instance user)))
             "Question"))
    (should (equal
             (alist-get 'text
                        (aref (alist-get
                               'content
                               (alist-get 'message
                                          (magnus-codex-trace-entry
                                           instance agent)))
                              0))
             "Answer"))
    (should-not (magnus-codex-trace-entry instance duplicate))
    (should-not (magnus-codex-trace-entry instance reasoning))))

(ert-deftest magnus-codex-trace-renders-visible-thinking-markers ()
  (let ((instance (magnus-instances-create "/tmp" "journal-codex" 'codex)))
    (with-temp-buffer
      (magnus-trace-mode)
      (setq magnus-trace--instance instance)
      (let ((inhibit-read-only t))
        (should
         (magnus-trace--render-json-line
          (magnus-test--codex-event-line
           "agent_message"
           (concat "[thinking]\nWorking hypothesis\n[end-thinking]\n"
                   "[response]\nFinal answer\n[end-response]"))))
        (should
         (magnus-trace--render-json-line
          (json-encode
           '((type . "response_item")
             (payload . ((type . "reasoning")
                         (encrypted_content . "private-ciphertext"))))))))
      (should (string-match-p "Working hypothesis" (buffer-string)))
      (should (string-match-p "Final answer" (buffer-string)))
      (should-not (string-match-p "private-ciphertext" (buffer-string)))
      (should (cl-find-if (lambda (overlay)
                            (overlay-get overlay 'magnus-thinking))
                          (overlays-in (point-min) (point-max)))))))

(ert-deftest magnus-trace-never-guesses-a-project-session ()
  (dolist (provider '(claude codex))
    (let ((instance
           (magnus-instances-create
            "/tmp" (format "waiting-%s" provider) provider)))
      (with-temp-buffer
        (magnus-trace-mode)
        (setq magnus-trace--instance instance)
        (cl-letf (((symbol-function 'magnus-process--list-sessions)
                   (lambda (&rest _arguments)
                     (ert-fail "Trace attempted project-wide session discovery"))))
          (magnus-trace-refresh))
        (should (string-match-p "Waiting for session" (buffer-string)))
        (should-not (magnus-instance-session-id instance))))))

(ert-deftest magnus-trace-retains-partial-jsonl-records ()
  (let* ((file (make-temp-file "magnus-trace-partial-"))
         (instance (magnus-instances-create "/tmp" "partial-claude" 'claude))
         (line (json-encode
                '((type . "user")
                  (timestamp . "2026-07-11T08:00:00.000Z")
                  (message . ((content . "split-once"))))))
         (split (/ (length line) 2)))
    (unwind-protect
        (with-temp-buffer
          (magnus-trace-mode)
          (setq magnus-trace--instance instance)
          (with-temp-file file
            (insert (substring line 0 split)))
          (magnus-trace--append-new-entries file)
          (should (string-empty-p (buffer-string)))
          (should-not (string-empty-p magnus-trace--pending-text))
          (write-region (concat (substring line split) "\n") nil file t 'silent)
          (magnus-trace--append-new-entries file)
          (should (= 1 (how-many "split-once" (point-min) (point-max))))
          (should (string-empty-p magnus-trace--pending-text)))
      (delete-file file))))

(ert-deftest magnus-trace-renders-valid-final-record-without-newline ()
  (let* ((file (make-temp-file "magnus-trace-no-newline-"))
         (instance (magnus-instances-create "/tmp" "no-newline" 'claude)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             (json-encode
              '((type . "user")
                (message . ((content . "complete-without-newline")))))))
          (with-temp-buffer
            (magnus-trace-mode)
            (setq magnus-trace--instance instance)
            (magnus-trace--append-new-entries file)
            (should (= 1 (how-many "complete-without-newline"
                                   (point-min) (point-max))))
            (should (string-empty-p magnus-trace--pending-text))))
      (delete-file file))))

(ert-deftest magnus-trace-malformed-line-does-not-hide-later-records ()
  (let* ((file (make-temp-file "magnus-trace-malformed-"))
         (instance (magnus-instances-create "/tmp" "malformed" 'claude)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{not-json}\n"
                    (json-encode
                     '((type . "user")
                       (message . ((content . "survived-malformed")))))
                    "\n"))
          (with-temp-buffer
            (magnus-trace-mode)
            (setq magnus-trace--instance instance)
            (magnus-trace--append-new-entries file)
            (should (string-match-p "survived-malformed" (buffer-string)))))
      (delete-file file))))

(ert-deftest magnus-trace-switching-files-resets-rendered-state ()
  (let* ((first (make-temp-file "magnus-trace-first-"))
         (second (make-temp-file "magnus-trace-second-"))
         (instance (magnus-instances-create "/tmp" "switch-claude" 'claude)))
    (unwind-protect
        (progn
          (with-temp-file first
            (insert
             (json-encode
              '((type . "user") (message . ((content . "first-file")))))
             "\n"))
          (with-temp-file second
            (insert
             (json-encode
              '((type . "user") (message . ((content . "second-file")))))
             "\n"))
          (with-temp-buffer
            (magnus-trace-mode)
            (setq magnus-trace--instance instance)
            (magnus-trace--append-new-entries first)
            (magnus-trace--append-new-entries second)
            (should-not (string-match-p "first-file" (buffer-string)))
            (should (string-match-p "second-file" (buffer-string)))))
      (delete-file first)
      (delete-file second))))

(ert-deftest magnus-trace-trim-adopts-one-atomic-snapshot ()
  (let* ((instance (magnus-instances-create "/tmp" "trim-claude" 'claude))
         (line (json-encode
                '((type . "user")
                  (message . ((content . "snapshot-message"))))))
         (magnus-trace-max-initial-entries 1))
    (with-temp-buffer
      (magnus-trace-mode)
      (setq magnus-trace--instance instance
            magnus-trace--pending-text "stale-fragment"
            magnus-trace--file-offset 999)
      (cl-letf (((symbol-function 'magnus-trace--tail-snapshot)
                 (lambda (_file _keep)
                   (list :lines (list line)
                         :start 0
                         :total 1
                         :skip 0
                         :pending "current-fragment"
                         :discarding nil
                         :size 42))))
        (magnus-trace--trim "snapshot.jsonl"))
      (should (= magnus-trace--file-offset 42))
      (should (equal magnus-trace--pending-text "current-fragment"))
      (should (string-match-p "snapshot-message" (buffer-string))))))

(ert-deftest magnus-trace-tail-pagination-and-trim-bound-read-ranges ()
  (let* ((file (make-temp-file "magnus-trace-bounded-"))
         (instance (magnus-instances-create "/tmp" "bounded-codex" 'codex))
         (magnus-trace-max-initial-entries 3)
         (magnus-trace-max-buffer-lines nil)
         (magnus-trace-read-chunk-bytes 257)
         (read-range (symbol-function 'magnus-trace--read-range))
         ranges)
    (unwind-protect
        (progn
          (with-temp-file file
            (dotimes (index 600)
              (insert
               (magnus-test--codex-event-line
                "user_message" (format "bounded-%03d" index))
               "\n")))
          (with-temp-buffer
            (magnus-trace-mode)
            (setq magnus-trace--instance instance)
            (cl-letf (((symbol-function 'magnus-trace--read-range)
                       (lambda (path start end)
                         (push (- end start) ranges)
                         (funcall read-range path start end))))
              (magnus-trace--append-new-entries file)
              (should (= magnus-trace--skip-count 597))
              (should-not (string-match-p "bounded-596" (buffer-string)))
              (should (string-match-p "bounded-599" (buffer-string)))
              (magnus-trace-load-earlier)
              (should (= magnus-trace--skip-count 594))
              (should (string-match-p "bounded-596" (buffer-string)))
              (magnus-trace--trim file)
              (should (= magnus-trace--skip-count 597))
              (should-not (string-match-p "bounded-596" (buffer-string)))
              (should (string-match-p "bounded-599" (buffer-string)))))
          (should (> (length ranges) 3))
          (should (cl-every
                   (lambda (length)
                     (<= length magnus-trace-read-chunk-bytes))
                   ranges)))
      (delete-file file))))

(ert-deftest magnus-trace-bounds-an-unfinished-record ()
  (let* ((file (make-temp-file "magnus-trace-oversized-"))
         (instance (magnus-instances-create "/tmp" "oversized" 'claude))
         (magnus-trace-read-chunk-bytes 11)
         (magnus-trace-max-record-bytes 128))
    (unwind-protect
        (with-temp-buffer
          (magnus-trace-mode)
          (setq magnus-trace--instance instance)
          (with-temp-file file
            (insert "{" (make-string 256 ?x)))
          (magnus-trace--append-new-entries file)
          (should (string-empty-p magnus-trace--pending-text))
          (should magnus-trace--discarding-incomplete-record-p)
          (write-region
           (concat
            "\n"
            (json-encode
             '((type . "user")
               (message . ((content . "after-oversized")))))
            "\n")
           nil file t 'silent)
          (magnus-trace--append-new-entries file)
          (should-not magnus-trace--discarding-incomplete-record-p)
          (should (string-empty-p magnus-trace--pending-text))
          (should (string-match-p "after-oversized" (buffer-string))))
      (delete-file file))))

(ert-deftest magnus-codex-trace-pagination-uses-provider-adapter ()
  (let* ((file (make-temp-file "magnus-trace-codex-"))
         (instance (magnus-instances-create "/tmp" "paged-codex" 'codex))
         (magnus-trace-max-initial-entries 1))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (magnus-test--codex-event-line "user_message" "old-message")
                    "\n"
                    (magnus-test--codex-event-line "agent_message" "latest")
                    "\n"))
          (with-temp-buffer
            (magnus-trace-mode)
            (setq magnus-trace--instance instance)
            (magnus-trace--append-new-entries file)
            (should-not (string-match-p "old-message" (buffer-string)))
            (should (string-match-p "latest" (buffer-string)))
            (magnus-trace-load-earlier)
            (should (string-match-p "old-message" (buffer-string)))))
      (delete-file file))))

(ert-deftest magnus-codex-stale-capture-cannot-reassign-replacement ()
  (let* ((instance (magnus-instances-create "/tmp" "replacement" 'codex))
         (old-terminal (magnus-test--codex-tui instance))
         (old-process (cdr old-terminal))
         (new-terminal (magnus-test--codex-tui instance)))
    (unwind-protect
        (progn
          (process-put old-process 'magnus-codex-capture-prompt "old")
          (process-put old-process 'magnus-codex-files-before nil)
          (process-put old-process 'magnus-codex-capture-deadline
                       (+ (float-time) 30))
          (cl-letf (((symbol-function 'magnus-codex--find-session-id)
                     (lambda (&rest _arguments) "stale-session")))
            (magnus-codex--poll-session old-process))
          (should-not (magnus-instance-session-id instance))
          (should (eq (magnus-codex--tui-process instance)
                      (cdr new-terminal))))
      (magnus-test--delete-terminal old-terminal)
      (magnus-test--delete-terminal new-terminal))))

(ert-deftest magnus-codex-messages-use-tui-as-sole-writer ()
  (let* ((instance (magnus-instances-create "/tmp" "tui-codex" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (process (cdr terminal))
         sent return-sent timers)
    (process-put process 'magnus-codex-ready t)
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (text &optional _paste-p) (setq sent text)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq return-sent t)))
                  ((symbol-function 'run-with-timer)
                   (lambda (_seconds _repeat function &rest arguments)
                     (push (cons function arguments) timers)
                     'test-timer)))
          (magnus-codex-send instance "read this next")
          (should (equal sent "read this next"))
          (should return-sent)
          (should-not
           (process-get process 'magnus-terminal-delivery-queue)))
      (magnus-test--delete-terminal terminal))))

(ert-deftest magnus-codex-accepted-callback-waits-for-tui-submission ()
  (let* ((instance (magnus-instances-create "/tmp" "receipt-codex" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (process (cdr terminal))
         (accepted 0)
         sent)
    ;; A not-yet-ready process is enough to exercise the crash window without
    ;; depending on the selected window in a batch Emacs.
    (process-put process 'magnus-codex-ready nil)
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (&rest _args) (setq sent t)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq sent t))))
          (should
           (eq (magnus-codex-send
                instance "accepted result"
                (lambda () (cl-incf accepted)))
               'queued))
          (should (= accepted 0))
          (should-not sent)
          (should (process-get process 'magnus-terminal-delivery-queue))
          ;; Stopping before readiness discards the process-local queue without
          ;; falsely acknowledging a delivery which never reached the TUI.
          (delete-process process)
          (should (= accepted 0)))
      (magnus-test--delete-terminal terminal))))

(ert-deftest magnus-codex-supplied-delivery-scope-is-cancellable ()
  (let* ((instance (magnus-instances-create "/tmp" "scoped-codex" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (process (cdr terminal))
         (scope (list 'review-controller "review-1"))
         (cancelled-accepted 0)
         (ordinary-accepted 0)
         sent)
    (process-put process 'magnus-codex-ready nil)
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (text &optional _paste-p) (push text sent)))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (push 'return sent)))
                  ((symbol-function 'run-with-timer)
                   (lambda (&rest _arguments) 'test-timer)))
          (should
           (eq (magnus-codex-send
                instance "cancel me"
                (lambda () (cl-incf cancelled-accepted))
                scope)
               'queued))
          (should
           (eq (magnus-codex-send
                instance "keep me"
                (lambda () (cl-incf ordinary-accepted)))
               'queued))
          (let ((queue
                 (process-get process 'magnus-terminal-delivery-queue)))
            (should (eq (plist-get (car queue) :scope) scope))
            (should (eq (plist-get (cadr queue) :scope) 'codex)))
          (magnus-terminal-cancel-scope scope)
          (should (= cancelled-accepted 0))
          (process-put process 'magnus-codex-ready t)
          (should (eq (magnus-terminal-drain process) 'submitted))
          (should (= cancelled-accepted 0))
          (should (= ordinary-accepted 1))
          (should (equal (nreverse sent) '("keep me" return))))
      (magnus-test--delete-terminal terminal))))

(ert-deftest magnus-codex-tui-send-error-retains-and-retries-entry ()
  (let* ((instance (magnus-instances-create "/tmp" "retry-codex" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (process (cdr terminal))
         (attempts 0)
         (accepted 0)
         timers events)
    (process-put process 'magnus-codex-ready t)
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (text &optional _paste-p)
                     (cl-incf attempts)
                     (if (= attempts 1)
                         (error "temporary vterm failure")
                       (push text events))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (push 'return events)))
                  ((symbol-function 'run-with-timer)
                   (lambda (_seconds _repeat function &rest arguments)
                     (setq timers
                           (append timers (list (cons function arguments))))
                     'test-timer)))
          (should
           (eq (magnus-codex-send
                instance "retry this"
                (lambda () (cl-incf accepted)))
               'queued))
          (should (= accepted 0))
          (should-not (process-get process 'magnus-terminal-delivery-busy))
          (should (process-get process 'magnus-terminal-delivery-queue))
          (while timers
            (let ((timer (pop timers)))
              (apply (car timer) (cdr timer))))
          (should (= attempts 2))
          (should (= accepted 1))
          (should (equal (nreverse events) '("retry this" return)))
          (should (magnus-terminal-delivery-idle-p process)))
      (magnus-test--delete-terminal terminal))))

(ert-deftest magnus-codex-concurrent-tui-messages-are-serialized ()
  (let* ((instance (magnus-instances-create "/tmp" "queued-codex" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (process (cdr terminal))
         (ready-hooks 0)
         (magnus-process-ready-hook
          (list (lambda (_instance) (cl-incf ready-hooks))))
         timers events)
    (process-put process 'magnus-codex-ready t)
    (process-put process 'magnus-codex-ready-hook-pending t)
    (unwind-protect
        (cl-letf (((symbol-function 'vterm-send-string)
                   (lambda (text &optional _paste-p)
                     (setq events (append events (list text)))))
                  ((symbol-function 'vterm-send-return)
                   (lambda () (setq events (append events '(return)))))
                  ((symbol-function 'run-with-timer)
                   (lambda (_seconds _repeat function &rest arguments)
                     (setq timers
                           (append timers (list (cons function arguments))))
                     'test-timer)))
          (magnus-codex-send instance "first")
          (magnus-codex-send instance "second")
          (should (equal events '("first" return)))
          (should (= ready-hooks 0))
          (while timers
            (let ((timer (pop timers)))
              (apply (car timer) (cdr timer))))
          (should (equal events '("first" return "second" return)))
          (should (= ready-hooks 1))
          (should-not
           (process-get process 'magnus-codex-ready-hook-pending)))
      (magnus-test--delete-terminal terminal))))

(ert-deftest magnus-codex-stop-interrupts-and-closes-native-tui ()
  (let* ((instance (magnus-instances-create "/tmp" "stoppable" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (process (cdr terminal))
         sent-key)
    (process-put process 'magnus-codex-capture-prompt "short-lived")
    (process-put process 'magnus-codex-files-before nil)
    (process-put process 'magnus-codex-capture-deadline (+ (float-time) 30))
    (cl-letf (((symbol-function 'vterm-send-key)
               (lambda (key &rest _arguments) (setq sent-key key)))
              ((symbol-function 'magnus-codex--find-session-id)
               (lambda (&rest _arguments) "session-at-stop"))
              ((symbol-function 'run-with-timer)
               (lambda (_seconds _repeat function &rest arguments)
                 (apply function arguments))))
      (magnus-codex-stop instance)
      (should (equal sent-key "C-c"))
      (should (equal (magnus-instance-session-id instance) "session-at-stop"))
      (should (eq (magnus-instance-status instance) 'stopped))
      (should-not (magnus-instance-buffer instance)))
    (magnus-test--delete-terminal terminal)))

(ert-deftest magnus-codex-delayed-stop-preserves-same-buffer-replacement ()
  (let* ((instance
          (magnus-instances-create "/tmp" "delayed-stop-replacement" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (buffer (car terminal))
         (old-process (cdr terminal))
         replacement-process
         delayed-stop
         sent-key)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'vterm-send-key)
                     (lambda (key &rest _arguments) (setq sent-key key)))
                    ((symbol-function 'run-with-timer)
                     (lambda (_seconds _repeat function &rest arguments)
                       (setq delayed-stop (cons function arguments))
                       'test-timer)))
            (magnus-codex-stop instance))
          (should (equal sent-key "C-c"))
          (should delayed-stop)
          ;; A new TUI can claim the same terminal buffer during the grace
          ;; period after C-c but before the delayed stop callback runs.
          (setq replacement-process
                (make-pipe-process
                 :name (generate-new-buffer-name
                        "magnus-delayed-stop-replacement")
                 :buffer buffer :noquery t))
          (process-put replacement-process 'magnus-codex-instance instance)
          (magnus-instances-update
           instance :buffer buffer :status 'running)
          (apply (car delayed-stop) (cdr delayed-stop))
          (should-not (process-live-p old-process))
          (should (process-live-p replacement-process))
          (should (buffer-live-p buffer))
          (should (eq (get-buffer-process buffer) replacement-process))
          (should (eq (magnus-instance-buffer instance) buffer))
          (should (eq (magnus-instance-status instance) 'running)))
      (when (process-live-p old-process)
        (delete-process old-process))
      (when (process-live-p replacement-process)
        (delete-process replacement-process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magnus-codex-stale-tui-exit-preserves-replacement ()
  (let* ((instance (magnus-instances-create "/tmp" "replacement" 'codex))
         (old-terminal (magnus-test--codex-tui instance))
         (old-process (cdr old-terminal))
         (new-terminal nil)
         sentinel)
    (unwind-protect
        (progn
          (magnus-codex--setup-tui-sentinel instance (car old-terminal))
          (setq sentinel (process-sentinel old-process))
          (setq new-terminal (magnus-test--codex-tui instance))
          (set-process-sentinel old-process nil)
          (delete-process old-process)
          (funcall sentinel old-process "exited")
          (should (magnus-codex-running-p instance))
          (should (eq (magnus-instance-status instance) 'running)))
      (magnus-test--delete-terminal old-terminal)
      (magnus-test--delete-terminal new-terminal))))

(ert-deftest magnus-codex-stale-tui-exit-preserves-same-buffer-replacement ()
  (let* ((instance
          (magnus-instances-create "/tmp" "same-buffer-replacement" 'codex))
         (buffer (generate-new-buffer " *magnus-same-buffer-codex-tui*"))
         (old-process
          (make-pipe-process
           :name (generate-new-buffer-name "magnus-old-same-buffer-tui")
           :buffer buffer :noquery t))
         replacement-process
         sentinel
         (magnus-buffer-name " *magnus-status-not-present*"))
    (unwind-protect
        (progn
          (magnus-instances-update
           instance :buffer buffer :status 'running)
          (magnus-codex--setup-tui-sentinel instance buffer)
          (setq sentinel (process-sentinel old-process))
          ;; A second process can claim the same terminal buffer before Emacs
          ;; dispatches the old process's terminal sentinel.
          (setq replacement-process
                (make-pipe-process
                 :name (generate-new-buffer-name
                        "magnus-new-same-buffer-tui")
                 :buffer buffer :noquery t))
          (process-put replacement-process 'magnus-codex-instance instance)
          (set-process-sentinel old-process nil)
          (delete-process old-process)
          (funcall sentinel old-process "exited")
          (should (eq (get-buffer-process buffer) replacement-process))
          (should (magnus-codex-running-p instance))
          (should (eq (magnus-instance-status instance) 'running)))
      (when (process-live-p old-process)
        (delete-process old-process))
      (when (process-live-p replacement-process)
        (delete-process replacement-process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magnus-codex-current-tui-exit-publishes-stopped-state ()
  (let* ((instance (magnus-instances-create "/tmp" "current-exit" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (buffer (car terminal))
         (process (cdr terminal))
         (magnus-buffer-name " *magnus-status-not-present*")
         sentinel)
    (unwind-protect
        (progn
          (magnus-codex--setup-tui-sentinel instance buffer)
          (setq sentinel (process-sentinel process))
          (set-process-sentinel process nil)
          (delete-process process)
          (funcall sentinel process "finished")
          (should (eq (magnus-instance-buffer instance) buffer))
          (should (eq (magnus-instance-status instance) 'stopped)))
      (magnus-test--delete-terminal terminal))))

(ert-deftest magnus-coord-stopped-agent-nudge-is-logged-and-does-not-signal ()
  (let* ((directory (make-temp-file "magnus-stopped-nudge-" t))
         (instance (magnus-instances-create directory "stopped-codex" 'codex))
         (magnus-coord-nudge-debounce nil)
         logged)
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest arguments)
                     (setq logged (apply #'format format-string arguments)))))
          (should-not
           (magnus-coord-nudge-agent
            instance "Please tell @bold-wren later" "Magnus"))
          (should (string-match-p "Undelivered nudge" logged))
          (should (string-match-p "(at) bold-wren" logged))
          (let ((file (expand-file-name ".magnus-coord.md" directory)))
            (should (file-exists-p file))
            (with-temp-buffer
              (insert-file-contents file)
              (should (search-forward "Undelivered nudge" nil t))
              (should (search-forward "(at) bold-wren" nil t)))))
      (delete-directory directory t))))

(ert-deftest magnus-coord-legacy-running-check-needs-no-process-module-call ()
  (let* ((instance (magnus-instances-create "/tmp" "legacy-live" 'claude))
         (buffer (generate-new-buffer " *magnus-legacy-live*"))
         (process (make-pipe-process
                   :name (generate-new-buffer-name "magnus-legacy-live")
                   :buffer buffer :noquery t)))
    (magnus-instances-update instance :buffer buffer :status 'running)
    (unwind-protect
        (should (magnus-coord--instance-running-p instance))
      (delete-process process)
      (kill-buffer buffer))))

(ert-deftest magnus-coord-headless-nudge-never-enters-terminal-transport ()
  (let* ((directory (make-temp-file "magnus-headless-nudge-" t))
         (instance
          (magnus-instances-create
           directory "headless-worker" 'claude 'headless))
         (magnus-coord-nudge-debounce nil)
         logged)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-coord--log-undelivered-nudge)
                   (lambda (_instance _text reason)
                     (setq logged reason)
                     nil))
                  ((symbol-function 'magnus-terminal-submit)
                   (lambda (&rest _arguments)
                     (ert-fail "headless nudge reached terminal transport")))
                  ((symbol-function 'magnus-provider-call)
                   (lambda (&rest _arguments)
                     (ert-fail "headless nudge reached provider transport"))))
          (should-not
           (magnus-coord-nudge-agent instance "hello" "Peer"))
          (should (equal logged
                         "headless task has no interactive terminal")))
      (delete-directory directory t))))

(ert-deftest magnus-coord-local-nudge-uses-terminal-single-writer ()
  (let* ((instance (magnus-instances-create "/tmp" "local-nudge" 'claude))
         (buffer (generate-new-buffer " *magnus-local-nudge*"))
         (process (make-pipe-process
                   :name (generate-new-buffer-name "magnus-local-nudge")
                   :buffer buffer :noquery t))
         (magnus-coord-nudge-debounce nil)
         submission)
    (magnus-instances-update instance :buffer buffer :status 'running)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-terminal-submit)
                   (lambda (&rest arguments)
                     (setq submission arguments)
                     'submitted))
                  ((symbol-function 'vterm-send-string)
                   (lambda (&rest _arguments)
                     (ert-fail "coord must use the terminal arbiter")))
                  ((symbol-function 'vterm-send-return)
                   (lambda (&rest _arguments)
                     (ert-fail "coord must use the terminal arbiter"))))
          (magnus-coord-nudge-agent instance "hello" "Peer")
          (should
           (equal submission
                  (list instance "[From Peer]: hello" nil
                        :settle-delay 0.1 :scope 'magnus-coord))))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magnus-codex-try-send-declines-without-prequeueing ()
  "Codex exposes ephemeral delivery with its native readiness contract."
  (let* ((instance (magnus-instances-create "/tmp" "codex-try" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (process (cdr terminal))
         submission)
    (unwind-protect
        (progn
          (process-put process 'magnus-codex-ready t)
          (cl-letf (((symbol-function 'magnus-terminal-try-submit)
                     (lambda (&rest arguments)
                       (setq submission arguments)
                       nil)))
            (should-not
             (magnus-provider-call
              instance 'try-send "ephemeral" #'ignore))
            (should (eq (car submission) instance))
            (should (equal (cadr submission) "ephemeral"))
            (should (eq (plist-get (cddr submission) :ready-p)
                        #'magnus-codex--delivery-ready-p))
            (should (eq (plist-get (cddr submission) :accepted) #'ignore))))
      (magnus-test--delete-terminal terminal))))

(ert-deftest magnus-health-check-supports-codex-tui-buffers ()
  (let* ((magnus-health--state (make-hash-table :test #'equal))
         (instance (magnus-instances-create "/tmp" "healthy-codex" 'codex))
         (terminal (magnus-test--codex-tui instance))
         (buffer (car terminal)))
    (with-current-buffer buffer
      (insert "streamed Codex output"))
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-process-running-p)
                   (lambda (candidate)
                     (magnus-provider-call candidate 'running-p))))
          (magnus-health--check-instance instance)
          (should (eq (magnus-health-get instance) 'ok)))
      (magnus-test--delete-terminal terminal))))

(provide 'magnus-provider-tests)
;;; magnus-provider-tests.el ends here
