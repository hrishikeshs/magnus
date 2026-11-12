;;; magnus-status-tests.el --- Status UX tests for Magnus -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
;; CI intentionally tests with `emacs -Q' and does not install vterm.  Status
;; rendering only needs the process function boundaries, not vterm itself.
(unless (featurep 'vterm)
  (provide 'vterm))
(require 'magnus-status)
(require 'magnus-transient)

(defconst magnus-test-status--base-oid
  "1111111111111111111111111111111111111111")

(defconst magnus-test-status--head-oid
  "2222222222222222222222222222222222222222")

(defun magnus-test-status--completed-round
    (&optional number verdict read-state finding-count)
  "Return one completed review round for presentation tests."
  (magnus-review-round--create
   :number (or number 1)
   :base-oid magnus-test-status--base-oid
   :head-oid magnus-test-status--head-oid
   :created-at 1
   :completed-at 2
   :verdict (or verdict 'comment)
   :read-state (or read-state 'read)
   :finding-count (or finding-count 0)))

(defun magnus-test-status--candidate-round (&optional number)
  "Return one prepared, not-yet-completed review round numbered NUMBER."
  (magnus-review-candidate--create
   :number (or number 2)
   :base-oid magnus-test-status--base-oid
   :head-oid magnus-test-status--head-oid
   :created-at 3))

(defun magnus-test-status--runtime-state (review)
  "Return REVIEW's fixture-only ephemeral execution state."
  (magnus-review-reviewer-expertise review))

(defun magnus-test-status--render-instance (instance reviews)
  "Render INSTANCE with REVIEWS and return its text plus slot position."
  (let ((magnus-review-runtime-state-function
         #'magnus-test-status--runtime-state))
    (with-temp-buffer
      (cl-letf (((symbol-function 'magnus-review-list) (lambda () reviews))
                ((symbol-function 'magnus-process-running-p)
                 (lambda (_instance) t))
                ((symbol-function 'magnus-coord-agent-busy-p)
                 (lambda (_instance) nil))
                ((symbol-function 'magnus-coord--neglected-p)
                 (lambda (_instance) nil))
                ((symbol-function 'magnus-health-indicator)
                 (lambda (_instance) "+"))
                ((symbol-function 'magnus--agents-index-get)
                 (lambda (_directory _name) nil)))
        (magnus-status--insert-instance instance)
        (list (buffer-string)
              (text-property-any (point-min) (point-max)
                                 'magnus-review-animation-slot t))))))

(defun magnus-test-status--review-in-state (execution &rest arguments)
  "Return a completed-lineage fixture with ephemeral EXECUTION.
ARGUMENTS are passed to `magnus-review--create'."
  (apply
   #'magnus-review--create
   (append
    arguments
    (list
     :reviewer-expertise execution
     :rounds
     (unless (eq execution 'asking-scope)
       (list (magnus-test-status--completed-round)))))))

(ert-deftest magnus-status-review-keys-are-directly-discoverable ()
  (should (eq (lookup-key magnus-status-mode-map (kbd "v"))
              'magnus-review-request-dispatch))
  (should (eq (lookup-key magnus-status-mode-map (kbd "V"))
              'magnus-review-actions)))

(ert-deftest magnus-status-codex-creation-is-directly-bound ()
  (should (eq (lookup-key magnus-status-mode-map (kbd "X"))
              'magnus-transient-create-codex))
  (with-temp-buffer
    (magnus-status-mode)
    (let ((inhibit-read-only t))
      (insert "A status heading with no selected entity"))
    (goto-char (point-min))
    (should (eq (key-binding (kbd "X"))
                'magnus-transient-create-codex))))

(ert-deftest magnus-status-archive-cleanup-is-directly-discoverable ()
  (should (eq (lookup-key magnus-status-mode-map (kbd "K"))
              'magnus-status-forget-archived))
  (should (eq (lookup-key magnus-status-mode-map (kbd "U"))
              'magnus-status-toggle-archived)))

(ert-deftest magnus-status-folds-only-the-three-workspace-sections ()
  (should (eq (lookup-key magnus-status-mode-map (kbd "TAB"))
              'magnus-status-toggle-section))
  (should (eq (lookup-key magnus-status-mode-map (kbd "<tab>"))
              'magnus-status-toggle-section))
  (should
   (equal (mapcar #'car magnus-status--foldable-section-titles)
          '(reviews coordination archived-agents)))
  (with-temp-buffer
    (magnus-status-mode)
    (let ((inhibit-read-only t))
      (dolist (section '(reviews coordination archived-agents))
        (magnus-status--insert-foldable-heading section))
      (insert "Instances\nMagnus\n"))
    (goto-char (point-min))
    (dolist (section '(reviews coordination archived-agents))
      (should (eq (magnus-status--section-at-point) section))
      (forward-line 1))
    (should-not (magnus-status--section-at-point))
    (forward-line 1)
    (should-not (magnus-status--section-at-point))))

(ert-deftest magnus-status-tab-and-return-toggle-section-headings ()
  (with-temp-buffer
    (magnus-status-mode)
    (let ((inhibit-read-only t))
      (magnus-status--insert-foldable-heading 'reviews)
      (insert "Instances\n"))
    (goto-char (point-min))
    (cl-letf (((symbol-function 'magnus-status-refresh) #'ignore))
      (magnus-status-toggle-section)
      (should (equal magnus-status--collapsed-sections '(reviews)))
      ;; RET retains its entity-visit command and dispatches only on a heading.
      (magnus-status-visit)
      (should-not magnus-status--collapsed-sections)
      (forward-line 1)
      (should-error (magnus-status-toggle-section) :type 'user-error)))
  (let* ((instance
          (magnus-instance--create
           :id "active" :name "quick-wren" :status 'running))
         (magnus-instances (list instance))
         visited)
    (with-temp-buffer
      (insert (propertize "quick-wren" 'magnus-instance-id "active"))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'magnus-process-switch-to)
                 (lambda (selected) (setq visited selected))))
        (magnus-status-visit)))
    (should (eq visited instance))))

(ert-deftest magnus-status-refresh-preserves-buffer-local-heading-state ()
  (let* ((magnus-buffer-name " *magnus-fold-refresh-test*")
         (buffer (get-buffer-create magnus-buffer-name))
         shifted)
    (unwind-protect
        (cl-letf (((symbol-function 'magnus-status--insert-header)
                   (lambda ()
                     (insert (if shifted "Header\nShifted\n" "Header\n"))))
                  ((symbol-function 'magnus-status--insert-instances)
                   (lambda () (insert "Instances\n")))
                  ((symbol-function 'magnus-status--insert-reviews)
                   (lambda ()
                     (insert "\n")
                     (magnus-status--insert-foldable-heading 'reviews)
                     (unless (magnus-status--section-collapsed-p 'reviews)
                       (insert "review body\n"))))
                  ((symbol-function 'magnus-status--insert-coordination)
                   #'ignore)
                  ((symbol-function 'magnus-status--insert-purged) #'ignore)
                  ((symbol-function 'magnus-status--sync-review-animation)
                   #'ignore))
          (with-current-buffer buffer
            (magnus-status-mode)
            (magnus-status-refresh)
            (goto-char (point-min))
            (search-forward "Reviews")
            (beginning-of-line)
            (setq-local magnus-status--collapsed-sections '(reviews))
            (setq shifted t)
            (magnus-status-refresh)
            (should (eq (magnus-status--section-at-point) 'reviews))
            (should (looking-at-p "▸ Reviews"))
            (should (equal magnus-status--collapsed-sections '(reviews))))
          (with-temp-buffer
            (magnus-status-mode)
            (should-not magnus-status--collapsed-sections)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest magnus-status-unbound-printable-key-explains-how-to-proceed ()
  (with-temp-buffer
    (magnus-status-mode)
    (let ((inhibit-read-only t))
      (insert "A status heading with no selected entity"))
    (goto-char (point-min))
    (should (eq (key-binding (kbd "Y"))
                'magnus-status-explain-unavailable-key))
    (let ((err
           (cl-letf (((symbol-function 'this-command-keys-vector)
                      (lambda () (kbd "Y"))))
             (should-error (magnus-status-explain-unavailable-key)
                           :type 'user-error))))
      (should (string-match-p
               "Y is not available here"
               (error-message-string err)))
      (should (string-match-p
               "use n/p to select an agent or review"
               (error-message-string err)))
      (should (string-match-p
               (regexp-quote "press ? for all actions")
               (error-message-string err))))))

(ert-deftest magnus-status-contextual-action-errors-name-the-required-row ()
  (with-temp-buffer
    (magnus-status-mode)
    (let ((err (should-error (magnus-status-trace) :type 'user-error)))
      (should
       (equal (error-message-string err)
              (concat "Put point on an agent row to open its thinking trace; "
                      "use n/p to select one, then retry"))))
    (let ((err (should-error (magnus-review-actions) :type 'user-error)))
      (should
       (equal (error-message-string err)
              (concat "Put point on a review row to open its actions; "
                      "use n/p to select one, then retry"))))))

(ert-deftest magnus-status-coordination-command-is-explicitly-discoverable ()
  (should-not (lookup-key magnus-status-mode-map (kbd "J")))
  (should (eq (lookup-key magnus-status-mode-map (kbd "C"))
              'magnus-status-coordination))
  (should-error (transient-get-suffix 'magnus-dispatch "J"))
  (should
   (equal (transient-get-suffix 'magnus-dispatch "C")
          (transient-get-suffix 'magnus-dispatch
                                'magnus-status-coordination))))

(ert-deftest magnus-status-manual-refresh-polls-markdown-coordination ()
  (let ((magnus-buffer-name " *magnus-manual-refresh-test*")
        calls)
    (cl-letf (((symbol-function 'magnus-coord-refresh-all)
               (lambda () (push 'refresh calls)))
              ((symbol-function 'magnus-coord-reconcile-all)
               (lambda () (push 'reconcile calls)))
              ((symbol-function 'called-interactively-p)
               (lambda (&rest _arguments) t)))
      (magnus-status-refresh))
    (should (equal (nreverse calls) '(reconcile refresh)))))

(ert-deftest magnus-status-coordination-section-uses-markdown-state ()
  (let ((directories '("/first/" "/second/" "/empty/")))
    (with-temp-buffer
      (cl-letf (((symbol-function 'magnus-status--get-project-directories)
                 (lambda () directories))
                ((symbol-function 'magnus-coord-has-state-p)
                 (lambda (directory)
                   (member directory '("/first/" "/second/"))))
                ((symbol-function 'magnus-coord-parse)
                 (lambda (_directory) '(:active nil :log nil))))
        (magnus-status--insert-coordination))
      (should (string-match-p "Coordination" (buffer-string)))
      (should (string-match-p "/first/" (buffer-string)))
      (should (string-match-p "/second/" (buffer-string)))
      (should-not (string-match-p "/empty/" (buffer-string))))))

(ert-deftest magnus-status-projects-deduplicate-physical-directory-aliases ()
  (let* ((directory (make-temp-file "magnus-status-project-" t))
         (link (concat directory "-link"))
         (real-instance
          (magnus-instance--create
           :id "real" :name "real" :directory directory :status 'running))
         (link-instance
          (magnus-instance--create
           :id "link" :name "link" :directory link :status 'running))
         (magnus-instances (list real-instance link-instance)))
    (unwind-protect
        (progn
          (make-symbolic-link directory link)
          (should
           (equal (magnus-status--get-project-directories)
                  (list (magnus-coord--normalized-directory directory)))))
      (when (file-symlink-p link) (delete-file link))
      (delete-directory directory t))))

(ert-deftest magnus-status-recent-log-shows-the-chronological-tail ()
  (let ((log
         (mapcar (lambda (message)
                   (list :time "12:00" :agent "agent" :message message))
                 '("one" "two" "three" "four"))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'magnus-coord-parse)
                 (lambda (_directory) (list :active nil :log log))))
        (magnus-status--insert-coordination-for-dir default-directory))
      (let ((rendered (buffer-string)))
        (should-not (string-match-p "one" rendered))
        (should (< (string-match "two" rendered)
                   (string-match "three" rendered)))
        (should (< (string-match "three" rendered)
                   (string-match "four" rendered)))))))

(ert-deftest magnus-status-coordination-row-retains-its-project-context ()
  (let ((first
         (magnus-instance--create
          :id "first" :name "first" :directory "/first/" :status 'running)))
    (with-temp-buffer
      (cl-letf (((symbol-function 'magnus-coord-parse)
                 (lambda (_directory) '(:active nil :log nil))))
        (magnus-status--insert-coordination-for-dir "/selected/"))
      (goto-char (point-min))
      (search-forward "selected")
      (let ((magnus-instances (list first)))
        (should (equal (magnus-status--coordination-directory)
                       "/selected/"))))))

(ert-deftest magnus-status-opens-shared-coordination-file ()
  (let ((instance (magnus-instance--create
                   :id "author" :name "quick-wren"
                   :directory "/project/"))
        opened)
    (cl-letf (((symbol-function 'magnus-status--get-instance-at-point)
               (lambda () instance))
              ((symbol-function 'magnus-coord-open)
               (lambda (directory) (setq opened directory))))
      (magnus-status-coordination))
    (should (equal opened "/project/"))))

(ert-deftest magnus-status-context-hints-use-buffer-local-eldoc ()
  (let ((magnus-status-show-context-hints t))
    (with-temp-buffer
      (magnus-status-mode)
      (should eldoc-mode)
      (should (eq eldoc-message-function
                  #'magnus-status--context-hint-message))
      (should (memq #'magnus-status--context-hint
                    eldoc-documentation-functions))
      (should (intern-soft "magnus-status-next" eldoc-message-commands))
      (should (intern-soft "magnus-status-previous"
                           eldoc-message-commands))))
  (let ((magnus-status-show-context-hints nil))
    (with-temp-buffer
      (magnus-status-mode)
      (should-not (memq #'magnus-status--context-hint
                        eldoc-documentation-functions)))))

(ert-deftest magnus-status-context-hints-describe-section-folding ()
  (with-temp-buffer
    (magnus-status-mode)
    (let ((inhibit-read-only t))
      (magnus-status--insert-foldable-heading 'coordination))
    (goto-char (point-min))
    (should
     (equal (substring-no-properties (magnus-status--context-hint nil))
            "Coordination — TAB or RET collapse"))
    (setq magnus-status--collapsed-sections '(coordination))
    (should
     (equal (substring-no-properties (magnus-status--context-hint nil))
            "Coordination — TAB or RET expand"))))

(ert-deftest magnus-status-context-hints-preserve-unrelated-messages ()
  (let ((echo "Review completed for quick-wren")
        (eldoc-last-message "stale ElDoc hint")
        (magnus-status--last-context-hint "old Magnus hint"))
    (cl-letf (((symbol-function 'current-message) (lambda () echo))
              ((symbol-function 'eldoc-minibuffer-message)
               (lambda (format-string &rest args)
                 (setq echo
                       (and format-string
                            (apply #'format-message format-string args))))))
      ;; A timer or process notification owns the echo area, so a delayed hint
      ;; neither replaces it nor leaves a stale ElDoc message to resurrect.
      (magnus-status--context-hint-message "%s" "new Magnus hint")
      (should (equal echo "Review completed for quick-wren"))
      (should-not magnus-status--last-context-hint)
      (should-not eldoc-last-message)
      ;; Silence is available, and the exact rendered message becomes Magnus's
      ;; ownership token for the next contextual update.
      (setq echo nil)
      (magnus-status--context-hint-message "%s" "new Magnus hint")
      (should (equal echo "new Magnus hint"))
      (should (equal magnus-status--last-context-hint "new Magnus hint"))
      (magnus-status--context-hint-message "%s" "next Magnus hint")
      (should (equal echo "next Magnus hint"))
      (magnus-status--context-hint-message nil)
      (should-not echo)
      (should-not magnus-status--last-context-hint))))

(ert-deftest magnus-status-context-hints-follow-review-lines-and-bindings ()
  (let* ((round
          (magnus-test-status--completed-round 1 'approve 'unread 2))
         (review
          (magnus-review--create
           :id "review-hint" :reviewer-name "keen-owl"
           :author-name "quick-wren" :lifecycle 'open
           :created-at (float-time)
           :updated-at (float-time) :rounds (list round)))
         (asking
          (magnus-test-status--review-in-state
           'asking-scope
           :id "review-asking" :reviewer-name "swift-hare"
           :author-name "bright-crow" :lifecycle 'open
           :created-at (float-time)
           :updated-at (float-time))))
    (with-temp-buffer
      (magnus-status-mode)
      (let ((inhibit-read-only t))
        (magnus-status--insert-review review))
      (cl-letf (((symbol-function 'magnus-review-get)
                 (lambda (id)
                   (and (string= id "review-hint") review))))
        (goto-char (point-min))
        (search-forward "keen-owl")
        (let ((reviewer-hint
               (substring-no-properties
                (magnus-status--context-hint nil))))
          (search-forward "round 1")
          (let ((round-hint
                 (substring-no-properties
                  (magnus-status--context-hint nil))))
            (should (equal reviewer-hint round-hint))
            (should (equal round-hint
                           (concat "keen-owl · round 1 — RET open · "
                                   "v review actions · ? all actions")))))
        ;; Hints resolve command keys at display time, so user rebinding remains
        ;; truthful instead of baking Magnus's default keys into prose.
        (let ((map (copy-keymap magnus-status-mode-map)))
          (define-key map (kbd "v") nil)
          (define-key map (kbd "u") #'magnus-review-request-dispatch)
          (use-local-map map)
          (should
           (string-match-p
            "u review actions"
            (substring-no-properties
             (magnus-status--context-hint nil))))
          (use-local-map magnus-status-mode-map)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "asking" 'magnus-review-id "review-asking")))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'magnus-review-get)
                 (lambda (id)
                   (and (string= id "review-asking") asking))))
        (let ((hint (substring-no-properties
                     (magnus-status--context-hint nil))))
          (should (string-match-p "v review actions" hint))
          (should-not (string-match-p "RET open" hint)))))))

(ert-deftest magnus-status-context-hints-distinguish-agent-lifecycle ()
  (let ((active
         (magnus-instance--create
          :id "active" :name "quick-wren" :status 'running))
        (purged
         (magnus-instance--create
          :id "purged" :name "wise-deer" :status 'purged)))
    (with-temp-buffer
      (magnus-status-mode)
      (let ((inhibit-read-only t))
        (insert (propertize "active" 'magnus-instance-id "active") "\n")
        (insert (propertize "purged" 'magnus-instance-id "purged") "\n")
        (insert "heading"))
      (cl-letf (((symbol-function 'magnus-instances-get)
                 (lambda (id)
                   (cond ((string= id "active") active)
                         ((string= id "purged") purged)))))
        (goto-char (point-min))
        (should
         (equal
          (substring-no-properties (magnus-status--context-hint nil))
          (concat "quick-wren — RET visit · m message · v request review · "
                  "t thinking trace · ? all actions")))
        (forward-line 1)
        (should
         (equal
          (substring-no-properties (magnus-status--context-hint nil))
          (concat "wise-deer (archived) — R resurrect · "
                  "K forget permanently · ? all actions")))
        (forward-line 1)
        (should
         (equal
          (substring-no-properties (magnus-status--context-hint nil))
          (concat "Magnus — n/p navigate · c create Claude · "
                  "X create Codex · ? all actions")))))))

(ert-deftest magnus-status-folded-sections-omit-and-refresh-their-bodies ()
  (let* ((review
          (magnus-test-status--review-in-state
           'running :id "active-review" :reviewer-name "keen-owl"
           :author-name "quick-wren" :lifecycle 'open
           :created-at 1 :updated-at 2))
         (archived
          (magnus-instance--create
           :id "archived-agent" :name "wise-deer"
           :directory "/tmp/project" :status 'purged
           :purged-at (float-time)))
         (magnus-instances (list archived))
         (magnus-reviews (list review))
         (magnus-review-runtime-state-function
          #'magnus-test-status--runtime-state))
    (cl-letf (((symbol-function 'magnus-status--get-project-directories)
               (lambda () '("/project/")))
              ((symbol-function 'magnus-coord-has-state-p) (lambda (_) t))
              ((symbol-function 'magnus-coord-parse)
               (lambda (_)
                 '(:active nil
                   :log ((:time "12:00" :agent "swift-hare"
                          :message "coordination-marker")))))
              ((symbol-function 'magnus--agents-index-get)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq-local magnus-status--collapsed-sections
                    '(reviews coordination archived-agents))
        (let ((magnus-status-show-archived t))
          (magnus-status--insert-reviews)
          (magnus-status--insert-coordination)
          (magnus-status--insert-purged))
        (let ((text (buffer-string)))
          (should (string-match-p "▸ Reviews" text))
          (should (string-match-p "▸ Coordination" text))
          (should (string-match-p "▸ Archived agents" text))
          (should-not (string-match-p "keen-owl" text))
          (should-not (string-match-p "coordination-marker" text))
          (should-not (string-match-p "wise-deer" text)))
        ;; Re-expansion renders the current model instead of revealing stale
        ;; invisible rows.
        (setq-local magnus-status--collapsed-sections nil)
        (erase-buffer)
        (let ((magnus-status-show-archived t))
          (magnus-status--insert-reviews)
          (magnus-status--insert-coordination)
          (magnus-status--insert-purged))
        (let ((text (buffer-string)))
          (should (string-match-p "▾ Reviews" text))
          (should (string-match-p "keen-owl" text))
          (should (string-match-p "coordination-marker" text))
          (should (string-match-p "wise-deer" text)))))))

(ert-deftest magnus-status-summarizes-archived-history-by-default ()
  (let* ((instance
          (magnus-instance--create
           :id "archived-agent" :name "wise-deer" :directory "/tmp/project"
           :status 'purged :purged-at (float-time)))
         (review
          (magnus-review--create
           :id "archived-review" :reviewer-name "keen-owl"
           :reviewer-provider 'codex :author-name "wise-deer"
           :lifecycle 'archived :created-at 1 :updated-at 3 :archived-at 3
           :rounds (list (magnus-test-status--completed-round))))
         (magnus-instances (list instance))
         (magnus-reviews (list review)))
    (cl-letf (((symbol-function 'magnus--agents-index-get)
               (lambda (&rest _arguments) nil)))
      (with-temp-buffer
        (let ((magnus-status-show-archived nil))
          (magnus-status--insert-reviews)
          (magnus-status--insert-purged))
        (let ((text (buffer-string)))
          (should (string-match-p "1 archived review hidden" text))
          (should (string-match-p "1 archived agent hidden" text))
          (should-not (string-match-p "keen-owl" text))
          (should-not (string-match-p "wise-deer" text))))
      (with-temp-buffer
        (let ((magnus-status-show-archived t))
          (magnus-status--insert-reviews)
          (magnus-status--insert-purged))
        (let ((text (buffer-string)))
          (should (string-match-p "keen-owl" text))
          (should (string-match-p "wise-deer" text)))))))

(ert-deftest magnus-status-archive-visibility-is-orthogonal-to-folding ()
  (with-temp-buffer
    (setq-local magnus-status--collapsed-sections '(reviews))
    (let ((magnus-status-show-archived nil)
          notice)
      (cl-letf (((symbol-function 'magnus-status-refresh) #'ignore)
                ((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (setq notice (apply #'format format-string arguments)))))
        (magnus-status-toggle-archived))
      (should magnus-status-show-archived)
      (should (equal magnus-status--collapsed-sections '(reviews)))
      (should (string-match-p "hidden rows in collapsed sections" notice)))))

(ert-deftest magnus-instances-create-rejects-a-workspace-name-collision ()
  (let* ((directory (make-temp-file "magnus-instance-name-" t))
         (link (concat directory "-link"))
         (existing
          (magnus-instance--create
           :id "existing" :name "quick-wren" :directory directory
           :status 'purged))
         (magnus-instances (list existing)))
    (unwind-protect
        (progn
          (make-symbolic-link directory link)
          (should-error
           (magnus-instances-create link "quick-wren" 'codex)
           :type 'user-error))
      (when (file-symlink-p link) (delete-file link))
      (delete-directory directory t))))

(ert-deftest magnus-instances-create-honors-adjacent-identity-reservations ()
  (let ((magnus-instances nil)
        (magnus-instances-name-reservation-functions
         (list (lambda (_project) '("api-reviewer")))))
    (should-error
     (magnus-instances-create default-directory "api-reviewer" 'codex)
     :type 'user-error)
    (should
     (magnus-instance-p
      (magnus-instances-create default-directory "implementation-agent"
                               'codex)))))

(ert-deftest magnus-status-review-state-labels-span-runtime-and-lineage ()
  (let ((magnus-review-runtime-state-function
         #'magnus-test-status--runtime-state))
    (dolist (case '((asking-scope . "asking author")
                    (running . "running")
                    (failed . "failed")
                    (interrupted . "interrupted")))
      (let* ((state (car case))
             (review
              (magnus-test-status--review-in-state
               state :id (symbol-name state) :lifecycle 'open))
             (round (magnus-review-latest-round review)))
        (should (equal (magnus-status--review-state-label review round)
                       (cdr case)))))
    (let* ((round
            (magnus-test-status--completed-round
             1 'changes-requested 'unread 3))
           (review
            (magnus-review--create
             :id "complete" :lifecycle 'open :rounds (list round))))
      (should (equal (magnus-status--review-state-label review round)
                     "changes requested")))
    (let ((review
           (magnus-test-status--review-in-state
            'running :id "archived" :lifecycle 'archived)))
      (should (equal
               (magnus-status--review-state-label
                review (magnus-review-latest-round review))
               "archived")))))

(ert-deftest magnus-status-review-rows-show-round-findings-and-read-state ()
  (let* ((unread-round
          (magnus-test-status--completed-round 1 'approve 'unread 12))
         (read-round
          (magnus-test-status--completed-round 2 'comment 'read 0))
         (unread
          (magnus-review--create
           :id "unread" :reviewer-name "keen-owl" :reviewer-provider 'codex
           :effort 'high :author-name "quick-wren" :task "Review API changes"
           :lifecycle 'open :created-at 1 :updated-at 2
           :rounds (list unread-round)))
         (read
          (magnus-review--create
           :id "read" :reviewer-name "swift-hare" :reviewer-provider 'claude
           :effort 'medium :author-name "bright-crow" :lifecycle 'open
           :created-at 1 :updated-at 2 :rounds (list read-round))))
    (let ((magnus-review-runtime-state-function nil))
      (with-temp-buffer
        (magnus-status--insert-review unread)
        (let ((text (buffer-string)))
          (should (string-match-p "● keen-owl \\[codex/high\\]" text))
          (should (string-match-p "round 1 · approve · 12 findings" text)))
        (erase-buffer)
        (magnus-status--insert-review read)
        (let ((text (buffer-string)))
          (should (string-match-p "· swift-hare \\[claude/medium\\]" text))
          (should (string-match-p "round 2 · comment · 0 findings" text))))
      (with-temp-buffer
        (cl-letf (((symbol-function 'magnus-review-list)
                   (lambda () (list unread read))))
          (magnus-status--insert-header))
        (should (string-match-p "\\[1 unread review\\]"
                                (buffer-string)))))))

(ert-deftest magnus-status-review-runtime-row-describes-candidate-not-completed-round ()
  (let* ((completed
          (magnus-test-status--completed-round 1 'comment 'read 12))
         (candidate (magnus-test-status--candidate-round 2))
         (review
          (magnus-review--create
           :id "runtime" :reviewer-name "keen-owl"
           :reviewer-provider 'codex :effort 'high
           :author-name "quick-wren" :lifecycle 'open
           :created-at (float-time) :updated-at (float-time)
           :reviewer-expertise 'failed
           :rounds (list completed)))
         (magnus-review-runtime-state-function
          #'magnus-test-status--runtime-state))
    (with-temp-buffer
      (cl-letf (((symbol-function
                  'magnus-review-controller-candidate-round)
                 (lambda (_review) candidate))
                ((symbol-function 'magnus-review-controller-error)
                 (lambda (_review)
                   "provider exited 1\n  after its session disappeared")))
        (magnus-status--insert-review review))
      (let ((text (buffer-string)))
        (should (string-match-p
                 "round 2 · failed · provider exited 1 after its session disappeared"
                 text))
        (should-not (string-match-p "round 1" text))
        (should-not (string-match-p "12 findings" text))))))

(ert-deftest magnus-status-scope-query-names-next-round-without-stale-findings ()
  (let* ((completed
          (magnus-test-status--completed-round 3 'approve 'read 4))
         (review
          (magnus-review--create
           :id "asking" :reviewer-name "keen-owl"
           :reviewer-provider 'codex :effort 'high
           :author-name "quick-wren" :lifecycle 'open
           :created-at (float-time) :updated-at (float-time)
           :reviewer-expertise 'asking-scope
           :rounds (list completed)))
         (magnus-review-runtime-state-function
          #'magnus-test-status--runtime-state))
    (with-temp-buffer
      (cl-letf (((symbol-function
                  'magnus-review-controller-candidate-round)
                 (lambda (_review) nil)))
        (magnus-status--insert-review review))
      (let ((text (buffer-string)))
        (should (string-match-p "round 4 · asking author" text))
        (should-not (string-match-p "4 findings" text))))))

(ert-deftest magnus-status-animates-only-active-review-execution ()
  (let* ((instance
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"
           :created-at (current-time) :status 'running))
         (magnus-status-review-animation-interval 0.4))
    (dolist (state '(asking-scope running))
      (let* ((review
              (magnus-test-status--review-in-state
               state
               :id (symbol-name state) :author-instance-id "author"
               :reviewer-name "keen-owl" :lifecycle 'open))
             (rendered (magnus-test-status--render-instance
                        instance (list review))))
        (should (string-match-p (regexp-quote "[review |]") (car rendered)))
        (should (number-or-marker-p (cadr rendered)))))
    (dolist (state '(complete failed interrupted idle))
      (let* ((review
              (magnus-test-status--review-in-state
               state
               :id (symbol-name state) :author-instance-id "author"
               :reviewer-name "keen-owl" :lifecycle 'open))
             (rendered (magnus-test-status--render-instance
                        instance (list review))))
        (should-not (string-match-p "\\[review" (car rendered)))
        (should-not (cadr rendered))))
    (let* ((review
            (magnus-test-status--review-in-state
             'running
             :id "archived" :author-instance-id "author"
             :reviewer-name "keen-owl" :lifecycle 'archived))
           (rendered (magnus-test-status--render-instance
                      instance (list review))))
      (should-not (cadr rendered)))))

(ert-deftest magnus-status-review-badge-matches-durable-author-id ()
  (let* ((instance
          (magnus-instance--create
           :id "author-a" :name "quick-wren" :directory "/tmp/project"
           :created-at (current-time) :status 'running))
         (review
          (magnus-test-status--review-in-state
           'running
           :id "other-author" :author-instance-id "author-b"
           :reviewer-name "keen-owl" :lifecycle 'open))
         (rendered (magnus-test-status--render-instance instance (list review))))
    (should-not (cadr rendered))))

(ert-deftest magnus-status-review-badge-handles-multiple-and-static-reviews ()
  (let* ((instance
          (magnus-instance--create
           :id "author" :name "quick-wren" :directory "/tmp/project"
           :created-at (current-time) :status 'running))
         (reviews
          (list
           (magnus-test-status--review-in-state
            'running
            :id "first" :author-instance-id "author"
            :reviewer-name "keen-owl" :lifecycle 'open)
           (magnus-test-status--review-in-state
            'asking-scope
            :id "second" :author-instance-id "author"
            :reviewer-name "swift-hare" :lifecycle 'open))))
    (let* ((magnus-status-review-animation-interval 0.4)
           (rendered (magnus-test-status--render-instance instance reviews)))
      (should (string-match-p (regexp-quote "[2 reviews |]") (car rendered)))
      (should (cadr rendered)))
    (let* ((magnus-status-review-animation-interval nil)
           (rendered (magnus-test-status--render-instance
                      instance (list (car reviews)))))
      (should (string-match-p (regexp-quote "[review]") (car rendered)))
      (should-not (cadr rendered)))))

(ert-deftest magnus-status-review-animation-tick-is-presentation-only ()
  (let* ((magnus-buffer-name " *magnus-status-animation-test*")
         (magnus-status-review-animation-interval 0.4)
         (magnus-status--review-animation-frame 0)
         (buffer (get-buffer-create magnus-buffer-name)))
    (unwind-protect
        (with-current-buffer buffer
          (insert (propertize "|" 'magnus-review-animation-slot t
                             'magnus-instance-id "author"))
          (goto-char (point-min))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          (cl-letf (((symbol-function 'get-buffer-window)
                     (lambda (&rest _args) t)))
            (magnus-status--review-animation-tick))
          (should (equal (buffer-string) "|"))
          (should (equal (get-text-property (point-min) 'display) "/"))
          (should (equal (get-text-property (point-min) 'magnus-instance-id)
                         "author"))
          (should (= (point) (point-min)))
          (should-not (buffer-modified-p)))
      (magnus-status-stop-review-animation)
      (kill-buffer buffer))))

(ert-deftest magnus-status-review-animation-reuses-and-stops-one-timer ()
  (let ((magnus-status-review-animation-interval 0.4)
        (magnus-status--review-animation-timer nil)
        (started 0)
        (cancelled 0)
        (fake-timer (list 'fake-timer)))
    (with-temp-buffer
      (magnus-status-mode)
      (let ((inhibit-read-only t))
        (insert (propertize "|" 'magnus-review-animation-slot t)))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _args) t))
                ((symbol-function 'run-with-timer)
                 (lambda (&rest _args)
                   (cl-incf started)
                   fake-timer))
                ((symbol-function 'timerp)
                 (lambda (value) (eq value fake-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (_timer) (cl-incf cancelled))))
        (magnus-status--sync-review-animation)
        (magnus-status--sync-review-animation)
        (should (= started 1))
        (let ((inhibit-read-only t))
          (remove-text-properties (point-min) (point-max)
                                  '(magnus-review-animation-slot nil)))
        (magnus-status--sync-review-animation)
        (should (= cancelled 1))
        (should-not magnus-status--review-animation-timer)))))

(ert-deftest magnus-status-hidden-buffer-stops-review-animation ()
  (let ((magnus-status-review-animation-interval 0.4)
        (cancelled 0)
        (started 0)
        (visible nil)
        (fake-timer (list 'fake-timer)))
    (with-temp-buffer
      (magnus-status-mode)
      (let ((inhibit-read-only t))
        (insert (propertize "|" 'magnus-review-animation-slot t)))
      (setq magnus-status--review-animation-timer fake-timer)
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _args) visible))
                ((symbol-function 'run-with-timer)
                 (lambda (&rest _args)
                   (cl-incf started)
                   fake-timer))
                ((symbol-function 'timerp)
                 (lambda (value) (eq value fake-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (_timer) (cl-incf cancelled))))
        (magnus-status--sync-review-animation)
        (should (= cancelled 1))
        (should-not magnus-status--review-animation-timer)
        ;; Ordinary window history/buffer switching reaches this buffer-local
        ;; visibility hook without going through `magnus-status'.
        (setq visible t)
        (magnus-status--window-state-change nil)
        (should (= started 1))
        (should (eq magnus-status--review-animation-timer fake-timer))
        (magnus-status-stop-review-animation)))))

(ert-deftest magnus-status-motion-opt-out-normalizes-a-live-badge ()
  (let ((magnus-status-review-animation-interval nil)
        (magnus-status--review-animation-timer nil)
        (refreshes 0))
    (with-temp-buffer
      (magnus-status-mode)
      (let ((inhibit-read-only t))
        (insert (propertize "|" 'magnus-review-animation-slot t)))
      (cl-letf (((symbol-function 'magnus-status-refresh)
                 (lambda () (cl-incf refreshes))))
        (magnus-status--sync-review-animation))
      (should (= refreshes 1)))))

(ert-deftest magnus-status-review-refresh-sees-other-frames ()
  (let* ((magnus-buffer-name " *magnus-other-frame-test*")
         (buffer (get-buffer-create magnus-buffer-name))
         (all-frames nil)
         (refreshes 0))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (_buffer all)
                     (setq all-frames all)
                     t))
                  ((symbol-function 'magnus-status-refresh)
                   (lambda () (cl-incf refreshes))))
          (magnus-status--maybe-refresh)
          (should all-frames)
          (should (= refreshes 1)))
      (kill-buffer buffer))))

(provide 'magnus-status-tests)
;;; magnus-status-tests.el ends here
