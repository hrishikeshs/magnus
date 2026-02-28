EMACS ?= emacs
EL_FILES ?= $(wildcard *.el)
# Files that require vterm — skip for byte-compile without vterm installed
VTERM_FILES = magnus-process.el magnus-status.el magnus-transient.el
COMPILE_FILES = $(filter-out $(VTERM_FILES),$(EL_FILES))

.PHONY: lint lint-parens lint-nesting lint-compile lint-patterns clean

lint: lint-parens lint-nesting lint-compile lint-patterns

lint-parens:
	@echo "Checking paren balance..."
	@$(EMACS) --batch --eval '\
	(let ((ok t)) \
	  (dolist (file (list $(patsubst %,"%",$(EL_FILES)))) \
	    (with-temp-buffer \
	      (insert-file-contents file) \
	      (emacs-lisp-mode) \
	      (condition-case err \
	          (check-parens) \
	        (error \
	         (message "FAIL: %s: %s" file (error-message-string err)) \
	         (setq ok nil))))) \
	  (unless ok (kill-emacs 1)))' 2>&1
	@echo "Parens OK."

lint-nesting:
	@echo "Checking for nested defuns..."
	@$(EMACS) --batch --eval '\
	(let ((ok t)) \
	  (dolist (file (list $(patsubst %,"%",$(EL_FILES)))) \
	    (with-temp-buffer \
	      (insert-file-contents file) \
	      (emacs-lisp-mode) \
	      (goto-char (point-min)) \
	      (while (re-search-forward "^(def\\(un\\|var\\|custom\\|macro\\) " nil t) \
	        (goto-char (match-beginning 0)) \
	        (let ((depth (car (syntax-ppss)))) \
	          (unless (= depth 0) \
	            (message "FAIL: %s:%d: %s at depth %d" \
	                     file (line-number-at-pos) \
	                     (buffer-substring (point) (line-end-position)) depth) \
	            (setq ok nil))) \
	        (forward-line 1)))) \
	  (unless ok (kill-emacs 1)))' 2>&1
	@echo "Nesting OK."

lint-compile:
	@echo "Byte-compiling..."
	@fail=0; \
	for f in $(COMPILE_FILES); do \
	  $(EMACS) --batch -L . -f batch-byte-compile $$f 2>&1 \
	    | grep -i error && fail=1 || true; \
	done; \
	if [ $$fail -eq 1 ]; then exit 1; fi
	@echo "Compile OK."

lint-patterns:
	@echo "Checking for banned patterns..."
	@if grep -n '(error nil)' $(EL_FILES); then \
	  echo "FAIL: found (error nil) — log errors instead of swallowing them"; \
	  exit 1; \
	fi
	@echo "Patterns OK."

clean:
	rm -f *.elc
