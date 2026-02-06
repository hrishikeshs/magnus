;;; magnus-context.el --- Shared context buffer for magnus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Hrishikesh S
;; Author: Hrishikesh S <hrish2006@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides per-project scratch-like context buffers that
;; can be shared with all Claude Code instances.  Think of it as a
;; shared notepad where you can paste URLs, notes, and context that
;; any agent can read at any time.
;;
;; The context persists across Emacs sessions but is stored outside
;; the project directory (in ~/.emacs.d/magnus-context/).

;;; Code:

(require 'url)
(require 'url-http)

;;; Customization

(defcustom magnus-context-directory
  (expand-file-name "magnus-context" user-emacs-directory)
  "Directory to store context files."
  :type 'directory
  :group 'magnus)

(defcustom magnus-context-cache-directory
  (expand-file-name "magnus-url-cache" user-emacs-directory)
  "Directory to cache fetched URL content."
  :type 'directory
  :group 'magnus)

(defcustom magnus-context-cache-ttl 3600
  "Time-to-live for cached URL content in seconds.
Default is 1 hour."
  :type 'integer
  :group 'magnus)

;;; Faces

(defface magnus-context-url
  '((t :inherit link))
  "Face for URLs in context buffer."
  :group 'magnus)

(defface magnus-context-heading
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for headings in context buffer."
  :group 'magnus)

(defface magnus-context-cached
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for cached content markers."
  :group 'magnus)

;;; Mode definition

(defvar magnus-context-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") #'magnus-context-fetch-url-at-point)
    (define-key map (kbd "C-c C-u") #'magnus-context-insert-url)
    (define-key map (kbd "C-c C-s") #'magnus-context-save)
    (define-key map (kbd "C-c C-e") #'magnus-context-export-for-agent)
    (define-key map (kbd "C-c C-c") #'magnus-context-copy-for-agent)
    map)
  "Keymap for `magnus-context-mode'.")

;; Mode is defined conditionally based on markdown-mode availability
(declare-function magnus-context-mode "magnus-context")

(if (fboundp 'markdown-mode)
    (define-derived-mode magnus-context-mode markdown-mode "Magnus-Context"
      "Major mode for magnus context buffers.

\\{magnus-context-mode-map}"
      :group 'magnus
      (setq-local auto-save-visited-mode t)
      (add-hook 'after-change-functions #'magnus-context--schedule-save nil t))
  ;; Fallback if markdown-mode not available
  (define-derived-mode magnus-context-mode text-mode "Magnus-Context"
    "Major mode for magnus context buffers (text fallback)."
    :group 'magnus
    (add-hook 'after-change-functions #'magnus-context--schedule-save nil t)))

;;; Buffer management

(defvar magnus-context--buffers (make-hash-table :test 'equal)
  "Hash table mapping project paths to context buffers.")

(defvar magnus-context--save-timers (make-hash-table :test 'equal)
  "Hash table of pending save timers per project.")

(defun magnus-context--project-key (directory)
  "Generate a unique key for DIRECTORY."
  (secure-hash 'sha256 (expand-file-name directory)))

(defun magnus-context--buffer-name (directory)
  "Generate buffer name for DIRECTORY's context."
  (format "*magnus-context: %s*"
          (file-name-nondirectory (directory-file-name directory))))

(defun magnus-context--context-file (directory)
  "Get the context file path for DIRECTORY."
  (let ((key (magnus-context--project-key directory)))
    (expand-file-name (concat key ".md") magnus-context-directory)))

;;;###autoload
(defun magnus-context (&optional directory)
  "Open or switch to the context buffer for DIRECTORY.
If DIRECTORY is nil, uses the current project or default-directory."
  (interactive)
  (let* ((dir (or directory (magnus-context--get-project-dir)))
         (buffer-name (magnus-context--buffer-name dir))
         (existing (gethash dir magnus-context--buffers)))
    ;; Return existing buffer if alive
    (when (and existing (buffer-live-p existing))
      (switch-to-buffer existing)
      (cl-return-from magnus-context existing))
    ;; Create new buffer
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (magnus-context-mode)
        (setq-local magnus-context--directory dir)
        ;; Load existing content
        (magnus-context--load dir)
        (set-buffer-modified-p nil))
      (puthash dir buffer magnus-context--buffers)
      (switch-to-buffer buffer)
      buffer)))

(defun magnus-context--get-project-dir ()
  "Get the current project directory."
  (or (when (fboundp 'project-current)
        (when-let ((project (project-current)))
          (if (fboundp 'project-root)
              (project-root project)
            (car (with-no-warnings (project-roots project))))))
      default-directory))

;;; Persistence

(defun magnus-context--ensure-directories ()
  "Ensure context directories exist."
  (unless (file-exists-p magnus-context-directory)
    (make-directory magnus-context-directory t))
  (unless (file-exists-p magnus-context-cache-directory)
    (make-directory magnus-context-cache-directory t)))

(defun magnus-context--load (directory)
  "Load context for DIRECTORY into current buffer."
  (let ((file (magnus-context--context-file directory)))
    (if (file-exists-p file)
        (insert-file-contents file)
      ;; Insert default template
      (magnus-context--insert-template directory))))

(defun magnus-context--insert-template (directory)
  "Insert default template for DIRECTORY."
  (insert (format "# Context: %s\n\n"
                  (file-name-nondirectory (directory-file-name directory))))
  (insert "<!-- Shared context for Claude Code instances -->\n")
  (insert "<!-- Keybindings: -->\n")
  (insert "<!--   C-c C-u  Insert and fetch URL -->\n")
  (insert "<!--   C-c C-f  Fetch URL at point -->\n")
  (insert "<!--   C-c C-e  Export to temp file for agent -->\n")
  (insert "<!--   C-c C-c  Copy buffer for agent -->\n")
  (insert "\n## Notes\n\n")
  (insert "\n## Links\n\n"))

(defun magnus-context-save ()
  "Save the current context buffer."
  (interactive)
  (when (bound-and-true-p magnus-context--directory)
    (magnus-context--ensure-directories)
    (let ((file (magnus-context--context-file magnus-context--directory)))
      (write-region (point-min) (point-max) file nil 'quiet)
      (set-buffer-modified-p nil)
      (message "Context saved"))))

(defun magnus-context--schedule-save (&rest _)
  "Schedule a save after buffer changes."
  (when (bound-and-true-p magnus-context--directory)
    (let* ((dir magnus-context--directory)
           (existing-timer (gethash dir magnus-context--save-timers)))
      (when existing-timer
        (cancel-timer existing-timer))
      (puthash dir
               (run-with-idle-timer 2 nil #'magnus-context--do-save dir)
               magnus-context--save-timers))))

(defun magnus-context--do-save (directory)
  "Actually save the context for DIRECTORY."
  (remhash directory magnus-context--save-timers)
  (when-let ((buffer (gethash directory magnus-context--buffers)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (magnus-context-save)))))

;;; URL fetching

(defun magnus-context-insert-url (url)
  "Insert URL and fetch its content."
  (interactive "sURL: ")
  (insert (format "\n### %s\n" url))
  (insert (format "[%s](%s)\n\n" url url))
  (magnus-context--fetch-and-insert url))

(defun magnus-context-fetch-url-at-point ()
  "Fetch the URL at point and insert its content."
  (interactive)
  (if-let ((url (thing-at-point 'url)))
      (magnus-context--fetch-and-insert url)
    (user-error "No URL at point")))

(defun magnus-context--fetch-and-insert (url)
  "Fetch URL and insert content at point."
  (let ((cached (magnus-context--get-cached url)))
    (if cached
        (progn
          (insert (format "<!-- Cached: %s -->\n"
                          (format-time-string "%Y-%m-%d %H:%M")))
          (insert "```\n")
          (insert cached)
          (insert "\n```\n\n"))
      ;; Fetch fresh
      (magnus-context--fetch-url url))))

(defun magnus-context--cache-file (url)
  "Get cache file path for URL."
  (let ((key (secure-hash 'sha256 url)))
    (expand-file-name key magnus-context-cache-directory)))

(defun magnus-context--get-cached (url)
  "Get cached content for URL if still valid."
  (let ((cache-file (magnus-context--cache-file url)))
    (when (and (file-exists-p cache-file)
               (< (float-time (time-subtract
                               (current-time)
                               (file-attribute-modification-time
                                (file-attributes cache-file))))
                  magnus-context-cache-ttl))
      (with-temp-buffer
        (insert-file-contents cache-file)
        (buffer-string)))))

(defun magnus-context--save-cache (url content)
  "Cache CONTENT for URL."
  (magnus-context--ensure-directories)
  (let ((cache-file (magnus-context--cache-file url)))
    (with-temp-file cache-file
      (insert content))))

(defun magnus-context--fetch-url (url)
  "Fetch URL asynchronously and insert at point."
  (let ((buffer (current-buffer))
        (pos (point)))
    (insert "<!-- Fetching... -->\n")
    (url-retrieve
     url
     (lambda (status)
       (if-let ((err (plist-get status :error)))
           (magnus-context--fetch-error buffer pos url err)
         (magnus-context--fetch-success buffer pos url)))
     nil t)))

(defun magnus-context--fetch-success (buffer pos url)
  "Handle successful fetch of URL into BUFFER at POS."
  (let ((content (magnus-context--extract-content)))
    (magnus-context--save-cache url content)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char pos)
          (when (search-forward "<!-- Fetching... -->" nil t)
            (replace-match ""))
          (insert (format "<!-- Fetched: %s -->\n"
                          (format-time-string "%Y-%m-%d %H:%M")))
          (insert "```\n")
          (insert (string-trim content))
          (insert "\n```\n\n"))))))

(defun magnus-context--fetch-error (buffer pos _url err)
  "Handle fetch error in BUFFER at POS with ERR."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (when (search-forward "<!-- Fetching... -->" nil t)
          (replace-match
           (format "<!-- Fetch failed: %s -->\n<!-- You may need to paste content manually for authenticated URLs -->\n"
                   (error-message-string err))))))))

(defun magnus-context--extract-content ()
  "Extract readable content from the current URL response buffer."
  (goto-char (point-min))
  ;; Skip headers
  (re-search-forward "\n\n" nil t)
  (let ((content (buffer-substring-no-properties (point) (point-max))))
    ;; Try to extract text from HTML
    (if (string-match-p "<html" content)
        (magnus-context--html-to-text content)
      ;; Return as-is for non-HTML
      content)))

(defun magnus-context--html-to-text (html)
  "Convert HTML to readable text."
  (with-temp-buffer
    (insert html)
    ;; Remove script and style tags
    (goto-char (point-min))
    (while (re-search-forward "<script[^>]*>[^<]*</script>" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "<style[^>]*>[^<]*</style>" nil t)
      (replace-match ""))
    ;; Convert some tags to text equivalents
    (goto-char (point-min))
    (while (re-search-forward "<h[1-6][^>]*>\\([^<]*\\)</h[1-6]>" nil t)
      (replace-match "\n## \\1\n"))
    (goto-char (point-min))
    (while (re-search-forward "<p[^>]*>" nil t)
      (replace-match "\n"))
    (goto-char (point-min))
    (while (re-search-forward "<br[^>]*>" nil t)
      (replace-match "\n"))
    (goto-char (point-min))
    (while (re-search-forward "<li[^>]*>" nil t)
      (replace-match "\n- "))
    ;; Remove remaining tags
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match ""))
    ;; Decode entities
    (goto-char (point-min))
    (while (re-search-forward "&nbsp;" nil t)
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward "&amp;" nil t)
      (replace-match "&"))
    (goto-char (point-min))
    (while (re-search-forward "&lt;" nil t)
      (replace-match "<"))
    (goto-char (point-min))
    (while (re-search-forward "&gt;" nil t)
      (replace-match ">"))
    ;; Clean up whitespace
    (goto-char (point-min))
    (while (re-search-forward "\n\n\n+" nil t)
      (replace-match "\n\n"))
    (string-trim (buffer-string))))

;;; Agent integration

(defun magnus-context-export-for-agent ()
  "Export context to a temp file and return the path.
The file is placed in the project directory for easy access."
  (interactive)
  (when (bound-and-true-p magnus-context--directory)
    (let ((export-file (expand-file-name ".magnus-context.md"
                                         magnus-context--directory)))
      (write-region (point-min) (point-max) export-file nil 'quiet)
      (message "Context exported to: %s" export-file)
      (kill-new export-file)
      export-file)))

(defun magnus-context-copy-for-agent ()
  "Copy buffer content to clipboard for pasting to agent."
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Context copied to clipboard (%d chars)"
           (- (point-max) (point-min))))

(defun magnus-context-get-content (directory)
  "Get the context content for DIRECTORY as a string."
  (let ((file (magnus-context--context-file directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

;;; Cleanup on exit

(defun magnus-context--save-all ()
  "Save all context buffers."
  (maphash
   (lambda (_dir buffer)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (magnus-context-save))))
   magnus-context--buffers))

(add-hook 'kill-emacs-hook #'magnus-context--save-all)

(provide 'magnus-context)
;;; magnus-context.el ends here
