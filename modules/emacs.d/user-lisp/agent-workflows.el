;;; agent-workflows.el --- personal agent-shell workflows -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal task entry points built on top of `gatsby-ai' helpers.
;; Keep this module standalone: the symbols it imports from `gatsby-ai'
;; are declared explicitly below.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'button)
(require 'text-property-search)

(require 'markdown-mode nil t)

(defgroup agent-workflows nil
  "Personal agent-shell workflows."
  :group 'tools)

(defcustom agent-workflows-agent-config "codex"
  "Name of the agent-shell config profile used by workflow entry points."
  :type 'string
  :group 'agent-workflows)

(defcustom agent-workflows-auto-approve-kinds '("read")
  "Permission request kinds that are auto-approved by workflow sessions."
  :type '(repeat string)
  :group 'agent-workflows)

(defcustom agent-workflows-commit-message-prompt
  "generate a commit message for the currently staged changes"
  "Prompt used to generate a commit message."
  :type 'string
  :group 'agent-workflows)

(defcustom agent-workflows-review-prompt
  (concat
   "Review the current branch or working tree. Focus on correctness, regressions, "
   "edge cases, and missing tests.\n\n"
   "Write a short summary first, then each diagnostic as a Markdown heading on "
   "its own line in this exact format:\n"
   "### severity: path/to/file:123 - short title\n\n"
   "Use severity values high, medium, or low. Put the diagnostic details in the "
   "paragraphs below each heading. Separate diagnostics with a blank line. If "
   "there are no actionable findings, write a short summary and nothing else.\n"
   "Do not use code fences around the whole answer.")
  "Prompt used for review sessions."
  :type 'string
  :group 'agent-workflows)

(defvar gatsby>agent-shell-configs)

(declare-function agent-shell-permission-allow-always "agent-shell" (permission))
(declare-function agent-shell-previous-item "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell" (buffer alist))
(declare-function agent-shell--start "agent-shell" (&rest args))

(declare-function shell-maker-submit "shell-maker")

(declare-function magit-anything-staged-p "magit")
(declare-function magit-run-git-with-editor "magit")
(declare-function markdown-mode "markdown-mode")

(defvar comint-prompt-regexp)
(defvar-local agent-workflows-review--shell-buffer nil)
(defvar-local agent-workflows-review--tmpfile nil)
(defvar-local agent-workflows-review--findings nil)
(defvar-local agent-workflows-review--summary nil)
(defvar-local agent-workflows-review--positions nil)
(defvar agent-workflows-review-mode-map)
(defconst agent-workflows--review-heading-regexp
  "^###[ \t]+\\(high\\|medium\\|low\\):[ \t]+\\([^:\n]+\\):\\([0-9]+\\)[ \t]+-[ \t]+\\(.+\\)$"
  "Regular expression for review diagnostic headings.")

(defun agent-workflows--review-parse-output (output)
  "Parse OUTPUT into a summary string and review findings."
  (let ((lines (split-string output "\n"))
        (summary-lines nil)
        (findings nil)
        (current nil)
        (body-lines nil)
        (seen-finding nil))
    (cl-labels
        ((flush-finding ()
           (when current
             (let ((body (string-trim-right (mapconcat #'identity (nreverse body-lines) "\n"))))
               (push (append current (list (cons 'body body))) findings)))
           (setq current nil
                 body-lines nil)))
      (dolist (line lines)
        (if (and (not current)
                 (not seen-finding)
                 (string-match-p agent-workflows--review-heading-regexp line))
            (setq seen-finding t
                  current (agent-workflows--review-parse-heading line))
          (if current
              (if (string-match-p agent-workflows--review-heading-regexp line)
                  (progn
                    (flush-finding)
                    (setq current (agent-workflows--review-parse-heading line)))
                (push line body-lines))
            (push line summary-lines))))
      (flush-finding))
    (list (string-trim (mapconcat #'identity (nreverse summary-lines) "\n"))
          (nreverse findings))))

(defun agent-workflows--review-parse-heading (line)
  "Parse a review heading LINE into an alist."
  (unless (string-match agent-workflows--review-heading-regexp line)
    (user-error "Invalid review heading: %s" line))
  (let ((severity (match-string 1 line))
        (file (match-string 2 line))
        (line-no (string-to-number (match-string 3 line)))
        (title (match-string 4 line)))
    (list (cons 'severity severity)
          (cons 'file file)
          (cons 'line line-no)
          (cons 'title title)
          (cons 'heading line))))

(defun agent-workflows--review-read-markdown-file (file)
  "Return the contents of markdown FILE."
  (unless (file-readable-p file)
    (user-error "Review markdown file not found: %s" file))
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun agent-workflows--review-source-markdown (review-file)
  "Return review markdown from REVIEW-FILE."
  (unless (and review-file (file-readable-p review-file))
    (user-error "Review markdown file not found: %s" review-file))
  (agent-workflows--review-read-markdown-file review-file))

(defun agent-workflows--review-delete-tempfile (review-file)
  "Delete REVIEW-FILE if it still exists."
  (when (and review-file (file-exists-p review-file))
    (delete-file review-file)))

(defun agent-workflows--review-current-index ()
  "Return the zero-based finding index at point."
  (or (get-text-property (point) 'agent-workflows-review-finding-index)
      (user-error "No finding at point")))

(defun agent-workflows--review-findings ()
  "Return the parsed finding list."
  (or agent-workflows-review--findings (user-error "No review findings loaded")))

(defun agent-workflows--review-finding-at-point ()
  "Return the finding object at point."
  (nth (agent-workflows--review-current-index) (agent-workflows--review-findings)))

(defun agent-workflows--review-goto-index (index)
  "Jump to finding INDEX in the review buffer."
  (let ((pos (nth index agent-workflows-review--positions)))
    (unless pos
      (user-error "No finding %s" index))
    (goto-char pos)
    (beginning-of-line)
    (when (get-buffer-window (current-buffer))
      (recenter 0))))

(defun agent-workflows-review-next ()
  "Jump to the next finding."
  (interactive)
  (let ((index (agent-workflows--review-current-index)))
    (if (< index (1- (length agent-workflows-review--positions)))
        (agent-workflows--review-goto-index (1+ index))
      (user-error "No next finding"))))

(defun agent-workflows-review-prev ()
  "Jump to the previous finding."
  (interactive)
  (let ((index (agent-workflows--review-current-index)))
    (if (> index 0)
        (agent-workflows--review-goto-index (1- index))
      (user-error "No previous finding"))))

(defun agent-workflows--review-show-source (finding)
  "Visit the source location for FINDING."
  (let ((file (alist-get 'file finding))
        (line (alist-get 'line finding)))
    (unless file
      (user-error "Finding does not specify a file"))
    (find-file-other-window file)
    (when (and line (numberp line))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter 0)))

(defun agent-workflows--review-fix-finding (finding)
  "Submit a fix request for FINDING using the stored shell buffer."
  (let ((shell-buffer agent-workflows-review--shell-buffer))
    (unless (and shell-buffer (buffer-live-p shell-buffer))
      (user-error "Review shell buffer is no longer available"))
    (with-current-buffer shell-buffer
      (shell-maker-submit
       :input
       (concat
        "Fix this review finding in the current worktree.\n\n"
        "Finding:\n"
        (alist-get 'heading finding)
        "\n"
        (or (alist-get 'body finding) "")
        "\n\n"
        "Make the smallest correct change. Return a short summary of the change."))))
  (message "Submitted fix request"))

(defun agent-workflows-review-fix ()
  "Request a fix for the finding at point."
  (interactive)
  (agent-workflows--review-fix-finding (agent-workflows--review-finding-at-point)))

(defun agent-workflows-review-open-source ()
  "Open the source location for the finding at point."
  (interactive)
  (agent-workflows--review-show-source (agent-workflows--review-finding-at-point)))

(define-derived-mode
 agent-workflows-review-mode
 markdown-mode
 "Agent-Review"
 "Major mode for interactive review findings."
 :group
 'agent-workflows
 (setq-local
  buffer-read-only t
  truncate-lines nil
  word-wrap t
  header-line-format "n/p: next/prev finding  RET/o: open source  f: fix  q: quit"))

(let ((map agent-workflows-review-mode-map))
  (define-key map [remap next-line] #'agent-workflows-review-next)
  (define-key map [remap previous-line] #'agent-workflows-review-prev)
  (define-key map (kbd "f") #'agent-workflows-review-fix)
  (define-key map (kbd "RET") #'agent-workflows-review-open-source)
  (define-key map (kbd "o") #'agent-workflows-review-open-source)
  (define-key map (kbd "q") #'quit-window))

(defun agent-workflows--review-insert-finding (finding index)
  "Insert FINDING using INDEX into the current review buffer."
  (let ((start (point)))
    (insert
     (format "### %s: %s\n\n"
             (upcase (or (alist-get 'severity finding) "unknown"))
             (or (alist-get 'title finding) "Untitled finding")))
    (insert (format "- File: `%s`\n" (or (alist-get 'file finding) "unknown")))
    (when-let* ((line (alist-get 'line finding)))
      (insert (format "- Line: %s\n" line)))
    (when-let* ((details (alist-get 'details finding)))
      (insert "\n")
      (insert details)
      (insert "\n"))
    (when-let* ((fix (alist-get 'suggested_fix finding)))
      (insert "\n")
      (insert "**Suggested fix**\n\n")
      (insert fix)
      (insert "\n"))
    (when-let* ((body (alist-get 'body finding)))
      (insert "\n")
      (insert body)
      (insert "\n"))
    (insert "\n")
    (let ((jump-start (point)))
      (insert-text-button "[open source]"
                          'action
                          (lambda (_button)
                            (agent-workflows--review-show-source finding))
                          'follow-link
                          t
                          'help-echo
                          "Open source location for this finding")
      (insert " ")
      (insert-text-button "[fix]"
                          'action
                          (lambda (_button)
                            (agent-workflows--review-fix-finding finding))
                          'follow-link
                          t
                          'help-echo
                          "Submit a fix request for this finding")
      (insert "\n")
      (add-text-properties
       start (point)
       `(agent-workflows-review-finding-index
         ,index agent-workflows-review-finding ,finding))
      (add-text-properties
       jump-start (point)
       `(agent-workflows-review-finding-index
         ,index agent-workflows-review-finding ,finding)))))

(defun agent-workflows--review-render (buffer payload shell-buffer)
  "Render review PAYLOAD into BUFFER, keeping SHELL-BUFFER available for fixes."
  (let ((summary (or (alist-get 'summary payload) ""))
        (findings (or (alist-get 'findings payload) '())))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (agent-workflows-review-mode)
        (setq-local
         agent-workflows-review--shell-buffer shell-buffer
         agent-workflows-review--tmpfile nil
         agent-workflows-review--findings findings
         agent-workflows-review--summary summary
         agent-workflows-review--positions nil)
        (insert "# Review Results\n\n")
        (if (string-match-p "\\`[[:space:]]*\\'" summary)
            (insert "No summary provided.\n\n")
          (insert "## Summary\n\n")
          (insert summary)
          (insert "\n\n"))
        (insert (format "## Findings (%d)\n\n" (length findings)))
        (if findings
            (cl-loop
             for
             finding
             in
             findings
             for
             index
             from
             0
             do
             (push (point) agent-workflows-review--positions)
             (agent-workflows--review-insert-finding finding index))
         (insert "No actionable findings.\n"))
        (setq-local agent-workflows-review--positions
                    (nreverse agent-workflows-review--positions))
        (goto-char (point-min)))))
  (pop-to-buffer buffer))

(defun agent-workflows--review-display-raw (buffer output shell-buffer err)
  "Display raw OUTPUT in BUFFER after a parsing ERR."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (agent-workflows-review-mode)
      (setq-local
       agent-workflows-review--shell-buffer shell-buffer
       agent-workflows-review--tmpfile nil
       agent-workflows-review--findings nil
       agent-workflows-review--summary nil
       agent-workflows-review--positions nil)
      (insert "# Review Results\n\n")
      (insert "The review output could not be parsed.\n\n")
      (insert (format "Error: %s\n\n" (error-message-string err)))
      (insert "## Raw output\n\n")
      (insert output)
      (goto-char (point-min))))
  (pop-to-buffer buffer))

(defun agent-workflows--permission-responder (permission)
  "Auto-approve selected PERMISSION requests."
  (let ((kind (map-elt (map-elt permission :tool-call) :kind)))
    (when (member kind agent-workflows-auto-approve-kinds)
      (agent-shell-permission-allow-always permission))))

(defun agent-workflows--start-session (on-ready)
  "Start or reuse an agent shell and call ON-READY with the shell buffer."
  (let* ((agent-shell-buffer
          (cl-letf (((symbol-function #'agent-shell--display-buffer) #'ignore))
            (agent-shell--start
             :no-focus t
             :config agent-workflows-agent-config
             :new-session t
             :session-strategy 'new))))
    (with-current-buffer agent-shell-buffer
      (setq-local agent-shell-permission-responder-function
                  #'agent-workflows--permission-responder))
    (let ((prompt-sub nil)
          (error-sub nil))
      (setq prompt-sub
            (agent-shell-subscribe-to
             :shell-buffer agent-shell-buffer
             :event 'prompt-ready
             :on-event
             (lambda (_event)
               (agent-shell-unsubscribe :subscription prompt-sub)
               (agent-shell-unsubscribe :subscription error-sub)
               (funcall on-ready agent-shell-buffer))))
      (setq error-sub
            (agent-shell-subscribe-to
             :shell-buffer agent-shell-buffer
             :event 'error
             :on-event
             (lambda (event)
               (agent-shell-unsubscribe :subscription prompt-sub)
               (agent-shell-unsubscribe :subscription error-sub)
               (kill-buffer agent-shell-buffer)
               (let ((data (map-elt event :data)))
                 (user-error "Agent shell error: %s (code: %s)"
                             (map-elt data :message)
                             (map-elt data :code)))))))))

(defun agent-workflow--last-text-output ()
  "Return plain text of last agent-shell response, stripping fragment blocks.
Must be called from within an agent-shell buffer."
  (save-excursion
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (forward-line -1)
    (agent-shell-previous-item)
    (re-search-forward comint-prompt-regexp nil 'noerror)
    (forward-line -1)
    (let ((end (point))
          (start
           (progn
             (text-property-search-backward 'agent-shell-ui-state)
             (point))))
      (let ((s (string-trim (buffer-substring-no-properties start end))))
        (cond
         ((string-match
           "\\`[ \t\n]*```[a-zA-Z]*\n\\(\\(?:.\\|\n\\)*\\)```[ \t\n]*\\'" s)
          (string-trim (match-string 1 s)))
         ((string-match "\\``\\(\\(?:.\\|\n\\)*\\)`\\'" s)
          (string-trim (match-string 1 s)))
         (t
          s))))))

(defun agent-workflows--submit-commit-message (agent-shell-buffer)
  "Submit the commit-message request from AGENT-SHELL-BUFFER."
  (with-current-buffer agent-shell-buffer
    (let ((turn-sub nil)
          (error-sub nil))
      (setq turn-sub
            (agent-shell-subscribe-to
             :shell-buffer agent-shell-buffer
             :event 'turn-complete
             :on-event
             (lambda (_event)
               (agent-shell-unsubscribe :subscription turn-sub)
               (agent-shell-unsubscribe :subscription error-sub)
               (let ((output (agent-workflow--last-text-output)))
                 (let ((tmpfile (make-temp-file "commit-msg")))
                   (with-temp-file tmpfile
                     (insert output))
                   (magit-run-git-with-editor "commit" "--edit" "-F" tmpfile))))))
      (setq error-sub
            (agent-shell-subscribe-to
             :shell-buffer agent-shell-buffer
             :event 'error
             :on-event
             (lambda (event)
               (agent-shell-unsubscribe :subscription turn-sub)
               (agent-shell-unsubscribe :subscription error-sub)
               (let ((data (map-elt event :data)))
                 (user-error "Agent shell error: %s (code: %s)"
                             (map-elt data :message)
                             (map-elt data :code)))))))
    (shell-maker-submit :input agent-workflows-commit-message-prompt)))

(defun agent-workflows--submit-review (agent-shell-buffer)
  "Submit the review request from AGENT-SHELL-BUFFER."
  (let ((review-file (make-temp-file "agent-review-" nil ".md")))
    (with-current-buffer agent-shell-buffer
      (setq-local agent-workflows-review--tmpfile review-file)
      (let ((turn-sub nil)
            (error-sub nil))
      (setq turn-sub
            (agent-shell-subscribe-to
             :shell-buffer agent-shell-buffer
             :event 'turn-complete
             :on-event
             (lambda (_event)
               (agent-shell-unsubscribe :subscription turn-sub)
               (agent-shell-unsubscribe :subscription error-sub)
               (unwind-protect
                   (let* ((results-buffer (get-buffer-create "*Agent Review*"))
                          (markdown (agent-workflows--review-source-markdown
                                     review-file)))
                     (condition-case err
                         (cl-destructuring-bind (summary findings)
                             (agent-workflows--review-parse-output markdown)
                           (agent-workflows--review-render
                            results-buffer
                            (list (cons 'summary summary)
                                  (cons 'findings findings))
                            agent-shell-buffer))
                       (error
                        (agent-workflows--review-display-raw
                         results-buffer markdown agent-shell-buffer err))))
                 (agent-workflows--review-delete-tempfile review-file)))))
        (setq error-sub
              (agent-shell-subscribe-to
               :shell-buffer agent-shell-buffer
               :event 'error
               :on-event
               (lambda (event)
                 (agent-shell-unsubscribe :subscription turn-sub)
                 (agent-shell-unsubscribe :subscription error-sub)
                 (agent-workflows--review-delete-tempfile review-file)
                 (let ((data (map-elt event :data)))
                   (user-error "Agent shell error: %s (code: %s)"
                               (map-elt data :message)
                               (map-elt data :code)))))))
      (shell-maker-submit
       :input
       (concat
        agent-workflows-review-prompt
        "\n\nWrite the review to this file as Markdown, using the exact heading format above:\n"
        review-file
        "\n\nAfter writing the file, respond with a short confirmation only.")))))

;;;###autoload
(defun agent-workflows-commit ()
  "Generate a commit message for the currently staged files."
  (interactive)
  (unless (magit-anything-staged-p)
    (user-error "Nothing staged - can't generate commit message"))
  (message "Generating commit message...")
  (agent-workflows--start-session
   (lambda (agent-shell-buffer)
     (agent-workflows--submit-commit-message agent-shell-buffer))))

;;;###autoload
(defun agent-workflows-review ()
  "Start a fresh agent shell for reviewing the current work."
  (interactive)
  (message "Launching review agent...")
  (agent-workflows--start-session
   (lambda (agent-shell-buffer) (agent-workflows--submit-review agent-shell-buffer))))

(provide 'agent-workflows)
;;; agent-workflows.el ends here
