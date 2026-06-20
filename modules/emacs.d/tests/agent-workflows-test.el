;;; agent-workflows-test.el --- tests for agent-workflows.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-workflows)

(ert-deftest agent-workflows--review-parse-output--review-payload ()
  (cl-destructuring-bind (summary findings)
      (agent-workflows--review-parse-output
       (concat
        "Short summary.\n\n"
        "### high: a.el:10 - First\n"
        "Body one.\n\n"
        "### low: b.el:20 - Second\n"
        "Body two.\n"))
    (let ((first (car findings)))
    (should (equal summary "Short summary."))
    (should (equal (alist-get 'severity first) "high"))
    (should (equal (alist-get 'file first) "a.el"))
    (should (= (alist-get 'line first) 10))
    (should (equal (alist-get 'title first) "First"))
    (should (equal (alist-get 'body first) "Body one."))
    (should (= (length findings) 2)))))

(ert-deftest agent-workflows--review-read-markdown-file--reads-file ()
  (let* ((tmpfile (make-temp-file "agent-review-" nil ".md"))
         (content "Short summary.\n\n### high: a.el:10 - First\nBody one.\n"))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert content))
          (should (equal (agent-workflows--review-read-markdown-file tmpfile)
                         content)))
      (when (file-exists-p tmpfile)
        (delete-file tmpfile)))))

(ert-deftest agent-workflows--review-source-markdown--falls-back-to-shell-output ()
  (should-error (agent-workflows--review-source-markdown "/tmp/does-not-exist")))

(ert-deftest agent-workflows--review-render--populates-navigation ()
  (cl-destructuring-bind (summary findings)
      (agent-workflows--review-parse-output
       (concat
        "Short summary.\n\n"
        "### high: a.el:10 - First\n"
        "Body one.\n\n"
        "### low: b.el:20 - Second\n"
        "Body two.\n"))
    (let ((shell-buffer (generate-new-buffer " *agent-shell*"))
          (review-buffer (get-buffer-create "*Agent Review*")))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) buffer))
                  ((symbol-function 'find-file-other-window) (lambda (&rest _) (current-buffer))))
          (agent-workflows--review-render
           review-buffer
           (list (cons 'summary summary)
                 (cons 'findings findings))
           shell-buffer)
          (with-current-buffer review-buffer
            (should (eq major-mode 'agent-workflows-review-mode))
            (goto-char (point-min))
            (re-search-forward "First")
            (beginning-of-line)
            (should (equal (alist-get 'title (agent-workflows--review-finding-at-point))
                           "First"))
            (agent-workflows-review-next)
            (should (equal (alist-get 'title (agent-workflows--review-finding-at-point))
                           "Second"))
            (agent-workflows-review-prev)
            (should (equal (alist-get 'title (agent-workflows--review-finding-at-point))
                           "First"))))
      (kill-buffer shell-buffer)
      (kill-buffer review-buffer)))))

(provide 'agent-workflows-test)
;;; agent-workflows-test.el ends here
