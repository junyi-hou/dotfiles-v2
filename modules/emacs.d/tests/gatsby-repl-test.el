;;; gatsby-repl-test.el --- tests for gatsby>repl.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'gatsby-repl)

(ert-deftest gatsby>jupyter-insert-cell-separator--no-markdown ()
  "Without prefix arg: insert newline, trimmed comment-start, ` +`, newline."
  (with-temp-buffer
    (let ((comment-start "# "))
      (gatsby>jupyter-insert-cell-separator nil)
      (should (equal (buffer-string) "\n# +\n")))))

(ert-deftest gatsby>jupyter-insert-cell-separator--with-markdown ()
  "With prefix arg: insert cell separator with [markdown] tag and docstring delimiters."
  (with-temp-buffer
    (let ((comment-start "# "))
      (gatsby>jupyter-insert-cell-separator t)
      (should (equal (buffer-string) "\n# + [markdown]\n\"\"\"\"\"\"")))))

(ert-deftest gatsby>jupyter-insert-cell-separator--markdown-cursor-position ()
  "With prefix arg: cursor lands between the two triple-quote groups."
  (with-temp-buffer
    (let ((comment-start "# "))
      (gatsby>jupyter-insert-cell-separator t)
      (should (equal (point) (- (point-max) 3))))))

(ert-deftest gatsby>jupyter-insert-cell-separator--trims-comment-start-whitespace ()
  "comment-start whitespace is trimmed before inserting."
  (with-temp-buffer
    (let ((comment-start "-- "))
      (gatsby>jupyter-insert-cell-separator nil)
      (should (equal (buffer-string) "\n-- +\n")))))

(ert-deftest gatsby>jupyter-insert-cell-separator--inserts-after-point ()
  "Content is inserted at point, leaving preceding text intact."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "existing")
      (gatsby>jupyter-insert-cell-separator nil)
      (should (string-prefix-p "existing\n# +\n" (buffer-string))))))

(ert-deftest gatsby>jupyter-next-cell--moves-past-separator ()
  "Point moves to end of next cell separator line."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "code\n# +\nmore code\n")
      (goto-char (point-min))
      (gatsby>jupyter-next-cell)
      (should (equal (point) (1+ (length "code\n# +\n")))))))

(ert-deftest gatsby>jupyter-next-cell--no-separator-stays-at-end ()
  "When no separator exists, returns nil and point moves to end of buffer."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "just some code\n")
      (goto-char (point-min))
      (should (null (gatsby>jupyter-next-cell)))
      (should (equal (point) (point-max))))))

(ert-deftest gatsby>jupyter-next-cell--skips-to-second-separator ()
  "Calling next-cell twice advances past two separators."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "cell1\n# +\ncell2\n# +\ncell3\n")
      (goto-char (point-min))
      (gatsby>jupyter-next-cell)
      (gatsby>jupyter-next-cell)
      (should (equal (point) (1+ (length "cell1\n# +\ncell2\n# +\n")))))))

(ert-deftest gatsby>jupyter-next-cell--matches-markdown-separator ()
  "next-cell matches markdown-style separator lines."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "code\n# + [markdown]\ntext\n")
      (goto-char (point-min))
      (gatsby>jupyter-next-cell)
      (should (equal (point) (1+ (length "code\n# + [markdown]\n")))))))

(ert-deftest gatsby>jupyter-prev-cell--moves-to-separator-start ()
  "Point moves to beginning of previous cell separator line."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "code\n# +\nmore code\n")
      (goto-char (point-max))
      (gatsby>jupyter-prev-cell)
      (should (equal (point) (1+ (length "code\n")))))))

(ert-deftest gatsby>jupyter-prev-cell--no-separator-stays-at-start ()
  "When no separator exists, returns nil and point moves to start of buffer."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "just some code\n")
      (goto-char (point-max))
      (should (null (gatsby>jupyter-prev-cell)))
      (should (equal (point) (point-min))))))

(ert-deftest gatsby>jupyter-prev-cell--skips-to-second-separator ()
  "Calling prev-cell twice retreats past two separators."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "cell1\n# +\ncell2\n# +\ncell3\n")
      (goto-char (point-max))
      (gatsby>jupyter-prev-cell)
      (gatsby>jupyter-prev-cell)
      (should (equal (point) (1+ (length "cell1\n")))))))

(ert-deftest gatsby>jupyter-next-cell--alternate-comment-start ()
  "next-cell uses comment-start to build the regexp."
  (with-temp-buffer
    (let ((comment-start "-- "))
      (insert "code\n-- +\nmore\n")
      (goto-char (point-min))
      (gatsby>jupyter-next-cell)
      (should (equal (point) (1+ (length "code\n-- +\n")))))))

(ert-deftest gatsby>jupyter-prev-cell--alternate-comment-start ()
  "prev-cell uses comment-start to build the regexp."
  (with-temp-buffer
    (let ((comment-start "-- "))
      (insert "code\n-- +\nmore\n")
      (goto-char (point-max))
      (gatsby>jupyter-prev-cell)
      (should (equal (point) (1+ (length "code\n")))))))

(defmacro gatsby>>with-eval-mock (&rest body)
  "Execute BODY with `jupyter-eval-string' mocked to return its argument."
  `(cl-letf (((symbol-function 'jupyter-eval-string) #'identity)
             ((symbol-function 'evil-normal-state) #'ignore))
     ,@body))

(ert-deftest gatsby>jupyter-eval-region-or-cell--region ()
  "With active region, evaluates only the selected text."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "line1\nline2\nline3\n")
      (set-mark (point-min))
      (goto-char (1+ (length "line1\n")))
      (gatsby>>with-eval-mock
        (let ((mark-active t)
              (transient-mark-mode t))
          (should (equal (gatsby>jupyter-eval-region-or-cell nil) "line1\n")))))))

(ert-deftest gatsby>jupyter-eval-region-or-cell--no-separators ()
  "Without separators, evaluates entire buffer."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "all code\n")
      (goto-char (point-min))
      (gatsby>>with-eval-mock
        (should (equal (gatsby>jupyter-eval-region-or-cell nil) "all code\n"))))))

(ert-deftest gatsby>jupyter-eval-region-or-cell--next-separator-only ()
  "With only a following separator, evaluates from point-min to end of that separator."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "cell1\n# +\ncell2\n")
      (goto-char (point-min))
      (gatsby>>with-eval-mock
        (should (equal (gatsby>jupyter-eval-region-or-cell nil) "cell1\n# +\n"))))))

(ert-deftest gatsby>jupyter-eval-region-or-cell--prev-separator-only ()
  "With only a preceding separator, evaluates from start of that separator to point-max."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "cell1\n# +\ncell2\n")
      (goto-char (point-max))
      (gatsby>>with-eval-mock
        (should (equal (gatsby>jupyter-eval-region-or-cell nil) "# +\ncell2\n"))))))

(ert-deftest gatsby>jupyter-eval-region-or-cell--between-two-separators ()
  "With separators on both sides, evaluates from prev separator to end of next separator."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "cell1\n# +\ncell2\n# +\ncell3\n")
      ;; place point inside cell2
      (goto-char (1+ (length "cell1\n# +\n")))
      (gatsby>>with-eval-mock
        (should (equal (gatsby>jupyter-eval-region-or-cell nil) "# +\ncell2\n# +\n"))))))

(ert-deftest gatsby>jupyter-eval-region-or-cell--from-top ()
  "With FROM-TOP non-nil, ignores previous separator and evaluates from point-min."
  (with-temp-buffer
    (let ((comment-start "# "))
      (insert "cell1\n# +\ncell2\n# +\ncell3\n")
      ;; place point inside cell2
      (goto-char (1+ (length "cell1\n# +\n")))
      (gatsby>>with-eval-mock
        (should (equal (gatsby>jupyter-eval-region-or-cell t) "cell1\n# +\ncell2\n# +\n"))))))

(ert-deftest gatsby>jupyter-start-or-switch-to-repl--has-kernel ()
  "When a live kernel exists, pops to its REPL without starting anything."
  (with-temp-buffer
    (let (pop-called)
      (cl-letf (((symbol-function 'gatsby>>has-jupyter-kernel) (lambda (&optional _) t))
                ((symbol-function 'jupyter-repl-pop-to-buffer) (lambda () (setq pop-called t))))
        (gatsby>jupyter-start-or-switch-to-repl nil)
        (should pop-called)))))

(ert-deftest gatsby>jupyter-start-or-switch-to-repl--connect ()
  "With connect arg, calls jupyter-connect-server-repl interactively then pops to REPL."
  (with-temp-buffer
    (let (interactively-called pop-called)
      (cl-letf (((symbol-function 'gatsby>>has-jupyter-kernel) (lambda (&optional _) nil))
                ((symbol-function 'call-interactively) (lambda (fn &rest _) (setq interactively-called fn)))
                ((symbol-function 'jupyter-repl-pop-to-buffer) (lambda () (setq pop-called t))))
        (gatsby>jupyter-start-or-switch-to-repl t)
        (should (eq interactively-called #'jupyter-connect-server-repl))
        (should pop-called)))))

(ert-deftest gatsby>jupyter-start-or-switch-to-repl--existing-server ()
  "Without connect, uses an existing server to run the REPL then pops to it."
  (with-temp-buffer
    (let* ((fake-server 'mock-server)
           (fake-kernel 'mock-kernel)
           run-args pop-called)
      (cl-letf (((symbol-function 'gatsby>>has-jupyter-kernel) (lambda (&optional _) nil))
                ((symbol-function 'gatsby>>get-server-at) (lambda (_) fake-server))
                ((symbol-function 'jupyter-kernelspecs) (lambda (_) 'mock-specs))
                ((symbol-function 'jupyter-completing-read-kernelspec) (lambda (_) fake-kernel))
                ((symbol-function 'jupyter-run-server-repl) (lambda (&rest args) (setq run-args args)))
                ((symbol-function 'jupyter-repl-pop-to-buffer) (lambda () (setq pop-called t))))
        (gatsby>jupyter-start-or-switch-to-repl nil)
        (should (eq (nth 0 run-args) fake-server))
        (should (eq (nth 1 run-args) fake-kernel))
        (should (eq (nth 2 run-args) nil))
        (should (eq (nth 4 run-args) nil))
        (should (eq (nth 5 run-args) t))
        (should pop-called)))))

(ert-deftest gatsby>jupyter-start-or-switch-to-repl--new-server ()
  "Without connect and no existing server, launches a notebook and constructs a new server."
  (with-temp-buffer
    (let* ((fake-server 'mock-server)
           (fake-kernel 'mock-kernel)
           launch-called constructed-url run-args pop-called)
      (cl-letf (((symbol-function 'gatsby>>has-jupyter-kernel) (lambda (&optional _) nil))
                ((symbol-function 'gatsby>>get-server-at) (lambda (_) nil))
                ((symbol-function 'jupyter-launch-notebook) (lambda () (setq launch-called t) 8888))
                ((symbol-function 'jupyter-server)
                 (lambda (&rest args) (setq constructed-url (plist-get args :url)) fake-server))
                ((symbol-function 'jupyter-notebook-process) (lambda (_) nil))
                ((symbol-function 'jupyter-kernelspecs) (lambda (_) 'mock-specs))
                ((symbol-function 'jupyter-completing-read-kernelspec) (lambda (_) fake-kernel))
                ((symbol-function 'jupyter-run-server-repl) (lambda (&rest args) (setq run-args args)))
                ((symbol-function 'jupyter-repl-pop-to-buffer) (lambda () (setq pop-called t))))
        (gatsby>jupyter-start-or-switch-to-repl nil)
        (should launch-called)
        (should (equal constructed-url "http://localhost:8888"))
        (should (eq (nth 0 run-args) fake-server))
        (should (eq (nth 1 run-args) fake-kernel))
        (should pop-called)))))

(ert-deftest gatsby>jupyter-start-or-switch-to-repl--remote ()
  "With a remote default-directory, dispatches to gatsby>>jupyter-start-remote-repl."
  (with-temp-buffer
    (let* ((default-directory "/ssh:host:/remote/path/")
           remote-repl-called)
      (cl-letf (((symbol-function 'gatsby>>has-jupyter-kernel) (lambda (&optional _) nil))
                ((symbol-function 'file-remote-p) (lambda (_) "/ssh:host:"))
                ((symbol-function 'gatsby>>jupyter-start-remote-repl)
                 (lambda (buf) (setq remote-repl-called buf))))
        (gatsby>jupyter-start-or-switch-to-repl nil)
        (should (eq remote-repl-called (current-buffer)))))))

(ert-deftest gatsby>>jupyter-start-remote-repl--starts-ssh-with-port-forward ()
  "Starts an SSH process with the correct host, port-forward arg, and jupyter server command."
  (with-temp-buffer
    (let* ((fake-proc (list 'fake))
           proc-name proc-args)
      (cl-letf (((symbol-function 'tramp-dissect-file-name) (lambda (_) 'fake-vec))
                ((symbol-function 'tramp-file-name-host) (lambda (_) "myhost"))
                ((symbol-function 'tramp-file-name-localname) (lambda (_) "/remote/dir"))
                ((symbol-function 'gatsby>>find-free-port) (lambda () 12345))
                ((symbol-function 'start-process)
                 (lambda (&rest args)
                   (setq proc-name (nth 0 args)
                         proc-args (cddr args))
                   fake-proc))
                ((symbol-function 'set-process-filter) #'ignore))
        (gatsby>>jupyter-start-remote-repl (current-buffer))
        (should (equal proc-name "jupyter-remote-myhost"))
        (should (equal (nth 0 proc-args) "ssh"))
        (should (equal (nth 1 proc-args) "-L"))
        (should (string-match-p "^127\\.0\\.0\\.1:12345:127\\.0\\.0\\.1:[0-9]+$" (nth 2 proc-args)))
        (should (equal (nth 3 proc-args) "myhost"))
        (should (string-match-p "cd.*/remote/dir" (nth 4 proc-args)))
        (should (string-match-p "jupyter server.*--no-browser" (nth 4 proc-args)))))))

(ert-deftest gatsby>>jupyter-start-remote-repl--filter-connects-on-is-running ()
  "Process filter creates server and REPL when output contains 'is running'."
  (let ((saved-sessions gatsby>>jupyter-sessions))
    (unwind-protect
        (with-temp-buffer
          (let* ((fake-proc (list 'fake))
                 (fake-server 'mock-server)
                 (fake-kernel 'mock-kernel)
                 captured-filter timer-fn run-args pop-called)
            (cl-letf (((symbol-function 'tramp-dissect-file-name) (lambda (_) 'fake-vec))
                      ((symbol-function 'tramp-file-name-host) (lambda (_) "myhost"))
                      ((symbol-function 'tramp-file-name-localname) (lambda (_) "/remote/dir"))
                      ((symbol-function 'gatsby>>find-free-port) (lambda () 12345))
                      ((symbol-function 'start-process) (lambda (&rest _) fake-proc))
                      ((symbol-function 'set-process-filter)
                       (lambda (_proc fn) (setq captured-filter fn)))
                      ((symbol-function 'run-with-timer)
                       (lambda (_d _r fn) (setq timer-fn fn)))
                      ((symbol-function 'jupyter-server) (lambda (&rest _) fake-server))
                      ((symbol-function 'jupyter-kernelspecs) (lambda (_) 'mock-specs))
                      ((symbol-function 'jupyter-completing-read-kernelspec) (lambda (_) fake-kernel))
                      ((symbol-function 'jupyter-run-server-repl)
                       (lambda (&rest args) (setq run-args args)))
                      ((symbol-function 'jupyter-repl-pop-to-buffer)
                       (lambda () (setq pop-called t))))
              (setq gatsby>>jupyter-sessions nil)
              (gatsby>>jupyter-start-remote-repl (current-buffer))
              (funcall captured-filter fake-proc "Jupyter server is running at http://localhost:8888")
              (should timer-fn)
              (funcall timer-fn)
              (should (eq (nth 0 run-args) fake-server))
              (should (eq (nth 1 run-args) fake-kernel))
              (should pop-called)
              (should (equal (plist-get (car gatsby>>jupyter-sessions) :label)
                             "remote: myhost:/remote/dir"))
              (should (equal (plist-get (car gatsby>>jupyter-sessions) :procs)
                             (list fake-proc))))))
      (setq gatsby>>jupyter-sessions saved-sessions))))

(ert-deftest gatsby>>jupyter-start-remote-repl--filter-ignores-non-running-output ()
  "Process filter does not trigger a connection when 'is running' is absent."
  (with-temp-buffer
    (let* ((fake-proc (list 'fake))
           captured-filter timer-called)
      (cl-letf (((symbol-function 'tramp-dissect-file-name) (lambda (_) 'fake-vec))
                ((symbol-function 'tramp-file-name-host) (lambda (_) "myhost"))
                ((symbol-function 'tramp-file-name-localname) (lambda (_) "/remote/dir"))
                ((symbol-function 'gatsby>>find-free-port) (lambda () 12345))
                ((symbol-function 'start-process) (lambda (&rest _) fake-proc))
                ((symbol-function 'set-process-filter)
                 (lambda (_proc fn) (setq captured-filter fn)))
                ((symbol-function 'run-with-timer) (lambda (&rest _) (setq timer-called t))))
        (gatsby>>jupyter-start-remote-repl (current-buffer))
        (funcall captured-filter fake-proc "Starting Jupyter server...")
        (should-not timer-called)))))

(ert-deftest gatsby>>jupyter-start-remote-repl--filter-connects-only-once ()
  "Process filter does not reconnect if 'is running' appears a second time."
  (with-temp-buffer
    (let* ((fake-proc (list 'fake))
           captured-filter timer-count)
      (cl-letf (((symbol-function 'tramp-dissect-file-name) (lambda (_) 'fake-vec))
                ((symbol-function 'tramp-file-name-host) (lambda (_) "myhost"))
                ((symbol-function 'tramp-file-name-localname) (lambda (_) "/remote/dir"))
                ((symbol-function 'gatsby>>find-free-port) (lambda () 12345))
                ((symbol-function 'start-process) (lambda (&rest _) fake-proc))
                ((symbol-function 'set-process-filter)
                 (lambda (_proc fn) (setq captured-filter fn)))
                ((symbol-function 'run-with-timer)
                 (lambda (&rest _) (setq timer-count (1+ (or timer-count 0))))))
        (gatsby>>jupyter-start-remote-repl (current-buffer))
        (funcall captured-filter fake-proc "is running")
        (funcall captured-filter fake-proc "is running again")
        (should (= timer-count 1))))))

(provide 'gatsby-repl-test)
;;; gatsby-repl-test.el ends here
