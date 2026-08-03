;;; gatsby-utility-test.el --- tests for gatsby>>utility.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'gatsby--utility)

;; Macro expansion tests

(ert-deftest gatsby>use-internal-package--basic-expansion ()
  "Test that gatsby>use-internal-package expands to use-package with :ensure nil."
  (let ((expanded (macroexpand-1 '(gatsby>use-internal-package foo))))
    (should (eq (car expanded) 'use-package))
    (should (eq (cadr expanded) 'foo))
    (should (member :ensure expanded))
    (should (member nil expanded))))

(ert-deftest gatsby>use-internal-package--with-keywords ()
  "Test gatsby>use-internal-package preserves other keywords."
  (let ((expanded (macroexpand-1 '(gatsby>use-internal-package foo :hook (bar . baz)))))
    (should (eq (car expanded) 'use-package))
    (should (member :hook expanded))
    (should (member :ensure expanded))))

(ert-deftest gatsby>defcommand--no-args ()
  "Test gatsby>defcommand with no arguments expands to (interactive)."
  (let ((expanded (macroexpand-1 '(gatsby>defcommand foo () "doc" (message "test")))))
    (should (eq (car expanded) 'defun))
    (should (eq (cadr expanded) 'foo))
    (should (stringp (cadddr expanded)))
    (should (member '(interactive) expanded))))

(ert-deftest gatsby>defcommand--region-args ()
  "Test gatsby>defcommand with (beg end) args expands to (interactive \"r\")."
  (let ((expanded (macroexpand-1 '(gatsby>defcommand foo (beg end) (message "test")))))
    (should (eq (car expanded) 'defun))
    (should (equal (caddr expanded) '(beg end)))
    (should (member '(interactive "r") expanded))))

(ert-deftest gatsby>defcommand--single-arg ()
  "Test gatsby>defcommand with single arg expands to (interactive \"P\")."
  (let ((expanded (macroexpand-1 '(gatsby>defcommand foo (x) (message "test")))))
    (should (eq (car expanded) 'defun))
    (should (equal (caddr expanded) '(x)))
    (should (member '(interactive "P") expanded))))

(ert-deftest gatsby>defcommand--rest-args ()
  "Test gatsby>defcommand with &rest args expands to (interactive)."
  (let ((expanded (macroexpand-1 '(gatsby>defcommand foo (&rest args) (message "test")))))
    (should (eq (car expanded) 'defun))
    (should (member '(interactive) expanded))))

(ert-deftest gatsby>defcommand--keyword-args ()
  "Test gatsby>defcommand with keyword args creates proper interactive form."
  (let ((expanded (macroexpand-1 '(gatsby>defcommand foo (:x 42) (message "test")))))
    (should (eq (car expanded) 'defun))
    (should (eq (cadr expanded) 'foo))
    (should (equal (caddr expanded) '(x)))
    (should (member '(interactive (list 42)) expanded))))

(ert-deftest gatsby>defcommand--docstring ()
  "Test gatsby>defcommand preserves docstring."
  (let ((expanded (macroexpand-1 '(gatsby>defcommand foo () "my docstring" (message "test")))))
    (should (eq (car expanded) 'defun))
    (should (equal (cadddr expanded) "my docstring"))))

;; Functional tests using real filesystem

(ert-deftest gatsby>retrieve-or-save-item--read-existing-file ()
  "Test reading an existing cache file."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert ";;; -*- coding: utf-8 -*-\n")
            (pp '(item1 item2) (current-buffer))
            (write-region (point-min) (point-max) cache-file))
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (equal result '(item1 item2)))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--save-to-empty-file ()
  "Test saving a new item to an empty cache file."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          (gatsby>retrieve-or-save-item cache-file 'new-item)
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (equal result '("new-item")))))
      (delete-file cache-file))))


(ert-deftest gatsby>retrieve-or-save-item--write-format ()
  "Test that saving creates proper format with header comment."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          (gatsby>retrieve-or-save-item cache-file 'test-item)
          (with-temp-buffer
            (insert-file-contents cache-file)
            (let ((content (buffer-string)))
              (should (string-match-p ";;; .*coding: utf-8" content))
              (should (string-match-p "\"test-item\"" content)))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--preserves-item-type ()
  "Test that saving symbols converts them to strings."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          (gatsby>retrieve-or-save-item cache-file 'my-symbol)
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (equal result '("my-symbol")))
            (should (stringp (car result)))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--deduplicates-with-equal ()
  "Test that deduplication uses equal, not eq."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          ;; Save duplicate string items
          (gatsby>retrieve-or-save-item cache-file "item1")
          (gatsby>retrieve-or-save-item cache-file "item1")
          (gatsby>retrieve-or-save-item cache-file "item2")
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (= (length result) 2))
            (should (equal result '("item1" "item2")))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--deduplicates-symbols ()
  "Test that symbol deduplication works (converts to strings)."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          (gatsby>retrieve-or-save-item cache-file 'sym1)
          (gatsby>retrieve-or-save-item cache-file 'sym1)
          (gatsby>retrieve-or-save-item cache-file 'sym2)
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (= (length result) 2))
            (should (equal result '("sym1" "sym2")))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--max-length-limits-items ()
  "Test that max-length parameter limits the number of stored items."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          ;; Save multiple items with max-length 3
          (gatsby>retrieve-or-save-item cache-file 'a)
          (gatsby>retrieve-or-save-item cache-file 'b)
          (gatsby>retrieve-or-save-item cache-file 'c)
          (gatsby>retrieve-or-save-item cache-file 'd 3)
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (= (length result) 3))
            (should (member "d" result))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--null-max-length-keeps-all ()
  "Test that null max-length keeps all items."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          ;; Save multiple items with no max-length limit
          (gatsby>retrieve-or-save-item cache-file 'a)
          (gatsby>retrieve-or-save-item cache-file 'b)
          (gatsby>retrieve-or-save-item cache-file 'c)
          (gatsby>retrieve-or-save-item cache-file 'd)
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (= (length result) 4))
            (should (equal result '("a" "b" "c" "d")))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--max-length-one ()
  "Test max-length of 1 keeps only the most recent item."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          (gatsby>retrieve-or-save-item cache-file 'a)
          (gatsby>retrieve-or-save-item cache-file 'b 1)
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (= (length result) 1))
            (should (equal result '("b")))))
      (delete-file cache-file))))

;; gatsby>>read-lock-refs tests

(ert-deftest gatsby>>read-lock-refs--reads-refs ()
  "Test that `gatsby>>read-lock-refs' extracts package refs from the lock file."
  (let ((elpaca-lock-file (make-temp-file "lock" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file elpaca-lock-file
            (insert "((foo :source \"test\" :recipe (:package \"foo\" :ref \"abc123\" :type git))\n")
            (insert " (bar :source \"test\" :recipe (:package \"bar\" :ref \"def456\" :type git)))\n"))
          (let ((table (let ((elpaca-lock-file elpaca-lock-file))
                         (gatsby>>read-lock-refs)))
                (alist nil))
            (maphash (lambda (k v) (push (cons k v) alist)) table)
            (should (= (length alist) 2))
            (should (equal (cdr (assq 'foo alist)) "abc123"))
            (should (equal (cdr (assq 'bar alist)) "def456"))))
      (delete-file elpaca-lock-file))))

(ert-deftest gatsby>>read-lock-refs--missing-file ()
  "Test that `gatsby>>read-lock-refs' returns an empty table when file is missing."
  (let ((elpaca-lock-file "/nonexistent/elpaca-lock.el"))
    (let ((table (gatsby>>read-lock-refs)))
      (should (= (hash-table-count table) 0)))))

;; gatsby>switch-to-buffer-new-window tests

(ert-deftest gatsby>switch-to-buffer-new-window--switches-to-visible-buffer ()
  "Test switching to buffer that is already visible reuses the window."
  (let ((test-buffer (get-buffer-create "*test-buffer-visible*"))
        (initial-config (current-window-configuration)))
    (unwind-protect
        (progn
          ;; Set up: create two windows, display test-buffer in one
          (delete-other-windows)
          (split-window)
          (switch-to-buffer test-buffer)
          (let ((initial-window-count (length (window-list))))
            ;; Switch to the other window and call function
            (select-window (next-window))
            (gatsby>switch-to-buffer-new-window test-buffer)
            ;; Should reuse existing window, not create new one
            (should (= (length (window-list)) initial-window-count))
            (should (eq (current-buffer) test-buffer))))
      (set-window-configuration initial-config)
      (kill-buffer test-buffer))))

(ert-deftest gatsby>switch-to-buffer-new-window--creates-window-for-invisible-buffer ()
  "Test switching to invisible buffer creates a new window."
  (let ((test-buffer (get-buffer-create "*test-buffer-invisible*"))
        (initial-config (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (let ((initial-window-count (length (window-list))))
            (gatsby>switch-to-buffer-new-window test-buffer)
            ;; Should create new window
            (should (> (length (window-list)) initial-window-count))
            (should (eq (current-buffer) test-buffer))))
      (set-window-configuration initial-config)
      (kill-buffer test-buffer))))

(ert-deftest gatsby>switch-to-buffer-new-window--displays-correct-buffer ()
  "Test that the correct buffer is displayed after switching."
  (let ((test-buffer (get-buffer-create "*test-display-buffer*"))
        (initial-config (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (gatsby>switch-to-buffer-new-window test-buffer)
          (should (eq (current-buffer) test-buffer))
          (should (eq (window-buffer (selected-window)) test-buffer)))
      (set-window-configuration initial-config)
      (kill-buffer test-buffer))))

(ert-deftest gatsby>switch-to-buffer-new-window--multiple-windows ()
  "Test switching with multiple windows where buffer is visible in non-current window."
  (let ((test-buffer (get-buffer-create "*test-multi-window*"))
        (initial-config (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (split-window)
          (split-window)
          ;; Display test buffer in the second window
          (select-window (next-window))
          (switch-to-buffer test-buffer)
          (let ((target-window (selected-window)))
            ;; Move to first window
            (select-window (next-window))
            (gatsby>switch-to-buffer-new-window test-buffer)
            ;; Should have switched to the window where buffer is visible
            (should (eq (selected-window) target-window))
            (should (eq (current-buffer) test-buffer))))
      (set-window-configuration initial-config)
      (kill-buffer test-buffer))))

;;; Async process test helpers

(defun gatsby-test--wait-for (predicate &optional timeout)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds (default 10) pass."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defmacro gatsby-test--capture-messages (&rest body)
  "Run BODY capturing `message' calls; evaluate to the list of message strings."
  (declare (indent 0))
  `(let ((messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) messages))))
       ,@body)
     (nreverse messages)))

(defun gatsby-test--make-git-repo ()
  "Create a temp git repo with one commit and a working local origin.
Return (WORK-DIR . HEAD-SHA)."
  (let* ((dir (make-temp-file "gatsby-test-repo" t))
         (origin (expand-file-name "origin.git" dir))
         (work (expand-file-name "work" dir)))
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init" "-q" "--bare" origin)
      (call-process "git" nil nil nil "clone" "-q" origin work))
    (let ((default-directory work))
      (call-process "git" nil nil nil "config" "user.email" "test@example.com")
      (call-process "git" nil nil nil "config" "user.name" "test")
      (with-temp-file (expand-file-name "file.txt" work) (insert "hi"))
      (call-process "git" nil nil nil "add" "file.txt")
      (call-process "git" nil nil nil "commit" "-q" "-m" "init")
      (call-process "git" nil nil nil "push" "-q" "origin" "HEAD")
      (cons work (string-trim (shell-command-to-string "git rev-parse HEAD"))))))

;; gatsby>>sync-package-to-ref tests

(ert-deftest gatsby>>sync-package-to-ref--missing-repo-reports-error ()
  "When PKG has no repository, CALLBACK receives PKG and an error string."
  (let (result)
    (cl-letf (((symbol-function 'elpaca-get) (lambda (_) nil)))
      (gatsby>>sync-package-to-ref
       'foo "abc123" (lambda (pkg err) (setq result (list pkg err)))))
    (should (eq (car result) 'foo))
    (should (stringp (cadr result)))))

(ert-deftest gatsby>>sync-package-to-ref--success-rebuilds-and-reports ()
  "On success, CALLBACK gets (PKG nil), the package is rebuilt and HEAD moves to REF."
  (let* ((repo (gatsby-test--make-git-repo))
         (dir (car repo))
         (sha (cdr repo))
         result rebuilt)
    (unwind-protect
        (cl-letf (((symbol-function 'elpaca-get) (lambda (_) 'fake-e))
                  ((symbol-function 'elpaca-source-dir) (lambda (_) dir))
                  ((symbol-function 'elpaca-rebuild) (lambda (pkg) (push pkg rebuilt))))
          (gatsby>>sync-package-to-ref
           'foo sha (lambda (pkg err) (setq result (list pkg err))))
          (should (gatsby-test--wait-for (lambda () result)))
          (should (eq (car result) 'foo))
          (should (null (cadr result)))
          (should (equal rebuilt '(foo)))
          (let ((default-directory dir))
            (should (string= (string-trim
                              (shell-command-to-string "git rev-parse HEAD"))
                             sha))))
      (delete-directory (file-name-directory dir) t))))

(ert-deftest gatsby>>sync-package-to-ref--checkout-failure-reports-error ()
  "When `git checkout' fails, CALLBACK gets an error naming the command, no rebuild."
  (let* ((repo (gatsby-test--make-git-repo))
         (dir (car repo))
         (sha (cdr repo))
         result rebuilt)
    (unwind-protect
        (cl-letf (((symbol-function 'elpaca-get) (lambda (_) 'fake-e))
                  ((symbol-function 'elpaca-source-dir) (lambda (_) dir))
                  ((symbol-function 'elpaca-rebuild) (lambda (pkg) (push pkg rebuilt))))
          (gatsby>>sync-package-to-ref
           'foo "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
           (lambda (pkg err) (setq result (list pkg err))))
          (should (gatsby-test--wait-for (lambda () result)))
          (should (eq (car result) 'foo))
          (should (stringp (cadr result)))
          (should (string-match-p "git checkout" (cadr result)))
          (should (null rebuilt))
          (let ((default-directory dir))
            (should (string= (string-trim
                              (shell-command-to-string "git rev-parse HEAD"))
                             sha))))
      (delete-directory (file-name-directory dir) t))))

(ert-deftest gatsby>>sync-package-to-ref--fetch-failure-reports-error ()
  "When `git fetch' fails, CALLBACK gets an error naming the command, no rebuild."
  (let* ((repo (gatsby-test--make-git-repo))
         (dir (car repo))
         (sha (cdr repo))
         result rebuilt)
    (unwind-protect
        (let ((default-directory dir))
          (call-process "git" nil nil nil "remote" "set-url" "origin" "/nonexistent"))
        (cl-letf (((symbol-function 'elpaca-get) (lambda (_) 'fake-e))
                  ((symbol-function 'elpaca-source-dir) (lambda (_) dir))
                  ((symbol-function 'elpaca-rebuild) (lambda (pkg) (push pkg rebuilt))))
          (gatsby>>sync-package-to-ref
           'foo sha (lambda (pkg err) (setq result (list pkg err))))
          (should (gatsby-test--wait-for (lambda () result)))
          (should (eq (car result) 'foo))
          (should (stringp (cadr result)))
          (should (string-match-p "git fetch" (cadr result)))
          (should (null rebuilt)))
      (delete-directory (file-name-directory dir) t))))

;; gatsby>>check-packages-against-lock tests

(ert-deftest gatsby>>check-packages-against-lock--ignores-lock-only-packages ()
  "Packages in the lock file but no longer installed are treated as in sync."
  (let* ((elpaca-lock-file (make-temp-file "lock" nil ".el"))
         (checked-pkgs nil)
         result)
    (unwind-protect
        (progn
          (with-temp-file elpaca-lock-file
            (insert "((removed :source \"test\" :recipe (:package \"removed\" :ref \"abc123\" :type git)))\n"))
          (cl-letf (((symbol-function 'elpaca-get)
                     (lambda (pkg)
                       (push pkg checked-pkgs)
                       nil))
                    ((symbol-function 'gatsby>>package-current-ref)
                     (lambda (_pkg _cb)
                       (error "gatsby>>package-current-ref should not be called for lock-only packages"))))
            (let ((elpaca-lock-file elpaca-lock-file))
              (gatsby>>check-packages-against-lock
               (lambda (mismatches) (setq result mismatches))))
            (should (null result))
            (should (equal checked-pkgs '(removed)))))
      (delete-file elpaca-lock-file))))

(ert-deftest gatsby>>check-packages-against-lock--reports-wrong-ref ()
  "Installed packages whose current ref differs from the lock file are reported."
  (let* ((elpaca-lock-file (make-temp-file "lock" nil ".el"))
         result)
    (unwind-protect
        (progn
          (with-temp-file elpaca-lock-file
            (insert "((pkg :source \"test\" :recipe (:package \"pkg\" :ref \"locked\" :type git)))\n"))
          (cl-letf (((symbol-function 'elpaca-get) (lambda (_) 'e))
                    ((symbol-function 'gatsby>>package-current-ref)
                     (lambda (pkg cb) (funcall cb pkg "current"))))
            (let ((elpaca-lock-file elpaca-lock-file))
              (gatsby>>check-packages-against-lock
               (lambda (mismatches) (setq result mismatches))))
            (should (equal result '((pkg "locked" "current"))))))
      (delete-file elpaca-lock-file))))

(ert-deftest gatsby>>check-packages-against-lock--ok-when-ref-matches ()
  "Installed packages with matching ref produce no mismatches."
  (let* ((elpaca-lock-file (make-temp-file "lock" nil ".el"))
         result)
    (unwind-protect
        (progn
          (with-temp-file elpaca-lock-file
            (insert "((pkg :source \"test\" :recipe (:package \"pkg\" :ref \"abc123\" :type git)))\n"))
          (cl-letf (((symbol-function 'elpaca-get) (lambda (_) 'e))
                    ((symbol-function 'gatsby>>package-current-ref)
                     (lambda (pkg cb) (funcall cb pkg "abc123"))))
            (let ((elpaca-lock-file elpaca-lock-file))
              (gatsby>>check-packages-against-lock
               (lambda (mismatches) (setq result mismatches))))
            (should (null result))))
      (delete-file elpaca-lock-file))))

;; gatsby>>sync-packages-to-refs tests

(ert-deftest gatsby>>sync-packages-to-refs--all-succeed ()
  "All packages sync: each is attempted in order, summary lists all as updated."
  (let (attempted done)
    (cl-letf (((symbol-function 'gatsby>>sync-package-to-ref)
               (lambda (pkg ref cb) (push (list pkg ref) attempted) (funcall cb pkg nil))))
      (let ((messages
             (gatsby-test--capture-messages
               (gatsby>>sync-packages-to-refs
                '((a "r1") (b "r2"))
                (lambda (updated failed) (setq done (list updated failed)))))))
        (should (equal (nreverse attempted) '((a "r1") (b "r2"))))
        (should (equal done '((a b) nil)))
        (should (cl-some (lambda (m) (string-match-p "Updated: a, b" m)) messages))
        (should (cl-some (lambda (m) (string-match-p "Failed: none" m)) messages))))))

(ert-deftest gatsby>>sync-packages-to-refs--failure-does-not-stop-rest ()
  "A failed package is reported, remaining packages still sync, summary lists both."
  (let (attempted done)
    (cl-letf (((symbol-function 'gatsby>>sync-package-to-ref)
               (lambda (pkg _ref cb)
                 (push pkg attempted)
                 (funcall cb pkg (when (eq pkg 'b) "git checkout failed")))))
      (let ((messages
             (gatsby-test--capture-messages
               (gatsby>>sync-packages-to-refs
                '((a "r1") (b "r2") (c "r3"))
                (lambda (updated failed) (setq done (list updated failed)))))))
        (should (equal (nreverse attempted) '(a b c)))
        (should (equal done '((a c) (b))))
        (should (cl-some (lambda (m)
                           (and (string-match-p "Failed to sync b" m)
                                (string-match-p "git checkout failed" m)))
                         messages))
        (should (cl-some (lambda (m) (string-match-p "Updated: a, c" m)) messages))
        (should (cl-some (lambda (m) (string-match-p "Failed: b" m)) messages))))))

(ert-deftest gatsby>>sync-packages-to-refs--empty-list ()
  "Empty input still reports a summary and calls DONE-CALLBACK."
  (let (done)
    (let ((messages
           (gatsby-test--capture-messages
             (gatsby>>sync-packages-to-refs
              nil (lambda (updated failed) (setq done (list updated failed)))))))
      (should (equal done '(nil nil)))
      (should (cl-some (lambda (m) (string-match-p "Updated: none" m)) messages)))))

;; gatsby>>maybe-auto-sync-packages-to-lock-file tests

(ert-deftest gatsby>>maybe-auto-sync--disabled-does-nothing ()
  "When `gatsby>auto-sync-packages-to-lock-file' is nil, no check runs."
  (let (checked)
    (cl-letf (((symbol-function 'gatsby>>check-packages-against-lock)
               (lambda (_) (setq checked t)))
              (gatsby>auto-sync-packages-to-lock-file nil))
      (gatsby>>maybe-auto-sync-packages-to-lock-file)
      (should (null checked)))))

(ert-deftest gatsby>>maybe-auto-sync--no-mismatches ()
  "When nothing is out of sync, report so and do not sync anything."
  (let (synced)
    (cl-letf (((symbol-function 'gatsby>>check-packages-against-lock)
               (lambda (cb) (funcall cb nil)))
              ((symbol-function 'gatsby>>sync-packages-to-refs)
               (lambda (pairs &optional _) (setq synced pairs)))
              (gatsby>auto-sync-packages-to-lock-file t))
      (let ((messages
             (gatsby-test--capture-messages
               (gatsby>>maybe-auto-sync-packages-to-lock-file))))
        (should (null synced))
        (should (cl-some
                 (lambda (m) (string-match-p "All installed packages match lock file" m))
                 messages))))))

(ert-deftest gatsby>>maybe-auto-sync--mismatches-are-synced ()
  "Mismatches are passed to the batch sync as (PKG LOCKED-REF) pairs."
  (let (synced)
    (cl-letf (((symbol-function 'gatsby>>check-packages-against-lock)
               (lambda (cb)
                 (funcall cb '((a "locked-a" "current-a")
                               (b "locked-b" "current-b")))))
              ((symbol-function 'gatsby>>sync-packages-to-refs)
               (lambda (pairs &optional _) (setq synced pairs)))
              (gatsby>auto-sync-packages-to-lock-file t))
      (gatsby-test--capture-messages
        (gatsby>>maybe-auto-sync-packages-to-lock-file))
      (should (equal synced '((a "locked-a") (b "locked-b")))))))

(provide 'gatsby-utility-test)
;;; gatsby-utility-test.el ends here
