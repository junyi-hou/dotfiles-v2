;;; side-by-side-diff-test.el --- tests for side-by-side-diff.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'side-by-side-diff)

;;; Fixture helpers

(defvar ssdf-test--fixtures-dir
  (expand-file-name "fixtures" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing captured delta output fixtures.")

(defun ssdf-test--load-fixture (name)
  "Read and return the Elisp-`prin1'-encoded string from fixture NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name ssdf-test--fixtures-dir))
    (read (current-buffer))))

(defmacro ssdf-test--with-delta-fixture (name &rest body)
  "Execute BODY with `ssdf--run-delta' mocked to return the fixture named NAME.
Prints the ANSI-stripped fixture content so test output shows what is being tested."
  (declare (indent 1))
  `(let ((fixture (ssdf-test--load-fixture ,name)))
     (message "\n=== fixture: %s ===\n%s\n===================" ,name
              (ansi-color-filter-apply fixture))
     (cl-letf (((symbol-function 'ssdf--run-delta) (lambda (_) fixture)))
       ,@body)))

(defun ssdf-test--strip-props (hunks)
  "Return HUNKS with text properties stripped from line content."
  (mapcar (lambda (h)
            (ssdf--hunk-create
             :file       (ssdf--hunk-file h)
             :header     (ssdf--hunk-header h)
             :old-start  (ssdf--hunk-old-start h)
             :new-start  (ssdf--hunk-new-start h)
             :lines      (mapcar (lambda (l)
                                   (cons (car l) (substring-no-properties (cdr l))))
                                 (ssdf--hunk-lines h))))
          hunks))

;;; ssdf--align

(ert-deftest ssdf--align--empty ()
  (let ((result (ssdf--align nil)))
    (should (null (car result)))
    (should (null (cdr result)))))

(ert-deftest ssdf--align--context-line ()
  (let ((result (ssdf--align '((context . "foo")))))
    (should (equal (car result) '((context . "foo"))))
    (should (equal (cdr result) '((context . "foo"))))))

(ert-deftest ssdf--align--removed-only ()
  (let ((result (ssdf--align '((removed . "old")))))
    (should (equal (car result) '((removed . "old"))))
    (should (equal (cdr result) '((padding . ""))))))

(ert-deftest ssdf--align--added-only ()
  (let ((result (ssdf--align '((added . "new")))))
    (should (equal (car result) '((padding . ""))))
    (should (equal (cdr result) '((added . "new"))))))

(ert-deftest ssdf--align--removed-added-pair ()
  (let ((result (ssdf--align '((removed . "old") (added . "new")))))
    (should (equal (car result) '((removed . "old"))))
    (should (equal (cdr result) '((added . "new"))))))

(ert-deftest ssdf--align--more-removed-than-added ()
  (let ((result (ssdf--align '((removed . "a") (removed . "b") (added . "x")))))
    (should (equal (car result) '((removed . "a") (removed . "b"))))
    (should (equal (cdr result) '((added . "x") (padding . ""))))))

(ert-deftest ssdf--align--more-added-than-removed ()
  (let ((result (ssdf--align '((removed . "a") (added . "x") (added . "y")))))
    (should (equal (car result) '((removed . "a") (padding . ""))))
    (should (equal (cdr result) '((added . "x") (added . "y"))))))

(ert-deftest ssdf--align--context-surrounds-change ()
  (let ((result (ssdf--align '((context . "pre")
                               (removed . "old") (added . "new")
                               (context . "post")))))
    (should (equal (car result) '((context . "pre") (removed . "old") (context . "post"))))
    (should (equal (cdr result) '((context . "pre") (added . "new") (context . "post"))))))

(ert-deftest ssdf--align--multiple-context-runs ()
  (let ((result (ssdf--align '((context . "c1") (context . "c2")
                               (removed . "r") (added . "a")
                               (context . "c3")))))
    (should (equal (car result) '((context . "c1") (context . "c2") (removed . "r") (context . "c3"))))
    (should (equal (cdr result) '((context . "c1") (context . "c2") (added . "a") (context . "c3"))))))

;;; ssdf--propertize-hunk-header

(ert-deftest ssdf--propertize-hunk-header--contains-ranges ()
  (let ((result (ssdf--propertize-hunk-header "@@ -1,5 +2,8 @@ myfunc")))
    (should (stringp result))
    (should (string-match-p "-1,5" result))
    (should (string-match-p "+2,8" result))
    (should (string-match-p "myfunc" result))))

(ert-deftest ssdf--propertize-hunk-header--no-func-context ()
  (let ((result (ssdf--propertize-hunk-header "@@ -1,3 +1,3 @@")))
    (should (stringp result))
    (should (string-match-p "-1,3" result))
    (should (string-match-p "+1,3" result))))

(ert-deftest ssdf--propertize-hunk-header--malformed-falls-through ()
  (let ((result (ssdf--propertize-hunk-header "not a hunk header")))
    (should (stringp result))
    (should (string-match-p "not a hunk header" result))))

(ert-deftest ssdf--propertize-hunk-header--ends-with-newline ()
  (let ((result (ssdf--propertize-hunk-header "@@ -1 +1 @@")))
    (should (string-suffix-p "\n" result))))

;;; Navigation

(defmacro ssdf-test--with-buffer (content &rest body)
  "Run BODY in a temp buffer pre-filled with CONTENT, point at start."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(ert-deftest ssdf-next-hunk--moves-to-next-hunk ()
  (ssdf-test--with-buffer
   "@@ -1,3 +1,3 @@\n context\n-old\n+new\n@@ -10,2 +10,2 @@\n more\n"
   (ssdf-next-hunk)
   (should (looking-at "@@ -10,2"))))

(ert-deftest ssdf-next-hunk--no-next-hunk-does-not-move ()
  (ssdf-test--with-buffer
   "@@ -1,3 +1,3 @@\n context\n"
   (let ((pos (point)))
     (ssdf-next-hunk)
     (should (= (point) pos)))))

(ert-deftest ssdf-prev-hunk--moves-to-previous-hunk ()
  (ssdf-test--with-buffer
   "@@ -1,3 +1,3 @@\n ctx\n@@ -10,2 +10,2 @@\n more\n"
   (goto-char (point-max))
   (ssdf-prev-hunk)
   (should (looking-at "@@ -10,2"))
   (ssdf-prev-hunk)
   (should (looking-at "@@ -1,3"))))

(ert-deftest ssdf-next-file--moves-to-next-file ()
  (ssdf-test--with-buffer
   "=== a.el ===\n@@ -1 +1 @@\n foo\n=== b.el ===\n@@ -2 +2 @@\n bar\n"
   (ssdf-next-file)
   (should (looking-at "=== b.el ==="))))

(ert-deftest ssdf-next-file--no-next-file-does-not-move ()
  (ssdf-test--with-buffer
   "=== a.el ===\n@@ -1 +1 @@\n foo\n"
   (let ((pos (point)))
     (ssdf-next-file)
     (should (= (point) pos)))))

(ert-deftest ssdf-prev-file--moves-to-previous-file ()
  (ssdf-test--with-buffer
   "=== a.el ===\n@@ -1 +1 @@\n foo\n=== b.el ===\n@@ -2 +2 @@\n bar\n"
   (goto-char (point-max))
   (ssdf-prev-file)
   (should (looking-at "=== b.el ==="))))

;;; ssdf-agent-shell-mode

(ert-deftest ssdf-agent-shell-mode--explicit-enable-is-idempotent ()
  (unwind-protect
      (progn
        (ssdf-agent-shell-mode -1)
        (ssdf-agent-shell-mode 1)
        (ssdf-agent-shell-mode 1)
        (should ssdf-agent-shell-mode))
    (ssdf-agent-shell-mode -1)))

;;; ssdf--hunk-bounds

(ert-deftest ssdf--hunk-bounds--at-hunk-header ()
  (with-temp-buffer
    (insert "@@ -1,3 +1,3 @@\n context\n-old\n+new\n")
    (goto-char (point-min))
    (let ((bounds (ssdf--hunk-bounds)))
      (should bounds)
      (should (= (car bounds) (point-min))))))

(ert-deftest ssdf--hunk-bounds--inside-hunk ()
  (with-temp-buffer
    (insert "@@ -1,3 +1,3 @@\n context\n-old\n+new\n@@ -10 +10 @@\n")
    (goto-char (point-min))
    (forward-line 2)
    (let ((bounds (ssdf--hunk-bounds)))
      (should bounds)
      (should (< (car bounds) (cdr bounds))))))

(ert-deftest ssdf--hunk-bounds--two-hunks-selects-correct-one ()
  (with-temp-buffer
    (insert "@@ -1 +1 @@\n line\n@@ -10 +10 @@\n other\n")
    (goto-char (point-min))
    (re-search-forward "^@@ -10")
    (beginning-of-line)
    (let* ((bounds (ssdf--hunk-bounds))
           (header-text (buffer-substring (car bounds)
                                          (min (+ (car bounds) 9) (point-max)))))
      (should bounds)
      (should (string-prefix-p "@@ -10" header-text)))))

(ert-deftest ssdf--hunk-bounds--no-hunk-returns-nil ()
  (with-temp-buffer
    (insert "just plain text\nno hunks here\n")
    (goto-char (point-min))
    (should (null (ssdf--hunk-bounds)))))

;;; ssdf--parse-delta-line

;; Synthetic inputs: no ANSI escapes, just the │ (U+2502) gutter separator
;; that delta always emits between line numbers and diff content.

(ert-deftest ssdf--parse-delta-line--context ()
  (should (equal (ssdf--parse-delta-line "  1   1 │ (defun foo ())")
                 '(context . "(defun foo ())"))))

(ert-deftest ssdf--parse-delta-line--removed ()
  (should (equal (ssdf--parse-delta-line "  2      │-  old line")
                 '(removed . "  old line"))))

(ert-deftest ssdf--parse-delta-line--added ()
  (should (equal (ssdf--parse-delta-line "     2   │+  new line")
                 '(added . "  new line"))))

(ert-deftest ssdf--parse-delta-line--no-gutter-returns-nil ()
  (should (null (ssdf--parse-delta-line "no pipe separator here"))))

(ert-deftest ssdf--parse-delta-line--empty-content ()
  (should (equal (ssdf--parse-delta-line "  5   5 │ ")
                 '(context . ""))))

;;; ssdf--parse-delta (mocked fixtures — no delta binary required)

(ert-deftest ssdf--parse-delta--single-hunk ()
  "Golden: one removed/added pair in foo.el with two context lines."
  (ssdf-test--with-delta-fixture "delta-single-hunk.el"
    (let ((hunks (ssdf-test--strip-props (ssdf--parse-delta ""))))
      (should (= 1 (length hunks)))
      (let ((h (car hunks)))
        (should (equal (ssdf--hunk-file h) "foo.el"))
        (should (equal (ssdf--hunk-header h) "@@ -1,4 +1,4 @@"))
        (should (= (ssdf--hunk-old-start h) 1))
        (should (= (ssdf--hunk-new-start h) 1))
        (should (equal (ssdf--hunk-lines h)
                       '((context . "(defun foo ()")
                         (removed . "  (message \"hello\")")
                         (added   . "  (message \"world\")")
                         (context . "  nil)"))))))))

(ert-deftest ssdf--parse-delta--two-hunks-same-file ()
  "Golden: two hunks in a.el — first adds lines, second removes one."
  (ssdf-test--with-delta-fixture "delta-two-hunks.el"
    (let ((hunks (ssdf-test--strip-props (ssdf--parse-delta ""))))
      (should (= 2 (length hunks)))
      (let ((h1 (nth 0 hunks))
            (h2 (nth 1 hunks)))
        (should (equal (ssdf--hunk-file h1) "a.el"))
        (should (equal (ssdf--hunk-header h1) "@@ -1,3 +1,4 @@"))
        (should (= (ssdf--hunk-old-start h1) 1))
        (should (= (ssdf--hunk-new-start h1) 1))
        (should (equal (ssdf--hunk-lines h1)
                       '((context . "(setq x 1)")
                         (added   . "(setq y 2)")
                         (added   . "(setq z 3)")
                         (context . "(setq w 4)"))))
        (should (equal (ssdf--hunk-file h2) "a.el"))
        (should (equal (ssdf--hunk-header h2) "@@ -10,3 +11,2 @@"))
        (should (= (ssdf--hunk-old-start h2) 10))
        (should (= (ssdf--hunk-new-start h2) 11))
        (should (equal (ssdf--hunk-lines h2)
                       '((context . "(foo)")
                         (removed . "(bar)")
                         (context . "(baz)"))))))))

(ert-deftest ssdf--parse-delta--two-files ()
  "Golden: one hunk per file — x.py and y.py each have one substitution."
  (ssdf-test--with-delta-fixture "delta-two-files.el"
    (let ((hunks (ssdf-test--strip-props (ssdf--parse-delta ""))))
      (should (= 2 (length hunks)))
      (let ((h1 (nth 0 hunks))
            (h2 (nth 1 hunks)))
        (should (equal (ssdf--hunk-file h1) "x.py"))
        (should (equal (ssdf--hunk-header h1) "@@ -1,5 +1,5 @@"))
        (should (= (ssdf--hunk-old-start h1) 1))
        (should (= (ssdf--hunk-new-start h1) 1))
        (should (equal (ssdf--hunk-lines h1)
                       '((context . "def foo():")
                         (removed . "    return 1")
                         (added   . "    return 42"))))
        (should (equal (ssdf--hunk-file h2) "y.py"))
        (should (equal (ssdf--hunk-header h2) "@@ -1,3 +1,3 @@"))
        (should (= (ssdf--hunk-old-start h2) 1))
        (should (= (ssdf--hunk-new-start h2) 1))
        (should (equal (ssdf--hunk-lines h2)
                       '((context . "def bar():")
                         (removed . "    pass")
                         (added   . "    return True"))))))))

;;; ssdf--parse-delta (integration — real delta binary)

(ert-deftest ssdf--parse-delta--integration-single-hunk ()
  "Integration: drive real delta; results must match the mocked golden."
  (skip-unless (executable-find ssdf-delta-program))
  (let* ((diff (concat "diff --git a/foo.el b/foo.el\n"
                       "index 111..222 100644\n"
                       "--- a/foo.el\n"
                       "+++ b/foo.el\n"
                       "@@ -1,4 +1,4 @@\n"
                       " (defun foo ()\n"
                       "-  (message \"hello\")\n"
                       "+  (message \"world\")\n"
                       "   nil)\n"))
         (hunks (ssdf-test--strip-props (ssdf--parse-delta diff))))
    (should (= 1 (length hunks)))
    (let ((h (car hunks)))
      (should (equal (ssdf--hunk-file h) "foo.el"))
      (should (equal (ssdf--hunk-header h) "@@ -1,4 +1,4 @@"))
      (should (equal (ssdf--hunk-lines h)
                     '((context . "(defun foo ()")
                       (removed . "  (message \"hello\")")
                       (added   . "  (message \"world\")")
                       (context . "  nil)")))))))

(ert-deftest ssdf--parse-delta--integration-two-files ()
  "Integration: two-file diff splits into two hunks with correct file attribution."
  (skip-unless (executable-find ssdf-delta-program))
  (let* ((diff (concat "diff --git a/x.py b/x.py\n"
                       "index aaa..bbb 100644\n"
                       "--- a/x.py\n"
                       "+++ b/x.py\n"
                       "@@ -1,5 +1,5 @@\n"
                       " def foo():\n"
                       "-    return 1\n"
                       "+    return 42\n"
                       "diff --git a/y.py b/y.py\n"
                       "index ccc..ddd 100644\n"
                       "--- a/y.py\n"
                       "+++ b/y.py\n"
                       "@@ -1,3 +1,3 @@\n"
                       " def bar():\n"
                       "-    pass\n"
                       "+    return True\n"))
         (hunks (ssdf-test--strip-props (ssdf--parse-delta diff))))
    (should (= 2 (length hunks)))
    (should (equal (ssdf--hunk-file (nth 0 hunks)) "x.py"))
    (should (equal (ssdf--hunk-file (nth 1 hunks)) "y.py"))))

(provide 'side-by-side-diff-test)
;;; side-by-side-diff-test.el ends here
