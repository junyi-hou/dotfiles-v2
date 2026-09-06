;;; gatsby-early-init-test.el --- tests for lock file functions in early-init.el -*- lexical-binding: t; -*-

(require 'ert)

(defun gatsby>>test--lock-file-lines (file)
  "Return the lines of FILE as a list of strings."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(ert-deftest gatsby>>elpaca-update-lock-alist--creates-when-missing ()
  "Returns a single-entry alist when lock-contents is nil."
  (let* ((recipe '(:repo "https://example.com" :ref "abc"))
         (result (gatsby>>elpaca-update-lock-alist nil 'my-pkg recipe)))
    (should (= (length result) 1))
    (should (equal (map-elt result 'my-pkg)
                   `(:source "elpaca-menu-lock-file" :recipe ,recipe)))))

(ert-deftest gatsby>>elpaca-update-lock-alist--appends-new-package ()
  "Appends a new entry when the package is not already present."
  (let* ((recipe '(:repo "https://example.com" :ref "abc"))
         (existing '((other-pkg :source "elpaca-menu-lock-file" :recipe (:repo "https://other.com"))))
         (result (gatsby>>elpaca-update-lock-alist existing 'my-pkg recipe)))
    (should (= (length result) 2))
    (should (assq 'other-pkg result))
    (should (equal (map-elt result 'my-pkg)
                   `(:source "elpaca-menu-lock-file" :recipe ,recipe)))))

(ert-deftest gatsby>>elpaca-update-lock-alist--updates-existing-package ()
  "Updates the entry in-place when the package is already present."
  (let* ((new-recipe '(:repo "https://example.com" :ref "new"))
         (existing '((my-pkg :source "elpaca-menu-lock-file" :recipe (:repo "https://example.com" :ref "old"))
                     (other-pkg :source "elpaca-menu-lock-file" :recipe (:repo "https://other.com"))))
         (result (gatsby>>elpaca-update-lock-alist existing 'my-pkg new-recipe)))
    (should (= (length result) 2))
    (should (equal (map-elt result 'my-pkg)
                   `(:source "elpaca-menu-lock-file" :recipe ,new-recipe)))))

(ert-deftest gatsby>>elpaca-write-lock-file--format-is-stable ()
  "Each entry is printed on its own line with `prin1'.
`pp' line-broke entries differently across Emacs versions, so
regenerating the lock file produced formatting-only diffs (e551535)."
  (let* ((contents '((pkg-a :source "elpaca-menu-lock-file"
                            :recipe (:package "pkg-a" :ref "11111"))
                    (pkg-b :source "elpaca-menu-lock-file"
                           :recipe (:package "pkg-b" :ref "22222"))))
         (file (make-temp-file "elpaca-lock" nil ".el")))
    (unwind-protect
        (progn
          (gatsby>>elpaca-write-lock-file file contents)
          (should (equal (gatsby>>test--lock-file-lines file)
                         '("("
                           " (pkg-a :source \"elpaca-menu-lock-file\" :recipe (:package \"pkg-a\" :ref \"11111\"))"
                           " (pkg-b :source \"elpaca-menu-lock-file\" :recipe (:package \"pkg-b\" :ref \"22222\")))")))
          (should (equal (gatsby>>elpaca-read-lock-file file) contents)))
      (delete-file file))))

(ert-deftest gatsby>>elpaca-write-lock-file--update-has-no-format-only-diffs ()
  "Rebuilding one package leaves every other package's line untouched."
  (let* ((contents
          '((pkg-a :source "elpaca-menu-lock-file" :recipe
                   (:package "pkg-a" :fetcher github :repo "user/pkg-a"
                             :files ("*.el" (:exclude "*-test.el")) :ref "11111"))
            (pkg-b :source "elpaca-menu-lock-file" :recipe
                   (:package "pkg-b" :fetcher github :repo "user/pkg-b"
                             :files ("*.el" (:exclude "*-test.el")) :ref "22222"))
            (pkg-c :source "elpaca-menu-lock-file" :recipe
                   (:package "pkg-c" :fetcher github :repo "user/pkg-c"
                             :files ("*.el" (:exclude "*-test.el")) :ref "33333"))))
         (file (make-temp-file "elpaca-lock" nil ".el"))
         (updated-file (make-temp-file "elpaca-lock" nil ".el")))
    (unwind-protect
        (progn
          (gatsby>>elpaca-write-lock-file file contents)
          (let ((updated
                  (gatsby>>elpaca-update-lock-alist
                   (gatsby>>elpaca-read-lock-file file)
                   'pkg-b
                   '(:package "pkg-b" :fetcher github :repo "user/pkg-b"
                     :files ("*.el" (:exclude "*-test.el")) :ref "99999"))))
            (gatsby>>elpaca-write-lock-file updated-file updated))
          (let ((old-lines (gatsby>>test--lock-file-lines file))
                (new-lines (gatsby>>test--lock-file-lines updated-file)))
            (should (equal (nth 1 old-lines) (nth 1 new-lines)))
            (should (equal (nth 3 old-lines) (nth 3 new-lines)))
            (should-not (equal (nth 2 old-lines) (nth 2 new-lines)))
            (should (string-match-p ":ref \"99999\"" (nth 2 new-lines)))))
      (delete-file file)
      (delete-file updated-file))))

(ert-deftest gatsby>>elpaca-lock-file--checked-out-refs-match ()
  "Each package in the lock file has the same HEAD as its :ref in the recipe."
  (let* ((lock-contents (gatsby>>elpaca-read-lock-file elpaca-lock-file))
         (mismatches nil))
    (dolist (entry lock-contents)
      (let* ((id (car entry))
             (recipe (plist-get (cdr entry) :recipe))
             (expected-ref (plist-get recipe :ref))
             (source-dir (expand-file-name (symbol-name id) elpaca-sources-directory)))
        (when (and expected-ref (file-directory-p source-dir))
          (let* ((default-directory source-dir)
                 (actual-ref (string-trim
                              (shell-command-to-string "git rev-parse HEAD"))))
            (unless (string= expected-ref actual-ref)
              (push (list id expected-ref actual-ref) mismatches))))))
    (should-not mismatches)))

(provide 'gatsby-early-init-test)
;;; gatsby-early-init-test.el ends here
