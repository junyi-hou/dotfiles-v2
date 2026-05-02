;;; gatsby-early-init-test.el --- tests for lock file functions in early-init.el -*- lexical-binding: t; -*-

(require 'ert)

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

(ert-deftest gatsby>>elpaca-lock-file--checked-out-refs-match ()
  "Each package in the lock file has the same HEAD as its :ref in the recipe."
  (let* ((lock-contents
          (with-temp-buffer
            (insert-file-contents elpaca-lock-file)
            (car (read-from-string (buffer-string)))))
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
