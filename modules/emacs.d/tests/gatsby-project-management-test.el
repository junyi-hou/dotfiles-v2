;;; gatsby-project-management-test.el --- tests for gatsby>project-management.el  -*- lexical-binding: t; -*-

(require 'ert)

(require 'gatsby>project-management)

(ert-deftest gatsby>project-try--returns-project-for-subdir ()
  "Test that project struct is returned for subdirectories."
  (let ((project-root (make-temp-file "project" t))
        (subdir nil))
    (unwind-protect
        (progn
          (setq subdir (expand-file-name "subdir" project-root))
          (make-directory subdir)
          (let ((gatsby>project-list (list project-root)))
            (let ((result (gatsby>project-try subdir)))
              (should (gatsby>project-p result))
              (should (equal (gatsby>project-root result) project-root)))))
      (delete-directory project-root t))))

(ert-deftest gatsby>project-try--returns-nil-for-untracked-dir ()
  "Test that nil is returned for directories not in project list."
  (let ((untracked (make-temp-file "untracked" t)))
    (unwind-protect
        (let ((gatsby>project-list '()))
          (let ((result (gatsby>project-try untracked)))
            (should-not result)))
      (delete-directory untracked t))))

(defmacro gatsby>>with-envrc-fixture (template-files &rest body)
  "Run BODY with a fake dotfiles repo and template containing TEMPLATE-FILES.
TEMPLATE-FILES is a list of relative paths to create under the template dir.
Binds `fixture-template-dir' (the selected template) and `fixture-dest-dir'."
  (declare (indent 1))
  `(let* ((dotfiles-dir (make-temp-file "dotfiles" t))
          (templates-dir (file-name-concat dotfiles-dir "direnv_templates"))
          (fixture-template-dir
           (progn
             (make-directory templates-dir t)
             (make-temp-file (file-name-concat templates-dir "tmpl") t)))
          (fixture-dest-dir (make-temp-file "dest" t))
          (gatsby>dotfiles-repo-location dotfiles-dir))
     (unwind-protect
         (progn
           (dolist (rel-path (list ,@template-files))
             (let ((full-path (file-name-concat fixture-template-dir rel-path)))
               (make-directory (file-name-directory full-path) t)
               (write-region rel-path nil full-path)))
           (cl-letf (((symbol-function 'completing-read)
                      (lambda (&rest _) fixture-template-dir))
                     ((symbol-function 'envrc-allow) #'ignore))
             ,@body))
       (delete-directory dotfiles-dir t)
       (delete-directory fixture-dest-dir t))))

(ert-deftest gatsby>envrc-init-from-template--copies-envrc-as-dotenvrc ()
  "Test that 'envrc' is copied as '.envrc'."
  (gatsby>>with-envrc-fixture ("envrc")
    (let ((default-directory fixture-dest-dir))
      (gatsby>envrc-init-from-template nil))
    (should (file-exists-p (file-name-concat fixture-dest-dir ".envrc")))
    (should-not (file-exists-p (file-name-concat fixture-dest-dir "envrc")))))

(ert-deftest gatsby>envrc-init-from-template--copies-dir-locals ()
  "Test that 'dir-locals.el' is copied as '.dir-locals.el'."
  (gatsby>>with-envrc-fixture ("dir-locals.el")
    (let ((default-directory fixture-dest-dir))
      (gatsby>envrc-init-from-template nil))
    (should (file-exists-p (file-name-concat fixture-dest-dir ".dir-locals.el")))
    (should-not (file-exists-p (file-name-concat fixture-dest-dir "dir-locals.el")))))

(ert-deftest gatsby>envrc-init-from-template--copies-gitignore-as-dotgitignore ()
  "Test that 'gitignore' is copied as '.gitignore' when none exists."
  (gatsby>>with-envrc-fixture ("gitignore")
    (let ((default-directory fixture-dest-dir))
      (gatsby>envrc-init-from-template nil))
    (should (file-exists-p (file-name-concat fixture-dest-dir ".gitignore")))
    (should-not (file-exists-p (file-name-concat fixture-dest-dir "gitignore")))))

(ert-deftest gatsby>envrc-init-from-template--appends-gitignore-when-exists ()
  "Test that 'gitignore' is appended to '.gitignore' when one already exists."
  (gatsby>>with-envrc-fixture ("gitignore")
    (let ((existing (file-name-concat fixture-dest-dir ".gitignore")))
      (write-region "existing-entry\n" nil existing)
      (let ((default-directory fixture-dest-dir))
        (gatsby>envrc-init-from-template nil))
      (let ((content (with-temp-buffer
                       (insert-file-contents existing)
                       (buffer-string))))
        (should (string-match-p "existing-entry" content))
        (should (string-match-p "gitignore" content))))))

(ert-deftest gatsby>envrc-init-from-template--copies-other-files-as-is ()
  "Test that other files are copied preserving their relative paths."
  (gatsby>>with-envrc-fixture ("subdir/config.toml")
    (let ((default-directory fixture-dest-dir))
      (gatsby>envrc-init-from-template nil))
    (should (file-exists-p (file-name-concat fixture-dest-dir "subdir/config.toml")))))

(ert-deftest gatsby>envrc-init-from-template--creates-missing-destination ()
  "Test that destination directory is created if it does not exist."
  (gatsby>>with-envrc-fixture ("envrc")
    (let* ((new-dest (file-name-concat fixture-dest-dir "new-subdir"))
           (default-directory new-dest))
      (gatsby>envrc-init-from-template nil))
    (should (file-directory-p (file-name-concat fixture-dest-dir "new-subdir")))))

(ert-deftest gatsby>envrc-init-from-template--errors-when-destination-is-file ()
  "Test that a user-error is raised when destination is an existing file."
  (gatsby>>with-envrc-fixture ("envrc")
    (let* ((file-path (make-temp-file "not-a-dir"))
           (default-directory file-path))
      (unwind-protect
          (should-error (gatsby>envrc-init-from-template nil) :type 'user-error)
        (delete-file file-path)))))

(ert-deftest gatsby>envrc-init-from-template--calls-envrc-allow ()
  "Test that envrc-allow is called in the destination directory."
  (gatsby>>with-envrc-fixture ("envrc")
    (let ((allow-dir nil))
      (cl-letf (((symbol-function 'envrc-allow)
                 (lambda () (setq allow-dir default-directory))))
        (let ((default-directory fixture-dest-dir))
          (gatsby>envrc-init-from-template nil)))
      (should (string= allow-dir fixture-dest-dir)))))

(provide 'gatsby-project-management-test)
;;; gatsby-project-management-test.el ends here
