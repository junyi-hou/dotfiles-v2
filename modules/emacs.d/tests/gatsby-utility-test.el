;;; gatsby-utility-test.el --- tests for gatsby>>utility.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'gatsby>>utility)

;; Stubs for elpaca struct accessors — real ones come from cl-defstruct in elpaca.el
;; (field offsets must match: 0=tag 1=id 2=package 3=order 4=statuses 5=repo-dir ... 11=recipe)
(unless (fboundp 'elpaca<-repo-dir)
  (defun elpaca<-repo-dir (e) (nth 5 e)))
(unless (fboundp 'elpaca<-recipe)
  (defun elpaca<-recipe (e) (nth 11 e)))

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

;; Tests for gatsby>update-emacs-package

(defun gatsby-test--make-git-repo ()
  "Create a temp git repo with an initial commit. Return its path."
  (let ((dir (make-temp-file "gatsby-test-repo" t)))
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init" "-b" "main")
      (call-process "git" nil nil nil "config" "user.email" "test@test.com")
      (call-process "git" nil nil nil "config" "user.name" "Test")
      (with-temp-file (expand-file-name "README" dir)
        (insert "initial"))
      (call-process "git" nil nil nil "commit" "-am" "initial"))
    dir))

(defun gatsby-test--make-fake-elpaca (repo-dir recipe)
  "Create a fake elpaca struct with REPO-DIR and RECIPE.
Field positions match elpaca's cl-defstruct definition:
  0:type 1:id 2:package 3:order 4:statuses 5:repo-dir 6:build-dir
  7:mono-repo 8:main 9:files 10:build-steps 11:recipe ..."
  (list 'elpaca
        'test-pkg nil nil nil  ; id package order statuses
        repo-dir               ; repo-dir (position 5)
        nil nil nil nil nil    ; build-dir mono-repo main files build-steps
        recipe                 ; recipe (position 11)
        nil nil nil nil        ; blocking blockers dependencies dependents
        0 nil t nil nil nil))  ; queue-id queue-time init process log builtp

(ert-deftest gatsby>update-emacs-package--custom-branch ()
  "When recipe has :branch, use it for git checkout."
  (let* ((repo (make-temp-file "gatsby-pkg" t))
         (fake-e (gatsby-test--make-fake-elpaca repo '(:branch "custom-branch")))
         captured-commands)
    (unwind-protect
        (cl-letf (((symbol-function 'elpaca--queued) (lambda () `((test-pkg . ,fake-e))))
                  ((symbol-function 'completing-read) (lambda (&rest _) "test-pkg"))
                  ((symbol-function 'elpaca-get) (lambda (_) fake-e))
                  ((symbol-function 'shell-command-to-string) (lambda (_) ""))
                  ((symbol-function 'gatsby>>run-process-with-callback)
                   (lambda (cmds &rest _) (setq captured-commands cmds))))
          (gatsby>update-emacs-package)
          (should (equal (cadr captured-commands) '("git" "checkout" "custom-branch")))
          (should (equal (caddr captured-commands) '("git" "pull"))))
      (delete-directory repo t))))

(ert-deftest gatsby>update-emacs-package--no-branch-configured ()
  "When recipe has no :branch, detect the default branch from git remote."
  (let* ((repo (make-temp-file "gatsby-pkg" t))
         (fake-e (gatsby-test--make-fake-elpaca repo nil))
         captured-commands)
    (unwind-protect
        (cl-letf (((symbol-function 'elpaca--queued) (lambda () `((test-pkg . ,fake-e))))
                  ((symbol-function 'completing-read) (lambda (&rest _) "test-pkg"))
                  ((symbol-function 'elpaca-get) (lambda (_) fake-e))
                  ((symbol-function 'shell-command-to-string)
                   (lambda (cmd)
                     (if (string-match-p "remote show" cmd) "main\n" "")))
                  ((symbol-function 'gatsby>>run-process-with-callback)
                   (lambda (cmds &rest _) (setq captured-commands cmds))))
          (gatsby>update-emacs-package)
          (should (equal (cadr captured-commands) '("git" "checkout" "main"))))
      (delete-directory repo t))))

(ert-deftest gatsby>update-emacs-package--different-branch ()
  "When currently on a different branch, checkout to the configured locked branch."
  (let ((repo (gatsby-test--make-git-repo))
        captured-commands)
    (unwind-protect
        (progn
          (let ((default-directory repo))
            (call-process "git" nil nil nil "checkout" "-b" "other-branch"))
          (let* ((fake-e (gatsby-test--make-fake-elpaca repo '(:branch "main"))))
            (cl-letf (((symbol-function 'elpaca--queued) (lambda () `((test-pkg . ,fake-e))))
                      ((symbol-function 'completing-read) (lambda (&rest _) "test-pkg"))
                      ((symbol-function 'elpaca-get) (lambda (_) fake-e))
                      ((symbol-function 'shell-command-to-string)
                       (lambda (cmd)
                         (if (string-match-p "rev-parse" cmd) "other-branch\n" "")))
                      ((symbol-function 'gatsby>>run-process-with-callback)
                       (lambda (cmds &rest _) (setq captured-commands cmds))))
              (gatsby>update-emacs-package)
              (should (equal (cadr captured-commands) '("git" "checkout" "main"))))))
      (delete-directory repo t))))

(ert-deftest gatsby>update-emacs-package--already-on-target-branch ()
  "When already on the target branch, skip git checkout."
  (let* ((repo (make-temp-file "gatsby-pkg" t))
         (fake-e (gatsby-test--make-fake-elpaca repo '(:branch "main")))
         captured-commands)
    (unwind-protect
        (cl-letf (((symbol-function 'elpaca--queued) (lambda () `((test-pkg . ,fake-e))))
                  ((symbol-function 'completing-read) (lambda (&rest _) "test-pkg"))
                  ((symbol-function 'elpaca-get) (lambda (_) fake-e))
                  ((symbol-function 'shell-command-to-string)
                   (lambda (cmd)
                     (if (string-match-p "rev-parse" cmd) "main\n" "")))
                  ((symbol-function 'gatsby>>run-process-with-callback)
                   (lambda (cmds &rest _) (setq captured-commands cmds))))
          (gatsby>update-emacs-package)
          (should (equal (car captured-commands) '("git" "diff-index" "--quiet" "HEAD" "--")))
          (should (equal (cadr captured-commands) '("git" "pull")))
          (should (= (length captured-commands) 2)))
      (delete-directory repo t))))

(ert-deftest gatsby>update-emacs-package--detached-head ()
  "When in detached HEAD with no configured branch, detect default branch from remote."
  (let ((repo (gatsby-test--make-git-repo))
        captured-commands)
    (unwind-protect
        (progn
          (let* ((default-directory repo)
                 (commit (string-trim
                          (with-output-to-string
                            (call-process "git" nil standard-output nil "rev-parse" "HEAD")))))
            (call-process "git" nil nil nil "checkout" commit))
          (let* ((fake-e (gatsby-test--make-fake-elpaca repo nil)))
            (cl-letf (((symbol-function 'elpaca--queued) (lambda () `((test-pkg . ,fake-e))))
                      ((symbol-function 'completing-read) (lambda (&rest _) "test-pkg"))
                      ((symbol-function 'elpaca-get) (lambda (_) fake-e))
                      ((symbol-function 'shell-command-to-string)
                       (lambda (cmd)
                         (if (string-match-p "remote show" cmd) "main\n" "")))
                      ((symbol-function 'gatsby>>run-process-with-callback)
                       (lambda (cmds &rest _) (setq captured-commands cmds))))
              (gatsby>update-emacs-package)
              (should (equal (cadr captured-commands) '("git" "checkout" "main"))))))
      (delete-directory repo t))))

(ert-deftest gatsby>update-emacs-package--uncommitted-changes ()
  "When uncommitted changes conflict with the checkout target, git checkout
fails, the sequence halts, and elpaca-rebuild is never called."
  (let ((repo (gatsby-test--make-git-repo)))
    (unwind-protect
        (let ((default-directory repo))
          ;; Create feature branch with different README content
          (with-temp-file (expand-file-name "README" repo)
            (insert "dirty content"))
          (let* ((fake-e (gatsby-test--make-fake-elpaca repo '(:branch "main"))))
            (cl-letf (((symbol-function 'elpaca--queued) (lambda () `((test-pkg . ,fake-e))))
                      ((symbol-function 'completing-read) (lambda (&rest _) "test-pkg"))
                      ((symbol-function 'elpaca-get) (lambda (_) fake-e))
                      ((symbol-function 'elpaca-wait) #'ignore)
                      ((symbol-function 'gatsby>>update-elpaca-lock-file) #'ignore))
              ;; Wait while the process is still running
              (let ((proc (gatsby>update-emacs-package)))
                (while (accept-process-output proc 0.1))
                (should-not (= 0 (process-exit-status proc)))))))
      (delete-directory repo t))))

;; Tests for gatsby>sync-packages-to-lock-file

(defun gatsby-test--write-lock-file (dir entries)
  "Write ENTRIES as elpaca lock file at DIR/modules/emacs.d/elpaca-lock.el."
  (let* ((lock-dir (expand-file-name "modules/emacs.d" dir))
         (lock-file (expand-file-name "elpaca-lock.el" lock-dir)))
    (make-directory lock-dir t)
    (with-temp-file lock-file
      (let ((print-length nil) (print-level nil))
        (pp entries (current-buffer))))))

(ert-deftest gatsby>sync-packages-to-lock-file--calls-git-checkout-with-locked-refs ()
  "Each package with :ref in lock file gets git checkout called with that ref."
  (let* ((tmp (make-temp-file "gatsby-sync" t))
         (repo-a (make-temp-file "repo-a" t))
         (repo-b (make-temp-file "repo-b" t))
         captured-calls
         (gatsby>dotfiles-repo-location tmp))
    (gatsby-test--write-lock-file
     tmp
     `((pkg-a :recipe (:ref "aaa111aaa111"))
       (pkg-b :recipe (:ref "bbb222bbb222"))))
    (unwind-protect
        (cl-letf (((symbol-function 'elpaca-get)
                   (lambda (pkg)
                     (cond ((eq pkg 'pkg-a) (gatsby-test--make-fake-elpaca repo-a nil))
                           ((eq pkg 'pkg-b) (gatsby-test--make-fake-elpaca repo-b nil)))))
                  ((symbol-function 'gatsby>>run-process-with-callback)
                   (lambda (cmds &rest _) (push cmds captured-calls))))
          (gatsby>sync-packages-to-lock-file)
          (should (= (length captured-calls) 2))
          (should (cl-some (lambda (cmds)
                             (equal (car cmds) '("git" "checkout" "aaa111aaa111")))
                           captured-calls))
          (should (cl-some (lambda (cmds)
                             (equal (car cmds) '("git" "checkout" "bbb222bbb222")))
                           captured-calls)))
      (delete-directory tmp t)
      (delete-directory repo-a t)
      (delete-directory repo-b t))))

(ert-deftest gatsby>sync-packages-to-lock-file--skips-entries-without-ref ()
  "Packages whose lock entry has no :ref are not processed."
  (let* ((tmp (make-temp-file "gatsby-sync" t))
         (repo (make-temp-file "repo" t))
         captured-calls
         (gatsby>dotfiles-repo-location tmp))
    (gatsby-test--write-lock-file
     tmp
     `((pkg-with-ref :recipe (:ref "abc123"))
       (pkg-no-ref :recipe (:fetcher github))))
    (unwind-protect
        (cl-letf (((symbol-function 'elpaca-get)
                   (lambda (pkg)
                     (when (eq pkg 'pkg-with-ref)
                       (gatsby-test--make-fake-elpaca repo nil))))
                  ((symbol-function 'gatsby>>run-process-with-callback)
                   (lambda (cmds &rest _) (push cmds captured-calls))))
          (gatsby>sync-packages-to-lock-file)
          (should (= (length captured-calls) 1))
          (should (equal (caar captured-calls) '("git" "checkout" "abc123"))))
      (delete-directory tmp t)
      (delete-directory repo t))))

(ert-deftest gatsby>sync-packages-to-lock-file--skips-uninstalled-packages ()
  "Packages in lock file but not installed (elpaca-get returns nil) are skipped."
  (let* ((tmp (make-temp-file "gatsby-sync" t))
         captured-calls
         (gatsby>dotfiles-repo-location tmp))
    (gatsby-test--write-lock-file
     tmp
     `((not-installed :recipe (:ref "abc123"))))
    (unwind-protect
        (cl-letf (((symbol-function 'elpaca-get) (lambda (_) nil))
                  ((symbol-function 'gatsby>>run-process-with-callback)
                   (lambda (cmds &rest _) (push cmds captured-calls))))
          (gatsby>sync-packages-to-lock-file)
          (should (null captured-calls)))
      (delete-directory tmp t))))

(ert-deftest gatsby>sync-packages-to-lock-file--skips-missing-repo-dirs ()
  "Packages whose repo directory does not exist on disk are skipped."
  (let* ((tmp (make-temp-file "gatsby-sync" t))
         captured-calls
         (gatsby>dotfiles-repo-location tmp))
    (gatsby-test--write-lock-file
     tmp
     `((pkg-missing :recipe (:ref "abc123"))))
    (unwind-protect
        (cl-letf (((symbol-function 'elpaca-get)
                   (lambda (_)
                     (gatsby-test--make-fake-elpaca "/tmp/gatsby-nonexistent-repo-xyz" nil)))
                  ((symbol-function 'gatsby>>run-process-with-callback)
                   (lambda (cmds &rest _) (push cmds captured-calls))))
          (gatsby>sync-packages-to-lock-file)
          (should (null captured-calls)))
      (delete-directory tmp t))))

(provide 'gatsby-utility-test)
;;; gatsby-utility-test.el ends here
