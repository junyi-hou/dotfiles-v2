;;; gatsby-terminal-test.el --- tests for gatsby>terminal.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'gatsby>terminal)

(ert-deftest eshell/ssh--single-arg ()
  "Test ssh with single host."
  (let ((cd-called nil)
        (cd-args nil))
    (cl-letf (((symbol-function 'eshell/cd)
               (lambda (&rest args)
                 (setq cd-called t)
                 (setq cd-args args))))
      (eshell/ssh "example.com")
      (should cd-called)
      (should (string-match "ssh:.*example\\.com" (car cd-args))))))

(ert-deftest eshell/ssh--multiple-args-error ()
  "Test that multiple hosts raise an error."
  (should-error (eshell/ssh "host1" "host2") :type 'user-error))

(ert-deftest eshell/ff--single-file ()
  "Test file finding with single file."
  (let ((find-file-called nil)
        (find-file-args nil))
    (cl-letf (((symbol-function 'find-file)
               (lambda (file)
                 (setq find-file-called t)
                 (push file find-file-args))))
      (eshell/ff "/path/to/file.txt")
      (should find-file-called)
      (should (member "/path/to/file.txt" find-file-args)))))

(ert-deftest eshell/ff--multiple-files ()
  "Test file finding with multiple files."
  (let ((find-file-calls 0))
    (cl-letf (((symbol-function 'find-file)
               (lambda (_)
                 (setq find-file-calls (1+ find-file-calls)))))
      (eshell/ff "/file1.txt" "/file2.txt" "/file3.txt")
      (should (= find-file-calls 3)))))

(ert-deftest gatsby>eshell-cd--no-args-at-project-root ()
  "No args, cwd is project root: call cd with no args (go home)."
  (let ((cd-args 'not-called)
        (default-directory "/project/root/"))
    (cl-letf (((symbol-function 'file-remote-p) (lambda (&rest _) nil))
              ((symbol-function 'project-current) (lambda (&rest _) t))
              ((symbol-function 'project-root) (lambda (_) "/project/root/"))
              ((symbol-function 'file-truename) (lambda (p) p)))
      (gatsby>eshell-cd (lambda (&rest a) (setq cd-args a)))
      (should (equal cd-args nil)))))

(ert-deftest gatsby>eshell-cd--no-args-in-project-subdir ()
  "No args, cwd is inside a project: cd to project root."
  (let ((cd-args 'not-called)
        (default-directory "/project/root/src/"))
    (cl-letf (((symbol-function 'file-remote-p) (lambda (&rest _) nil))
              ((symbol-function 'project-current) (lambda (&rest _) t))
              ((symbol-function 'project-root) (lambda (_) "/project/root/"))
              ((symbol-function 'file-truename) (lambda (p) p)))
      (gatsby>eshell-cd (lambda (&rest a) (setq cd-args a)))
      (should (equal cd-args '("/project/root/"))))))

(ert-deftest gatsby>eshell-cd--no-args-remote-home-exists ()
  "No args, remote cwd, remote home exists: cd to remote home."
  (let ((cd-args 'not-called)
        (default-directory "/ssh:user@host:/some/dir/"))
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (_ &optional id)
                 (if (eq id 'user) "user" "/ssh:user@host:")))
              ((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'file-exists-p) (lambda (_) t)))
      (gatsby>eshell-cd (lambda (&rest a) (setq cd-args a)))
      (should (equal cd-args '("/ssh:user@host:/home/user"))))))

(ert-deftest gatsby>eshell-cd--no-args-remote-home-missing ()
  "No args, remote cwd, remote home absent: cd to host root."
  (let ((cd-args 'not-called)
        (default-directory "/ssh:user@host:/some/dir/"))
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (_ &optional id)
                 (if (eq id 'user) "user" "/ssh:user@host:")))
              ((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'file-exists-p) (lambda (_) nil)))
      (gatsby>eshell-cd (lambda (&rest a) (setq cd-args a)))
      (should (equal cd-args '("/ssh:user@host:/"))))))

(ert-deftest gatsby>eshell-cd--with-args-passthrough ()
  "With explicit args: pass them through to the original cd."
  (let ((cd-args 'not-called)
        (default-directory "/some/dir/"))
    (cl-letf (((symbol-function 'file-remote-p) (lambda (&rest _) nil))
              ((symbol-function 'project-current) (lambda (&rest _) nil)))
      (gatsby>eshell-cd (lambda (&rest a) (setq cd-args a)) "/other/dir/")
      (should (equal cd-args '(("/other/dir/")))))))

(ert-deftest gatsby>eshell-open-or-switch--no-idle-opens-new ()
  "No idle eshell in cwd: open new eshell."
  (let ((eshell-called nil)
        (default-directory "/some/dir/"))
    (cl-letf (((symbol-function 'buffer-list) (lambda () nil))
              ((symbol-function 'eshell) (lambda (&rest _) (setq eshell-called t)))
              ((symbol-function 'evil-insert-state) #'ignore))
      (gatsby>eshell-open-or-switch nil)
      (should eshell-called))))

(ert-deftest gatsby>eshell-open-or-switch--home-prefix-changes-dir ()
  "With home prefix: opens new eshell from home directory."
  (let ((captured-dir nil)
        (default-directory "/some/dir/"))
    (cl-letf (((symbol-function 'buffer-list) (lambda () nil))
              ((symbol-function 'eshell)
               (lambda (&rest _) (setq captured-dir default-directory)))
              ((symbol-function 'evil-insert-state) #'ignore))
      (gatsby>eshell-open-or-switch t)
      (should (string= captured-dir (expand-file-name "~/"))))))

(ert-deftest gatsby>eshell-open-or-switch--idle-buffer-switches-to-it ()
  "Idle eshell in same dir: display and select it."
  (let* ((display-called nil)
         (select-called nil)
         (insert-called nil)
         (default-directory "/some/dir/")
         (fake-buf (with-current-buffer (get-buffer-create " *test-eshell-idle*")
                     (setq major-mode 'eshell-mode)
                     (setq default-directory "/some/dir/")
                     (setq-local eshell-foreground-command nil)
                     (current-buffer)))
         (fake-win (make-symbol "fake-win")))
    (unwind-protect
        (cl-letf (((symbol-function 'buffer-list) (lambda () (list fake-buf)))
                  ((symbol-function 'file-equal-p) (lambda (a b) (string= a b)))
                  ((symbol-function 'display-buffer) (lambda (_) (setq display-called t)))
                  ((symbol-function 'get-buffer-window) (lambda (_) fake-win))
                  ((symbol-function 'select-window) (lambda (_) (setq select-called t)))
                  ((symbol-function 'evil-insert-state) (lambda () (setq insert-called t))))
          (gatsby>eshell-open-or-switch nil)
          (should display-called)
          (should select-called)
          (should insert-called))
      (kill-buffer fake-buf))))

(ert-deftest gatsby>eshell-open-or-switch--busy-buffer-opens-new ()
  "Eshell with foreground process in cwd: open new eshell instead."
  (let* ((eshell-called nil)
         (default-directory "/some/dir/")
         (fake-buf (with-current-buffer (get-buffer-create " *test-eshell-busy*")
                     (setq major-mode 'eshell-mode)
                     (setq default-directory "/some/dir/")
                     (setq-local eshell-foreground-command 'some-process)
                     (current-buffer))))
    (unwind-protect
        (cl-letf (((symbol-function 'buffer-list) (lambda () (list fake-buf)))
                  ((symbol-function 'file-equal-p) (lambda (a b) (string= a b)))
                  ((symbol-function 'eshell) (lambda (&rest _) (setq eshell-called t)))
                  ((symbol-function 'evil-insert-state) #'ignore))
          (gatsby>eshell-open-or-switch nil)
          (should eshell-called))
      (kill-buffer fake-buf))))

(provide 'gatsby-terminal-test)
;;; gatsby-terminal-test.el ends here
