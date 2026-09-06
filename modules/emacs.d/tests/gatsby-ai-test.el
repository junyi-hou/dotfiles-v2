;;; gatsby-ai-test.el --- tests for gatsby-ai.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gatsby-ai)

(ert-deftest gatsby>>agent-shell-new-worktree--commands--subproject ()
  "Commands for a project subdirectory use sparse checkout for that directory."
  (let ((cmds (gatsby>>agent-shell-new-worktree--commands
               "/repo" "modules/emacs.d" "/repo/.agent-shell/worktrees/foo")))
    (should (= (length cmds) 3))
    (let ((add (nth 0 cmds))
          (sparse (nth 1 cmds))
          (checkout (nth 2 cmds)))
      (should (eq (plist-get add :kind) 'add))
      (should (equal (plist-get add :cwd) "/repo"))
      (should (equal (plist-get add :args)
                     '("git" "worktree" "add" "--no-checkout"
                       "/repo/.agent-shell/worktrees/foo")))
      (should (eq (plist-get sparse :kind) 'sparse))
      (should (equal (plist-get sparse :cwd) "/repo/.agent-shell/worktrees/foo"))
      (should (equal (plist-get sparse :args)
                     '("git" "sparse-checkout" "set" "--no-cone" "modules/emacs.d")))
      (should (eq (plist-get checkout :kind) 'checkout))
      (should (equal (plist-get checkout :cwd) "/repo/.agent-shell/worktrees/foo"))
      (should (equal (plist-get checkout :args)
                     '("git" "checkout" "HEAD"))))))

(ert-deftest gatsby>>agent-shell-new-worktree--commands--repo-root ()
  "Commands for the repository root use `.' as the sparse checkout path."
  (let ((cmds (gatsby>>agent-shell-new-worktree--commands
               "/repo" "." "/repo/.agent-shell/worktrees/bar")))
    (should (= (length cmds) 3))
    (should (equal (plist-get (nth 1 cmds) :args)
                   '("git" "sparse-checkout" "set" "--no-cone" ".")))))

(ert-deftest gatsby>agent-shell-send-file--multiple-matches-prompts ()
  "When multiple shells contain the file, prompt for which shell to use."
  (let* ((temp-dir (make-temp-file "gatsby-test-" t))
         (temp-file (expand-file-name "foo.el" temp-dir))
         (buf-a (generate-new-buffer " *test-shell-a*"))
         (buf-b (generate-new-buffer " *test-shell-b*"))
         (state (list :inserted-buffer nil :read-buffers nil))
         (advice-buffers (lambda () (list buf-a buf-b)))
         (advice-cwd (lambda () temp-dir))
         (advice-context (lambda (&rest _) "file context"))
         (advice-read
          (lambda (&rest args)
            (plist-put state :read-buffers (plist-get args :buffers))
            buf-a))
         (advice-insert
          (lambda (&rest args)
            (plist-put state :inserted-buffer (plist-get args :shell-buffer))))
         (advice-send-file (lambda (&rest _) (error "Should not fall back"))))
    (unwind-protect
        (progn
          (write-region "" nil temp-file)
          (advice-add #'agent-shell-buffers :override advice-buffers)
          (advice-add #'agent-shell-cwd :override advice-cwd)
          (advice-add #'agent-shell--get-files-context :override advice-context)
          (advice-add #'agent-shell--read-shell-buffer :override advice-read)
          (advice-add #'agent-shell-insert :override advice-insert)
          (advice-add #'agent-shell-send-file :override advice-send-file)
          (with-temp-buffer
            (setq buffer-file-name temp-file)
            (gatsby>agent-shell-send-file nil)
            (should (eq (plist-get state :inserted-buffer) buf-a))
            (should (equal (plist-get state :read-buffers) (list buf-a buf-b)))))
      (advice-remove #'agent-shell-buffers advice-buffers)
      (advice-remove #'agent-shell-cwd advice-cwd)
      (advice-remove #'agent-shell--get-files-context advice-context)
      (advice-remove #'agent-shell--read-shell-buffer advice-read)
      (advice-remove #'agent-shell-insert advice-insert)
      (advice-remove #'agent-shell-send-file advice-send-file)
      (delete-directory temp-dir t)
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(ert-deftest gatsby>agent-shell-send-file--no-match-falls-back ()
  "When no shell contains the file, fall back to `agent-shell-send-file'."
  (let* ((state (list :fallback-called nil))
         (advice-buffers (lambda () nil))
         (advice-send-file (lambda (&rest _)
                             (plist-put state :fallback-called t))))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name "/project/foo.el")
          (advice-add #'agent-shell-buffers :override advice-buffers)
          (advice-add #'agent-shell-send-file :override advice-send-file)
          (gatsby>agent-shell-send-file nil)
          (should (plist-get state :fallback-called)))
      (advice-remove #'agent-shell-buffers advice-buffers)
      (advice-remove #'agent-shell-send-file advice-send-file))))

(ert-deftest gatsby>agent-shell-send-file--region-sends-region ()
  "When the region is active, send the region instead of the file."
  (let* ((state (list :region-sent nil))
         (advice-region (lambda (&rest _)
                          (plist-put state :region-sent t)))
         (advice-send-file (lambda (&rest _)
                             (error "Should send region, not file"))))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name "/project/foo.el")
          (insert "line1\nline2\nline3\n")
          (set-mark (point-min))
          (goto-char (line-end-position 2))
          (activate-mark)
          (advice-add #'agent-shell-send-region :override advice-region)
          (advice-add #'agent-shell-send-file :override advice-send-file)
          (gatsby>agent-shell-send-file nil)
          (should (plist-get state :region-sent)))
      (advice-remove #'agent-shell-send-region advice-region)
      (advice-remove #'agent-shell-send-file advice-send-file))))

(ert-deftest gatsby>>agent-shell-manager-launch--no-buffers-launches-new ()
  "When there are no project agent shells, launch a new one directly."
  (let ((launched nil))
    (cl-letf (((symbol-function #'gatsby>>agent-shell-current-client)
               (lambda () nil))
              ((symbol-function #'gatsby>agent-shell-launch)
               (lambda (&rest _) (setq launched t))))
      (gatsby>>agent-shell-manager-launch nil)
      (should launched))))

(ert-deftest gatsby>>agent-shell-manager-launch--new-choice-launches-new ()
  "Selecting \"new\" launches a new agent shell."
  (let* ((buf (generate-new-buffer " *test-shell*"))
         (launched nil))
    (unwind-protect
        (cl-letf (((symbol-function #'gatsby>>agent-shell-current-client)
                   (lambda () (list buf)))
                  ((symbol-function #'completing-read)
                   (lambda (&rest _) "new"))
                  ((symbol-function #'agent-shell--read-shell-buffer)
                   (lambda (&rest _) (error "Should not read existing shell")))
                  ((symbol-function #'gatsby>agent-shell-launch)
                   (lambda (&rest _) (setq launched t))))
          (gatsby>>agent-shell-manager-launch nil)
          (should launched))
      (kill-buffer buf))))

(ert-deftest gatsby>>agent-shell-manager-launch--worktree-choice-launches-worktree ()
  "Selecting \"new (in a new worktree)\" launches a worktree shell."
  (let* ((buf (generate-new-buffer " *test-shell*"))
         (worktree nil))
    (unwind-protect
        (cl-letf (((symbol-function #'gatsby>>agent-shell-current-client)
                   (lambda () (list buf)))
                  ((symbol-function #'completing-read)
                   (lambda (&rest _) "new (in a new worktree)"))
                  ((symbol-function #'agent-shell--read-shell-buffer)
                   (lambda (&rest _) (error "Should not read existing shell")))
                  ((symbol-function #'gatsby>>agent-shell-new-worktree-shell)
                   (lambda (&rest _) (setq worktree t))))
          (gatsby>>agent-shell-manager-launch nil)
          (should worktree))
      (kill-buffer buf))))

(ert-deftest gatsby>>agent-shell-manager-launch--existing-choice-uses-read-shell-buffer ()
  "Selecting \"existing...\" prompts for an existing shell."
  (let* ((buf (generate-new-buffer " *test-shell*"))
         (state (list :read-buffers nil :displayed-buffer nil)))
    (unwind-protect
        (cl-letf (((symbol-function #'gatsby>>agent-shell-current-client)
                   (lambda () (list buf)))
                  ((symbol-function #'completing-read)
                   (lambda (&rest _) "existing..."))
                  ((symbol-function #'agent-shell--read-shell-buffer)
                   (lambda (&rest args)
                     (plist-put state :read-buffers (plist-get args :buffers))
                     buf))
                  ((symbol-function #'display-buffer)
                   (lambda (buffer _action)
                     (plist-put state :displayed-buffer buffer)))
                  ((symbol-function #'select-window) #'ignore)
                  ((symbol-function #'evil-insert-state) #'ignore))
          (gatsby>>agent-shell-manager-launch nil)
          (should (equal (plist-get state :read-buffers) (list buf)))
          (should (eq (plist-get state :displayed-buffer) buf)))
      (kill-buffer buf))))

(provide 'gatsby-ai-test)
;;; gatsby-ai-test.el ends here
