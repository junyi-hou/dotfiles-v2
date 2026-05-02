;;; gatsby>project-management.el --- version control settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(gatsby>use-internal-package project
  :custom (vc-follow-symlinks t)
  :config

  ;; Create a new project type whose roots are defined in `gatsby>project-list'.
  (defvar gatsby>project-list '()
    "List of project root directories for custom project detection.")

  (cl-defstruct gatsby>project
    root)

  (cl-defmethod project-root ((project gatsby>project))
    (gatsby>project-root project))

  (defun gatsby>project-try (dir)
    "Check if DIR is a subfolder of any paths defined in `gatsby>project-list'."
    (when gatsby>project-list
      (let ((expanded-dir (expand-file-name dir)))
        (cl-some
         (lambda (project-path)
           (let ((expanded-project (expand-file-name project-path)))
             (when (and (file-directory-p expanded-project)
                        (string-prefix-p expanded-project expanded-dir))
               (make-gatsby>project :root expanded-project))))
         gatsby>project-list))))

  (add-hook 'project-find-functions #'gatsby>project-try -10)

  ;; run tests
  (defvar-local gatsby>project-tests-command nil
    "Shell command string to run all tests in the current project.
Set this in .dir-locals.el, e.g. (\"clj\" \"-T:build\" \"test\").")
  (put 'gatsby>project-tests-command 'safe-local-variable #'listp)

  (defvar-local gatsby>get-individual-test-function nil
    "Function called interactively to select and return a command for a single test.
The function takes no arguments, prompts the user for a test to run, and returns
a list of strings suitable for passing to `compile', e.g. (\"clj\" \"-M:test\" \"-n\" \"my.ns\").")
  (put 'gatsby>get-individual-test-function 'safe-local-variable #'listp)

  (gatsby>defcommand gatsby>run-test (test-file)
    "Run project tests using `gatsby>project-tests-command'.
With a prefix argument TEST-FILE, call `gatsby>get-individual-test-function' to
interactively select a single test to run instead."
    (let ((default-directory
           (or (and (project-current) (project-root (project-current)))
               default-directory))
          (cmd gatsby>project-tests-command))
      (unless cmd
        (user-error
         "Do not know now to run this for this project, set `gatsby>project-tests-command' first"))

      (when (and test-file (functionp gatsby>get-individual-test-function))
        (setq cmd (funcall gatsby>get-individual-test-function)))

      (message "running tests...")
      (compile
       (if (listp cmd)
           (mapconcat #'shell-quote-argument cmd " ")
         cmd))))

  :evil-bind ((:maps normal) ("SPC o p" . #'project-find-file) ("SPC r t" . #'gatsby>run-test)))

(use-package envrc
  :ensure (:host github :repo "purcell/envrc")
  :hook (elpaca-after-init . envrc-global-mode)
  :config
  ;; wrap commands that need to be aware of the local environment
  (advice-add #'executable-find :around #'envrc-propagate-environment)

  (with-eval-after-load 'jupyter
    (advice-add #'jupyter-command :around #'envrc-propagate-environment)

    (defun gatsby>>update-jupyter-kernelspecs (&rest _)
      "Update kernelspecs before running any REPL."
      (jupyter-available-kernelspecs 'refresh))

    (advice-add #'jupyter-run-repl :before #'gatsby>>update-jupyter-kernelspecs))

  (defun gatsby>>envrc-update-after-cd (&rest _)
    "Update the current direnv environment after `eshell/cd'."
    (let ((buf (current-buffer)))
      (if (locate-dominating-file default-directory ".envrc")
          (envrc--update)
        (envrc--clear buf)
        (setq envrc--status 'none))))

  (with-eval-after-load 'eshell
    (advice-add #'eshell/cd :after #'gatsby>>envrc-update-after-cd))

  (with-eval-after-load 'vterm
    (advice-add #'vterm--set-directory :after #'gatsby>>envrc-update-after-cd))

  (gatsby>defcommand gatsby>envrc-init-from-template (ask)
    "Initialize a direnv location in `default-directory'.
     If the prefix argument ASK is non-nil, prompt for the direnv location instead."
    (let ((destination
           (if ask
               (read-file-name "Create .envrc at: "
                               nil
                               default-directory
                               nil
                               nil
                               #'file-directory-p)
             default-directory))
          (template
           (completing-read
            "Choose templates: "
            (directory-files (file-name-concat gatsby>dotfiles-repo-location
                                               "direnv_templates")
                             t "^[^.]"))))
      ;; rule out corner cases:
      (unless (file-exists-p destination)
        (make-directory destination t))

      (when (and (file-exists-p destination) (not (file-directory-p destination)))
        (user-error "%s is an existing file" destination))

      ;; copy over the files
      (dolist (file (directory-files-recursively template "^[^.]"))
        (cond
         ((equal "envrc" (file-name-base file))
          (copy-file file (file-name-concat destination ".envrc") 1))
         ((equal "dir-locals" (file-name-base file))
          (copy-file file (file-name-concat destination ".dir-locals.el") 1))
         ((equal "gitignore" (file-name-base file))
          (let ((gitignore (file-name-concat destination ".gitignore")))
            (if (file-exists-p gitignore)
                (shell-command (format "cat %s >> %s" file gitignore))
              (copy-file file gitignore))))
         (t
          (let ((template-file (file-relative-name file template)))
            ;; if template-file sits in a folder inside the template
            ;; create that folder in the destination first
            (when-let* ((dir (file-name-directory template-file)))
              (make-directory (file-name-concat destination dir) t))
            (copy-file file (file-name-concat destination template-file) 1)))))

      (let ((default-directory destination))
        (envrc-allow))))

  ;; setup lighter on the status line
  (defun gatsby>>envrc-lighter ()
    "`envrc--lighter' with mouse hover showing current root directory"
    `("[e:" (:propertize
       ,(if (memq envrc--status '(none error))
            (symbol-name envrc--status)
          ;; if current buffer has direnv, return its path
          (abbreviate-file-name (envrc--find-env-dir)))
       face
       ,(pcase envrc--status
          (`on 'envrc-mode-line-on-face)
          (`error 'envrc-mode-line-error-face)
          (`none 'envrc-mode-line-none-face))
       mouse-face mode-line-highlight)
      "]"))
  (setq gatsby>right-mode-line
        `((:eval (gatsby>>envrc-lighter)) " " ,@gatsby>right-mode-line))

  (gatsby>defcommand gatsby>envrc-log-buffer ()
    (switch-to-buffer-other-window "*envrc*"))

  :evil-bind
  ((:maps normal)
   ("SPC n i" . #'gatsby>envrc-init-from-template)
   ("SPC n n" . #'envrc-reload)
   ("SPC n a" . #'envrc-allow)
   ("SPC n l" . #'gatsby>envrc-log-buffer)))

;; This is a missing dependency in magit
(use-package transient
  :ensure (:host github :repo "magit/transient"))

;; TODO: replace magit with vc
;; ref: https://github.com/jixiuf/vmacs/blob/master/config/emacs/lazy/vcgit.el
;; and https://github.com/jixiuf/vmacs/blob/master/config/emacs/lazy/lazy-version-control.el
(use-package magit
  :ensure (:host github :repo "magit/magit")
  :demand t
  :hook
  ((evil-mode
    .
    (lambda ()
      (gatsby>>put-mode-to-evil-state
       '(magit-status-mode
         magit-diff-mode
         magit-log-mode
         magit-revision-mode
         magit-revision-mode
         magit-process-mode
         git-rebase-mode)
       'motion)))
   (evil-mode . (lambda () (gatsby>>put-mode-to-evil-state 'git-commit-mode 'insert))))
  :config

  (make-variable-buffer-local 'magit-log-section-commit-count)

  ;; make recent-commits always visible
  (magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-recent-commits
   'magit-insert-unpushed-to-upstream-or-recent
   'replace)

  (magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-unpushed-to-upstream
   'magit-insert-unpushed-to-pushremote
   'append)

  (make-variable-buffer-local 'magit-log-section-commit-count)

  (defun gatsby>>magit-change-number-of-commits (n increase)
    "Change the number of commits shown by N.
  If INCREASE is non-nil, show `magit-log-section-commit-count'+N commits,
  otherwise show `magit-log-section-commit-count' - N commits."
    (if increase
        (setq-local magit-log-section-commit-count (+ magit-log-section-commit-count n))
      (setq-local magit-log-section-commit-count (- magit-log-section-commit-count n)))
    (let ((magit-section-initial-visibility-alist
           `(,@magit-section-initial-visibility-alist (recent . show)))
          (point (point)))
      (call-interactively #'magit-refresh)
      (goto-char (min (point-max) point))))

  (defun gatsby>magit-increase-number-of-commits (&optional n)
    "Increase the current shown number of commits by N.
  If N is not given, increase by 1"
    (interactive "p")
    (gatsby>>magit-change-number-of-commits n t))

  (defun gatsby>magit-decrease-number-of-commits (&optional n)
    "Decrease the current shown number of commits by N.
  If N is not given, increase by 1"
    (interactive "p")
    (gatsby>>magit-change-number-of-commits n nil))

  (gatsby>defcommand gatsby>magit-visit-thing-at-point ()
    "Get file at point in magit buffers."
    (cond
     ((magit-section-match '([file] [hunk]))
      (let ((file (magit-file-at-point t)))
        (unless file
          (error "No file at point"))
        (magit-diff-visit-worktree-file t)))
     ((magit-section-match [commit])
      ;; commits: show the commit details
      (call-interactively #'magit-show-commit))
     ((magit-section-match [* error])
      (call-interactively #'magit-process-buffer))
     ;; TODO: use smerge instead of ediff
     ;; ((magit-section-match [stash])
     ;;  (call-interactively #'magit-ediff-show-stash))
     ;; ((and (magit-section-match '(issue pullreq))
     ;;       (featurep 'forge))
     ;;  ;; for `forge-issue' and `forge-pullreq' block, visit corresponding issue
     ;;  (call-interactively #'forge-visit-topic))
     ;; fallback - `magit-visit-thing'
     (t
      'magit-visit-thing)))

  :evil-bind
  ((:maps normal)
   ("SPC g g" . #'magit-status)
   ("SPC g l" . #'magit-log-buffer-file)
   (:maps magit-status-mode-map :states motion)
   ("+" . #'gatsby>magit-increase-number-of-commits)
   ("-" . #'gatsby>magit-decrease-number-of-commits)
   ("d" . #'magit-discard)
   ("s" . #'magit-stage)
   ("u" . #'magit-unstage)
   ("p" . #'magit-push)
   (:maps (magit-hunk-section-map magit-file-section-map))
   ("C-j" . #'windmove-down)
   (:maps magit-status-mode-map :states visual)
   ("u" . #'magit-unstage)
   (:maps (magit-status-mode-map magit-diff-mode-map magit-log-mode-map) :states motion)
   (">" . #'magit-section-forward-sibling)
   ("<" . #'magit-section-backward-sibling)
   ("`" . #'magit-dispatch)
   ("z o" . #'magit-section-show)
   ("z c" . #'magit-section-hide)
   ("RET" . #'gatsby>magit-visit-thing-at-point)
   (:maps
    (magit-status-mode-map
     magit-diff-mode-map magit-log-mode-map magit-revision-mode-map)
    :states motion)
   ("SPC r" . #'magit-refresh-buffer)))

(gatsby>use-internal-package git-rebase
  :evil-bind
  ((:maps git-rebase-mode-map :states motion)
   ("p" . #'git-rebase-pick)
   ("e" . #'git-rebase-edit)
   ("l" . #'git-rebase-label)
   ("r" . #'git-rebase-reword)
   ("s" . #'git-rebase-squash)
   ("d" . #'git-rebase-kill-line)
   ("f" . #'git-rebase-fixup)
   ("M-j" . #'git-rebase-move-line-down)
   ("M-k" . #'git-rebase-move-line-up)))

(use-package magit-delta
  :ensure (:host github :repo "dandavison/magit-delta")
  :hook (magit-mode . magit-delta-mode))

(gatsby>use-internal-package diff-mode
  :custom
  (diff-font-lock-prettify t)
  (diff-font-lock-syntax nil)
  :hook (diff-mode . outline-minor-mode)
  :evil-bind
  ((:maps diff-mode-map :states normal)
   (">" . #'diff-hunk-next)
   ("<" . #'diff-hunk-prev)
   ("a" . #'diff-apply-hunk)
   ("s" . #'diff-split-hunk)
   ("A" . #'diff-apply-buffer)
   ("q" . #'kill-buffer-and-window)))

(provide 'gatsby>project-management)
;;; gatsby>project-management.el ends here
