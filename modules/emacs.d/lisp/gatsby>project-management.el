;;; gatsby>project-management.el --- version control settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(gatsby>use-internal-pacakge project
  :custom
  (vc-follow-symlinks t)
  :config
  ;; Create a new project type whose roots are defined in `gatsby>project-list'.
  
  (defvar gatsby>project-list '()
    "List of project root directories for custom project detection.")

  (cl-defstruct gatsby>project root)

  (cl-defmethod project-root ((project gatsby>project))
    (gatsby>project-root project))

  (defun gatsby>project-try (dir)
    "Check if DIR is a subfolder of any paths defined in `gatsby>project-list'."
    (when gatsby>project-list
      (let ((expanded-dir (expand-file-name dir)))
        (cl-some (lambda (project-path)
                   (let ((expanded-project (expand-file-name project-path)))
                     (when (and (file-directory-p expanded-project)
                                (string-prefix-p expanded-project expanded-dir))
                       (make-gatsby>project :root expanded-project))))
                 gatsby>project-list))))

  (add-hook 'project-find-functions #'gatsby>project-try -10)
  
  :commands project-find-file
  :general
  (:keymaps '(normal motion) :prefix "SPC"
            "op" #'project-find-file))

(gatsby>use-internal-pacakge smerge-mode
  :defer t
  :config
  (gatsby>defcommand gatsby>smerge-pick-at-point (both)
    (if both (call-interactively #'smerge-keep-all) (call-interactively #'smerge-keep-current)))
  :general
  (:keymaps 'smerge-mode-map :states '(normal visual) :prefix "SPC"
            "m" #'gatsby>smerge-pick-at-point))

;; (use-package projtree
;;  :ensure (:host github :repo "petergardfjall/emacs-projtree")
;;  :custom
;;  (projtree-show-git-status nil)
;;  (projtree-profiling-enabled nil)
;;  :config
;;  ;; don't use project-known-project-list to identify the root
;;  (defun gatsby>>projtree-root (buffer)
;;      "Return the root of the project in BUFFER using `project-root'."
;;      (when (buffer-live-p buffer)
;;          (with-current-buffer buffer
;;              (let* ((buf-file (buffer-file-name buffer))
;;                           ;; Note: for a non-file buffer (like `*scratch*') we consider its
;;                           ;; project tree root to be default-directory.
;;                           (buffer-dir (file-name-directory (or buf-file default-directory)))
;;                           (project (project-current nil buffer-dir)))
;;                  (if project
;;                          (project-root project)
;;                      nil)))))

;;  (gatsby>defcommand gatsby>projtree-open-or-toggle ()
;;      (message "%s" (button-at (point))))

;;  (advice-add #'projtree--project-root :override #'gatsby>>projtree-root))

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

  (with-eval-after-load 'eshell
    (defun gatsby>>envrc-update-after-cd (&rest _)
      "Update the current direnv environment after `eshell/cd'."
      (let ((buf (current-buffer)))
        (if (locate-dominating-file default-directory ".envrc")
            (envrc--update)
          (envrc--clear buf)
          (setq envrc--status 'none))))

    (advice-add #'eshell/cd :after #'gatsby>>envrc-update-after-cd))

  ;; setup lighter on the status line
  (defun gatsby>>envrc-lighter ()
    "`envrc--lighter' with mouse hover showing current root directory"
    `("[e:"
      (:propertize ,(if (memq envrc--status '(none error))
                        (symbol-name envrc--status)
                      ;; if current buffer has direnv, return its path
                      (abbreviate-file-name (envrc--find-env-dir)))
                   face
                   ,(pcase envrc--status
                      (`on 'envrc-mode-line-on-face)
                      (`error 'envrc-mode-line-error-face)
                      (`none 'envrc-mode-line-none-face))
                   mouse-face mode-line-highlight
                   )
      "]"))
  (setq gatsby>right-mode-line `((:eval (gatsby>>envrc-lighter)) " " ,@gatsby>right-mode-line))

  (gatsby>defcommand gatsby>envrc-log-buffer ()
    (switch-to-buffer-other-window "*envrc*"))

  :general
  (:keymaps '(normal visual motion) :prefix "SPC"
            "nn" #'envrc-reload
            "nd" #'envrc-deny
            "na" #'envrc-allow
            "nl" #'gatsby>envrc-log-buffer))

(gatsby>use-internal-pacakge git-rebase
  :config
  (add-to-list 'evil-motion-state-modes 'git-rebase-mode)
  :general
  (:keymaps 'git-rebase-mode-map :states 'motion
            "p" #'git-rebase-pick
            "e" #'git-rebase-edit
            "l" #'git-rebase-label
            "r" #'git-rebase-reword
            "s" #'git-rebase-squash
            "d" #'git-rebase-kill-line
            "f" #'git-rebase-fixup
            "M-j" #'git-rebase-move-line-down
            "M-k" #'git-rebase-move-line-up))

(provide 'gatsby>project-management)
;;; gatsby>project-management.el ends here
