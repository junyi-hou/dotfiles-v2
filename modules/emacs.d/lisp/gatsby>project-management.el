;;; gatsby>project-management.el --- version control settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(gatsby>use-internal-pacakge project
  :custom (vc-follow-symlinks t)
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
        (cl-some
         (lambda (project-path)
           (let ((expanded-project (expand-file-name project-path)))
             (when (and (file-directory-p expanded-project)
                        (string-prefix-p expanded-project expanded-dir))
               (make-gatsby>project :root expanded-project))))
         gatsby>project-list))))
  (add-hook 'project-find-functions #'gatsby>project-try -10))

;; (gatsby>use-internal-pacakge smerge-mode
;;   :evil-bind
;;   ((:maps (normal visual))
;;    ("SPC ]" . #'smerge-next)
;;    ("SPC [" . #'smerge-prev)
;;    ("SPC m" . #'smerge-keep-current)
;;    ("SPC M" . #'smerge-keep-all)))

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

  ;; setup lighter on the status line
  ;; (defun gatsby>>envrc-lighter ()
  ;;   "`envrc--lighter' with mouse hover showing current root directory"
  ;;   `("[e:"
  ;;     (:propertize ,(if (memq envrc--status '(none error))
  ;;                       (symbol-name envrc--status)
  ;;                     ;; if current buffer has direnv, return its path
  ;;                     (abbreviate-file-name (envrc--find-env-dir)))
  ;;                  face
  ;;                  ,(pcase envrc--status
  ;;                     (`on 'envrc-mode-line-on-face)
  ;;                     (`error 'envrc-mode-line-error-face)
  ;;                     (`none 'envrc-mode-line-none-face))
  ;;                  mouse-face mode-line-highlight
  ;;                  )
  ;;     "]"))
  ;; (setq gatsby>right-mode-line `((:eval (gatsby>>envrc-lighter)) " " ,@gatsby>right-mode-line))

  (gatsby>defcommand gatsby>envrc-log-buffer ()
    (switch-to-buffer-other-window "*envrc*"))

  :evil-bind
  ((:maps normal)
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
  :custom
  (magit-status-section-hook
   '(magit-insert-status-headers
     magit-insert-untracked-files
     magit-insert-unstaged-changes
     magit-insert-staged-changes
     magit-insert-recent-commits))
  :config

  (make-variable-buffer-local 'magit-log-section-commit-count)

  (defun gatsby>>magit-change-number-of-commits (n increase)
    "Change the number of commits shown by N.
  If INCREASE is non-nil, show `magit-log-section-commit-count'+N commits,
  otherwise show `magit-log-section-commit-count' - N commits."
    (if increase
        (setq-local magit-log-section-commit-count (+ magit-log-section-commit-count n))
      (setq-local magit-log-section-commit-count (- magit-log-section-commit-count n)))
    (let ((inhibit-read-only t)
          (magit-section-initial-visibility-alist
           `(,@magit-section-initial-visibility-alist (recent . show)))
          (point (point)))
      (erase-buffer)
      (magit-status-refresh-buffer)
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
        (magit-diff-visit-file--internal nil #'switch-to-buffer-other-window)))
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

  (gatsby>defcommand gatsby>magit-refresh-status (all-section)
    (if (and all-section (eq major-mode 'magit-status-mode))
        (let ((magit-status-section-hook
               '(magit-insert-status-headers
                 magit-insert-merge-log
                 magit-insert-rebase-sequence
                 magit-insert-am-sequence
                 magit-insert-sequencer-sequence
                 magit-insert-bisect-output
                 magit-insert-bisect-rest
                 magit-insert-bisect-log
                 magit-insert-untracked-files
                 magit-insert-unstaged-changes
                 magit-insert-staged-changes
                 magit-insert-stashes
                 magit-insert-unpushed-to-pushremote
                 magit-insert-unpushed-to-upstream
                 magit-insert-unpulled-from-pushremote
                 magit-insert-unpulled-from-upstream
                 magit-insert-recent-commits))
              (inhibit-read-only t)
              (point (point)))
          (erase-buffer)
          (magit-status-refresh-buffer)
          (goto-char (min (point-max) point)))
      (call-interactively #'magit-refresh-buffer)))

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
   (:maps magit-hunk-section-map)
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
   ("SPC r" . #'gatsby>magit-refresh-status)
   (:maps git-rebase-mode-map :states motion)
   ("p" . #'git-rebase-pick)
   ("e" . #'git-rebase-edit)
   ("l" . #'git-rebase-label)
   ("r" . #'git-rebase-reword)
   ("s" . #'git-rebase-squash)
   ("d" . #'git-rebase-kill-line)
   ("f" . #'git-rebase-fixup)
   ("M-j" . #'git-rebase-move-line-down)
   ("M-k" . #'git-rebase-move-line-up)))

(gatsby>use-internal-pacakge diff-mode
  :defer t
  :custom-face
  (gatsby>diff-context ((t :extend t :background "#212121" :foreground "#8e9999")))
  (gatsby>diff-context-hl ((t :extend t :background "#3E3E3E" :foreground "#EEFFFF")))
  (gatsby>diff-added-hl
   ((t :extend t :weight bold :background "#414836" :foreground ,(doom-color 'green))))
  (gatsby>diff-removed-hl
   ((t :extend t :weight bold :background "#5f4545" :foreground ,(doom-color 'red))))
  (gatsby>diff-hunk-heading-hl
   ((t
     :extend t
     :weight bold
     :background ,(doom-color 'violet)
     :foreground ,(doom-color 'bg))))
  :custom
  (diff-font-lock-prettify t)
  (diff-font-lock-syntax nil)
  :hook
  (diff-mode . outline-minor-mode)
  (diff-mode . gatsby>>diff-setup-hunk-highlighting)
  (diff-mode . gatsby>diff-highlight-current-hunk)
  :config

  (advice-add #'diff-apply-hunk :after #'diff-kill-applied-hunks)

  ;; mimic magit faces
  (defun gatsby>>diff-apply-overlay (ov face &optional priority)
    (overlay-put ov 'font-lock-face face)
    (overlay-put ov 'priority (or priority 1))
    (overlay-put ov 'evaproate t)
    (overlay-put ov 'gatsby>diff-overlay t))

  (defun gatsby>>diff-dim-region (beg end)
    "Apply `gatsby>diff-context' face overlay to BEG and END"
    (let ((ov (make-overlay beg end)))
      (gatsby>>diff-apply-overlay ov 'gatsby>diff-context)))

  (defun gatsby>>diff-highligh-current-hunk (beg end)
    "Apply `gatsby>diff-context-hl', `gatsby>diff-removed-hl' and `gatsby>diff-added-hl' face between BEG and END."
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((line-start (line-beginning-position))
              (line-end (1+ (line-end-position))))
          (when (< line-start end)
            (let ((face
                   (cond
                    ((looking-at "^\\+")
                     'gatsby>diff-added-hl)
                    ((looking-at "^-")
                     'gatsby>diff-removed-hl)
                    ((looking-at diff-hunk-header-re-unified)
                     'gatsby>diff-hunk-heading-hl)
                    (t
                     'gatsby>diff-context-hl))))
              (let ((ov (make-overlay line-start line-end)))
                (gatsby>>diff-apply-overlay ov face)))))
        (forward-line 1))))

  (defun gatsby>diff-highlight-current-hunk ()
    (remove-overlays (point-min) (point-max) 'gatsby>diff-overlay t)
    (let ((hunk-bounds
           (ignore-errors
             (diff-bounds-of-hunk)))
          (file-bounds
           (ignore-errors
             (diff-bounds-of-file))))
      (if hunk-bounds
          (pcase-let ((`(,beg ,end) hunk-bounds)
                      (`(,file-header _) file-bounds))
            (gatsby>>diff-dim-region (point-min) (1- beg))
            (gatsby>>diff-dim-region (1+ end) (point-max))
            (gatsby>>diff-highligh-current-hunk beg end))
        (gatsby>>diff-dim-region (point-min) (point-max)))))

  (defun gatsby>>diff-setup-hunk-highlighting ()
    (add-hook 'post-command-hook #'gatsby>diff-highlight-current-hunk nil t))

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
