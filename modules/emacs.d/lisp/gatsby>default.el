;;; gatsby>default.el --- sane emacs default -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(setq gc-cons-threshold most-positive-fixnum)

(use-package gcmh
  :demand t
  :ensure (:host github :repo "emacsmirror/gcmh" :wait t)
  :custom (gcmh-high-cons-threshold 33554432)
  :hook (elpaca-after-init . gatsby>>reset-gc)
  :config
  (gcmh-mode 1)

  (defun gatsby>>reset-gc ()
    (setq gc-cons-threshold 25165824)))

(use-package no-littering
  :demand t
  :ensure (:host github :repo "emacscollective/no-littering" :wait t)
  :custom
  (auto-save-file-name-transforms  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  :config
  (setq auto-save-file-name-transforms  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backup/")))
        custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror))

(use-package emacs
  :ensure nil
  :custom
  (visible-bell t)
  (ring-bell-function 'ignore)
  (server-client-instructions nil)
  (max-mini-window-height 1)
  (use-short-answers t)
  (save-silently t)
  (byte-compile-warnings nil)
  (native-comp-async-report-warnings-errors nil)
  (scroll-step 1)
  (use-short-answers t)
  (inhibit-startup-screen t)
  (fill-column 88)
  (split-window-preferred-function #'gatsby>split-window-sensibly)
  :config
  (defalias #'yes-or-no-p #'y-or-n-p)

  (defun gatsby>split-window-sensibly (&optional window)
    "Split WINDOW sensibly, preferring horizontal split if wide enough.  Splits
horizontally if window width > 2 times of `fill-column', returning right window.
Otherwise splits vertically, returning bottom window."
    (let ((window (or window (selected-window))))
      (with-selected-window window
        (if (> (window-width window) (* 2 fill-column))
            (split-window-right)
          (split-window-below)))))
  
  (defconst gatsby>>unkillable-buffers '("*scratch*" "*Messages*")
    "List of buffers that should not be killed")

  (defun gatsby>>bury-not-kill ()
    (if (member (buffer-name (current-buffer)) gatsby>>unkillable-buffers)
        (progn
          (bury-buffer)
          nil)
      t))

  (add-hook 'kill-buffer-query-functions #'gatsby>>bury-not-kill)

  (defun gatsby>>newline (newline-fun &rest args)
    "When calling `newline', check whether current line is a comment line.
If so, automatically indent and insert `comment-start-skip' after calling `newline' for
the first call.  Delete the auto-inserted comment for the second call.  Otherwise call
`newline' as default."
    (let* (;; line - the current line as string
           (line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           ;; only-comment - t if the current line starts with comment
           (only-comment (and comment-start-skip
                              (string-match (concat "\\(^[\t ]*\\)\\(" comment-start-skip "\\)") line)))
           ;; newline-string - string insert into newline
           (newline-string (if only-comment
                               (match-string 2 line)
                             "")))
      (if (and only-comment
               (eq last-command 'newline))
          (progn
            (delete-region (line-beginning-position) (point))
            (insert (match-string 1 line)))
        (apply newline-fun args)
        (insert newline-string))))

  (advice-add #'newline :around #'gatsby>>newline))

(gatsby>use-internal-pacakge server
  :hook (elpaca-after-init . gatsby>>start-server)
  :config
  (defun gatsby>>start-server (&rest _)
    (if (server-running-p)
        (display-warning 'Dotfiles "Another emacs server is running. Do not launch new server.")
      (server-start))))

(use-package axis
  :ensure (:host github :repo "junyi-hou/emacs-axis")
  :custom (axis-db-location (concat (no-littering-expand-var-file-name "axis-data.sqlite")))
  :hook (elpaca-after-init . global-axis-mode))

(gatsby>use-internal-pacakge compile
  :init
  (require 'ansi-color)
  (defun gatsby>>colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point-max)))

  :hook
  (compilation-filter . gatsby>>colorize-compilation-buffer)

  :config
  (defun gatsby>>make-compilation-buffer-name-unique (cmd &rest _)
    (with-current-buffer (get-buffer "*compilation*")
      (pcase cmd
        ((pred listp) (rename-buffer (car cmd) t))
        ((pred stringp) (rename-buffer (car (s-split " " cmd)) t)))))

  (defun gatsby>>compile-comint (cmd &optional comint)
    "Put point at the end and switch to normal state."
    (goto-char (point-max))
    (evil-normal-state))

  (advice-add #'compile :after #'gatsby>>make-compilation-buffer-name-unique)
  (advice-add #'compile :after #'gatsby>>compile-comint)

  (defun gatsby>compilation-delete-buffer-if-process-finished ()
    (interactive)
    (if (get-buffer-process (current-buffer))
        (call-interactively #'delete-window)
      (call-interactively #'kill-buffer-and-window)))

  :evil-bind
  ((:maps compilation-shell-minor-mode-map :states normal)
   ("q" . #'gatsby>compilation-delete-buffer-if-process-finished)))

(gatsby>use-internal-pacakge autorevert
  :hook (elpaca-after-init . global-auto-revert-mode))

(gatsby>use-internal-pacakge recentf
  :hook (elpaca-after-init . recentf-mode))

(gatsby>use-internal-pacakge saveplace
  :hook (elpaca-after-init . save-place-mode)
  :custom
  (save-place-file (expand-file-name "save-place.el" no-littering-var-directory)))

(gatsby>use-internal-pacakge savehist
  :hook (elpaca-after-init . savehist-mode))

(gatsby>use-internal-pacakge simple
  :init
  (setq-default tab-width 4
                indent-tabs-mode nil
                electric-indent-inhibit t)
  :hook (evil-mode . (lambda () (gatsby>>put-mode-to-evil-state 'special-mode 'motion)))
  :config
  (global-visual-line-mode 1)

  (gatsby>defcommand gatsby>message-cls ()
    (let ((last-line (save-excursion (goto-char (point-max)) (beginning-of-line) (point))))
      (set-window-start (selected-window) last-line)))

  :evil-bind
  ((:maps special-mode-map :states motion)
   ("q" . #'kill-buffer-and-window)
   (:maps messages-buffer-mode-map :states motion)
   ("q" . #'delete-window)
   ("C-c C-l" . #'gatsby>message-cls)))

(gatsby>use-internal-pacakge backtrace
  :hook (evil-mode . (lambda () (gatsby>>put-mode-to-evil-state 'backtrace-mode 'motion)))
  :evil-bind
  ((:maps backtrace-mode-map :states motion)
   ("q" . #'kill-buffer-and-window)))

(gatsby>use-internal-pacakge recentf
  :hook (elpaca-after-init . recentf-mode))


(provide 'gatsby>default)
;;; gatsby>default.el ends here
