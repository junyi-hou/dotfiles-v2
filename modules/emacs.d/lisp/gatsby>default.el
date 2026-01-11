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
  :config (gcmh-mode 1)

  (defun gatsby>>reset-gc ()
    (setq gc-cons-threshold 25165824)))

(use-package no-littering
  :demand t
  :ensure (:host github :repo "emacscollective/no-littering" :wait t)
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  :config
  (setq
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
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
  :config (defalias #'yes-or-no-p #'y-or-n-p)

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
    (let* ( ;; line - the current line as string
           (line
            (buffer-substring-no-properties
             (line-beginning-position) (line-end-position)))
           ;; only-comment - t if the current line starts with comment
           (only-comment
            (and comment-start-skip
                 (string-match
                  (concat "\\(^[\t ]*\\)\\(" comment-start-skip "\\)") line)))
           ;; newline-string - string insert into newline
           (newline-string
            (if only-comment
                (match-string 2 line)
              "")))
      (if (and only-comment (eq last-command 'newline))
          (progn
            (delete-region (line-beginning-position) (point))
            (insert (match-string 1 line)))
        (apply newline-fun args)
        (insert newline-string))))

  (advice-add #'newline :around #'gatsby>>newline))

(gatsby>use-internal-package server
  :hook (elpaca-after-init . gatsby>>start-server)
  :config
  (defun gatsby>>start-server (&rest _)
    (if (server-running-p)
        (display-warning
         'Dotfiles "Another emacs server is running. Do not launch new server.")
      (server-start))))

(use-package axis
  :ensure (:host github :repo "junyi-hou/emacs-axis")
  :custom (axis-db-location (concat (no-littering-expand-var-file-name "axis-data.sqlite")))
  :hook (elpaca-after-init . global-axis-mode))

(gatsby>use-internal-package compile
  :init
  (require 'ansi-color)
  (defun gatsby>>colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point-max)))

  :hook (compilation-filter . gatsby>>colorize-compilation-buffer)

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

  (defun gatsby>>compilation-start-no-popup (fun &rest args)
    (cl-letf (((symbol-function #'display-buffer) #'ignore))
      (apply fun args)))

  (advice-add #'compilation-start :around #'gatsby>>compilation-start-no-popup)

  (defun gatsby>>compilation-sentinel (proc _)
    "Pop the compilation buffer if the process errored out"
    (unless (= (process-exit-status proc) 0)
      (display-buffer (process-buffer proc) '(nil (allow-no-window . t)))))

  (advice-add #'compilation-sentinel :before #'gatsby>>compilation-sentinel)

  :evil-bind
  ((:maps compilation-shell-minor-mode-map :states normal)
   ("q" . #'gatsby>compilation-delete-buffer-if-process-finished)))

(gatsby>use-internal-package autorevert
  :hook (elpaca-after-init . global-auto-revert-mode))

(gatsby>use-internal-package recentf
  :hook (elpaca-after-init . recentf-mode))

(gatsby>use-internal-package saveplace
  :hook (elpaca-after-init . save-place-mode)
  :custom (save-place-file (expand-file-name "save-place.el" no-littering-var-directory)))

(gatsby>use-internal-package savehist
  :hook (elpaca-after-init . savehist-mode))

(gatsby>use-internal-package simple
  :init
  (setq-default
   tab-width 4
   indent-tabs-mode nil
   electric-indent-inhibit t)
  :hook
  (evil-mode
   .
   (lambda ()
     (gatsby>>put-mode-to-evil-state '(special-mode messages-buffer-mode) 'motion)))
  :config (global-visual-line-mode 1)

  (gatsby>defcommand gatsby>message-cls ()
    (let ((last-line
           (save-excursion
             (goto-char (point-max))
             (beginning-of-line)
             (point))))
      (set-window-start (selected-window) last-line)))

  :evil-bind
  ((:maps special-mode-map :states (normal motion))
   ("q" . #'kill-buffer-and-window)
   ;; TODO - q doesn't work in the messages-buffer-mode
   (:maps messages-buffer-mode-map :states (normal motion))
   ("q" . #'delete-window)
   ("C-c C-l" . #'gatsby>message-cls)))

(gatsby>use-internal-package backtrace
  :hook (evil-mode . (lambda () (gatsby>>put-mode-to-evil-state 'backtrace-mode 'motion)))
  :evil-bind ((:maps backtrace-mode-map :states (normal motion)) ("q" . #'kill-buffer-and-window)))

(gatsby>use-internal-package debug
  :hook (evil-mode . (lambda () (gatsby>>put-mode-to-evil-state 'debugger-mode 'motion)))
  :evil-bind ((:maps debugger-mode-map :states (normal motion)) ("q" . #'debugger-quit)))

(gatsby>use-internal-package recentf
  :hook (elpaca-after-init . recentf-mode))

(gatsby>use-internal-package window
  :config
  (defun gatsby>>balance-windows-ignore-side (orig-fun &rest args)
    "Advice for `balance-windows' to exclude side windows from balancing.
Side windows are temporarily deleted and then restored after balancing."
    (let ((side-window-states nil))
      ;; Collect side window information before deleting
      (dolist (win (window-list))
        (when-let ((side (window-parameter win 'window-side)))
          (push (list
                 :side side
                 :slot (window-parameter win 'window-slot)
                 :buffer (window-buffer win)
                 :width (window-width win)
                 :height (window-height win))
                side-window-states)))
      ;; Delete side windows
      (dolist (win (window-list))
        (when (window-parameter win 'window-side)
          (delete-window win)))
      ;; Balance the remaining windows
      (unwind-protect
          (apply orig-fun args)
        ;; Restore side windows
        (dolist (state side-window-states)
          (let ((win
                 (display-buffer-in-side-window
                  (plist-get state :buffer)
                  `((side . ,(plist-get state :side))
                    (slot . ,(plist-get state :slot))))))
            (set-window-dedicated-p win t)
            ;; Explicitly resize to original dimensions
            (let ((side (plist-get state :side)))
              (cond
               ((memq side '(left right))
                (window-resize win (- (plist-get state :width) (window-width win)) t t))
               ((memq side '(top bottom))
                (window-resize win (- (plist-get state :height) (window-height win))
                               nil
                               t)))))))))

  (advice-add #'balance-windows :around #'gatsby>>balance-windows-ignore-side))


(provide 'gatsby>default)
;;; gatsby>default.el ends here
