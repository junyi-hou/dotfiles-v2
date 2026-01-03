;;; gatsby>lsp.el --- everything related to lsp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

;; completion
(use-package corfu
  :ensure
  (:host
   github
   :repo "emacs-straight/corfu"
   :files ("*" "extensions/*.el" (:exclude ".git")))
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :hook
  (prog-mode . corfu-mode)
  (corfu-mode . corfu-popupinfo-mode)
  :config
  (gatsby>defcommand gatsby>corfu-complete ()
    "Complete common parts of all the candidates, or insert the current selection.
Insert the current selection when
1. there is only one candidate;
2. last command is `gatsby>corfu-complete';
3. last command is `corfu-next'or `corfu-previous'."
    (if (memq
         last-command '(gatsby>corfu-complete corfu-complete corfu-next corfu-previous))
        (let ((corfu--index (max 0 corfu--index)))
          (corfu-insert))
      (corfu-expand)))

  ;; for some reason I cannot bind <TAB> to any different variables, let's just override that function
  (advice-add #'corfu-complete :override #'gatsby>corfu-complete)

  :evil-bind
  ((:maps corfu-map)
   ("M-j" . #'corfu-next)
   ("M-i" . #'corfu-info-documentation)
   ("M-k" . #'corfu-previous)
   ("SPC" . #'corfu-insert-separator)
   ("<return>" . #'corfu-complete)
   (:maps corfu-popupinfo-map)
   ("J" . #'corfu-popupinfo-scroll-up)
   ("K" . #'corfu-popupinfo-scroll-down)
   ("<esc>" . #'corfu-popupinfo-toggle)))

(use-package orderless
  :ensure (:host github :repo "oantolin/orderless")
  :custom
  (orderless-matching-styles '(orderless-prefixes orderless-literal orderless-regexp))
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

;; (use-package lsp-mode
;;   :ensure (:host github :repo "emacs-lsp/lsp-mode")
;;   :preface (setq lsp-use-plists t)
;;   :custom-face
;;   (lsp-face-highlight-read ((t :inherit underline)))
;;   (lsp-face-highlight-write ((t :inherit underline)))
;;   :custom
;;   ;; increase data read from subrocess
;;   (read-process-output-max (* 1024 1024))
;;   ;; lsp functionalities
;;   (lsp-diagnostics-provider :flymake)
;;   (lsp-lens-enable nil)
;;   (lsp-modeline-diagnostics-enable nil)
;;   (lsp-modeline-workspace-status-enable nil)
;;   (lsp-modeline-code-actions-enable nil)
;;   (lsp-headerline-breadcrumb-enable nil)
;;   (lsp-signature-render-documentation t)
;;   (lsp-signature-auto-activate nil)
;;   (lsp-enable-file-watchers nil)
;;   (lsp-enable-snippet nil)
;;   (lsp-auto-guess-root t)
;;   :config
;;   (gatsby>defcommand gatsby>lsp-toggle-format-before-save ()
;;     "Toggle LSP format-before-save. With prefix arg ON, enable if positive."
;;     (let ((enable (not (memq #'lsp-format-buffer before-save-hook))))
;;       (if enable
;;           (when (lsp-feature? "textDocument/formatting")
;;             (add-hook 'before-save-hook #'lsp-format-buffer nil t)
;;             (message "LSP format-before-save enabled"))
;;         (remove-hook 'before-save-hook #'lsp-format-buffer t)
;;         (message "LSP format-before-save disabled"))))
;;   :hook (lsp-managed-mode . gatsby>lsp-toggle-format-before-save)
;;   :evil-bind
;;   ((:maps (normal visual))
;;    ("SPC r a" . #'lsp-execute-code-action)
;;    ("SPC r l" . #'xref-find-definitions)
;;    ("SPC r L" . #'xref-find-references)
;;    ("SPC r b" . #'xref-go-back)
;;    ("SPC r f" . #'xref-go-forward)))

(use-package eglot
  :ensure (:host github :repo "joaotavora/eglot" :inherit nil)
  :custom
  ;; increase data read from subrocess
  (read-process-output-max (* 1024 1024))
  (eglot-extend-to-xref t)
  ;; UI
  (xref-prompt-for-identifier nil)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)

  :custom-face
  (eglot-inlay-hint-face ((t (:height 1.0))))
  (eglot-type-hint-face ((t (:height 1.0))))
  (eglot-parameter-hint-face ((t (:height 1.0))))

  :init
  (setq eglot-server-programs nil)
  (defvar-local gatsby>eglot-auto-format-before-save nil
    "A file-local variable, if not-nil, enable auto format buffer whenever possible")

  (put 'gatsby>eglot-auto-format-before-save 'safe-local-variable #'booleanp)

  (defun gatsby>>maybe-format-buffer ()
    (add-hook 'before-save-hook
              (defun gatsby>>maybe-format-before-save (&rest _)
                (when (and gatsby>eglot-auto-format-before-save
                           (eglot-current-server)
                           (eglot-server-capable :documentFormattingProvider))
                  (eglot-format-buffer)))
              nil t))

  :hook (eglot-managed-mode . gatsby>>maybe-format-buffer)
  :evil-bind
  ((:maps (normal visual))
   ("SPC r a" . #'eglot-code-actions)
   ("SPC r l" . #'xref-find-definitions)
   ("SPC r L" . #'xref-find-references)
   ("SPC r b" . #'xref-go-back)
   ("SPC r f" . #'xref-go-forward)))

;; template system
(use-package tempel
  :ensure (:host github :repo "minad/tempel")
  :custom (tempel-path (no-littering-expand-etc-file-name "tempel/*.eld"))
  :hook (corfu-mode . gatsby>>init-tempel)
  :init
  (defun gatsby>tempel-expand ()
    (condition-case nil
        (progn
          (call-interactively #'tempel-expand)
          t)
      (user-error
       nil))
    nil)

  (defun gatsby>>init-tempel (&rest _)
    "Put tempel-expand next to `gatsby>insert-tab' in `gatsby>tab-commands'."
    (setq gatsby>tab-commands
          '(gatsby>insert-tab gatsby>tempel-expand completion-at-point)))

  :evil-bind
  ((:maps tempel-map)
   ("C-g" . #'tempel-abort)
   ("M-j" . #'tempel-next)
   ("M-k" . #'tempel-prev)))

(use-package eglot-tempel
  :ensure (:host github :repo "fejfighter/eglot-tempel")
  :hook (elpaca-after-init . eglot-tempel-mode))

;; display flymake information in a childframe
(use-package flymake-childframe
  :ensure (:host github :repo "junyi-hou/flymake-childframe")
  :hook ((flymake eglot-managed-mode) . flymake-childframe-mode))

(use-package eldoc-mouse
  :ensure (:host github :repo "huangfeiyu/eldoc-mouse")
  :config
  (gatsby>defcommand gatsby>eldoc-pop ()
    "Use alternative poshandler, since the package did not provide this option."
    (cl-letf (((symbol-function #'posframe-poshandler-point-bottom-left-corner-upward)
               #'posframe-poshandler-point-bottom-left-corner))
      (call-interactively #'eldoc-mouse-pop-doc-at-cursor)))
  ;; enable transiant map
  ;; commands
  (defmacro gatsby>>eldoc-transiant-command (fn form)
    (declare (indent defun))
    `(gatsby>defcommand ,fn ()
       (let* ((frame
               (with-current-buffer (get-buffer-create eldoc-mouse-posframe-buffer-name)
                 posframe--frame))
              (window (get-buffer-window eldoc-mouse-posframe-buffer-name frame)))
         (with-selected-window window
           ,form))))

  (gatsby>>eldoc-transiant-command
   gatsby>eldoc-scroll-up
   (scroll-down (max 1 (/ (1- (window-height (selected-window))) 2))))

  (gatsby>>eldoc-transiant-command
   gatsby>eldoc-scroll-down
   (scroll-up (max 1 (/ (1- (window-height (selected-window))) 2))))

  (defconst gatsby>eldoc-transiant-map
    (let ((map (make-sparse-keymap)))
      (suppress-keymap map t)
      (define-key map (kbd "J") #'gatsby>eldoc-scroll-down)
      (define-key map (kbd "K") #'gatsby>eldoc-scroll-up)
      (define-key map (kbd "C-u") #'gatsby>eldoc-scroll-up)
      (define-key map (kbd "C-d") #'gatsby>eldoc-scroll-down)
      map))

  (defvar gatsby>>eldoc-restore-keymap-fn nil
    "Controlling when transient map is enabled")

  (defun gatsby>>eldoc-enable-transiant-map (&rest _)
    (with-current-buffer (get-buffer-create eldoc-mouse-posframe-buffer-name)
      (setq-local gatsby>>eldoc-restore-keymap-fn
                  (set-transient-map gatsby>eldoc-transiant-map
                                     t
                                     #'eldoc-mouse--hide-posframe))))

  (defun gatsby>>eldoc-disable-transiant-map (&rest _)
    (with-current-buffer (get-buffer-create eldoc-mouse-posframe-buffer-name)
      (let ((fn gatsby>>eldoc-restore-keymap-fn))
        (setq-local gatsby>>eldoc-restore-keymap-fn nil)
        (when (functionp fn)
          (funcall fn)))))

  (advice-add #'eldoc-mouse--pop-doc :after #'gatsby>>eldoc-enable-transiant-map)
  (advice-add #'eldoc-mouse--hide-posframe :after #'gatsby>>eldoc-disable-transiant-map)

  :evil-bind ((:maps (normal visual motion)) ("SPC r h" . #'gatsby>eldoc-pop)))

(provide 'gatsby>lsp)
;;; gatsby>lsp.el ends here
