;;; gatsby>lsp.el --- everything related to lsp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

;; completion
(use-package corfu
  :ensure (:host github :repo "emacs-straight/corfu" :files ("*" "extensions/*.el" (:exclude ".git")))
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :hook
  (prog-mode . corfu-mode)
  (corfu-mode . corfu-popupinfo-mode)
  (corfu-mode . gatsby>>set-capf-order)
  :config
  (gatsby>defcommand gatsby>corfu-complete ()
    "Complete common parts of all the candidates, or insert the current selection.
Insert the current selection when
1. there is only one candidate;
2. last command is `gatsby>corfu-complete';
3. last command is `corfu-next'or `corfu-previous'."
    (if (memq last-command '(gatsby>corfu-complete corfu-complete corfu-next corfu-previous))
        (let ((corfu--index (max 0 corfu--index)))
              (corfu-insert))
      (corfu-expand)))

  ;; for some reason I cannot bind <TAB> to any different variables, let's just override that function
  (advice-add #'corfu-complete :override #'gatsby>corfu-complete)

  ;; If no completion is found, insert a tab:
  (defun gatsby>>insert-tab-if-no-compliaton-is-found ()
    (when (save-excursion (skip-chars-backward " \t") (bolp)) (insert-tab)) nil)

  ;; make sure `gatsby>>insert-tab-if-no-compliaton-is-found' is always the first element in `capf'
  (defun gatsby>>set-capf-order (&rest _)
    (if-let ((tab (cl-find-if (lambda (it) (eq it #'gatsby>>insert-tab-if-no-compliaton-is-found)) completion-at-point-functions)))
        (setq-local completion-at-point-functions `(gatsby>>insert-tab-if-no-compliaton-is-found ,@(cl-remove #'gatsby>>insert-tab-if-no-compliaton-is-found completion-at-point-functions)))
      (setq-local completion-at-point-functions `(gatsby>>insert-tab-if-no-compliaton-is-found ,@completion-at-point-functions))))

  :evil-bind
  ((:maps insert)
   ("<tab>" . #'completion-at-point)
   (:maps corfu-map)
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

(gatsby>use-internal-pacakge eglot
  :custom-face
  (eglot-inlay-hint-face ((t (:height 1.0))))
  (eglot-type-hint-face ((t (:height 1.0))))
  (eglot-parameter-hint-face ((t (:height 1.0))))
  :custom
  ;; (eglot-server-programs nil)
  (eglot-extend-to-xref t)
  :init
  (setq eglot-server-programs nil)
  (defun gatsby>>maybe-format-buffer ()
    (add-hook 'before-save-hook (defun gatsby>>maybe-format-before-save (&rest _)
                                  (when (and (eglot-current-server)
                                             (eglot-server-capable :documentFormattingProvider))
                                    (eglot-format-buffer)))
              nil
              t))
  :hook
  (eglot-managed-mode . gatsby>>maybe-format-buffer)
  (eglot-managed-mode . gatsby>>set-capf-order)
  :evil-bind
  ((:maps (normal visual))
   ("SPC r a" . #'eglot-code-actions)))

(use-package consult-eglot
  :ensure (:host github :repo "mohkale/consult-eglot")
  :config
  (gatsby>defcommand gatsby>consult-symbols-or-line (prefix)
    "Run `consult-lsp-file-symbols' if PREFIX, or `consult-line'."
    (if prefix
        (call-interactively #'consult-eglot-symbols)
      (call-interactively #'consult-outline)))

  :evil-bind
  ((:maps (motion normal))
   ([remap consult-outline] . #'gatsby>consult-symbols-or-line)))

;; template system
;; (use-package tempel
;;   :ensure (:host github :repo "minad/tempel")
;;   :custom (tempel-path (no-littering-expand-etc-file-name "tempel/*.eld"))
;;   :evil-bind
;;   ((:maps tempel-map)
;;    ("C-g" . #'tempel-abort)
;;    ("M-j" . #'tempel-next)
;;    ("M-k" . #'tempel-prev))
;;   :hook (corfu-mode . gatsby>>enable-tempel)
;;   :commands (tempel--templates)
;;   :init
;;   ;; TODO: how to make sure that this is always the first in the `completion-at-point-functions' list?
;;   (defun gatsby>>enable-tempel ()
;;     (setq-local completion-at-point-functions `(gatsby>tempel-capf ,@completion-at-point-functions)))

;;   (defun gatsby>tempel-capf ()
;;     "CAPF that only complete the snippet names (and do not expand). If there's exact match, expand"
;;     (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
;;       (let* ((name (buffer-substring-no-properties
;;                     (car bounds) (cdr bounds)))
;;              (sym (intern-soft name))
;;              (templates (tempel--templates)))
;;         (if-let ((template (assq sym templates)))
;;             ;; exact match - expand
;;             (list (car bounds) (cdr bounds) (list template)
;;                   :category 'tempel
;;                   :exclusive 'no
;;                   :exit-function (apply-partially #'tempel--exit templates nil))
;;           (let ((template-names (mapcar #'car templates)))
;;             (list (car bounds) (cdr bounds) template-names
;;                   :category 'templ
;;                   :company-kind (lambda (_) 'snippet)
;;                   :exclusive 'no
;;                   :company-doc-buffer
;;                   (apply-partially #'tempel--info-buffer templates
;;                                    (lambda (elts)
;;                                      (insert (tempel--print-template elts))
;;                                      (tempel--insert-doc elts)
;;                                      (current-buffer)))
;;                   :company-location
;;                   (apply-partially #'tempel--info-buffer templates
;;                                    (lambda (elts)
;;                                      (pp (cl-loop for x in elts
;;                                                   until (keywordp x) collect x)
;;                                          (current-buffer))
;;                                      (tempel--insert-doc elts)
;;                                      (list (current-buffer))))
;;                   :annotation-function
;;                   (and tempel-complete-annotation
;;                        (apply-partially #'tempel--annotate
;;                                         templates tempel-complete-annotation " ")))))))))

;; (use-package eglot-tempel
;;   :ensure (:host github :repo "fejfighter/eglot-tempel")
;;   :hook (elpaca-after-init . eglot-tempel-mode))

;; (use-package lspce
;;   ;; TODO: find a way to allow elpaca to accept `(expand-file-name ".cargo/bin/cargo" gatsby>dotfiles-repo-location)'
;;   ;; this won't work now since elpaca does
;;   ;; `emacs -Q' when building
;;   :ensure (:host github :repo "zbelial/lspce"
;;                  :files (:defaults)
;;                  :pre-build (("/User/dad/Projects/dotfiles/.cargo/bin/cargo" "build" "--release"))))

(gatsby>use-internal-pacakge xref
  :demand t
  :custom
  (xref-prompt-for-identifier nil)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :evil-bind
  ((:maps (normal visual motion))
   ("SPC r l" . #'xref-find-definitions)
   ("SPC r L" . #'xref-find-references)
   ("SPC r b" . #'xref-go-back)
   ("SPC r f" . #'xref-go-forward)))

;; ;; TODO: until I figure out a way to install emacs-lsp-booster reliably
;; (use-package eglot-booster
;;  :ensure (:host github :repo "jdtsmith/eglot-booster")
;;  :custom (eglot-booster-io-only (>= emacs-major-version 30))
;;   :hook (elpaca-after-init . eglot-booster-mode))

;; (add-to-list 'eglot-server-programs
;;              `(python-ts-mode . ,(eglot-alternatives
;;                 '("pylsp" "pyls" ("basedpyright-langserver" "--stdio")
;;                   ("pyright-langserver" "--stdio")
;;                   ("pyrefly" "lsp")
;;                   "jedi-language-server" ("ruff" "server") "ruff-lsp"))
;;              ))

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
    (declare
     (indent defun))
    `(gatsby>defcommand ,fn ()
       (let* ((frame
               (with-current-buffer (get-buffer-create eldoc-mouse-posframe-buffer-name)
                 posframe--frame))
              (window
               (get-buffer-window
                eldoc-mouse-posframe-buffer-name
                frame)))
         (with-selected-window window
           ,form))))

  (gatsby>>eldoc-transiant-command gatsby>eldoc-scroll-up
    (scroll-down (max 1 (/ (1- (window-height (selected-window))) 2))))

  (gatsby>>eldoc-transiant-command gatsby>eldoc-scroll-down
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
                  (set-transient-map
                   gatsby>eldoc-transiant-map t #'eldoc-mouse--hide-posframe))))

  (defun gatsby>>eldoc-disable-transiant-map (&rest _)
    (with-current-buffer (get-buffer-create eldoc-mouse-posframe-buffer-name)
      (let ((fn gatsby>>eldoc-restore-keymap-fn))
        (setq-local gatsby>>eldoc-restore-keymap-fn nil)
        (when (functionp fn)
         (funcall fn)))))

  (advice-add #'eldoc-mouse--pop-doc :after #'gatsby>>eldoc-enable-transiant-map)
  (advice-add #'eldoc-mouse--hide-posframe :after #'gatsby>>eldoc-disable-transiant-map)

  :evil-bind
  ((:maps (normal visual motion))
   ("SPC r h" . #'gatsby>eldoc-pop)))

(provide 'gatsby>lsp)
;;; gatsby>lsp.el ends here
