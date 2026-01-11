;;; gatsby>text.el --- specific config for editing text files -*- lexical-binding: t; -*-

;;; Commentary:

;; Code:
(require 'gatsby>>utility)

(use-package markdown-mode
  :ensure (:host github :repo "jrblevin/markdown-mode"))

(use-package markdown-ts-mode
  :ensure (:host github :repo "LionyxML/markdown-ts-mode")
  :mode ("\\.md\\'" . markdown-ts-mode)
  :hook
  (markdown-ts-mode . eglot-ensure)
  (markdown-ts-mode . corfu-mode)
  :after markdown-mode
  :init
  ;; treesitter
  (gatsby>install-treesitter-grammar
   'markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
   "v0.4.1" "tree-sitter-markdown/src")
  (gatsby>install-treesitter-grammar
   'markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
   "v0.4.1" "tree-sitter-markdown-inline/src")

  ;; lsp
  ;; TODO: this breaks capf
  ;; (with-eval-after-load 'eglot
  ;;   (add-to-list 'eglot-server-programs '(markdown-ts-mode "markdown-oxide")))
  :config
  ;; this provides useful commands like `markdown-insert-link'
  (require 'markdown-mode))

(use-package typst-ts-mode
  :ensure (:host sourcehut :repo "meow_king/typst-ts-mode")
  :defer t
  :custom-face
  (typst-ts-markup-raw-blob-face ((t :inherit 'default)))
  (typst-ts-markup-rawspan-lang-face ((t :inherit 'default :weight bold)))
  :hook
  (typst-ts-mode . eglot-ensure)
  (typst-ts-mode . corfu-mode)
  (typst-ts-mode . display-line-numbers-mode)
  (typst-ts-mode . typst-ts-watch-mode)
  (typst-ts-mode . gatsby>>typst-stop-watching)
  :init
  ;; tree-sitter
  (gatsby>install-treesitter-grammar 'typst "https://github.com/uben0/tree-sitter-typst"
                                     "v0.11.0")

  ;; lsp
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(typst-ts-mode "tinymist" "lsp")))

  :config
  (defun gatsby>>typst-stop-watching (&rest _)
    (add-hook 'kill-buffer-hook #'typst-ts-watch-stop nil t))

  :evil-bind
  ((:maps typst-ts-mode-map :states normal)
   ("SPC r r" . #'typst-ts-compile-and-preview)
   ("SPC r o" . #'typst-ts-preview)))

(gatsby>use-internal-package org
  :defer t
  :hook (org-mode . corfu-mode)
  :config
  (defun gatsby>org-show-block (&optional element)
    "Open folded elements at point."
    (interactive)
    (let* ((element (or element (org-element-at-point)))
           (element-type (org-element-type element)))
      ;; (message (format "current element type is %s" element-type))
      (cond
       ((eq element-type 'org-data)
        (message "No foldable element at point"))
       ((memq
         element-type
         '(src-block special-block example-block export-block dynamic-block))
        (save-excursion
          (goto-char (org-element-begin element))
          (org-fold-hide-block-toggle 'off)))
       ((memq element-type '(drawer property-drawer))
        (save-excursion
          (goto-char (org-element-begin element))
          (org-fold-hide-drawer-toggle 'off)))
       ((memq element-type '(section headline))
        (evil-open-fold))
       (t
        (gatsby>org-show-block (org-element-parent element))))))

  (defun gatsby>org-hide-block (&optional element)
    "Hide/fold elements at point."
    (interactive)
    (let* ((element (or element (org-element-at-point)))
           (element-type (org-element-type element)))
      (message "current element is a %s" element-type)
      (cond
       ((eq element-type 'org-data)
        (message "No foldable element at point"))
       ((memq
         element-type
         '(src-block special-block example-block export-block dynamic-block))
        (save-excursion
          (goto-char (org-element-begin element))
          (org-fold-hide-block-toggle t)))
       ((memq element-type '(drawer property-drawer))
        (save-excursion
          (goto-char (org-element-begin element))
          (org-fold-hide-drawer-toggle t)))
       ((memq element-type '(section headline))
        (evil-close-fold))
       (t
        (gatsby>org-hide-block (org-element-parent element))))))

  :evil-bind
  ((:maps org-mode-map :states (normal visual))
   ("z o" . #'gatsby>org-show-block)
   ("z c" . #'gatsby>org-hide-block)
   ("<" . #'org-previous-visible-heading)
   (">" . #'org-next-visible-heading)))

(provide 'gatsby>text)
;;; gatsby>text.el ends here
