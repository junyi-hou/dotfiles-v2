;;; gatsby>text.el --- specific config for editing text files -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby>>utility)

(use-package markdown-ts-mode
  :ensure (:host github :repo "LionyxML/markdown-ts-mode")
  :mode ("\\.md\\'" . markdown-ts-mode)
  :hook (markdown-ts-mode . eglot-ensure)
  :defer t
  :init
  ;; treesitter
  (gatsby>install-treesitter-grammar
   'markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.4.1" "tree-sitter-markdown/src")
  (gatsby>install-treesitter-grammar 'markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.4.1" "tree-sitter-markdown-inline/src")

  ;; lsp
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(markdown-ts-mode "iwes")))
  )

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
  (gatsby>install-treesitter-grammar
   'typst "https://github.com/uben0/tree-sitter-typst" "v0.11.0")

  ;; lsp
  (with-eval-after-load 'eglot
   (add-to-list 'eglot-server-programs `(typst-ts-mode "tinymist")))

  :config
  (defun gatsby>>typst-stop-watching (&rest _)
    (add-hook 'kill-buffer-hook #'typst-ts-watch-stop nil t))

  :evil-bind
  ((:maps typst-ts-mode-map :states normal)
   ("SPC r r" . #'typst-ts-compile-and-preview)
   ("SPC r o" . #'typst-ts-preview)))

(provide 'gatsby>text)
;;; gatsby>text.el ends here
