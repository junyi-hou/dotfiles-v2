;;; gatsby>text.el --- specific config for editing text files -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby>>utility)

(use-package markdown-ts-mode
  :ensure (:host github :repo "LionyxML/markdown-ts-mode")
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer t
  :init
  ;; treesitter
  (gatsby>install-treesitter-grammar
   'markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.4.1" "tree-sitter-markdown/src")
  (gatsby>install-treesitter-grammar 'markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.4.1" "tree-sitter-markdown-inline/src")

  ;; lsp
  
  )

(use-package typst-ts-mode
  :ensure (:host sourcehut :repo "meow_king/typst-ts-mode")
  :custom-face
  (typst-ts-markup-raw-blob-face ((t :inherit 'default)))
  (typst-ts-markup-rawspan-lang-face ((t :inherit 'default :weight bold)))
  :init
  (gatsby>install-treesitter-grammar
   'typst "https://github.com/uben0/tree-sitter-typst" "v0.11.0")
  :hook (typst-ts-mode . display-line-numbers-mode)
  :evil-bind
  ((:maps typst-ts-mode-map :states insert)
   ("SPC r r" . #'typst-ts-compile-and-preview)
   ("SPC r o" . #'typst-ts-preview)))

(provide 'gatsby>text)
;;; gatsby>text.el ends here
