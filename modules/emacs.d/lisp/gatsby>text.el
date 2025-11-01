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

(provide 'gatsby>text)
;;; gatsby>text.el ends here
