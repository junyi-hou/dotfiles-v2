;;; gatsby-scala.el --- configuration for scala -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package scala-ts-mode
  :ensure (:host github :repo "KaranAhlawat/scala-ts-mode")
  :init
  (gatsby>install-treesitter-grammar
   'scala "https://github.com/tree-sitter/tree-sitter-scala"
   ;; v0.25.0 regenerates the parser with tree-sitter ABI 15, which is not
   ;; accepted by Ubuntu's older packaged tree-sitter/Emacs builds.
   "v0.24.1"))

(provide 'gatsby-scala)
;;; gatsby-scala.el ends here
