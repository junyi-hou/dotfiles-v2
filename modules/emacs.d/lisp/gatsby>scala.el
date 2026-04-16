;;; gatsby>scala.el --- configuration for scala -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package scala-ts-mode
  :ensure (:host github :repo "KaranAhlawat/scala-ts-mode")
  :init
  (gatsby>install-treesitter-grammar
   'scala "https://github.com/tree-sitter/tree-sitter-scala"
   "v0.25.0"))

(provide 'gatsby>scala)
;;; gatsby>scala.el ends here
