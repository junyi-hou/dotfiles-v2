;;; gatsby>sh-scripts.el --- major mode for editing shell scripts -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require gatsby>>utility)

(gatsby>use-internal-pacakge sh-script
  :mode ("\\.[z]sh" . bash-ts-mode)
  :init
  (gatsby>install-treesitter-grammar 'bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))

(provide 'gatsby>sh-scripts)
;;; gatsby>sh-scripts.el ends here
