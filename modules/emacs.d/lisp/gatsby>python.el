;;; gatsby>python.el --- python specific config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(gatsby>use-internal-package python
  :mode ("\\.py\\'" . python-ts-mode)
  :custom (python-indent-offset 4)
  :hook
  (python-ts-mode . gatsby>>python-set-indent-width)
  (python-ts-mode . eglot-ensure)
  (python-ts-mode . gatsby>jupyter-managed-mode)
  :init
  (defun gatsby>>python-set-indent-width (&rest _)
    (setq-local tab-width 4)
    (setq-local python-indent-offset 4))
  ;; tree-sitter
  (gatsby>install-treesitter-grammar
   'python "https://github.com/tree-sitter/tree-sitter-python"
   "v0.23.6")
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  ;; lsp
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(python-ts-mode "ty" "server")))
  :evil-bind
  ((:maps python-ts-mode-map :states visual)
   ("<" . #'python-indent-shift-left)
   (">" . #'python-indent-shift-right)))

(provide 'gatsby>python)
;;; gatsby>python.el ends here
