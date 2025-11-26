;;; gatsby>python.el --- python specific config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(gatsby>use-internal-pacakge python
  :mode ("\\.py\\'" . python-ts-mode)
  :custom (python-indent-offset 4)
  :hook
  (python-ts-mode . gatsby>>python-set-indent-width)
  (python-ts-mode . eglot-ensure)
  (python-ts-mode . gatsby>jupyter-managed-mode)
  :init
  (defun gatsby>>python-set-indent-width (&rest _)
    (setq-local tab-width 4))

  ;; tree-sitter
  (gatsby>install-treesitter-grammar
   'python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

  ;; lsp
  ;; (add-to-list 'eglot-server-programs '(python-ts-mode "basedpyright-langserver" "--stdio"))

  (gatsby>defcommand gatsby>start-alligator ()
    (call-interactively #'eglot-shutdown-all)
    (let* ((eglot-server-programs `((python-ts-mode ,(expand-file-name "src/main.py" (project-root (project-current)))))))
      (call-interactively #'eglot)))

  :evil-bind
  ((:maps python-ts-mode-map :states visual)
   ("<" . #'python-indent-shift-left)
   (">" . #'python-indent-shift-right)))

(provide 'gatsby>python)
;;; gatsby>python.el ends here
