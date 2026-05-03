;;; gatsby>config-files.el --- major modes for config files like yaml/toml/json -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby>>utility)

(gatsby>use-internal-package yaml-ts-mode
  :mode ("\\.ya?ml\\'" . yaml-ts-mode)
  :init

  ;; tree-sitter
  (defun gatsby>>yaml-set-indent-width (&rest _)
    (setq-local tab-width 2))
  (gatsby>install-treesitter-grammar
   'yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
   "v0.7.0")
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

  ;; lsp
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     `(yaml-ts-mode
       "direnv"
       "exec"
       ,(expand-file-name gatsby>dotfiles-repo-location)
       "pixi"
       "run"
       "-e"
       "all"
       "yaml-language-server"
       "--stdio")))

  :hook
  (yaml-ts-mode . display-line-numbers-mode) ;; turn on line-number
  (yaml-ts-mode . indent-bars-mode)
  (yaml-ts-mode . eglot-ensure)
  (yaml-ts-mode . gatsby>>yaml-set-indent-width))

(gatsby>use-internal-package json-ts-mode
  :mode ("\\.json\\'" . json-ts-mode)
  :init
  (gatsby>install-treesitter-grammar
   'json "https://github.com/tree-sitter/tree-sitter-json"
   "v0.24.8")
  :hook
  (json-ts-mode . gatsby>>json-add-formatting-hook)
  (json-ts-mode . display-line-numbers-mode)
  :commands (json-pretty-print-buffer)
  :config
  (defun gatsby>>json-add-formatting-hook ()
    (add-hook 'before-save-hook #'json-pretty-print-buffer nil t)))

(gatsby>use-internal-package toml-ts-mode
  :mode ("\\.toml\\'" . toml-ts-mode)
  :init
  (gatsby>install-treesitter-grammar
   'toml "https://github.com/tree-sitter/tree-sitter-toml")
  (add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))

  ;; lsp
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(toml-ts-mode "taplo" "lsp" "stdio")))
  :hook
  (toml-ts-mode . eglot-ensure)
  (toml-ts-mode . display-line-numbers-mode))

(provide 'gatsby>config-files)
;;; gatsby>config-files.el ends here
