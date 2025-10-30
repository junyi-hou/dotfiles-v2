;;; gatsby>config-files.el --- major modes for config files like yaml/toml/json -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(gatsby>use-internal-pacakge yaml-ts-mode
	:mode ("\\.ya?ml\\'" . yaml-ts-mode)
  :init

	;; tree-sitter
  (defun gatsby>>yaml-set-indent-width (&rest _)
    (setq-local tab-width 2))
	(gatsby>install-treesitter-grammar 'yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.0")
	(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

	;; lsp
	;; TODO - use local yaml-language-server
	(gatsby>defcommand gatsby>start-yamlls ()
		(let ((eglot-server-programs '((yaml-ts-mode "yaml-lanugage-server" "--stdio"))))
			(eglot-ensure)))
	
  :hook
  (yaml-ts-mode . display-line-numbers-mode)  ;; turn on line-number
  (yaml-ts-mode . indent-bars-mode)
  (yaml-ts-mode . gatsby>>yaml-set-indent-width))

(gatsby>use-internal-pacakge json-ts-mode
  :mode ("\\.json\\'" . json-ts-mode)
  :init
  (defun gatsby>>json-add-formatting-hook ()
    (add-hook 'before-save-hook #'json-pretty-print-buffer nil t))

	(gatsby>install-treesitter-grammar 'json "https://github.com/tree-sitter/tree-sitter-json" "v0.24.8")
  :hook
  (json-ts-mode . gatsby>>json-add-formatting-hook))

;; protofub-ts-mode upstream is down as of 2025-10-13
;; (use-package protobuf-ts-mode
;;   :ensure (:host github :repo "/protobuf-ts")
;;   :mode ("\\.proto\\'" . protobuf-ts-mode)
;;   :init
;;   ;; use the treywood fork of the protobuf syntex
;; 	(gatsby>install-treesitter-grammar 'proto "https://github.com/mitchellh/tree-sitter-proto"))

(gatsby>use-internal-pacakge toml-ts-mode
	:mode ("\\.toml\\'" . toml-ts-mode)
	:init
	(gatsby>install-treesitter-grammar 'toml "https://github.com/tree-sitter/tree-sitter-toml")
  (add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode)))

(provide 'gatsby>config-files)
;;; gatsby>config-files.el ends here
