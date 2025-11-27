;;; gatsby>clojure.el --- clojure specific config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby>>utility)

(use-package clojure-ts-mode
  :ensure (:host github :repo "clojure-emacs/clojure-ts-mode")
  :mode
  ("\\.clj\\'" . clojure-ts-mode)
  ("\\.edn\\'" . clojure-ts-mode)
  :hook
  (clojure-ts-mode . eglot-ensure)
  (clojure-ts-mode . gatsby>comint-managed-mode)
  (clojure-ts-mode . gatsby>>clojure-setup-repl)
  :init
  ;; do not need to setup treesitter - it is taken care of by the mode itself

  ;; lsp
  (with-eval-after-load 'eglot
   (add-to-list 'eglot-server-programs '(clojure-ts-mode "clojure-lsp" "listen")))

  (defun gatsby>>clojure-setup-repl ()
    (setq-local gatsby>comint-command "clj")
    (setq-local comment-start ";;")))

(provide 'gatsby>clojure)
;;; gatsby>clojure.el ends here
