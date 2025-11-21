;;; gatsby>clojure.el --- clojure specific config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby>>utility)

(use-package clojure-ts-mode
  :ensure (:host github :repo "clojure-emacs/clojure-ts-mode")
  :hook
  (clojure-ts-mode . eglot-ensure)
  (clojure-ts-mode . gatsby>jupyter-managed-mode)
  :init
  ;; do not need to setup treesitter - it is taken care of by the mode itself

  ;; lsp
  (add-to-list 'eglot-server-programs (clojure-ts-mode "clojure-lsp" "listen"))
  :config
  ;; TODO automatically sends `(require [ns.module] :reload)` to the repl after each save?
  t
  )


(provide 'gatsby>clojure)
;;; gatsby>clojure.el ends here
