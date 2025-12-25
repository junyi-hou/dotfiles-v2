;;; gatsby>clojure.el --- clojure specific config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(use-package clojure-ts-mode
  :ensure (:host github
           :repo "clojure-emacs/clojure-ts-mode")
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
    (add-to-list 'eglot-server-programs '(clojure-ts-mode "rass" "clojure")))
  (defun gatsby>>clojure-setup-repl ()
    (setq-local gatsby>comint-command "clj")
    (setq-local comment-start ";;"))
  :config
  (gatsby>defcommand gatsby>clojure-run-test ()
    (let ((default-directory (or
                              (and (project-current)
                                   (project-root (project-current)))
                              default-directory)))
      (message "running tests...")
      ;; assume the project is initialized using `neil new`
      (compile "clj -T:build test")))
  :evil-bind
  ((:maps clojure-ts-mode-map
    :states normal)
   ("SPC r t" . #'gatsby>clojure-run-test)))

;; TODO: use this or cider for better completion, etc to have better completion/doc supports
;; (use-package inf-clojure
;;   :ensure (:host github :repo "clojure-emacs/inf-clojure")
;;   :custom
;;   (inf-clojure-custom-startup "clojure -M:compliment")
;;   (inf-clojure-custom-repl-type 'clojure)
;;   :init
;;   (inf-clojure-update-feature 'clojure 'completion "(compliment.core/completions \"%s\")")
;;   )

;; This enables LSP to check codes in jar file
(use-package jarchive
  :ensure (:type git
           :repo "https://git.sr.ht/~dannyfreeman/jarchive")
  :hook (elpaca-after-init . jarchive-mode))

(provide 'gatsby>clojure)
;;; gatsby>clojure.el ends here
