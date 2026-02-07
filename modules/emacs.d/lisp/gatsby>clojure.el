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
  (clojure-ts-mode . gatsby>jupyter-managed-mode)
  (clojure-ts-mode . gatsby>>clojure-setup-local-var)
  :init
  ;; do not need to setup treesitter - it is taken care of by the mode itself
  ;; lsp
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(clojure-ts-mode "clojure-lsp" "listen")))

  (defun gatsby>>clojure-setup-local-var ()
    (setq-local gatsby>comint-command "clj")
    (setq-local comment-start ";;")
    (setq-local tab-width 2))

  :config
  (gatsby>defcommand gatsby>clojure-run-test (all)
    (save-excursion
      (let ((default-directory
             (or (and (project-current) (project-root (project-current)))
                 default-directory))
            ;; automatically kill buffer if the test succeed
            ;; (compilation-exit-message-function
            ;;  (lambda (status code msg)
            ;;    (when (and (eq status 'exit) (zerop code))
            ;;      (kill-buffer))
            ;;    (cons msg code)))
            )
        (if all
            (progn
              (message "running tests...")
              (compile "clj -T:build test"))
          (let ((test-to-run
                 (thread-last
                  (completing-read
                   "Run test: " (directory-files-recursively "test" ".+_test\\.clj"))
                  (replace-regexp-in-string "\\.clj$" "")
                  (replace-regexp-in-string "^test/" "")
                  (replace-regexp-in-string "/" ".")
                  (replace-regexp-in-string "_" "-"))))
            (message "running tests...")
            (compile
             (concat "clj -M:test -m cognitect.test-runner -n " test-to-run)))))))
  :evil-bind ((:maps clojure-ts-mode-map :states normal) ("SPC r t" . #'gatsby>clojure-run-test)))

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
  :ensure (:type git :repo "https://git.sr.ht/~dannyfreeman/jarchive")
  :hook (elpaca-after-init . jarchive-mode))

(provide 'gatsby>clojure)
;;; gatsby>clojure.el ends here
