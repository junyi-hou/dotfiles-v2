;;; gatsby>lsp.el --- everything related to lsp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(eval-when-compile
  (require 'cl-lib))

;; completion
(use-package corfu
  :ensure (:host github :repo "emacs-straight/corfu" :files ("*" "extensions/*.el" (:exclude ".git")))
  :custom
  (corfu-cycle t)
  (tab-always-indent 'complete)
  (corfu-quit-at-boundary nil) ;; don't quit when hitting non-word boundary like SPC
  (corfu-quit-no-match 'separator) ;; optionally allow typing unmatched input
  :hook (prog-mode . corfu-mode)
  :config
  (gatsby>defcommand gatsby>corfu-complete ()
    "Complete common parts of all the candidates, or insert the current selection.
Insert the current selection when
1. there is only one candidate;
2. last command is `gatsby>corfu-complete';
3. last command is `corfu-next'or `corfu-previous'."
    (if (or (= (length corfu--candidates) 1)
            (memq last-command '(gatsby>corfu-complete corfu-next corfu-previous)))
        (corfu-insert)
      (corfu-expand)))

  :general
  (:keymaps 'corfu-map
   "<tab>" #'gatsby>corfu-complete
   "M-j" #'corfu-next
   "M-k" #'corfu-previous
	 "SPC" #'corfu-insert-separator))

(use-package orderless
  :ensure (:host github :repo "oantolin/orderless")
  :custom
	(orderless-matching-styles '(orderless-prefixes orderless-literal orderless-regexp))
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(gatsby>use-internal-pacakge eglot
	:custom-face
	(eglot-inlay-hint-face ((t (:height 1.0))))
	(eglot-type-hint-face ((t (:height 1.0))))
	(eglot-parameter-hint-face ((t (:height 1.0))))
	:custom
	(eglot-server-programs nil))

(use-package eglot-booster
	:ensure (:host github :repo "jdtsmith/eglot-booster")
	:hook (elpaca-after-init . eglot-booster-mode)
	:custom
	(eglot-booster--boost `(,(expand-file-name (concat gatsby>dotfiles-repo-location ".tools/bin/emacs-lsp-booster")) "--json-false-value" ":json-false" "--"))
	(eglot-booster--boost-io-only `(,(expand-file-name (concat gatsby>dotfiles-repo-location ".tools/bin/emacs-lsp-booster")) "--disable-bytecode" "--"))
	(eglot-booster-io-only (>= emacs-major-version 30))
	:config
  (defun gatsby>>do-not-check-emacs-lsp-booster-executable (func &rest args)
    (cl-letf (((symbol-function #'executable-find) (lambda (&rest _) t)))
      (apply func args)))
	(advice-add #'eglot-booster-mode :around #'gatsby>>do-not-check-emacs-lsp-booster-executable))

;; display flymake information in a childframe
(use-package flymake-childframe
	:ensure (:host github :repo "junyi-hou/flymake-childframe")
  :custom (flymake-childframe-prefix '((note . "i") (warning . "?") (error . "!")))
	:hook ((flymake eglot-managed-mode) . flymake-childframe-mode))

(use-package eldoc-mouse
  :ensure (:host github :repo "huangfeiyu/eldoc-mouse")
  :general
  (:states 'normal :prefix "SPC"
           "rh" #'eldoc-mouse-pop-doc-at-cursor))

(gatsby>use-internal-pacakge xref
  :config
  ;; use consult
  (with-eval-after-load 'consult
    (setq xref-show-definitions-function #'consult-xref
          xref-show-xrefs-function #'consult-xref)
    (consult-customize
     consult-xref
     :preview-key 'any))
  
  :general
  (:states 'normal :prefix "SPC"
           "rl" #'xref-find-definitions
           "rL" #'xref-find-references
           "rb" #'xref-go-back
           "rf" #'xref-go-forward))

(provide 'gatsby>lsp)
;;; gatsby>lsp.el ends here
