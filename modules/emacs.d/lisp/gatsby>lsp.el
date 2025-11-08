;;; gatsby>lsp.el --- everything related to lsp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

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

  :evil-bind
  ((:maps corfu-map)
   ("<tab>" . #'gatsby>corfu-complete)
   ("M-j" . #'corfu-next)
   ("M-k" . #'corfu-previous)
   ("SPC" . #'corfu-insert-separator)))

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

;; TODO: xref-* functions are not autoloaded?!
(gatsby>use-internal-pacakge xref
  :demand t
  :custom
  (xref-prompt-for-identifier nil)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :evil-bind
  ((:maps (normal visual motion))
   ("SPC r l" . #'xref-find-definitions)
   ("SPC r L" . #'xref-find-references)
   ("SPC r b" . #'xref-go-back)
   ("SPC r f" . #'xref-go-forward)))

;; ;; TODO: until I figure out a way to install emacs-lsp-booster reliably
;; (use-package eglot-booster
;;  :ensure (:host github :repo "jdtsmith/eglot-booster")
;;  :custom (eglot-booster-io-only (>= emacs-major-version 30))
;;   :hook (elpaca-after-init . eglot-booster-mode))

;; display flymake information in a childframe
(use-package flymake-childframe
  :ensure (:host github :repo "junyi-hou/flymake-childframe")
  :custom (flymake-childframe-prefix '((note . "i") (warning . "?") (error . "!")))
  :hook ((flymake eglot-managed-mode) . flymake-childframe-mode))

(use-package eldoc-mouse
  :ensure (:host github :repo "huangfeiyu/eldoc-mouse")
  :config
  (gatsby>defcommand gatsby>eldoc-pop ()
    "Use alternative poshandler, since the package did not provide this option."
    (cl-letf (((symbol-function #'posframe-poshandler-point-bottom-left-corner-upward)
               #'posframe-poshandler-point-bottom-left-corner))
      (call-interactively #'eldoc-mouse-pop-doc-at-cursor)))

  ;; TODO: support transiant map
  :evil-bind
  ((:maps (normal visual motion))
   ("SPC r h" . #'gatsby>eldoc-pop)))

(provide 'gatsby>lsp)
;;; gatsby>lsp.el ends here
