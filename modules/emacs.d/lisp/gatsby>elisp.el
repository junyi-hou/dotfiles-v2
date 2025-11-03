;;; gatsby>elisp.el --- elisp setting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(gatsby>use-internal-pacakge elisp-mode
  :init
  (defun gatsby>>lisp-set-tab-width (&rest _)
    (setq-local tab-width 2))
  :hook (emacs-lisp-mode . gatsby>>lisp-set-tab-width)

  :general
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map) :states '(motion normal visual) :prefix "SPC"
            "el" #'eval-last-sexp
            "eL" #'eval-buffer))

(use-package helpful
  :ensure (:host github :repo "Wilfred/helpful")
  :custom (helpful-switch-buffer-function #'gatsby>switch-to-buffer-new-window)
  :config
  (add-to-list 'evil-motion-state-modes 'helpful-mode)
  :general
  (:keymaps 'helpful-mode-map :states 'motion :prefix "SPC"
            "q" #'kill-buffer-and-window
            "el" #'eval-last-sexp
            "eL" #'eval-buffer)

  (:keymaps '(motion normal visual) :prefix "SPC"
            "hf" #'helpful-callable
            "hk" #'helpful-key
            "hv" #'helpful-variable))


(provide 'gatsby>elisp)
;;; gatsby>elisp.el ends here
