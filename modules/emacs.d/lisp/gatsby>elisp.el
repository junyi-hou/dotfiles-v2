;;; gatsby>elisp.el --- elisp setting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(gatsby>use-internal-pacakge elisp-mode
  :init
  (defun gatsby>>lisp-set-tab-width (&rest _)
    (setq-local tab-width 2))
  :hook (emacs-lisp-mode . gatsby>>lisp-set-tab-width)
  :evil-bind
  ((:maps (emacs-lisp-mode-map lisp-interaction-mode-map) :states normal)
   ("SPC e l" . #'eval-last-sexp)
   ("SPC e L" . #'eval-buffer)))

(use-package helpful
  :ensure (:host github :repo "Wilfred/helpful")
  :custom (helpful-switch-buffer-function #'gatsby>switch-to-buffer-new-window)
  :config
  (add-to-list 'evil-motion-state-modes 'helpful-mode)
  :evil-bind
  ((:maps helpful-mode-map :states motion)
   ("SPC q" . #'kill-buffer-and-window)
   ("SPC e l" . #'eval-last-sexp)

   (:maps (motion normal visual))
   ("SPC h f" . #'helpful-callable)
   ("SPC h k" . #'helpful-key)
   ("SPC h v" . #'helpful-variable)))

(use-package prettier-elisp
  :ensure (:host github :repo "KarimAziev/prettier-elisp")
  :hook (emacs-lisp-mode . prettier-elisp-buffer-mode))

(provide 'gatsby>elisp)
;;; gatsby>elisp.el ends here
