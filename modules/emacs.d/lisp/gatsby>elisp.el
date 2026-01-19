;;; gatsby>elisp.el --- elisp setting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(gatsby>use-internal-package elisp-mode
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
  :config (add-to-list 'evil-motion-state-modes 'helpful-mode)
  :evil-bind
  ((:maps helpful-mode-map :states motion)
   ("SPC q" . #'kill-buffer-and-window)
   ("SPC e l" . #'eval-last-sexp)
   (:maps (motion normal visual))
   ("SPC h" . nil)
   ("SPC h f" . #'helpful-callable)
   ("SPC h k" . #'helpful-key)
   ("SPC h v" . #'helpful-variable)))

(use-package elisp-autofmt
  :ensure (:host codeberg :repo "ideasman42/emacs-elisp-autofmt")
  :hook (emacs-lisp-mode . elisp-autofmt-mode)
  :custom
  (elisp-autofmt-python-bin
   (format "%s.pixi/envs/bin/python" gatsby>dotfiles-repo-location)))

(provide 'gatsby>elisp)
;;; gatsby>elisp.el ends here
