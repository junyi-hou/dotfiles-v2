;;; gatsby>ui.el --- configuration for UI -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby>>utility)

;; theme
(use-package doom-themes
  :ensure (:host github :repo "doomemacs/themes")
  :demand t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-material-dark t)

  ;; default UI
  (line-number-mode -1)
  (column-number-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (menu-bar-mode (if (eq system-type 'darwin) 1 -1))
  (unless (frame-parameter nil 'fullscreen)
    (set-frame-parameter nil 'fullscreen 'maximized))

  ;; setup ligature if support
  (when (and (eq system-type 'darwin)
             (fboundp #'mac-auto-operator-composition-mode))
    (mac-auto-operator-composition-mode))

  ;; font
  (set-face-attribute
   'default nil
   :family "PragmataPro Liga"
   :height 130
   :width 'normal
   :weight 'Regular)

  (gatsby>defcommand gatsby>theme-fontsize-up ()
    "Increase the font size in the current frame."
    (let* ((current-size (plist-get (custom-face-attributes-get 'default nil) :height))
           (new-size (+ 10 current-size)))
      (set-face-attribute
       'default (selected-frame)
       :height new-size)))

  (gatsby>defcommand gatsby>theme-fontsize-down ()
    "Decrease the font size in the current frame."
    (interactive)
    (let* ((current-size (plist-get (custom-face-attributes-get 'default nil) :height))
           (new-size (- current-size 10)))
      (set-face-attribute
       'default (selected-frame)
       :height new-size)))

  ;; use mode line color to indicate mode
  (with-eval-after-load 'evil
    (defun gatsby>>set-evil-mode-line-color (&rest _)
      (let ((color (cond ((minibufferp) (face-background 'mode-line))
                         ((evil-normal-state-p) (doom-color 'blue))
                         ((evil-motion-state-p) (doom-color 'blue))
                         ((evil-visual-state-p) (doom-color 'yellow))
                         ((evil-insert-state-p) (doom-color 'red))
                         ((evil-emacs-state-p) (doom-color 'green))
                         ((evil-operator-state-p) (doom-color 'magenta))
                         ((evil-replace-state-p) (doom-color 'magenta)))))
        (set-face-background 'mode-line color)
        (set-face-foreground 'mode-line (doom-color 'bg))))

    (mapc (lambda (hook) (add-hook hook #'gatsby>>set-evil-mode-line-color))
          '(evil-emacs-state-entry-hook
            evil-normal-state-entry-hook
            evil-motion-state-entry-hook
            evil-visual-state-entry-hook
            evil-insert-state-entry-hook
            evil-operator-state-entry-hook
            evil-replace-state-entry-hook
            evil-emacs-state-exit-hook
            evil-normal-state-exit-hook
            evil-motion-state-exit-hook
            evil-visual-state-exit-hook
            evil-insert-state-exit-hook
            evil-operator-state-exit-hook
            evil-replace-state-exit-hook
            window-selection-change-functions)))

  :evil-bind
  ((:maps (motion normal visual emacs insert))
   ("C--" . #'gatsby>theme-fontsize-down)
   ("C-+" . #'gatsby>theme-fontsize-up)))

(use-package indent-bars
  :ensure (:host github :repo "jdtsmith/indent-bars")
  :hook
  (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters
                                      list list_comprehension
                                      dictionary dictionary_comprehension
                                      parenthesized_expression subscript)))
  (indent-bars-no-descend-string t)
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-width-frac 0.25)
  (indent-bars-pad-frac 0.1)
  (indent-bars-color-by-depth nil)
  (indent-bars-display-on-blank-lines nil))

(gatsby>use-internal-pacakge paren
  :hook (elpaca-after-init . show-paren-mode))

(gatsby>use-internal-pacakge display-line-numbers
  :custom
  (display-line-numbers-width-start t)
  (display-line-numbers-type 'visual)
  :hook
  (prog-mode . gatsby>set-line-numbers-width)
  (prog-mode . display-line-numbers-mode)
  :config
  (set-face-attribute 'line-number nil :background (face-background 'default))

  (defun gatsby>set-line-numbers-width ()
    "Set line number width based on total lines in buffer."
    (when display-line-numbers-mode
      (setq-local display-line-numbers-width
                  (+ 1 (length (number-to-string (count-lines (point-min) (point-max)))))))))

(use-package hl-todo
  :ensure (:host github :repo "tarsius/hl-todo")
  :hook (elpaca-after-init . global-hl-todo-mode)
  :custom
  (hl-todo-keyword-faces `(("TODO" . ,(doom-color 'red))
                           ("FIXME"  . ,(doom-color 'red))
                           ("NOTE"   . ,(doom-color 'yellow))
                           ("HACK"   . ,(doom-color 'yellow)))))

(use-package auto-dim-other-buffers
  :ensure (:host github :repo "mina86/auto-dim-other-buffers.el")
  :hook (elpaca-after-init . auto-dim-other-buffers-mode)
  :custom
  (auto-dim-other-buffers-affected-faces
  '((default            . (auto-dim-other-buffers . nil))
    (frige              . (auto-dim-other-buffers . nil))
    (mode-line-inactive . (auto-dim-other-buffers . nil))
    (line-number        . (auto-dim-other-buffers . nil))))
  :custom-face (auto-dim-other-buffers ((t :background ,(doom-color 'bg-alt)))))

;; minibuffer
(use-package vertico
  :ensure (vertico :host github :repo "minad/vertico")
  :demand t
  :hook (elpaca-after-init . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 10)
  (enable-recursive-minibuffers t)
  :config
  (defun gatsby>>vertico-remove-base (dir)
    "Insert the string of trimming the base of DIR into the minibuffer."
    (delete-minibuffer-contents)
    (insert (string-trim-right (if (string= dir "~/") (expand-file-name "~/") dir) "[^/]+/?")))

  (defun gatsby>vertico-better-backspace ()
    "If `point' is at \"/\", delete till the last \"/\"."
    (interactive)
    (if (thing-at-point-looking-at "/")
        (let ((dir (minibuffer-contents-no-properties)))
          (gatsby>>vertico-remove-base dir))
      (call-interactively #'backward-delete-char)))

  (defun gatsby>>vertico-select-current-candidate-if-not-dir (&optional arg)
    "Select the current candidate. If, however, the current selection is a directory,
enter the directory instead of opening it using `dired'. With ARG, always select the
current candidate"
    (interactive "P")
    (cond (arg (vertico-exit))
          ((directory-name-p (vertico--candidate)) (vertico-insert))
          (t (vertico-exit))))

  (defun gatsby>vertico-complete-common-or-select ()
    "<tab> does the following things
  1. if there is only one candidate, select the candidate;
  2. if there is a common part among candidates, complete the common part;
  3. if the last command is this command or selecting different candidates, it is
  virtually selecting the current candidate, so exit with it."
    (interactive)
    (if (= vertico--total 1)
        (gatsby>>vertico-select-current-candidate-if-not-dir)
      (let* ((common (try-completion "" (if (> (length vertico--candidates) vertico-count)
                                            (seq-subseq vertico--candidates 1 vertico-count)
                                          vertico--candidates))))

        (cond (;; virtual selection is done, selecting the candidate
               (memq last-command '(gatsby>vertico-complete-common-or-select vertico-next vertico-previous))
               (gatsby>>vertico-select-current-candidate-if-not-dir))
              (;; has common part
               (> (length common) 0)
               (let*  ((input (minibuffer-contents-no-properties))
                       (dir (if (directory-name-p input) input (file-name-directory input))))
                 (delete-minibuffer-contents)
                 (if dir
                     (insert (format "%s%s" dir common))
                   (insert common))))
              (;; one of the 3 conditions is satisfied, do nothing
               t nil)))))
  :evil-bind
  ((:maps vertico-map)
   ("M-j" . #'vertico-next)
   ("M-k" . #'vertico-previous)
   ("C-h" . #'windmove-left)
   ("C-j" . #'windmove-down)
   ("C-k" . #'windmove-up)
   ("C-l" . #'windmove-right)
   ("<backspace>" . #'gatsby>vertico-better-backspace)
   ("<tab>" . #'gatsby>vertico-complete-common-or-select)

   (:maps minibuffer-mode-map)
   ("C-k" . #'windmove-up)))


(provide 'gatsby>ui)
;;; gatsby>ui.el ends here
