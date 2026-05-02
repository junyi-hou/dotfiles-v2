;;; gatsby>editing.el --- evil+ -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(use-package evil
  :ensure (:host github :repo "emacs-evil/evil")
  :demand t
  :custom
  (evil-search-module 'isearch)
  (evil-undo-system 'undo-redo)
  (evil-want-C-d-scroll t)
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-C-w-delete t)
  :hook
  (elpaca-after-init . evil-mode)
  (minibuffer-setup . evil-insert-state)
  :config

  ;; Remove evil's default SPC→evil-forward-char from motion state so all
  ;; :evil-bind blocks can freely use SPC as a prefix key.
  (define-key evil-motion-state-map (kbd "SPC") nil)

  (evil-define-motion gatsby>evil-next-three-lines ()
    "Jump to the next 3 visual line"
    (evil-next-visual-line 3))

  (evil-define-motion gatsby>evil-prev-three-lines ()
    "Jump to the previous 3 visual line"
    (evil-previous-visual-line 3))

  (gatsby>defcommand gatsby>evil-scroll-up ()
    "Move screen up 5 lines without changing cursor position"
    (evil-scroll-line-up 5))

  (gatsby>defcommand gatsby>evil-scroll-down ()
    "Move screen down 5 lines without changing cursor position"
    (evil-scroll-line-down 5))

  (gatsby>defcommand gatsby>split-right ()
    "Split vertically & move to the right"
    (split-window-horizontally)
    (evil-window-right 1))

  (gatsby>defcommand gatsby>split-down ()
    "Split horizontally & move to the bottom"
    (split-window-vertically)
    (evil-window-down 1))

  (gatsby>defcommand gatsby>evil-visual-tab ()
    "Indent region if in visual-line-mode, otherwise expand the selections via `er/expand-region'"
    (if (eq evil-visual-selection 'line)
        (indent-region (region-beginning) (region-end))
      (call-interactively #'er/expand-region)))

  (gatsby>defcommand gatsby>switch-to-message ()
    (gatsby>switch-to-buffer-new-window (get-buffer-create "*Messages*")))

  (gatsby>defcommand gatsby>kill-buffer ()
    (kill-buffer (current-buffer)))

  (defun gatsby>insert-tab ()
    "Insert a tab when there's only blank char in front"
    (when (save-excursion
            (skip-chars-backward " \t")
            (bolp))
      (insert-tab)
      t))

  (defcustom gatsby>tab-commands '(gatsby>insert-tab completion-at-point)
    "A list of function called without argument"
    :type '(repeat symbol))

  (gatsby>defcommand gatsby>indent-or-complete ()
    (run-hook-with-args-until-success 'gatsby>tab-commands))

  (gatsby>defcommand gatsby>toggle-narrow ()
    "Widen if the current buffer is narrowed, else narrow to defun or region, depending if region is active."
    (cond
     ((buffer-narrowed-p)
      (call-interactively #'widen))
     ((region-active-p)
      (narrow-to-region))
     (t
      (narrow-to-defun))))

  (setq evil-insert-state-message nil)

  :evil-bind
  ((:maps global-map)
   ("<escape>" . #'gatsby>normal-or-motion-state)
   (:maps (visual emacs insert motion normal))
   ("M-u" . #'universal-argument)
   ("C-l" . #'windmove-right)
   ("C-h" . #'windmove-left)
   ("C-j" . #'windmove-down)
   ("C-k" . #'windmove-up)
   ("C-e" . #'gatsby>evil-scroll-down)
   ("C-y" . #'gatsby>evil-scroll-up)
   (:maps (motion normal visual))
   ("j" . #'evil-next-visual-line)
   ("k" . #'evil-previous-visual-line)
   ("J" . #'gatsby>evil-next-three-lines)
   ("K" . #'gatsby>evil-prev-three-lines)
   ("H" . #'evil-first-non-blank-of-visual-line)
   ("L" . #'evil-end-of-visual-line)
   (:maps (motion normal))
   ("<tab>" . #'evil-jump-item)
   (:maps (motion normal visual))
   ("SPC k" . #'delete-window)
   ("SPC w" . #'evil-write)
   ("SPC q" . #'gatsby>kill-buffer)
   ("SPC \\" . #'gatsby>split-right)
   ("SPC -" . #'gatsby>split-down)
   ("SPC f" . #'gatsby>toggle-narrow)
   ("SPC o b" . #'switch-to-buffer)
   ("SPC o f" . #'find-file)
   ("SPC o m" . #'gatsby>switch-to-message)
   (:maps visual)
   ("<tab>" . #'gatsby>evil-visual-tab)
   (:maps insert)
   ("<tab>" . #'gatsby>indent-or-complete)))

(use-package expand-region
  :ensure (expand-region :host github :repo "magnars/expand-region.el")
  :evil-bind ((:maps visual) ("v" . #'er/expand-region) ("V" . #'er/contract-region))
  :config
  (defun gatsby>>treesit-expand ()
    "Use treesitter to find next region if treesitter is available."
    (let* ((root (treesit-buffer-root-node))
           (node
            (treesit-node-descendant-for-range root (region-beginning) (region-end)))
           (node-start (treesit-node-start node))
           (node-end (treesit-node-end node)))
      ;; Node fits the region exactly. Try its parent node instead.
      (when (and (= (region-beginning) node-start) (= (region-end) node-end))
        (when-let ((node (treesit-node-parent node)))
          (setq
           node-start (treesit-node-start node)
           node-end (treesit-node-end node))))
      (set-mark node-end)
      (goto-char node-start)))
  (when (and (fboundp #'treesit-available-p) (treesit-available-p))
    (add-to-list 'er/try-expand-list #'gatsby>>treesit-expand)))

(use-package evil-nerd-commenter
  :ensure (:host github :repo "redguardtoo/evil-nerd-commenter")
  :evil-bind ((:maps (normal visual)) ("SPC t" . #'evilnc-comment-or-uncomment-lines)))

(use-package evil-surround
  :ensure (:host github :repo "emacs-evil/evil-surround")
  :after evil
  :hook (elpaca-after-init . global-evil-surround-mode))

(gatsby>use-internal-package elec-pair
  :hook (elpaca-after-init . electric-pair-mode))

(use-package ws-butler
  :ensure (:host github :repo "lewang/ws-butler" :branch "master")
  :hook (elpaca-after-init . ws-butler-global-mode))

(gatsby>use-internal-package subword
  :hook (elpaca-after-init . global-subword-mode))

(use-package consult
  :ensure (:host github :repo "minad/consult")
  :commands (consult--read consult--multi)
  :custom
  (consult-preview-key nil)
  (consult-narrow-key "?")
  :config
  (gatsby>defcommand gatsby>consult-outline (initial)
    "Override `consult-outline' to enable optional initial INPUT."
    (let ((cands (consult--with-increased-gc (consult--outline-candidates))))
      (consult--read
       cands
       :prompt "Go to heading: "
       :annotate (consult--line-fontify)
       :category 'consult-location
       :sort nil
       :require-match t
       :add-history (thing-at-point 'symbol)
       :history '(:input consult--line-history)
       :lookup #'consult--line-match
       :initial initial
       :state (consult--jump-state))))

  (advice-add #'consult-outline :override #'gatsby>consult-outline)

  (gatsby>defcommand gatsby>consult-search-visual-line (beg end)
    (evil-exit-visual-state)
    (consult-line (buffer-substring-no-properties beg end)))

  (gatsby>defcommand gatsby>consult-search-visual-outline (beg end)
    (evil-exit-visual-state)
    (consult-outline (buffer-substring-no-properties beg end)))

  ;; enable automatic preview
  (consult-customize
   consult-line
   gatsby>consult-outline
   consult-outline
   consult-xref
   gatsby>consult-search-visual-line
   gatsby>consult-search-visual-outline
   :preview-key 'any)

  :evil-bind
  ((:maps (motion normal))
   ([remap switch-to-buffer] . #'consult-buffer)
   ("SPC r g" . #'consult-ripgrep)
   (:maps isearch-mode-map)
   ("<C-return>" . #'gatsby>consult-line-from-evil)
   (:maps (motion normal))
   ("*" . #'consult-line)
   ("#" . #'consult-outline)
   (:maps visual)
   ("*" . #'gatsby>consult-search-visual-line)
   ("#" . #'gatsby>consult-search-visual-outline)))

(provide 'gatsby>editing)
;;; gatsby>editing.el ends here
