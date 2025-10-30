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
  :config
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
    (split-window-horizontally) (evil-window-right 1))

  (gatsby>defcommand gatsby>split-down ()
    "Split horizontally & move to the bottom"
    (split-window-vertically) (evil-window-down 1))

  (gatsby>defcommand gatsby>evil-visual-tab ()
    "Indent region if in visual-line-mode, otherwise select contains inside a pair of tags via `evil-jump-item'"
    (if (eq evil-visual-selection 'line)
        (indent-region (region-beginning) (region-end))
      (evil-jump-item)))

  (gatsby>defcommand gatsby>switch-to-message ()
    (gatsby>switch-to-buffer-new-window (get-buffer-create "*Messages*")))

  (evil-mode 1)

  :general
  (:keymaps '(visual emacs insert)
						"<escape>"  #'evil-normal-state)

  (:keymaps '(visual emacs insert motion normal)
						"M-u" #'universal-argument
						"C-l" #'windmove-right
						"C-h" #'windmove-left
						"C-j" #'windmove-down
						"C-k" #'windmove-up
						"C-e" #'gatsby>evil-scroll-down
						"C-y" #'gatsby>evil-scroll-up)

  (:keymaps '(motion normal visual)
						"j" #'evil-next-visual-line
						"k" #'evil-previous-visual-line
						"J" #'gatsby>evil-next-three-lines
						"K" #'gatsby>evil-prev-three-lines
						"H" #'evil-first-non-blank-of-visual-line
						"L" #'evil-end-of-visual-line

						"SPC" nil)

  (:keymaps '(normal motion)
						"<tab>" #'evil-jump-item)

  (:keymaps 'visual
						"<tab>" #'gatsby>evil-visual-tab)

  ;; leader key
  (:keymaps '(motion normal)
						:prefix "SPC"
						"k" #'delete-window
						"w" #'evil-write
						"q" #'kill-buffer-and-window
						"\\" #'gatsby>split-right
						"-" #'gatsby>split-down

						"ob" #'switch-to-buffer
						"of" #'find-file
						"om" #'gatsby>switch-to-message
						"os" #'gatsby>eshell-open-or-switch))

(use-package expand-region
  :ensure (expand-region :host github :repo "magnars/expand-region.el")
  :commands (er/expand-region er/contract-region)
  :general
  (:keymaps 'visual
						"v" #'er/expand-region
						"V" #'er/contract-region)
  :config
  (defun gatsby>>treesit-expand ()
    "Use treesitter to find next region if treesitter is available."
    (let* ((root (treesit-buffer-root-node))
           (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
           (node-start (treesit-node-start node))
           (node-end (treesit-node-end node)))
      ;; Node fits the region exactly. Try its parent node instead.
      (when (and (= (region-beginning) node-start) (= (region-end) node-end))
        (when-let ((node (treesit-node-parent node)))
          (setq node-start (treesit-node-start node)
                node-end (treesit-node-end node))))
      (set-mark node-end)
      (goto-char node-start)))

  (when (and (fboundp #'treesit-available-p)
             (treesit-available-p))
    (add-to-list 'er/try-expand-list #'gatsby>>treesit-expand)))

(use-package evil-nerd-commenter
  :ensure (:host github :repo "redguardtoo/evil-nerd-commenter")
  :after evil
  :commands evilnc-comment-or-uncomment-lines
  :general
  (:keymaps '(normal visual)
						:prefix "SPC"
						"t" #'evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :ensure (:host github :repo "emacs-evil/evil-surround")
  :after evil
  :hook (elpaca-after-init . global-evil-surround-mode))

(gatsby>use-internal-pacakge elec-pair
  :hook (elpaca-after-init . electric-pair-mode))

;; (use-package ws-butler
;;   :ensure (:host github :repo "lewang/ws-butler")
;;   :hook (elpaca-after-init . ws-butler-global-mode))

(gatsby>use-internal-pacakge subword
  :hook (elpaca-after-init . global-subword-mode))

(use-package consult
	:ensure (:host github :repo "minad/consult")
	:after evil
  :commands
  (consult--read consult--multi)
  :custom
  (consult-preview-key nil)
  :config
  (gatsby>defcommand gatsby>consult-outline (initial)
    "Override `consult-outline' to enable optional initial INPUT."
    (let ((cands (consult--with-increased-gc (consult--outline-candidates))))
      (consult--read
       cands
       :prompt "Go to heading: "
       :annotate (consult--line-prefix)
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
   gatsby>consult-search-visual-line
   gatsby>consult-search-visual-outline
   :preview-key 'any)


	;; during `/' or `?' search, <C-Return> will start a consult search 
  (gatsby>defcommand gatsby>consult-line-from-evil (arg)
    "Take current search string and run `consult-line' on it.
  If ARG is non-nil, run `consult-outline' instead."
    (let ((str isearch-string)
          (enable-recursive-minibuffers t)
          (fn (if arg #'consult-outline #'consult-line)))
      (run-at-time 0 nil fn str)
      (abort-recursive-edit)))
  
	;; save consult search in `search-ring' as well
  (defvar-local gatsby>>consult-current-input nil)
  
  (defun gatsby>>consult-add-current-input-to-search-ring (&rest _)
    "Add the current input to the front of `search-ring' so evil search can pick it up.
  
  Note: since `evil' uses `regexp-search-ring', so need to pass `t' as the second argument
  to update the correct ring."
    (isearch-update-ring (or gatsby>>consult-current-input (minibuffer-contents-no-properties)) t)
    (setq-local gatsby>>consult-current-input nil))
  
  (defun gatsby>>consult-tempoarily-add-to-minibuffer-hooks (fn &rest args)
    (cl-letf* ((minibuffer-exit-hook `(,@minibuffer-exit-hook
                                       gatsby>>consult-add-current-input-to-search-ring))
               ;; because vertico insert the selection to the minibuffer before it exits
               ;; need to save the minibuffer-contents before vertico inserts the current selection
               ((symbol-function #'vertico-exit)
                (lambda (&optional arg)
                  (interactive "P")
                  (when (and (not arg) (>= vertico--index 0))
                    (setq-local gatsby>consult--current-input (minibuffer-contents-no-properties))
                    (vertico-insert))
                  (when (vertico--match-p (minibuffer-contents-no-properties))
                    (exit-minibuffer)))
                ))
      (apply fn args)))
  
  (advice-add #'consult-line :around #'gatsby>>consult-tempoarily-add-to-minibuffer-hooks)
  (advice-add #'consult-outline :around #'gatsby>>consult-tempoarily-add-to-minibuffer-hooks)
  
  :general
  (:keymaps '(motion normal visual)
						[remap switch-to-buffer] #'consult-buffer)
  (:keymaps 'isearch-mode-map
						"<C-return>" #'gatsby>consult-line-from-evil)
  (:keymaps '(motion normal)
						"*" #'consult-line
						"#" #'consult-outline)
  
  (:keymaps 'visual
						"*" #'gatsby>consult-search-visual-line
						"#" #'gatsby>consult-search-visual-outline))


(provide 'gatsby>editing)
;;; gatsby>editing.el ends here
