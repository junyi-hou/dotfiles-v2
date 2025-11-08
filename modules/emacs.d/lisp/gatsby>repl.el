;;; gatsby>repl.el --- emacs builtin REPL -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(gatsby>use-internal-pacakge comint
  :config
  (gatsby>defcommand gatsby>comint-cls ()
    "clear current REPL buffer."
    (let ((last-line (save-excursion (goto-char (point-max)) (beginning-of-line) (point))))
      (set-window-start (selected-window) last-line)
      (call-interactively #'evil-scroll-line-up)))

  (gatsby>defcommand gatsby>comint-goto-last-prompt ()
    "clear current REPL buffer."
    (goto-char (point-max))
    (evil-insert-state))
  
  :evil-bind
  ((:maps comint-mode-map :states (normal visual insert))
   ("C-c C-l" . #'gatsby>comint-cls)
   ("C-c C-c" . #'comint-interrupt-subjob)
   
   (:maps comint-mode-map :states insert)
   ("<up>" . #'comint-previous-matching-input-from-input)
   ("<down>" . #'comint-next-matching-input-from-input)
   
   (:maps comint-mode-map :states (normal visual))
   ("<" . #'comint-previous-prompt)
   (">" . #'comint-next-prompt)
   ("A" . #'gatsby>comint-goto-last-prompt)
   ("H" . #'comint-bol)))

(use-package jupyter
  :ensure (:host github :repo "nnicandro/emacs-jupyter")
  :custom-face
  (jupyter-repl-traceback ((t (:extend t :background "firebrick"))))
  :commands (gatsby>jupyter-start-or-switch-to-repl jupyter-launch-notebook)
  :custom 
  (jupyter-repl-allow-RET-when-busy t)
  (jupyter-repl-echo-eval-p t)
  (jupyter-repl-maximum-size 12000)
  (jupyter-repl-history-maximum-length 5000)
  (jupyter-use-zmq nil)
  :hook (jupyter-repl-mode . corfu-mode)
  :config
  (gatsby>defcommand gatsby>jupyter-start-or-switch-to-repl (connect)
    "Switch to REPL associated the current buffer."
    (if (and jupyter-current-client
             (jupyter-kernel-alive-p jupyter-current-client)
             (buffer-live-p (oref jupyter-current-client buffer)))
        (jupyter-repl-pop-to-buffer)
      (let ((code-buffer (current-buffer)))
        (if connect
            ;; TODO: fix connecting to a repl v
            (let ((current-prefix-arg t))
              (call-interactively #'jupyter-connect-server-repl))
          (call-interactively #'jupyter-run-repl))
        (switch-to-buffer-other-window code-buffer))))

  (gatsby>defcommand gatsby>jupyter-goto-last-prompt ()
    (goto-char (point-max))
    (evil-insert-state))

  :evil-bind
  ((:maps jupyter-repl-mode-map :states (normal visual))
   ("A" . #'gatsby>jupyter-goto-last-prompt)
   ("<" . #'jupyter-repl-backward-cell)
   (">" . #'jupyter-repl-forward-cell)
   ("SPC q" . #'kill-buffer-and-window)
   ("SPC r" . #'jupyter-repl-restart-kernel)

   (:maps jupyter-repl-mode-map :states (normal visual insert))
   ("C-c C-c" . #'jupyter-repl-interrupt-kernel)
   ("C-c C-l" . #'jupyter-repl-clear-cells)
   
   (:maps jupyter-repl-mode-map :states insert)
   ("<up>" . #'jupyter-repl-history-previous-matching)
   ("<down>" . #'jupyter-repl-history-next-matching)

   ;; include keymaps for all supporting kernels here
   (:maps python-ts-mode-map :states normal)
   ("SPC r o" . #'gatsby>jupyter-start-or-switch-to-repl)
   ("SPC r z" . #'jupyter-repl-associate-buffer)))

(provide 'gatsby>repl)
;;; gatsby>repl.el ends here
