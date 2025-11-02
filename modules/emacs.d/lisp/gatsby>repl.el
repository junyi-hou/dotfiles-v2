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
			(evil-scroll-line-up)))
	
	:general
	(:keymaps 'comint-mode-map :states '(normal visual motion emacs insert) :prefix "C-c"
						"C-l" 'gatsby>comint-cls
						"C-c" 'comint-interrupt-subjob)
  
  (:keymaps 'comint-mode-map :states 'insert
						"<up>" #'comint-previous-matching-input-from-input
						"<down>" #'comint-next-matching-input-from-input)
  
  (:keymaps 'comint-mode-map :states '(normal motion visual)
						"<" #'comint-previous-prompt
						">" #'comint-next-prompt
						"A" #'gatsby>comint-goto-last-prompt
						"H" #'comint-bol))

;; FIXME - gatsby>jupyter-start-or-switch-to-repl and jupyter-launch-notebook are not autoloaded
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

	:general
  (:keymaps 'jupyter-repl-mode-map :states '(normal visual)
						"A" #'gatsby>jupyter-goto-last-prompt
						"<" #'jupyter-repl-backward-cell
						">" #'jupyter-repl-forward-cell
						"SPC" nil)
  
  (:keymaps 'jupyter-repl-mode-map :states '(normal visual insert) :prefix "C-c"
						"C-c" #'jupyter-repl-interrupt-kernel
						"C-l" #'jupyter-repl-clear-cells)
  
  (:keymaps 'jupyter-repl-mode-map :states '(normal visual) :prefix "SPC"
						"q" #'kill-buffer-and-window
						"r" #'jupyter-repl-restart-kernel)
  
  (:keymaps 'jupyter-repl-mode-map :states 'insert
						"<up>" #'jupyter-repl-history-previous-matching
						"<down>" #'jupyter-repl-history-next-matching))



(provide 'gatsby>repl)
;;; gatsby>repl.el ends here
