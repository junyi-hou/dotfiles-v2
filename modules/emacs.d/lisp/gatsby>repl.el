;;; gatsby>repl.el --- REPL via jupyter and comint -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(gatsby>use-internal-package comint
  :config

  (gatsby>defcommand gatsby>comint-goto-last-prompt ()
    "clear current REPL buffer."
    (goto-char (point-max))
    (evil-insert-state))

  ;; interacting with comint REPL
  :evil-bind
  ((:maps comint-mode-map :states (normal visual insert))
   ("C-c C-l" . #'comint-clear-buffer)
   ("C-c C-c" . #'comint-interrupt-subjob)

   (:maps comint-mode-map :states insert)
   ("<up>" . #'comint-previous-matching-input-from-input)
   ("<down>" . #'comint-next-matching-input-from-input)
   ("<S-return>" . #'comint-accumulate)

   (:maps comint-mode-map :states normal)
   ("<" . #'comint-previous-prompt)
   (">" . #'comint-next-prompt)
   ("A" . #'gatsby>comint-goto-last-prompt)
   ("SPC q" . #'kill-buffer-and-window)
   (:maps comint-mode-map :states (normal visual))
   ("H" . #'comint-bol)))

(use-package jupyter
  :ensure (:host github :repo "nnicandro/emacs-jupyter")
  :custom-face (jupyter-repl-traceback ((t (:extend t :background "firebrick"))))
  :commands (gatsby>jupyter-managed-mode gatsby>jupyter-start-or-switch-to-repl)
  :autoload jupyter-launch-notebook
  :custom
  ;; make sure that jupyter autocomplete shows up super late
  (jupyter-repl-completion-at-point-hook-depth 'back)
  (jupyter-repl-allow-RET-when-busy t)
  (jupyter-repl-echo-eval-p t)
  (jupyter-repl-maximum-size 12000)
  (jupyter-repl-history-maximum-length 5000)
  (jupyter-use-zmq nil)
  :hook (jupyter-repl-mode . corfu-mode)
  :init (defvar gatsby>jupyter-managed-mode-map (make-sparse-keymap))

  (define-minor-mode gatsby>jupyter-managed-mode
    "Minor mode that provides keybinds and commands to major-modes that support jupyter repl."
    :lighter nil
    :group jupyter
    :keymap gatsby>jupyter-managed-mode-map)

  (gatsby>defcommand gatsby>jupyter-insert-cell-separator (markdown)
    (insert (format "\n%s +" (string-trim comment-start)))
    (when markdown
      (insert " [markdown]"))
    (insert "\n")
    (when markdown
      (insert "\"\"\"\"\"\"")
      (backward-char 3)))

  (gatsby>defcommand gatsby>jupyter-next-cell ()
    (let ((cell-regexp (format "^%s\\+\\(.\\)*\n" comment-start)))
      (re-search-forward cell-regexp nil 'noerror)))

  (gatsby>defcommand gatsby>jupyter-prev-cell ()
    (let ((cell-regexp (format "^%s\\+\\(.\\)*\n" comment-start)))
      (re-search-backward cell-regexp nil 'noerror)))

  (gatsby>defcommand gatsby>jupyter-generate-notebook (run)
    "Run the current script and produce a notebook file using `jupytext'.

   If the prefix argument RUN is non-nil, execute all cells to produce output."
    (let* ((file (buffer-file-name))
           (jupytext (executable-find "jupytext"))
           (command (concat jupytext " --set-kernel - --to ipynb " file)))
      (when run
        (setq
         command
         (s-join
          " "
          `(,command
            "--pipe-fmt"
            "ipynb"
            "--pipe"
            "'jupyter nbconvert --to ipynb --execute --allow-errors --stdin --stdout'"))))
      (compile command)))

  (gatsby>defcommand gatsby>jupyter-eval-region-or-cell (from-top)
    (if (region-active-p)
        (let ((b (region-beginning))
              (e (region-end)))
          (jupyter-eval-string (buffer-substring-no-properties b e))
          (evil-normal-state))
      (let* ((cell-regexp (format "^%s\\+\\(.\\)*\n" comment-start))
             (b
              (save-excursion
                (or (and (not from-top) (re-search-backward cell-regexp nil 'noerror))
                    (point-min))))
             (e
              (save-excursion
                (or (re-search-forward cell-regexp nil 'noerror) (point-max)))))
        (jupyter-eval-string (buffer-substring-no-properties b e)))))

  (gatsby>defcommand gatsby>jupyter-start-or-switch-to-repl (connect)
    "Switch to REPL associated the current buffer."
    (if (and (boundp 'jupyter-current-client)
             jupyter-current-client
             (jupyter-kernel-alive-p jupyter-current-client)
             (buffer-live-p (oref jupyter-current-client buffer)))
        (jupyter-repl-pop-to-buffer)
      (let ((code-buffer (current-buffer)))
        (if connect
            ;; TODO: fix connecting to a remote repl
            (let ((current-prefix-arg t))
              (call-interactively #'jupyter-connect-server-repl))
          (call-interactively #'jupyter-run-repl))
        (switch-to-buffer-other-window code-buffer))))

  (gatsby>defcommand gatsby>jupyter-goto-last-prompt ()
    (goto-char (point-max))
    (evil-insert-state))

  (gatsby>defcommand gatsby>jupyter-interrupt-or-clean-input ()
    (if-let* ((client (jupyter-repl--get-client))
              (_ (jupyter-kernel-busy-p client)))
      (jupyter-repl-interrupt-kernel client)
      (call-interactively #'jupyter-repl-clear-input)))

  :evil-bind
  ((:maps jupyter-repl-mode-map :states normal)
   ("A" . #'gatsby>jupyter-goto-last-prompt)
   ("<" . #'jupyter-repl-backward-cell)
   (">" . #'jupyter-repl-forward-cell)
   ("SPC q" . #'kill-buffer-and-window)
   ("SPC r" . #'jupyter-repl-restart-kernel)

   (:maps jupyter-repl-mode-map :states (normal visual insert))
   ("C-c C-c" . #'gatsby>jupyter-interrupt-or-clean-input)
   ("C-c C-l" . #'jupyter-repl-clear-cells)

   (:maps jupyter-repl-mode-map :states insert)
   ("<up>" . #'jupyter-repl-history-previous-matching)
   ("<down>" . #'jupyter-repl-history-next-matching)

   ;; include keymaps for all supporting kernels here
   (:maps gatsby>jupyter-managed-mode-map :states normal)
   (">" . #'gatsby>jupyter-next-cell)
   ("<" . #'gatsby>jupyter-prev-cell)
   ("SPC r o" . #'gatsby>jupyter-start-or-switch-to-repl)
   ("SPC r z" . #'jupyter-repl-associate-buffer)

   (:maps gatsby>jupyter-managed-mode-map :states (normal visual))
   ("SPC r r" . #'gatsby>jupyter-eval-region-or-cell)

   (:maps gatsby>jupyter-managed-mode-map :states (insert normal))
   ("M-RET" . #'gatsby>jupyter-insert-cell-separator)))

(provide 'gatsby>repl)
;;; gatsby>repl.el ends here
