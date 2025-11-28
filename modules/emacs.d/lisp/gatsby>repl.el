;;; gatsby>repl.el --- REPL via jupyter and comint -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

;; TODO: migrate to termint + vterm?
;; (use-package termint
;;   :ensure (:host github :repo "milanglacier/termint.el")
;;   :custom (termint-))

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

  ;; interacting with comint REPL
  (defvar gatsby>comint-managed-mode-map (make-sparse-keymap))

  (defvar-local gatsby>comint-command nil
    "The command to start the REPL")

  (defvar-local gatsby>comint-buffer nil
    "The buffer that holds comint")

  (defvar gatsby>comint-buffer-list nil
    "A list of all managed comint buffer")

  (defun gatsby>>comint-is-running (&optional buffer)
    "Return t if a comint repl BUFFER exists and a live process is running in it.
If BUFFER is nil, use `gatsby>comint-buffer'."

    (let ((repl-buffer (or buffer gatsby>comint-buffer)))
      (and repl-buffer
           (buffer-live-p repl-buffer)
           (process-live-p (get-buffer-process repl-buffer)))))

  (define-minor-mode gatsby>comint-managed-mode
    "Minor mode that provides keybinds and commands to major-modes that support comint repl."
    :lighter nil
    :group comint
    :keymap gatsby>comint-managed-mode-map)

  (defun gatsby>start-comint (cmd code-buffer)
    "Run CMD in a new comint window and associated it with the code-buffer"

    ;; do not start a second repl if one exists
    (when (gatsby>>comint-is-running code-buffer)
      (user-error "An existing REPL is running"))

    (let* ((default-directory (if-let ((project (project-current)))
                                  (project-root project)
                                default-directory))
           (repl-buffer (get-buffer-create (format "*%s: %s*" cmd (thread-first default-directory
                                                                                directory-file-name
                                                                                file-name-base)))))
      (make-comint-in-buffer "clojure-REPL" repl-buffer cmd)
      (with-current-buffer code-buffer (setq-local gatsby>comint-buffer repl-buffer))
      (add-to-list 'gatsby>comint-buffer-list repl-buffer)
      (pop-to-buffer repl-buffer)))

  (gatsby>defcommand gatsby>comint-start-or-switch-to-repl (connect)
    "Pop to the REPL buffer if exists, or start a new one.

If the prefix argument CONNECT is non nil, connect the current code buffer to an
existing comint REPL."
    (cond
     (connect (let* ((code-buffer (current-buffer))
                     (updated-comint-list (cl-remove-if-not #'gatsby>>comint-is-running gatsby>comint-buffer-list))
                     (repl-buffer (completing-read "connect to: " (mapcar #'buffer-name updated-comint-list))))
                (setq gatsby>comint-buffer-list updated-comint-list)
                (setq-local gatsby>comint-buffer (get-buffer repl-buffer))
                (pop-to-buffer repl-buffer)))

     ((gatsby>>comint-is-running) (pop-to-buffer gatsby>comint-buffer))

     ;; use the same REPL for files in the same project
     ((let* ((updated-comint-list (cl-remove-if-not #'gatsby>>comint-is-running gatsby>comint-buffer-list))
             (repl-buffer (cl-find-if (lambda (comint-buffer)
                                        (file-equal-p
                                         (with-current-buffer comint-buffer default-directory)
                                         default-directory))
                                      updated-comint-list)))
        (setq-local gatsby>comint-buffer repl-buffer))
      (pop-to-buffer gatsby>comint-buffer))

     (t (gatsby>start-comint gatsby>comint-command (current-buffer)))))

  (defun gatsby>comint-eval-string (string)
    "Send STRING to the comint process associated with `gatsby>comint-buffer',
preserving any current input typed in the comint buffer.

This temporarily replaces the current input line with STRING, calls
`comint-send-input' to run it, and then restores the previously-typed input
at point-max."
    (unless (gatsby>>comint-is-running)
      (user-error "No live REPL"))
    (with-current-buffer gatsby>comint-buffer
      (let* ((inhibit-read-only t)
             (input-start (comint-line-beginning-position))
             (saved-input (buffer-substring-no-properties input-start (point-max))))
        ;; Replace current input with STRING and send it.
        (goto-char (point-max))
        (delete-region input-start (point-max))
        (insert string)
        (comint-send-input)
        ;; Restore previous typed input at the end of buffer.
        (goto-char (point-max))
        (insert saved-input))))

  (gatsby>defcommand gatsby>comint-eval-region-or-cell (from-top)
    (if (region-active-p)
        (let ((b (region-beginning))
              (e (region-end)))
          (gatsby>comint-eval-string (buffer-substring-no-properties b e))
          (evil-normal-state))
      (let* ((cell-regexp (format "^%s +\\(.\\)*\n" comment-start))
             (b (save-excursion (or (and (not from-top)
                                         (re-search-backward cell-regexp nil 'noerror))
                                    (point-min))))
             (e (save-excursion (or (re-search-forward cell-regexp nil 'noerror) (point-max)))))
        (gatsby>comint-eval-string (buffer-substring-no-properties b e)))))

  (gatsby>defcommand gatsby>comint-insert-cell-separator (markdown)
    (insert (format "\n%s +" comment-start))
    (when markdown (insert " [markdown]"))
    (insert "\n")
    (when markdown
      (insert "\"\"\"\"\"\"") (backward-char 3)))

  (gatsby>defcommand gatsby>comint-next-cell ()
    (let ((cell-regexp (format "^%s +\\(.\\)*\n" comment-start)))
      (re-search-forward cell-regexp nil 'noerror)))

  (gatsby>defcommand gatsby>comint-prev-cell ()
    (let ((cell-regexp (format "^%s +\\(.\\)*\n" comment-start)))
      (re-search-backward cell-regexp nil 'noerror)))

  :evil-bind
  ((:maps comint-mode-map :states (normal visual insert))
   ("C-c C-l" . #'gatsby>comint-cls)
   ("C-c C-c" . #'comint-interrupt-subjob)
   
   (:maps comint-mode-map :states insert)
   ("<up>" . #'comint-previous-matching-input-from-input)
   ("<down>" . #'comint-next-matching-input-from-input)
   ("<S-return>" . #'comint-accumulate)
   
   (:maps comint-mode-map :states normal)
   ("<" . #'comint-previous-prompt)
   (">" . #'comint-next-prompt)
   ("A" . #'gatsby>comint-goto-last-prompt)
   ("SPC . q" . #'kill-buffer-and-window)
   (:maps comint-mode-map :states (normal visual))
   ("H" . #'comint-bol)

   (:maps gatsby>comint-managed-mode-map :states normal)
   (">" . #'gatsby>comint-next-cell)
   ("<" . #'gatsby>comint-prev-cell)
   ("<SPC> r o" . #'gatsby>comint-start-or-switch-to-repl)

   (:maps gatsby>comint-managed-mode-map :states (normal visual))
   ("<SPC> r r" . #'gatsby>comint-eval-region-or-cell)

   (:maps gatsby>comint-managed-mode-map :states (insert normal))
   ("M-RET" . #'gatsby>comint-insert-cell-separator)))

(use-package jupyter
  :ensure (:host github :repo "nnicandro/emacs-jupyter")
  :custom-face
  (jupyter-repl-traceback ((t (:extend t :background "firebrick"))))
  :commands (gatsby>jupyter-managed-mode gatsby>jupyter-start-or-switch-to-repl)
  :autoload jupyter-launch-notebook
  :custom
  (jupyter-repl-allow-RET-when-busy t)
  (jupyter-repl-echo-eval-p t)
  (jupyter-repl-maximum-size 12000)
  (jupyter-repl-history-maximum-length 5000)
  (jupyter-use-zmq nil)
  :hook (jupyter-repl-mode . corfu-mode)
  :init
  (defvar gatsby>jupyter-managed-mode-map (make-sparse-keymap))

  (define-minor-mode gatsby>jupyter-managed-mode
    "Minor mode that provides keybinds and commands to major-modes that support jupyter repl."
    :lighter nil
    :group jupyter
    :keymap gatsby>jupyter-managed-mode-map)

  (gatsby>defcommand gatsby>jupyter-insert-cell-separator (markdown)
    (insert (format "\n%s +" comment-start))
    (when markdown (insert " [markdown]"))
    (insert "\n")
    (when markdown
      (insert "\"\"\"\"\"\"") (backward-char 3)))

  (gatsby>defcommand gatsby>jupyter-next-cell ()
    (let ((cell-regexp (format "^%s +\\(.\\)*\n" comment-start)))
      (re-search-forward cell-regexp nil 'noerror)))

  (gatsby>defcommand gatsby>jupyter-prev-cell ()
    (let ((cell-regexp (format "^%s +\\(.\\)*\n" comment-start)))
      (re-search-backward cell-regexp nil 'noerror)))

  (gatsby>defcommand gatsby>jupyter-generate-notebook (run)
    "Run the current script and produce a notebook file using `jupytext'.

   If the prefix argument RUN is non-nil, execute all cells to produce output."
    (let* ((file (buffer-file-name))
           (jupytext (executable-find "jupytext"))
           (command (concat jupytext " --set-kernel - --to ipynb " file)))
      (when run
        (setq command (s-join " " `(,command "--pipe-fmt" "ipynb" "--pipe" "'jupyter nbconvert --to ipynb --execute --allow-errors --stdin --stdout'"))))
      (compile command)))

  (gatsby>defcommand gatsby>jupyter-eval-region-or-cell (from-top)
    (if (region-active-p)
        (let ((b (region-beginning))
              (e (region-end)))
          (jupyter-eval-string (buffer-substring-no-properties b e))
          (evil-normal-state))
      (let* ((format "^%s +\\(.\\)*\n" comment-start)
             (b (save-excursion (or (and (not from-top)
                                         (re-search-backward cell-regexp nil 'noerror))
                                    (point-min))))
             (e (save-excursion (or (re-search-forward cell-regexp nil 'noerror) (point-max)))))
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
    (if-let ((client (jupyter-repl--get-client))
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
