;;; gatsby>repl.el --- REPL via jupyter and comint -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(use-package jupyter
  :ensure (:host github :repo "nnicandro/emacs-jupyter")
  :custom-face (jupyter-repl-traceback ((t (:extend t :background "firebrick"))))
  :commands (gatsby>jupyter-managed-mode gatsby>jupyter-start-or-switch-to-repl jupyter-servers)
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
  :init

  (defvar gatsby>jupyter-managed-mode-map (make-sparse-keymap))

  (defvar gatsby>>jupyter-sessions nil
    "List of Jupyter sessions started by gatsby commands.
Each entry is a plist with :label (string) and :procs (list of processes).")

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
    "Evaluate the active region or the current cell.
If the region is active, pass it to `jupyter-eval-string' and exit to
normal state.  Otherwise evaluate from the previous cell separator (or
`point-min' when FROM-TOP is non-nil) to the next cell separator (or
`point-max' when none follows)."
    (if (region-active-p)
        (let ((b (region-beginning))
              (e (region-end)))
          (prog1 (jupyter-eval-string (buffer-substring-no-properties b e))
            (evil-normal-state)))
      (let* ((cell-regexp (format "^%s\\+\\(.\\)*\n" comment-start))
             (b
              (save-excursion
                (or (and (not from-top) (re-search-backward cell-regexp nil 'noerror))
                    (point-min))))
             (e
              (save-excursion
                (or (re-search-forward cell-regexp nil 'noerror) (point-max)))))
        (jupyter-eval-string (buffer-substring-no-properties b e)))))

  (defun gatsby>>has-jupyter-kernel (&optional buffer)
    "Return true if BUFFER has a valid jupyter client that connects to a live jupyter kernel."
    (let ((buffer (or buffer (current-buffer))))
      (with-current-buffer buffer
        (and (boundp 'jupyter-current-client)
             jupyter-current-client
             (jupyter-kernel-alive-p jupyter-current-client)
             (buffer-live-p (oref jupyter-current-client buffer))))))

  (defun gatsby>>get-server-at (dir)
    "Return jupyter server whose root is DIR, or nil if no such server is found."
    (ignore-errors
      (seq-find
       (lambda (s)
         (file-equal-p
          dir
          (with-current-buffer (thread-last s jupyter-notebook-process (process-buffer))
            default-directory)))
       (jupyter-servers))))

  (defun gatsby>>find-free-port ()
    "Return a free local TCP port number."
    (let* ((proc
            (make-network-process
             :name "gatsby-port-finder"
             :server t
             :host "127.0.0.1"
             :service t
             :family 'ipv4
             :noquery t))
           (port (process-contact proc :service)))
      (delete-process proc)
      port))

  (defun gatsby>>jupyter-start-remote-repl (code-buffer)
    "Launch a Jupyter server on the TRAMP remote host of CODE-BUFFER.
Forwards the server port over SSH and connects a REPL to CODE-BUFFER."
    (let* ((dir
            (with-current-buffer code-buffer
              default-directory))
           (vec (tramp-dissect-file-name dir))
           (host (tramp-file-name-host vec))
           (remote-dir (tramp-file-name-localname vec))
           (local-port (gatsby>>find-free-port))
           (remote-port (+ 49152 (random 16383)))
           (log-buf (get-buffer-create (format " *jupyter-remote-%s*" host)))
           connected)
      (message "Starting Jupyter on %s (local port %d -> remote port %d)..."
               host
               local-port
               remote-port)
      (let
          ((proc
            (start-process
             (format "jupyter-remote-%s" host) log-buf "ssh"
             "-L"
             (format "127.0.0.1:%d:127.0.0.1:%d" local-port remote-port)
             host
             (format
              "cd %s && direnv exec . jupyter server --no-browser --port %d --IdentityProvider.token=''"
              (shell-quote-argument remote-dir) remote-port))))
        (set-process-filter
         proc
         (lambda (_proc output)
           (with-current-buffer log-buf
             (goto-char (point-max))
             (insert output))
           (when (and (not connected) (string-match "is running" output))
             (setq connected t)
             (message "Jupyter ready on local port %d; connecting..." local-port)
             (run-with-timer
              0.5 nil
              (lambda ()
                (condition-case err
                    (let* ((server
                            (jupyter-server
                             :url (format "http://127.0.0.1:%d" local-port)))
                           (kernel
                            (jupyter-completing-read-kernelspec
                             (jupyter-kernelspecs server))))
                      (jupyter-run-server-repl server kernel nil code-buffer nil t)
                      (push (list
                             :label (format "remote: %s:%s" host remote-dir)
                             :procs (list proc))
                            gatsby>>jupyter-sessions)
                      (jupyter-repl-pop-to-buffer))
                  (error
                   (message "Failed to connect to remote Jupyter: %s" err)
                   (kill-process proc)))))))))))

  (gatsby>defcommand gatsby>jupyter-start-or-switch-to-repl (connect)
    "Switch to REPL associated the current buffer."
    (if (gatsby>>has-jupyter-kernel)
        (jupyter-repl-pop-to-buffer)
      (let ((code-buffer (current-buffer)))
        (cond
         (connect
          (let ((current-prefix-arg t))
            (call-interactively #'jupyter-connect-server-repl))
          (jupyter-repl-pop-to-buffer))
         ((file-remote-p
           (with-current-buffer code-buffer
             default-directory))
          (gatsby>>jupyter-start-remote-repl code-buffer))
         (t
          (let* ((cwd
                  (with-current-buffer code-buffer
                    default-directory))
                 (server (gatsby>>get-server-at cwd)))
            (unless server
              (let ((port (jupyter-launch-notebook)))
                (setq server (jupyter-server :url (format "http://localhost:%s" port))))
              (push (list
                     :label (format "local: %s" cwd)
                     :procs (list (jupyter-notebook-process server)))
                    gatsby>>jupyter-sessions))
            (let ((kernel
                   (jupyter-completing-read-kernelspec (jupyter-kernelspecs server))))
              (jupyter-run-server-repl server kernel nil code-buffer nil t)))
          (jupyter-repl-pop-to-buffer))))))

  (gatsby>defcommand gatsby>jupyter-kill-notebook ()
    "Select and terminate a tracked Jupyter notebook server."
    (setq gatsby>>jupyter-sessions
          (seq-filter
           (lambda (s) (seq-some #'process-live-p (plist-get s :procs)))
           gatsby>>jupyter-sessions))
    (if (null gatsby>>jupyter-sessions)
        (message "No active Jupyter sessions.")
      (let* ((choice
              (completing-read "Kill session: "
                               (mapcar
                                (lambda (s) (plist-get s :label))
                                gatsby>>jupyter-sessions)
                               nil t))
             (session
              (seq-find
               (lambda (s) (equal (plist-get s :label) choice))
               gatsby>>jupyter-sessions)))
        (dolist (p (plist-get session :procs))
          (when (process-live-p p)
            (kill-process p)))
        (setq gatsby>>jupyter-sessions
              (seq-remove (lambda (s) (eq s session)) gatsby>>jupyter-sessions))
        (message "Terminated: %s" choice))))

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
   ("SPC r k" . #'gatsby>jupyter-kill-notebook)
   ("SPC r z" . #'jupyter-repl-associate-buffer)

   (:maps gatsby>jupyter-managed-mode-map :states (normal visual))
   ("SPC r r" . #'gatsby>jupyter-eval-region-or-cell)

   (:maps gatsby>jupyter-managed-mode-map :states (insert normal))
   ("M-RET" . #'gatsby>jupyter-insert-cell-separator)))

(provide 'gatsby>repl)
;;; gatsby>repl.el ends here
