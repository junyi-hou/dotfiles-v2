;;; gatsby>ai.el --- ai code helper -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(use-package agent-shell-tramp
  :ensure (:host github :repo "junyi-hou/agent-shell-tramp")
  :config (agent-shell-tramp-mode 1))

(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :hook (agent-shell-mode . corfu-mode)
  :custom-face (header-line ((t :inherit default)))
  :custom
  (agent-shell-display-action
   '(display-buffer-in-side-window (side . right) (window-width . 0.33) (slot . 0)))
  (agent-shell-file-completion-enabled t)
  (agent-shell-session-strategy 'new)
  (agent-shell-preferred-agent-config 'claude-code)
  :commands
  (agent-shell--start
   agent-shell-anthropic-make-claude-code-config
   agent-shell-anthropic-start-claude-code
   gatsby>agent-shell-toggle)
  :config
  (gatsby>defcommand gatsby>agent-shell-toggle (resume)
    (let* ((project-root (and (project-current) (project-root (project-current))))
           (current-client
            (thread-last
             (buffer-list) (seq-filter #'buffer-live-p)
             (seq-filter
              (lambda (b)
                (with-current-buffer b
                  (eq major-mode 'agent-shell-mode))))
             (cl-find-if
              (lambda (b)
                (with-current-buffer b
                  (file-equal-p default-directory project-root)))))))
      (cond
       (resume
        (agent-shell--start
         :no-focus nil
         :config (agent-shell--resolve-preferred-config)
         :new-session t
         :session-strategy 'prompt))
       ((not current-client)
        (agent-shell--start
         :no-focus nil
         :config (agent-shell--resolve-preferred-config)
         :new-session t))
       (t
        (display-buffer current-client agent-shell-display-action)
        (switch-to-buffer-other-window current-client)
        (evil-insert-state)))))

  ;; "<" and ">" jumps to prev/next permission button if there's a pending permission ask,
  ;; else go to next/prev prompt
  (gatsby>defcommand gatsby>agent-shell-next-prompt-or-permission ()
    (if (map-elt (agent-shell--state) :tool-calls)
        (unless (call-interactively #'agent-shell-next-permission-button)
          (call-interactively #'agent-shell-next-item))
      (call-interactively #'agent-shell-next-item)))

  (gatsby>defcommand gatsby>agent-shell-prev-prompt-or-permission ()
    (if (map-elt (agent-shell--state) :tool-calls)
        (unless (call-interactively #'agent-shell-previous-permission-button)
          (call-interactively #'agent-shell-previous-item))
      (call-interactively #'agent-shell-previous-item)))

  (cl-defun gatsby>>claude-cli
      (prompt
       &key
       (allowed-tools nil)
       (command "claude")
       (callback (lambda (output) (message output))))
    "Run `claude -p' with PROMPT and ALLOWED-TOOLS.
If COMMAND is not nil, use it instead of `claude'."
    (let ((command `(,command "-p" ,prompt "--model" "claude-haiku-4-5"))
          (proc-buf (generate-new-buffer " *claude-cli-output*")))
      (make-process
       :name "claude-cli"
       :command
       (if allowed-tools
           `(,@command "--allowed-tools" ,allowed-tools)
         command)
       :buffer proc-buf
       ;; TRAMP compatible
       :file-handler t
       :filter
       (lambda (proc string)
         (let ((string
                (thread-first string (string-split "\n") butlast (string-join "\n"))))
           (when (buffer-live-p (process-buffer proc))
             (with-current-buffer (process-buffer proc)
               (let ((moving (= (point) (process-mark proc))))
                 (save-excursion
                   ;; Insert the text, advancing the process marker.
                   (goto-char (process-mark proc))
                   (insert string)
                   (set-marker (process-mark proc) (point)))
                 (if moving
                     (goto-char (process-mark proc))))))))
       :sentinel
       (lambda (_proc event)
         ;; when success, return buffer-string of `proc-buf'
         ;; otherwise signal user-error with the buffer-string of `proc-buf'
         ;; alawys clean up - kill `proc-buf'
         (unwind-protect
             (let ((output
                    (with-current-buffer proc-buf
                      (buffer-string))))
               (if (string= event "finished\n")
                   (funcall callback output)
                 (user-error output)))
           (kill-buffer proc-buf))))))

  (gatsby>defcommand gatsby>claude-cli-commit ()
    "Automatically generate commit message for currently staged files."
    (unless (magit-anything-staged-p)
      (user-error "Nothing staged - can't generate commit message"))
    (message "generating commit message...")
    (let ((magit-buf (magit-get-mode-buffer 'magit-status-mode)))
      (gatsby>>claude-cli
       "Analyze currently staged changes and generate a message draft following the **Conventional Commits** specification (`type(scope): description`). Output only the commit message without the markdown quotes."
       :callback
       (lambda (s)
         (make-process
          :name "commit-using-claude"
          :command `("git" "commit" "-m" ,s "--edit")
          :connection-type 'pty
          :sentinel
          (lambda (&rest _)
            (when magit-buf
              (with-current-buffer magit-buf
                (magit-refresh))))))
       :allowed-tools "Bash(git diff *)")))

  (with-eval-after-load 'magit
    (transient-append-suffix
     'magit-commit #'magit-commit-create
     '("g" "Create commit with claude-generated message" gatsby>claude-cli-commit)))

  :evil-bind
  ((:maps normal)
   ("SPC a a" . #'gatsby>agent-shell-toggle)
   (:maps (visual normal))
   ("SPC a s" . #'agent-shell-send-file)
   (:maps agent-shell-mode-map :states insert)
   ("RET" . #'comint-accumulate)
   ("M-RET" . #'comint-send-input)
   ("C-r" . #'agent-shell-search-history)
   ;; ("M-j" . #'comint-previous-input)
   ;; ("M-k" . #'comint-next-input)
   (:maps agent-shell-mode-map :states (normal visual insert))
   ("C-c C-l" . #'comint-clear-buffer)
   ("C-c C-c" . #'agent-shell-interrupt)
   (:maps agent-shell-mode-map :states normal)
   (">" . #'gatsby>agent-shell-next-prompt-or-permission)
   ("<" . #'gatsby>agent-shell-prev-prompt-or-permission)
   ("z o" . #'agent-shell-ui-toggle-fragment-at-point)
   ("z c" . #'agent-shell-ui-toggle-fragment-at-point)
   ("m" . #'agent-shell-set-session-mode)
   ("M" . #'agent-shell-set-session-model)
   ("q" . #'delete-window)
   ("M-v" . #'agent-shell-yank-dwim)
   ([remap kill-buffer-and-window] . #'delete-window)))

(provide 'gatsby>ai)
;;; gatsby>ai.el ends here
