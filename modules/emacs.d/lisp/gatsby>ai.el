;;; gatsby>ai.el --- ai code helper -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(defun gatsby>>get-ai-api-key ()
  "Run passage to get the openai_api_key.  Return nil if no key is found."
  (thread-first
   "direnv exec %s passage show openrouter-api"
   (format (expand-file-name gatsby>dotfiles-repo-location))
   (shell-command-to-string)
   (string-trim)
   (string-split "\n")
   (last)
   (car)))

;; (use-package claude-code-ide
;;   :ensure (:type git :host github :repo "manzaltu/claude-code-ide.el")
;;   ;; :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
;;   :config (claude-code-ide-emacs-tools-setup))

(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :hook (agent-shell-mode . corfu-mode)
  :custom-face (header-line ((t :inherit default)))
  :custom
  (agent-shell-display-action
   '(display-buffer-in-side-window (side . right) (window-height . 0.4) (slot . 0)))
  (agent-shell-file-completion-enabled t)
  (agent-shell-session-strategy 'new)
  (agent-shell-header-style 'text)
  (agent-shell-preferred-agent-config 'claude-code)
  (agent-shell-path-resolver-function
   (lambda (path)
     (if (file-remote-p default-directory)
         (file-local-name path)
       path)))
  :commands
  (agent-shell--start
   agent-shell-anthropic-make-claude-code-config
   agent-shell-anthropic-start-claude-code
   gatsby>agent-shell-toggle)
  :config
  (setq
   agent-shell-anthropic-claude-environment
   (agent-shell-make-environment-variables
    ;; see https://www.reddit.com/r/ClaudeCode/comments/1sbwlmz/claude_code_with_openrouter_api_error_400/
    "CLAUDE_CODE_DISABLE_EXPERIMENTAL_BETAS"
    "TRUE"
    "ANTHROPIC_BASE_URL"
    "https://openrouter.ai/api"
    "ANTHROPIC_AUTH_TOKEN"
    (gatsby>>get-ai-api-key)
    "ANTHROPIC_API_KEY"
    ""))

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
       ((and resume (not current-client))
        (agent-shell--start
         :no-focus nil
         :config (agent-shell--resolve-preferred-config)
         :new-session t
         :session-strategy 'prompt))
       ((not current-client)
        (call-interactively #'agent-shell))
       (t
        (display-buffer current-client agent-shell-display-action)
        (switch-to-buffer-other-window current-client)
        (evil-insert-state)))))

  ;; "<" and ">" jumps to prev/next permission button if there's a pending permission ask,
  ;; else go to next/prev prompt
  (gatsby>defcommand gatsby>agent-shell-next-prompt-or-permission ()
    (if (map-elt (agent-shell--state) :tool-calls)
        (unless (call-interactively #'agent-shell-next-permission-button)
          (call-interactively #'comint-previous-prompt))
      (call-interactively #'comint-next-prompt)))

  (gatsby>defcommand gatsby>agent-shell-prev-prompt-or-permission ()
    (if (map-elt (agent-shell--state) :tool-calls)
        (unless (call-interactively #'agent-shell-previous-permission-button)
          (call-interactively #'comint-previous-prompt))
      (call-interactively #'comint-previous-prompt)))

  (cl-defun gatsby>>agent-shell-header (state &key qualifier bindings)
    "A simpler header for agent shell."
    (unless state
      (error "STATE is required"))
    (let* ((mode
            (when-let* ((mode-id (map-nested-elt state '(:session :mode-id))))
              (or (agent-shell--resolve-session-mode-name
                   mode-id
                   (agent-shell--get-available-modes state))
                  (map-nested-elt state '(:session :mode-id)))))
           (model
            (or (map-elt
                 (seq-find
                  (lambda (model)
                    (string=
                     (map-elt model :model-id)
                     (map-nested-elt state '(:session :model-id))))
                  (map-nested-elt state '(:session :models)))
                 :name)
                (map-nested-elt state '(:session :model-id)) "uninitiated"))
           (context-usage (or (agent-shell--context-usage-indicator) "")))
      (format "%s %s %s"
              (propertize model 'font-lock-face 'font-lock-negation-char-face)
              (if mode
                  (propertize (format "(%s)" mode) 'font-lock-face 'font-lock-type-face)
                "")
              context-usage)))

  (advice-add #'agent-shell--make-header :override #'gatsby>>agent-shell-header)
  (advice-add
   #'agent-shell-anthropic--claude-code-ascii-art
   :override
   (lambda ()
     ""
     "[claude code]"))

  ;; (defun gatsby>>acp--start-client-remote-advice (orig-fun &rest args)
  ;;   "Around advice for `acp--start-client' to enable TRAMP / remote support."
  ;;   (let ((client (plist-get args :client)))
  ;;     (if (file-remote-p default-directory)
  ;;         (cl-letf* ((old-make-process (symbol-function 'make-process))
  ;;                    ((symbol-function #'make-process)
  ;;                     (lambda (&rest props)
  ;;                       (apply old-make-process (append props (list :file-handler t)))))
  ;;                    ((symbol-function #'executable-find)
  ;;                     (lambda (command)
  ;;                       (with-no-warnings (executable-find command t)))))
  ;;           (apply orig-fun args))
  ;;       (apply orig-fun args))))

  ;; (with-eval-after-load 'acp
  ;;   (advice-add #'acp--start-client :around #'gatsby>>acp--start-client-remote-advice))

  ;; (defun gatsby>>agent-shell-insert-shell-command-output-remote-advice
  ;;     (orig-fun &rest args)
  ;;   "Around advice for `agent-shell-insert-shell-command-output' to enable TRAMP / remote support."
  ;;   (if (file-remote-p default-directory)
  ;;       (cl-letf* ((old-make-process (symbol-function 'make-process))
  ;;                  ((symbol-function 'make-process)
  ;;                   (lambda (&rest props)
  ;;                     (apply old-make-process (append props (list :file-handler t))))))
  ;;         (apply orig-fun args))
  ;;     (apply orig-fun args)))

  ;; (advice-add
  ;;  #'agent-shell-insert-shell-command-output
  ;;  :around #'gatsby>>agent-shell-insert-shell-command-output-remote-advice)

  ;; until tramp pacthes get merged:
  (cl-defun gatsby:acp--start-client (&key client)
    "Start CLIENT."
    (unless client
      (error ":client is required"))
    (unless (map-elt client :command)
      (error ":command is required"))
    (unless (executable-find (map-elt client :command)
                             (file-remote-p default-directory))
      (error
       "\"%s\" command line utility not found.  Please install it"
       (map-elt client :command)))
    (when (acp--client-started-p client)
      (error "Client already started"))
    (let* ((pending-input "")
           (message-queue nil)
           (message-queue-busy nil)
           (process-environment
            (append (map-elt client :environment-variables) process-environment))
           (stderr-buffer
            (get-buffer-create
             (format "acp-client-stderr(%s)-%s"
                     (map-elt client :command)
                     (map-elt client :instance-count))))
           ;; For TRAMP (file-handler), we can only use a buffer for stderr.
           ;; For local execution, we use a pipe process for live error parsing.
           (use-file-handler (file-remote-p default-directory))
           (stderr-proc
            (unless use-file-handler
              (make-pipe-process
               :name
               (format "acp-client-stderr(%s)-%s"
                       (map-elt client :command)
                       (map-elt client :instance-count))
               :buffer stderr-buffer
               :filter
               (lambda (_process raw-output)
                 (acp--log client "STDERR" "%s" (string-trim raw-output))
                 (when-let ((std-error
                             (cond
                              ((acp--parse-stderr-api-error raw-output)
                               (acp--parse-stderr-api-error raw-output))
                              ((not (string-empty-p (string-trim raw-output)))
                               ;; Fallback: create a generic error response
                               `((code . -32603) (message . ,raw-output))))))
                   (acp--log client "API-ERROR" "%s" (string-trim raw-output))
                   (dolist (handler (map-elt client :error-handlers))
                     (funcall handler std-error))))))))
      ;; Disable SSH ControlMaster for TRAMP - it can't handle large data (see eglot bug#61350)
      ;; Disable direct async process for TRAMP - it breaks process filters (see lsp-mode#4573)
      (cl-letf (((symbol-function 'tramp-direct-async-process-p)
                 (lambda (&rest _) nil)))
        (let ((tramp-use-ssh-controlmaster-options
               (if use-file-handler
                   'suppress
                 tramp-use-ssh-controlmaster-options))
              (tramp-ssh-controlmaster-options
               (if use-file-handler
                   "-o ControlMaster=no -o ControlPath=none"
                 tramp-ssh-controlmaster-options)))
          (let
              ((process
                (make-process
                 :name
                 (format "acp-client(%s)-%s"
                         (map-elt client :command)
                         (map-elt client :instance-count))
                 :command (cons (map-elt client :command) (map-elt client :command-params))
                 :stderr (or stderr-proc stderr-buffer)
                 :connection-type 'pipe
                 :coding 'utf-8-emacs-unix
                 :noquery t
                 :file-handler use-file-handler
                 :filter
                 (lambda (_proc input)
                   (acp--log client "INCOMING TEXT" "%s" input)
                   (setq pending-input (concat pending-input input))
                   (let ((start 0)
                         pos)
                     (while (setq pos (string-search "\n" pending-input start))
                       (let ((json (substring pending-input start pos)))
                         (acp--log client "INCOMING LINE" "%s" json)
                         (when-let* ((object
                                      (condition-case nil
                                          (acp--parse-json json)
                                        (error
                                         (acp--log
                                          client
                                          "JSON PARSE ERROR"
                                          "Invalid JSON: %s"
                                          json)
                                         nil))))
                           (setq message-queue
                                 (append
                                  message-queue
                                  (list (acp--make-message :json json :object object))))
                           (unless message-queue-busy
                             (setq message-queue-busy t)
                             (run-at-time
                              0 nil
                              (lambda ()
                                (while message-queue
                                  (let ((message (car message-queue)))
                                    (setq message-queue (cdr message-queue))
                                    (acp--route-incoming-message
                                     :message message
                                     :client client
                                     :on-notification
                                     (lambda (notification)
                                       (dolist (handler
                                                (map-elt client :notification-handlers))
                                         (condition-case-unless-debug err
                                             (funcall handler notification)
                                           (error
                                            (acp--log
                                             client
                                             "NOTIFICATION HANDLER ERROR"
                                             "Failed with error: %S"
                                             err)))))
                                     :on-request
                                     (lambda (request)
                                       (dolist (handler
                                                (map-elt client :request-handlers))
                                         (condition-case-unless-debug err
                                             (funcall handler request)
                                           (error
                                            (acp--log
                                             client
                                             "REQUEST HANDLER ERROR"
                                             "Failed with error: %S"
                                             err))))))))
                                (setq message-queue-busy nil))))))
                       (setq start (1+ pos)))
                     (setq pending-input (substring pending-input start))))
                 :sentinel
                 (lambda (_process _event)
                   (when (process-live-p stderr-proc)
                     (delete-process stderr-proc))
                   (when (buffer-live-p stderr-buffer)
                     (kill-buffer stderr-buffer))))))
            ;; For TRAMP connections, wait a moment for the SSH connection to fully establish
            (when use-file-handler
              (accept-process-output process 0.1))
            (map-put! client :process process))))))

  ;; (with-eval-after-load 'tramp
  ;;   (advice-add #'acp--start-client :override #'gatsby:acp--start-client))


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
