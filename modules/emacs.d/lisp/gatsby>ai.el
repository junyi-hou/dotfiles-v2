;;; gatsby>ai.el --- ai code helper -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(use-package agent-shell-tramp
  :ensure (:host github :repo "junyi-hou/agent-shell-tramp")
  :config (agent-shell-tramp-mode 1))

;; managing agents
(use-package agent-shell-manager
  :ensure (:host github :repo "jethrokuan/agent-shell-manager")
  :evil-bind
  ((:maps normal)
   ("SPC a p" . #'agent-shell-manager-toggle)
   (:maps agent-shell-manager-mode-map :states motion)
   ("RET" . #'agent-shell-manager-goto)
   ("C-c C-c" . #'agent-shell-interrupt)
   ("m" . #'agent-shell-manager-set-mode)
   ("M" . #'agent-shell-manager-set-model)
   ("q" . #'quit-window)
   ("c" . #'agent-shell-manager-new)
   ("g" . #'agent-shell-manager-refresh)
   ("r" . #'agent-shell-manager-restart)
   ("x" . #'agent-shell-manager-kill)
   ("d" . #'agent-shell-manager-delete-killed)))

(use-package agent-shell-to-go
  :ensure (:host github :repo "junyi-hou/agent-shell-to-go")
  :custom
  (agent-shell-to-go-discord-guild-id (sops-retrieve-secret "env/DISCORD_GUILD_ID"))
  (agent-shell-to-go-discord-bot-token (sops-retrieve-secret "env/DISCORD_BOT_TOKEN"))
  (agent-shell-to-go-discord-authorized-users
   (list (sops-retrieve-secret "env/DISCORD_USER_ID")))
  (agent-shell-to-go-debug t)
  (agent-shell-to-go-projects-directory "~/Projects/")
  (agent-shell-to-go-default-transport 'discord))

(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :hook
  ((agent-shell-mode . corfu-mode)
   (diff-mode . gatsby>>agent-shell-enable-permission-in-diff))
  :custom
  (agent-shell-display-action
   '(display-buffer-in-side-window (side . right) (window-width . 0.33) (slot . 0)))
  (agent-shell-file-completion-enabled t)
  (agent-shell-session-strategy 'new)
  (agent-shell-preferred-agent-config 'claude-code)
  (agent-shell-anthropic-claude-acp-command '("claude-agent-acp"))
  (agent-shell-mcp-servers
   `(((name . "context7")
      (type . "http") (url . "https://mcp.context7.com/mcp")
      (headers
       .
       (((name . "CONTEXT7_API_KEY")
         (value . ,(sops-retrieve-secret "env/CONTEXT7_API_KEY"))))))))

  :config
  ;; diff-mode integration
  (defun gatsby>>agent-shell-enable-permission-in-diff ()
    (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
      ;; Defer so evil's own mode hooks don't overwrite this state afterward.
      (run-with-idle-timer 0 nil #'evil-emacs-state)))

  (defcustom gatsby>agent-shell-configs
    `(("default" . gatsby>>agent-shell-default-config)
      ("self-host" . gatsby>>agent-shell-self-host-config)
      ("openrouter" . gatsby>>agent-shell-openrouter-config)
      ("deepseek" . gatsby>>agent-shell-deepseek-config))
    "List of agent-shell configs available for profile selection."
    :type 'function
    :group 'gatsby)

  (cl-defun gatsby>>agent-shell-make-custom-config
      (config-name &rest args &key env-var &allow-other-keys)
    (let ((config (copy-alist (agent-shell--resolve-preferred-config)))
          (rest-keys (map-delete args :env-var)))
      (push (cons :config-name config-name) config)
      (map-put!
       config
       :client-maker
       (lambda (buffer)
         ;; TODO: make `agent-shell-anthropic-claude-environment' and `agent-shell-anthropic-make-claude-client' protable across agents
         (let* ((agent-shell-anthropic-claude-environment
                 (apply #'agent-shell-make-environment-variables env-var)))
           (agent-shell-anthropic-make-claude-client :buffer buffer))))
      (map-do (lambda (key value) (map-put! config key value)) rest-keys)
      config))

  (defun gatsby>>agent-shell-default-config ()
    (gatsby>>agent-shell-make-custom-config "default"))

  (defun gatsby>>agent-shell-deepseek-config ()
    (gatsby>>agent-shell-make-custom-config
     "deepseek"
     :env-var
     `("ANTHROPIC_BASE_URL"
       "https://api.deepseek.com/anthropic"
       "ANTHROPIC_AUTH_TOKEN"
       ,(sops-retrieve-secret "env/DEEPSEEK_API_KEY")
       "ANTHROPIC_MODEL"
       "deepseek-v4-pro[1m]"
       "ANTHROPIC_DEFAULT_OPUS_MODEL"
       "deepseek-v4-pro[1m]"
       "ANTHROPIC_DEFAULT_SONNET_MODEL"
       "deepseek-v4-pro[1m]"
       "ANTHROPIC_DEFAULT_HAIKU_MODEL"
       "deepseek-v4-flash[1m]"
       "CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC"
       "1"
       "CLAUDE_CODE_EFFORT_LEVEL"
       "max")))

  (defun gatsby>>agent-shell-self-host-config (&optional url)
    (let ((url (or url (completing-read "ANTHROPIC_BASE_URL= " nil))))
      (gatsby>>agent-shell-make-custom-config
       "self-host"
       :env-var
       `("ANTHROPIC_BASE_URL"
         ,url
         "ANTHROPIC_API_KEY"
         ""
         "ANTHROPIC_OAUTH_TOKEN"
         "ollama"))))

  (defun gatsby>>agent-shell-openrouter-config ()
    (gatsby>>agent-shell-make-custom-config
     "openrouter"
     :env-var
     `("ANTHROPIC_BASE_URL" "https://openrouter.ai/api" "ANTHROPIC_AUTH_TOKEN"
       ,(or (sops-retrieve-secret "env/OPENROUTER_API_KEY")
            (user-error "No OPENROUTER_API_KEY set"))
       "ANTHROPIC_API_KEY" "")
     :default-model-id (lambda (&rest _) "deepseek/deepseek-v4-pro")))

  (defun gatsby>>agent-shell-select-config ()
    (let ((cfg
           (map-elt
            gatsby>agent-shell-configs
            (completing-read "Agent config: " (mapcar #'car gatsby>agent-shell-configs)
                             nil t))))
      (funcall cfg)))

  (defun gatsby>>agent-shell-current-client (&optional config-name)
    "Return the buffer of the first nonbusy agent-shell client for the current project.
If CONFIG-NAME is non-nil, only return a client with matching config name.
Return nil if no available client found."
    (let* ((project-root
            (or (and (project-current) (project-root (project-current)))
                default-directory)))
      (thread-last
       (buffer-list) (seq-filter #'buffer-live-p)
       ;; I might have more than one of those, but that's fine - I just need one
       ;; of them
       (cl-find-if
        (lambda (b)
          (with-current-buffer b
            (and (buffer-live-p b)
                 (file-equal-p project-root default-directory)
                 (eq major-mode 'agent-shell-mode)
                 (not (agent-shell--active-requests-p (agent-shell--state)))
                 (or (not config-name)
                     (equal
                      config-name
                      (map-nested-elt
                       (agent-shell--state) '(:agent-config :config-name)))))))))))

  (gatsby>defcommand gatsby>agent-shell-start-or-switch (config)
    "Switch to existing agent shell for current project, or start a new one.
With prefix argument CONFIG, select a config from `gatsby>agent-shell-configs'."
    (let* ((cfg
            (if config
                (gatsby>>agent-shell-select-config)
              (gatsby>>agent-shell-default-config)))
           (current-client
            (gatsby>>agent-shell-current-client
             (when config
               (map-elt cfg :config-name)))))
      (if (not current-client)
          (agent-shell--start :no-focus nil :config cfg :new-session t)
        (display-buffer current-client agent-shell-display-action)
        (switch-to-buffer-other-window current-client)
        (evil-insert-state))))

  (gatsby>defcommand gatsby>agent-shell-resume (config)
    "Start a new agent shell session using prompt strategy.
With prefix argument CONFIG, select a config from `gatsby>agent-shell-configs'."
    (agent-shell--start
     :no-focus nil
     :config
     (if config
         (gatsby>>agent-shell-select-config)
       (gatsby>>agent-shell-default-config))
     :new-session t
     :session-strategy 'prompt))

  (defun gatsby>>agent-shell-pending-permission-p ()
    (map-some
     (lambda (_id data) (map-elt data :permission-request-id))
     (map-elt (agent-shell--state) :tool-calls)))

  (defun gatsby>>agent-shell-activate-permission-button (char-str)
    "Find and activate the most recent permission button whose navigatable char is CHAR-STR.
Returns non-nil if a button was found and activated."
    (save-excursion
      (goto-char (point-max))
      (catch 'found
        (while t
          (let ((match
                 (text-property-search-backward 'agent-shell-permission-button t t)))
            (unless match
              (throw 'found nil))
            (when (string= (string (char-after)) char-str)
              (let* ((map (get-text-property (point) 'keymap))
                     (action (and map (lookup-key map (kbd "RET")))))
                (when action
                  (call-interactively action)
                  (throw 'found t)))))))))

  (gatsby>defcommand gatsby>agent-shell-permission-allow-once ()
    "Allow once (y) if pending permission, else yank."
    (if (gatsby>>agent-shell-pending-permission-p)
        (gatsby>>agent-shell-activate-permission-button "y")
      (call-interactively #'evil-yank)))

  (gatsby>defcommand gatsby>agent-shell-permission-allow-always ()
    "Allow always (!) if pending permission, else no-op."
    (when (gatsby>>agent-shell-pending-permission-p)
      (gatsby>>agent-shell-activate-permission-button "!")))

  (gatsby>defcommand gatsby>agent-shell-permission-view-diff ()
    "View diff (v) if pending permission with diff, else enter visual mode."
    (unless (and (gatsby>>agent-shell-pending-permission-p)
                 (gatsby>>agent-shell-activate-permission-button "v"))
      (call-interactively #'evil-visual-char)))

  (gatsby>defcommand gatsby>agent-shell-next-prompt-or-permission ()
    "Jump to the next permission button if there's a pending permission ask.
     Else go to next/prev prompt"
    (cl-letf*
        ( ;; Make sure `self-insert-command' does not get trigger when in the last prompt
         ((symbol-function #'shell-maker-point-at-last-prompt-p)
          (lambda (&rest _) nil)))
      (if (map-elt (agent-shell--state) :tool-calls)
          (unless (call-interactively #'agent-shell-next-permission-button)
            (call-interactively #'agent-shell-next-item))
        (call-interactively #'agent-shell-next-item))))

  (gatsby>defcommand gatsby>agent-shell-prev-prompt-or-permission ()
    "Jump to the prev permission button if there's a pending permission ask.
     Else go to next/prev prompt"
    (cl-letf*
        ( ;; Make sure `self-insert-command' does not get trigger when in the last prompt
         ((symbol-function #'shell-maker-point-at-last-prompt-p)
          (lambda (&rest _) nil)))
      (if (map-elt (agent-shell--state) :tool-calls)
          (unless (call-interactively #'agent-shell-previous-permission-button)
            (agent-shell-previous-item))
        (agent-shell-previous-item))))

  (defun gatsby>>agent-shell-last-text-output ()
    "Return plain text of last agent-shell response, stripping fragment blocks.
Must be called from within an agent-shell buffer."
    (save-excursion
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (forward-line -1)
      (agent-shell-previous-item)
      (re-search-forward comint-prompt-regexp nil 'noerror)
      (forward-line -1)
      (let ((end (point))
            (start
             (progn
               (text-property-search-backward 'agent-shell-ui-state)
               (point))))
        (let ((s (string-trim (buffer-substring-no-properties start end))))
          (cond
           ((string-match
             "\\`[ \t\n]*```[a-zA-Z]*\n\\(\\(?:.\\|\n\\)*\\)```[ \t\n]*\\'" s)
            (string-trim (match-string 1 s)))
           ((string-match "\\``\\(\\(?:.\\|\n\\)*\\)`\\'" s)
            (string-trim (match-string 1 s)))
           (t
            s))))))

  (defun gatsby>>agent-shell-set-mode-id (mode-id &optional on-success)
    "Set agent-shell session mode to MODE-ID without prompting.
Must be called from within an agent-shell buffer."
    (let ((state (agent-shell--state)))
      (agent-shell--send-request
       :state state
       :client (map-elt state :client)
       :request
       (acp-make-session-set-mode-request
        :session-id (map-nested-elt state '(:session :id))
        :mode-id mode-id)
       :buffer (current-buffer)
       :on-success
       (lambda (_)
         (let ((session (map-elt (agent-shell--state) :session)))
           (map-put! session :mode-id mode-id)
           (map-put! (agent-shell--state) :session session))
         (agent-shell--update-header-and-mode-line)
         (when on-success
           (funcall on-success)))
       :on-failure (lambda (err _) (message "Failed to set mode to %s: %s" mode-id err)))))

  (gatsby>defcommand gatsby>agent-shell-commit ()
    "Automatically generate commit message for currently staged files."
    (unless (magit-anything-staged-p)
      (user-error "Nothing staged - can't generate commit message"))
    (message "Generating commit message...")
    (let*
        ((existing-client (gatsby>>agent-shell-current-client))
         (agent-shell-buffer
          (or existing-client
              (cl-letf (((symbol-function #'agent-shell--display-buffer) #'ignore))
                (agent-shell--start
                 :no-focus t
                 :config (gatsby>>agent-shell-default-config)
                 :new-session t
                 :session-strategy 'new))))
         (proceed
          (lambda ()
            (with-current-buffer agent-shell-buffer
              (let*
                  ((original-mode-id
                    (map-nested-elt (agent-shell--state) '(:session :mode-id)))
                   (do-submit
                    (lambda ()
                      (with-current-buffer agent-shell-buffer
                        (let ((turn-sub)
                              (error-sub))
                          (setq turn-sub
                                (agent-shell-subscribe-to
                                 :shell-buffer agent-shell-buffer
                                 :event 'turn-complete
                                 :on-event
                                 (lambda (_event)
                                   (agent-shell-unsubscribe :subscription turn-sub)
                                   (agent-shell-unsubscribe :subscription error-sub)
                                   (let ((output
                                          (with-current-buffer agent-shell-buffer
                                            (gatsby>>agent-shell-last-text-output))))
                                     (with-current-buffer agent-shell-buffer
                                       (when (and original-mode-id
                                                  (not
                                                   (string=
                                                    original-mode-id
                                                    "bypassPermissions")))
                                         (gatsby>>agent-shell-set-mode-id
                                          original-mode-id)))
                                     (let ((tmpfile (make-temp-file "commit-msg")))
                                       (with-temp-file tmpfile
                                         (insert output))
                                       (magit-run-git-with-editor
                                        "commit" "--edit" "-F" tmpfile))))))
                          (setq error-sub
                                (agent-shell-subscribe-to
                                 :shell-buffer agent-shell-buffer
                                 :event 'error
                                 :on-event
                                 (lambda (event)
                                   (agent-shell-unsubscribe :subscription turn-sub)
                                   (agent-shell-unsubscribe :subscription error-sub)
                                   (if (not existing-client)
                                       (kill-buffer agent-shell-buffer)
                                     (with-current-buffer agent-shell-buffer
                                       (when (and original-mode-id
                                                  (not
                                                   (string=
                                                    original-mode-id
                                                    "bypassPermissions")))
                                         (gatsby>>agent-shell-set-mode-id
                                          original-mode-id))))
                                   (let ((data (map-elt event :data)))
                                     (user-error "Agent shell error: %s (code: %s)"
                                                 (map-elt data :message)
                                                 (map-elt data :code))))))
                          (shell-maker-submit
                           :input "generate a commit message for the currently staged changes"))))))
                (if (string= original-mode-id "bypassPermissions")
                    (funcall do-submit)
                  (gatsby>>agent-shell-set-mode-id "bypassPermissions" do-submit)))))))
      (if existing-client
          (funcall proceed)
        (let ((prompt-sub)
              (error-sub))
          (setq prompt-sub
                (agent-shell-subscribe-to
                 :shell-buffer agent-shell-buffer
                 :event 'prompt-ready
                 :on-event
                 (lambda (_event)
                   (agent-shell-unsubscribe :subscription prompt-sub)
                   (agent-shell-unsubscribe :subscription error-sub)
                   (funcall proceed))))
          (setq error-sub
                (agent-shell-subscribe-to
                 :shell-buffer agent-shell-buffer
                 :event 'error
                 :on-event
                 (lambda (event)
                   (agent-shell-unsubscribe :subscription prompt-sub)
                   (agent-shell-unsubscribe :subscription error-sub)
                   (kill-buffer agent-shell-buffer)
                   (let ((data (map-elt event :data)))
                     (user-error "Agent shell error: %s (code: %s)"
                                 (map-elt data :message)
                                 (map-elt data :code))))))))))

  (with-eval-after-load 'magit
    (transient-append-suffix
     'magit-commit #'magit-commit-create
     '("g" "Create commit with claude-generated message" gatsby>agent-shell-commit)))

  (gatsby>defcommand gatsby>agent-shell-send-or-queue-prompt ()
    "Send the current prompt if the shell is available. Otherwise put it in the request queue."
    (if (agent-shell--active-requests-p (agent-shell--state))
        (call-interactively #'agent-shell-queue-request)
      (call-interactively #'shell-maker-submit)))

  :evil-bind
  ((:maps normal)
   ("SPC a a" . #'gatsby>agent-shell-start-or-switch)
   ("SPC a r" . #'gatsby>agent-shell-resume)
   (:maps (visual normal))
   ("SPC a s" . #'agent-shell-send-file)
   (:maps agent-shell-mode-map :states insert)
   ("RET" . #'comint-accumulate)
   ("C-r" . #'agent-shell-search-history)
   (:maps agent-shell-mode-map :states (normal visual insert))
   ("C-c C-l" . #'comint-clear-buffer)
   ("C-c C-c" . #'agent-shell-interrupt)
   ("M-RET" . #'gatsby>agent-shell-send-or-queue-prompt)
   (:maps agent-shell-mode-map :states normal)
   ("y" . #'gatsby>agent-shell-permission-allow-once)
   ("!" . #'gatsby>agent-shell-permission-allow-always)
   ("v" . #'gatsby>agent-shell-permission-view-diff)
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
