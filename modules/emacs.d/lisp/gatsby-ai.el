;;; gatsby-ai.el --- ai code helper -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby--utility)

(use-package agent-shell-tramp
  :ensure (:host github :repo "junyi-hou/agent-shell-tramp")
  :config (agent-shell-tramp-mode 1))

;; managing agents
(use-package agent-shell-manager
  :ensure (:host github :repo "jethrokuan/agent-shell-manager")
  :after agent-shell
  :config
  (gatsby>defcommand gatsby>>agent-shell-manager-goto ()
    "Go to the agent shell at point using the default display action."
    (let ((agent-shell-display-action nil))
      (call-interactively #'agent-shell-manager-goto)))

  (gatsby>defcommand gatsby>>agent-shell-manager-launch (arg)
    "Switch to an existing agent shell for the current project, or launch a new one.
Shows running agents for the project; selecting one focuses it, selecting \"new\" calls
`gatsby>agent-shell-launch' with ARG."
    (if-let* ((collections (gatsby>>agent-shell-current-client)))
      (let* ((collections
              (mapcar
               (lambda (b)
                 (cons
                  (format "%s (%s) [%s]"
                          (buffer-name b)
                          (agent-shell-manager--get-model-id b)
                          (agent-shell-manager--get-combined-status b))
                  b))
               (gatsby>>agent-shell-current-client)))
             (picked
              (completing-read
               "Current Agents: "
               `("new" "new (in a new worktree)" ,@ (mapcar #'car collections)))))

        (cond
         ((equal picked "new")
          (gatsby>agent-shell-launch arg))
         ((equal picked "new (in a new worktree)")
          (cl-flet ((agent-shell
                     (&rest _)
                     (when (and (featurep 'envrc)
                                (locate-dominating-file default-directory ".envrc"))
                       (envrc-allow))
                     (gatsby>agent-shell-launch arg)))))
         (t
          (let ((shell (map-elt collections picked)))
            (select-window (display-buffer shell agent-shell-display-action))
            (evil-insert-state)))))
      (gatsby>agent-shell-launch arg)))

  :evil-bind
  ((:maps normal)
   ("SPC a a" . #'gatsby>>agent-shell-manager-launch)
   ("SPC a p" . #'agent-shell-manager-toggle)
   (:maps agent-shell-manager-mode-map :states motion)
   ("RET" . #'gatsby>>agent-shell-manager-goto)
   ("C-c C-c" . #'agent-shell-interrupt)
   ("M-j" . #'evil-next-visual-line)
   ("M-k" . #'evil-previous-visual-line)
   ("m" . #'agent-shell-manager-set-mode)
   ("M" . #'agent-shell-manager-set-model)
   ("q" . #'quit-window)
   ("c" . #'gatsby>agent-shell-launch)
   ("g" . #'agent-shell-manager-refresh)
   ("r" . #'agent-shell-manager-restart)
   ("x" . #'agent-shell-manager-kill)
   ("d" . #'agent-shell-manager-delete-killed)))

(use-package agent-shell-to-go
  :ensure (:host github :repo "junyi-hou/agent-shell-to-go" :branch "stable")
  :custom
  (agent-shell-to-go-discord-guild-id
   (sops-get-secret-try-env-variable "env/DISCORD_GUILD_ID"))
  (agent-shell-to-go-discord-bot-token
   (sops-get-secret-try-env-variable "env/DISCORD_BOT_TOKEN"))
  (agent-shell-to-go-discord-authorized-users
   (list (sops-get-secret-try-env-variable "env/DISCORD_USER_ID")))
  (agent-shell-to-go-show-tool-output nil)
  (agent-shell-to-go-projects-directory "~/Projects/")
  (agent-shell-to-go-default-transport 'discord)
  (agent-shell-to-go-start-agent-function
   (lambda () (gatsby>agent-shell-launch nil t))))

(gatsby>use-internal-package agent-workflows
  :commands (agent-workflows-commit agent-workflows-review)
  :hook
  (agent-workflows-review-mode
   . (lambda () (gatsby>>put-mode-to-evil-state 'agent-workflows-review-mode 'motion)))
  :init
  (with-eval-after-load 'magit
    (transient-append-suffix
     'magit-commit #'magit-commit-create
     '("g" "Create commit with claude-generated message" agent-workflows-commit)))

  :config
  (require 'agent-shell)
  (setq agent-workflows-agent-config
        (gatsby>>agent-shell-build-config (car gatsby>agent-shell-configs)))

  :evil-bind
  ((:maps normal)
   ("SPC a r" . #'agent-workflows-review)
   (:maps agent-workflows-review-mode-map :states (normal motion))
   ("n" . #'agent-workflows-review-next)
   ("p" . #'agent-workflows-review-prev)
   ("f" . #'agent-workflows-review-fix)
   ("RET" . #'agent-workflows-review-open-source)
   ("o" . #'agent-workflows-review-open-source)
   ("q" . #'quit-window)))

(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :hook
  ((agent-shell-mode . corfu-mode)
   (agent-shell-mode . gatsby>>agent-shell-disable-scroll-after-submit))
  :init
  (defun gatsby>>agent-shell-disable-scroll-after-submit (&rest _)
    "Sending prompt does not automatically scroll all the way to the bottom."
    (setq-local comint-scroll-show-maximum-output nil))

  :custom
  (agent-shell-display-action
   '(display-buffer-in-side-window (side . right) (window-width . 0.33) (slot . 0)))
  (agent-shell-file-completion-enabled t)
  (agent-shell-session-strategy 'new)
  (agent-shell-mcp-servers
   `(((name . "context7")
      (type . "http") (url . "https://mcp.context7.com/mcp")
      (headers
       .
       (((name . "CONTEXT7_API_KEY")
         (value . ,(sops-get-secret-try-env-variable "env/CONTEXT7_API_KEY"))))))
     ((name . "github")
      (type . "http") (url . "https://api.githubcopilot.com/mcp/")
      (headers
       .
       (((name . "Authorization")
         (value
          .
          ,(format "Bearer %s"
                   (sops-get-secret-try-env-variable "env/GITHUB_PAT_KEY")))))))))

  :config
  (defcustom gatsby>agent-shell-configs
    `(("codex" .
       (:base
        gatsby>>agent-shell-codex-config
        :default-model-id (lambda (&rest _) "gpt-5.5")
        :default-session-mode-id (lambda (&rest _) "full-access"))))
    "Alist of named agent-shell configuration profiles.
Each entry is (NAME . PLIST) where NAME is a string identifier and PLIST
must contain :base (a config-builder function symbol accepting keyword args).
Optional plist keys: :env — a flat list of (VAR VALUE ...) pairs injected as
environment variables."
    :type '(alist :key-type string :value-type sexp)
    :group 'gatsby)

  (defun gatsby>>agent-shell-codex-config (&rest args)
    (let ((config (agent-shell-openai-make-codex-config))
          (env (plist-get args :env)))
      ;; handle env
      (map-put!
       config
       :client-maker
       (lambda (buffer)
         (let ((agent-shell-openai-codex-environment
                (apply #'agent-shell-make-environment-variables env)))
           (agent-shell-openai-make-codex-client :buffer buffer))))
      ;; handle rest of the config keys
      (thread-last
       args
       (map-filter (lambda (key _) (not (eq key :env))))
       (map-do (lambda (key value) (map-put! config key value))))
      config))

  (defun gatsby>>agent-shell-build-config (cfg)
    "Build a runnable agent-shell config alist from profile plist CFG."
    (let* ((cfg (cdr cfg))
           (config-fn (plist-get cfg :base))
           (args (map-delete cfg :base)))
      (apply config-fn args)))

  (defun gatsby>>agent-shell-select-config ()
    "Prompt user to select a profile from `gatsby>agent-shell-configs'.
Returns the matching cons cell (NAME . PLIST)."
    (let* ((names (mapcar #'car gatsby>agent-shell-configs)))
      (if (cdr names)
          (let ((name (completing-read "Agent config: " names nil t)))
            (cl-find name gatsby>agent-shell-configs :test #'equal :key #'car))
        (cl-find (car names) gatsby>agent-shell-configs :test #'equal :key #'car))))

  (defun gatsby>>agent-shell-current-client (&optional available)
    "Return a list of agent-shell buffers belonging to the current project.
If AVAILABLE is non-nil, exclude buffers that have active requests in flight."
    (let* ((project-root
            (or (and (project-current) (project-root (project-current)))
                default-directory)))
      (seq-filter
       (lambda (b)
         (with-current-buffer b
           (and (buffer-live-p b)
                (file-equal-p project-root default-directory)
                (or (not available)
                    (and available
                         (not
                          (agent-shell--active-requests-p (agent-shell--state))))))))
       (agent-shell-buffers))))

  (defun gatsby>agent-shell-launch (resume &optional use-default-config)
    "Launch a new agent-shell session using the selected config profile.
With prefix argument RESUME, use the prompt session strategy to continue a previous session.
If USE-DEFAULT-CONFIG is non-nil, use the first entry of `gatsby>agent-shell-configs'
without prompting."
    (interactive "P")
    (let* ((cfg
            (if use-default-config
                (car gatsby>agent-shell-configs)
              (gatsby>>agent-shell-select-config)))
           (config (gatsby>>agent-shell-build-config cfg))
           (strategy
            (if resume
                'prompt
              'new)))
      (agent-shell--start
       :no-focus nil
       :config config
       :new-session t
       :session-strategy strategy)))

  (defconst gatsby>run-agent-remote-hosts-cache-file
    (no-littering-expand-var-file-name "run-agent-remote-hosts.el")
    "Cache file for run-agent remote SSH hosts.")

  (gatsby>defcommand gatsby>run-agent-on-remote ()
    "Run agent on a remote server via the run-agent script."
    (let* ((cfg (gatsby>>agent-shell-select-config))
           (env-var (map-elt cfg :env))
           (ssh-host
            (completing-read
             "SSH host: "
             (gatsby>retrieve-or-save-item gatsby>run-agent-remote-hosts-cache-file)))
           (_
            (gatsby>retrieve-or-save-item gatsby>run-agent-remote-hosts-cache-file
                                          ssh-host))
           (remote-dir
            (read-directory-name "Remote directory: "
                                 (format "/rpc:%s:~/Projects/" ssh-host)))
           (localname (tramp-file-name-localname (tramp-dissect-file-name remote-dir)))
           (env-args
            (let ((args nil)
                  (rest env-var))
              (while rest
                (push (format "%s=%s" (pop rest) (pop rest)) args))
              (nreverse args))))
      (let ((buf
             (compile
              (format "%s %s %s %s"
                      "run-agent"
                      localname
                      ssh-host
                      (mapconcat #'shell-quote-argument env-args " "))))
            (config-name (map-elt cfg :config-name)))
        (with-current-buffer buf
          (add-hook 'compilation-finish-functions
                    (lambda (_buf msg)
                      (if (string-match-p "finished" msg)
                          (message "Ran %s config on %s for %s"
                                   config-name
                                   ssh-host
                                   localname)
                        (message "Failed: %s config on %s for %s"
                                 config-name
                                 ssh-host
                                 localname)))
                    nil t)))))

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
    (if (and (gatsby>>agent-shell-pending-permission-p)
             (gatsby>>agent-shell-activate-permission-button "v"))
        nil
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

  (gatsby>defcommand gatsby>agent-shell-send-or-queue-prompt ()
    "Send the current prompt if the shell is available. Otherwise put it in the request queue."
    (if (agent-shell--active-requests-p (agent-shell--state))
        (call-interactively #'agent-shell-queue-request)
      (call-interactively #'shell-maker-submit)))

  :evil-bind
  (
   ;; (:maps normal)
   ;; ("SPC a r" . #'gatsby>run-agent-on-remote)
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
   ("z o" . #'agent-shell-ui-toggle-fragment)
   ("z c" . #'agent-shell-ui-toggle-fragment)
   ("m" . #'agent-shell-set-session-mode)
   ("M" . #'agent-shell-set-session-model)
   ("t" . #'agent-shell-set-session-thought-level)
   ("q" . #'delete-window)
   ("M-v" . #'agent-shell-yank-dwim)
   ([remap kill-buffer-and-window] . #'delete-window)
   (:maps agent-shell-diff-mode-map)
   (">" . #'diff-hunk-next)
   ("<" . #'diff-hunk-prev)
   ("a" . #'diff-apply-hunk)
   ("s" . #'diff-split-hunk)
   ("A" . #'diff-apply-buffer)
   ("q" . #'kill-buffer-and-window)
   ("y" . #'agent-shell-diff-accept-all)
   ("C-c C-c" . #'agent-shell-diff-reject-all)))

(provide 'gatsby-ai)
;;; gatsby-ai.el ends here
