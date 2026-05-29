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
  :after agent-shell
  :config
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
               "Current Agents: " `("new" ,@ (mapcar #'car collections)))))
        (if (equal picked "new")
            (gatsby>agent-shell-launch arg)
          (let ((shell (map-elt collections picked)))
            (display-buffer shell agent-shell-display-action)
            (switch-to-buffer-other-window shell)
            (evil-insert-state))))
      (gatsby>agent-shell-launch arg)))

  :evil-bind
  ((:maps normal)
   ("SPC a a" . #'gatsby>>agent-shell-manager-launch)
   ("SPC a p" . #'agent-shell-manager-toggle)
   (:maps agent-shell-manager-mode-map :states motion)
   ("RET" . #'agent-shell-manager-goto)
   ("C-c C-c" . #'agent-shell-interrupt)
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
  (agent-shell-to-go-discord-guild-id (sops-retrieve-secret "env/DISCORD_GUILD_ID"))
  (agent-shell-to-go-discord-bot-token (sops-retrieve-secret "env/DISCORD_BOT_TOKEN"))
  (agent-shell-to-go-discord-authorized-users
   (list (sops-retrieve-secret "env/DISCORD_USER_ID")))
  (agent-shell-to-go-show-tool-output nil)
  (agent-shell-to-go-projects-directory "~/Projects/")
  (agent-shell-to-go-default-transport 'discord))

(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :hook ((agent-shell-mode . corfu-mode))
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
  (defcustom gatsby>agent-shell-configs
    `(("claude" . (:base gatsby>>agent-shell-claude-config))
      ("deepseek" .
       (:base
        gatsby>>agent-shell-claude-config
        :env
        ("ANTHROPIC_BASE_URL"
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
      ("openrouter" .
       (:base
        gatsby>>agent-shell-claude-config
        :env
        ("ANTHROPIC_BASE_URL"
         "https://openrouter.ai/api"
         "ANTHROPIC_AUTH_TOKEN"
         ,(sops-retrieve-secret "env/OPENROUTER_API_KEY")
         "ANTHROPIC_API_KEY"
         "")
        :default-model-id "google/gemini-3.5-flash")))
    "Alist of named agent-shell configuration profiles.
Each entry is (NAME . PLIST) where NAME is a string identifier and PLIST
must contain :base (a config-builder function symbol accepting keyword args).
Optional plist keys: :env — a flat list of (VAR VALUE ...) pairs injected as
environment variables."
    :type '(alist :key-type string :value-type sexp)
    :group 'gatsby)

  (defun gatsby>>agent-shell-claude-config (&rest args)
    "Build an Anthropic/Claude agent-shell config alist applying ENV."
    (let ((config (agent-shell-anthropic-make-claude-code-config))
          (env (plist-get args :env)))
      ;; handle env
      (map-put!
       config
       :client-maker
       (lambda (buffer)
         (let ((agent-shell-anthropic-claude-environment
                (apply #'agent-shell-make-environment-variables env)))
           (agent-shell-anthropic-make-claude-client :buffer buffer))))
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
    (let* ((names (mapcar #'car gatsby>agent-shell-configs))
           (name (completing-read "Agent config: " names nil t)))
      (cl-find name gatsby>agent-shell-configs :test #'equal :key #'car)))

  (defmacro gatsby>>agent-shell-maybe-worktree (&rest body)
    "Execute BODY inside a fresh git worktree of if it has uncommitted changes.
Falls through to run BODY directly when worktree isolation is not needed."
    `(if (and
          (agent-shell-worktree--git-repo-root)
          (gatsby>>agent-shell-current-client)
          (not
           (string-empty-p
            (string-trim
             (shell-command-to-string "git status --porcelain | grep -v '^??'"))))
          (y-or-n-p
           "Uncommitted change detected in the current repo, create new worktree (C-g to cancel)? "))
         (let* ((worktrees-dir (agent-shell--dot-subdir "worktrees"))
                (worktree-path
                 (expand-file-name (agent-shell-worktree--generate-name) worktrees-dir))
                (base-sha (string-trim (shell-command-to-string "git rev-parse HEAD"))))
           (when (file-exists-p worktree-path)
             (user-error "Directory already exists: %s" worktree-path))
           (make-directory (file-name-directory worktree-path) t)
           (let ((output
                  (shell-command-to-string
                   (format "git worktree add %s 2>&1"
                           (shell-quote-argument worktree-path)))))
             (unless (file-exists-p worktree-path)
               (user-error "Failed to create worktree: %s" output))
             (let ((default-directory worktree-path))
               (when (featurep 'envrc)
                 (ignore-errors
                   (envrc-allow)))
               (let ((shell
                      (progn
                        ,@body)))
                 (with-current-buffer shell
                   (add-hook
                    'kill-buffer-hook
                    (lambda ()
                      (let* ((default-directory worktree-path)
                             (uncommitted
                              (not
                               (string-empty-p
                                (string-trim
                                 (shell-command-to-string "git status --porcelain")))))
                             (unmerged
                              (not
                               (string-empty-p
                                (string-trim
                                 (shell-command-to-string
                                  (format "git log %s..HEAD --oneline"
                                          (shell-quote-argument base-sha))))))))
                        (if (or (not (or uncommitted unmerged))
                                (yes-or-no-p
                                 (format "Worktree %s has %s. Remove anyway? "
                                         worktree-path
                                         (cond
                                          ((and uncommitted unmerged)
                                           "uncommitted and unmerged changes")
                                          (unmerged
                                           "unmerged commits")
                                          (t
                                           "uncommitted changes")))))
                            (shell-command
                             (format "git worktree remove --force %s"
                                     (shell-quote-argument worktree-path)))
                          (magit-status))))))))))
       ,@body))

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

  (gatsby>defcommand gatsby>agent-shell-launch (resume)
    "Launch a new agent-shell session using the selected config profile.
With prefix argument RESUME, use the prompt session strategy to continue a previous session."
    (let* ((cfg (gatsby>>agent-shell-select-config))
           (config (gatsby>>agent-shell-build-config cfg))
           (strategy
            (if resume
                'prompt
              'new)))
      (gatsby>>agent-shell-maybe-worktree
       (agent-shell--start
        :no-focus nil
        :config config
        :new-session t
        :session-strategy strategy))))

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

  (defvar-local gatsby>>agent-shell-diff-file nil
    "Original file name for the current agent-shell diff buffer.")

  (defun gatsby>>agent-shell-diff-store-file (_old _new file buf)
    (when (featurep 'side-by-side-diff)
      (with-current-buffer buf
        (setq-local gatsby>>agent-shell-diff-file file))))

  (advice-add
   'agent-shell-diff--insert-diff
   :after #'gatsby>>agent-shell-diff-store-file)

  (defun gatsby>>ssdf-agent-shell-do (action)
    "Run ACTION on the background agent-shell diff buffer, then quit ssdf."
    (when-let* ((buf
                 (seq-find
                  (lambda (b)
                    (with-current-buffer b
                      (derived-mode-p 'agent-shell-diff-mode)))
                  (buffer-list))))
      (with-current-buffer buf
        (funcall action)))
    (ssdf-quit))

  (gatsby>defcommand gatsby>>ssdf-agent-shell-accept ()
    "Accept the pending agent-shell diff and quit ssdf."
    (gatsby>>ssdf-agent-shell-do #'agent-shell-diff-accept-all))

  (gatsby>defcommand gatsby>>ssdf-agent-shell-reject ()
    "Reject the pending agent-shell diff and quit ssdf."
    (gatsby>>ssdf-agent-shell-do #'agent-shell-diff-reject-all))

  (gatsby>defcommand gatsby>agent-shell-permission-view-diff ()
    "View diff (v) if pending permission with diff, else enter visual mode."
    (if (and (gatsby>>agent-shell-pending-permission-p)
             (gatsby>>agent-shell-activate-permission-button "v"))
        (when (featurep 'side-by-side-diff)
          (let ((pre-config (current-window-configuration)))
            (when-let* ((buf
                         (seq-find
                          (lambda (b)
                            (with-current-buffer b
                              (derived-mode-p 'agent-shell-diff-mode)))
                          (buffer-list))))
              (let* ((file
                      (or (buffer-local-value 'gatsby>>agent-shell-diff-file buf)
                          "file"))
                     (diff-text
                      (with-current-buffer buf
                        (buffer-substring-no-properties (point-min) (point-max)))))
                (set-window-configuration pre-config)
                (ssdf-display-diff
                 (concat "diff --git a/" file " b/" file "\n" diff-text))
                (dolist (ssdf-buf
                         (list
                          (get-buffer ssdf--left-name) (get-buffer ssdf--right-name)))
                  (when ssdf-buf
                    (with-current-buffer ssdf-buf
                      (evil-local-set-key
                       'motion (kbd "y") #'gatsby>>ssdf-agent-shell-accept)
                      (evil-local-set-key
                       'motion (kbd "C-c C-c") #'gatsby>>ssdf-agent-shell-reject))))))))
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
        ((existing-client (car (gatsby>>agent-shell-current-client 'available)))
         (agent-shell-buffer
          (or existing-client
              (cl-letf (((symbol-function #'agent-shell--display-buffer) #'ignore))
                (agent-shell--start
                 :no-focus t
                 :config
                 (gatsby>>agent-shell-build-config (cdar gatsby>agent-shell-configs))
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
   ("SPC a r" . #'gatsby>run-agent-on-remote)
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

(provide 'gatsby>ai)
;;; gatsby>ai.el ends here
