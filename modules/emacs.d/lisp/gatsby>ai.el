;;; gatsby>ai.el --- ai code helper -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

;; TODO
;; there are a few things lacking
;; - remote support (https://github.com/xenodium/agent-shell/issues/122)
;; - capf has issues (https://github.com/xenodium/agent-shell/issues/60)
(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :hook (agent-shell-mode . corfu-mode)
  :custom-face (header-line ((t :inherit default)))
  :custom
  (agent-shell-display-action
   '(display-buffer-in-side-window (side . right) (window-height . 0.3) (slot . 0)))
  (agent-shell-file-completion-enabled t)
  :commands (agent-shell-opencode-start-agent gatsby>agent-shell-toggle)
  :init
  (defun gatsby>>get-ai-api-key ()
    "run passage to get the openai_api_key. Return nil if no key is found"
    (thread-first
     "direnv exec %s passage show openrouter-api"
     (format (expand-file-name gatsby>dotfiles-repo-location))
     (shell-command-to-string)
     (string-trim)
     (string-split "\n")
     (last)
     (car)))

  :config
  (setq agent-shell-opencode-environment
        (agent-shell-make-environment-variables
         "OPENROUTER_API_KEY"
         (gatsby>>get-ai-api-key)
         "OPENCODE_MODEL"
         "openrouter/google/gemini-3-flash-preview"
         "OPENCODE_SMALL_MODEL"
         "openrouter/z-ai/glm-4.5-air:free"))

  (gatsby>defcommand gatsby>agent-shell-toggle (force-new)
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
      (if (or force-new (not current-client))
          (call-interactively #'agent-shell-opencode-start-agent)
        (display-buffer current-client agent-shell-display-action)
        (switch-to-buffer-other-window current-client)
        (evil-insert-state))))

  ;; TODO: maybe doesn't need this when upstream fixed it?
  (gatsby>defcommand gatsby>agent-shell-send-file (ask)
    (cond
     ((region-active-p)
      (agent-shell-send-region))
     ((buffer-file-name)
      (agent-shell--insert-to-shell-buffer
       :text
       (agent-shell--get-files-context
        :files (list (file-relative-name buffer-file-name (agent-shell-cwd))))))
     ((or ask (derived-mode-p 'agent-shell-mode))
      (agent-shell--insert-to-shell-buffer
       :text
       (agent-shell--get-files-context
        :files
        (or (list (completing-read "Send file: " (agent-shell--project-files)))
            (user-error "No file to send")))))
     (t
      (user-error "No file to send"))))

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
                (map-nested-elt state '(:session :model-id)) "uninitiated")))
      (format "%s %s"
              (propertize model 'font-lock-face 'font-lock-negation-char-face)
              (if mode
                  (propertize (format "(%s)" mode) 'font-lock-face 'font-lock-type-face)
                ""))))

  (advice-add #'agent-shell--make-header :override #'gatsby>>agent-shell-header)

  (defmacro gatsby>agent-shell-with-model (model-id &rest body)
    (declare (indent 1))
    `(let* ((current-model-id
             (map-nested-elt (agent-shell--state) '(:session :model-id)))
            (session-id (map-nested-elt (agent-shell--state) '(:session :id)))
            (client (map-elt (agent-shell--state) :client))
            (on-success-fn
             (lambda (_response)
               ,@body
               (acp-send-request
                :client client
                :request
                (acp-make-session-set-model-request
                 :session-id session-id
                 :model-id current-model-id)))))
       (acp-send-request
        :client client
        :request
        (acp-make-session-set-model-request :session-id session-id :model-id ,model-id)
        :on-success on-success-fn
        :on-failure
        (lambda (error _raw-message)
          (user-error "`gatsby>agent-shell-with-model`: fail to run with model %s"
                      ,model-id)))))

  (gatsby>defcommand gatsby>agent-shell-commit ()
    "Run the /commit command to create commit message of the current staged files in the
  project agent-shell. Automatically create an agent-shell if none exists.

  Raise error if
  - `git-commit-message' skill or /commit command does not exists.
  - the current agent-shell is busy (via `shell-maker--busy')"
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

      ;; start a shell if none exists
      (unless current-client
        (setq current-client
              (let ((agent-shell-display-action '(display-buffer-no-window)))
                (agent-shell--start
                 :no-focus t
                 :config (agent-shell-opencode-make-agent-config)
                 :new-session t))))

      (with-current-buffer current-client
        (when shell-maker--busy
          (user-error "Current shell is busy, try again later"))
        (gatsby>agent-shell-with-model
         "openrouter/z-ai/glm-4.5-air:free"
         (message "Generating commit message...")
         (shell-maker-submit :input "/commit")))))

  ;; entrance point
  (with-eval-after-load 'magit
    (transient-append-suffix
     'magit-commit
     #'magit-commit-create
     '("g" "Generate commit" gatsby>agent-shell-commit)))

  :evil-bind
  ((:maps normal)
   ("SPC a a" . #'gatsby>agent-shell-toggle)
   (:maps (visual normal))
   ("SPC a s" . #'gatsby>agent-shell-send-file)
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
   ([remap kill-buffer-and-window] . #'delete-window)))

(provide 'gatsby>ai)
;;; gatsby>ai.el ends here
