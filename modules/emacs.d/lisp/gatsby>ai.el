;;; gatsby>ai.el --- ai code helper -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

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

;; TODO
;; there are a few things lacking
;; - remote support (https://github.com/xenodium/agent-shell/issues/122)
(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :hook (agent-shell-mode . corfu-mode)
  :custom-face (header-line ((t :inherit default)))
  :custom
  (agent-shell-display-action
   '(display-buffer-in-side-window (side . right) (window-height . 0.3) (slot . 0)))
  (agent-shell-file-completion-enabled t)
  (agent-shell-session-strategy 'latest)
  :commands (agent-shell-anthropic-start-claude-code gatsby>agent-shell-toggle)
  :config
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         "ANTHROPIC_BASE_URL"
         "https://openrouter.ai/api"
         "ANTHROPIC_AUTH_TOKEN"
         (gatsby>>get-ai-api-key)
         "ANTHROPIC_API_KEY"
         ""))

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
      (cond
       (force-new
        ;; FIXME: this let does not work
        (let ((agent-shell-session-strategy 'prompt))
          (call-interactively #'agent-shell-anthropic-start-claude-code)))
       ((not current-client)
        (call-interactively #'agent-shell-anthropic-start-claude-code))
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
                (map-nested-elt state '(:session :model-id)) "uninitiated")))
      (format "%s %s"
              (propertize model 'font-lock-face 'font-lock-negation-char-face)
              (if mode
                  (propertize (format "(%s)" mode) 'font-lock-face 'font-lock-type-face)
                ""))))

  (advice-add #'agent-shell--make-header :override #'gatsby>>agent-shell-header)
  (advice-add
   #'agent-shell-anthropic--claude-code-ascii-art
   :override
   (lambda ()
     ""
     "[claude code]"))

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
   ([remap kill-buffer-and-window] . #'delete-window)))

;; for commit message and simple rewrite
(use-package gptel
  :ensure (:host github :repo "karthink/gptel")
  :custom
  (gptel-model "")
  (gptel-use-header-line nil)
  (gptel-cache nil)
  (gptel-use-tools nil)
  :hook (gptel-post-response . (lambda (&rest _) (evil-normal-state)))
  :config
  (setq gptel-backend
        (gptel-make-openai
         "OpenRouter"
         :host "openrouter.ai"
         :endpoint "/api/v1/chat/completions"
         :stream t
         :key #'gatsby>>get-ai-api-key
         :models '(google/gemini-3-flash-preview)))
  :evil-bind ((:maps (normal visual)) ("SPC a r" . #'gptel-rewrite) ("SPC a C-g" . #'gptel-abort)))

(use-package gptel-magit
  :ensure (:host github :repo "ragnard/gptel-magit")
  :hook (magit-mode . gptel-magit-install))

(provide 'gatsby>ai)
;;; gatsby>ai.el ends here
