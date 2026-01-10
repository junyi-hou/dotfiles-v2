;;; gatsby>ai.el --- ai code helper -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

;; TODO
;; there are a few things lacking
;; - remote support (https://github.com/xenodium/agent-shell/issues/122)
;; - capf has issues (https://github.com/xenodium/agent-shell/issues/60)
;; for now let's use eca which provide better user-experience
(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :hook (agent-shell-mode . corfu-mode)
  :custom
  (agent-shell-display-action
   '(display-buffer-in-side-window (side . right) (window-width . 0.25) (slot . 0)))
  (agent-shell-file-completion-enabled t)
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
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         "ANTHROPIC_BASE_URL"
         "https://openrouter.ai/api"
         "ANTHROPIC_AUTH_TOKEN"
         (gatsby>>get-ai-api-key)
         "ANTHROPIC_API_KEY"
         ""))

  ;; ;; tramp integration
  ;; ;; first, mimic `agent-shell--resolve-devcontainer-path'
  ;; (defun agent-shell--resolve-tramp-path (path)
  ;;   "TODO"
  ;;   (if-let ((remote (file-remote-p default-directory)))
  ;;     ;; we are on remote path
  ;;     (if (file-remote-p path)
  ;;         (file-remote-p path 'localname)
  ;;       (concat remote path))
  ;;     ;; we are in local path
  ;;     path))

  ;; ;; set `agent-shell-path-resolver-function'
  ;; (setq
  ;;  ;; agent-shell-path-resolver-function #'agent-shell--resolve-tramp-path
  ;;  agent-shell-header-style 'text)
  )

(use-package eca
  :ensure (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el"))
  :hook (eca-chat-mode . corfu-mode)
  :custom
  (eca-rewrite-diff-tool 'simple-diff)
  (eca-chat-window-width 0.25)
  :custom-face (header-line ((t :inherit default)))
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

  (defun gatsby>>add-api-key-to-eca-config (fn &rest args)
    (with-environment-variables (("OPENROUTER_API_KEY" (gatsby>>get-ai-api-key)))
      (apply fn args)))

  (advice-add #'eca :around #'gatsby>>add-api-key-to-eca-config)

  (defun gatsby>>eca-server-command-tramp-aware (fn &rest args)
    "Make eca-process--server-command TRAMP-aware.
If the current project is on a remote host, look for the eca binary
on the remote system instead of locally."
    (if (file-remote-p default-directory)
        ;; Remote project: look for eca on remote system
        (let ((remote-eca (executable-find "eca" t)))
          (if remote-eca
              (list :decision 'system :command (list remote-eca "server"))
            (user-error
             "eca not found on remote system. Please install eca on the remote host")))
      ;; Local project: use default behavior
      (apply fn args)))

  (advice-add
   #'eca-process--server-command
   :around #'gatsby>>eca-server-command-tramp-aware)

  :evil-bind
  ((:maps normal)
   ("SPC a a" . #'eca)
   ("SPC a f" . #'eca-chat-select)
   (:maps visual)
   ("SPC a w" . #'eca-rewrite)
   (:maps eca-chat-mode-map :states normal)
   ("<tab>" . #'eca-chat--key-pressed-tab)
   ("z o" . #'eca-chat-toggle-expandable-block)
   ("z c" . #'eca-chat-toggle-expandable-block)
   ("<" . #'eca-chat-go-to-prev-expandable-block)
   (">" . #'eca-chat-go-to-next-expandable-block)
   ("`" . #'eca-transient-menu)
   ("q" . #'delete-window)
   (:maps eca-chat-mode-map :states insert)
   ("<return>" . #'eca-chat--key-pressed-newline)
   ("S-<return>" . #'markdown-insert-list-item)
   ("M-<return>" . #'eca-chat--key-pressed-return)))

(provide 'gatsby>ai)
;;; gatsby>ai.el ends here
