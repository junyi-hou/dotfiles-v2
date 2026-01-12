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
  :custom-face (header-line ((t :inherit default)))
  :custom
  (agent-shell-header-style 'text)
  (agent-shell-display-action
   '(display-buffer-in-side-window (side . right) (window-width . 0.25) (slot . 0)))
  (agent-shell-file-completion-enabled t)
  :commands (agent-shell-anthropic-start-claude-code gatsby>claude-code-toggle)
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

  (gatsby>defcommand gatsby>claude-code-toggle (force-new)
    (let* ((project-root (and (project-current) (project-root (project-current))))
           (current-client
            (thread-last
             (buffer-list)
             (seq-filter #'buffer-live-p)
             (seq-filter (lambda (b) (string-prefix-p "Claude Code " (buffer-name b))))
             (cl-find-if
              (lambda (b)
                (with-current-buffer b
                  (file-equal-p
                   default-directory
                   (and (project-current) (project-root (project-current))))))))))
      (if (or force-new (not current-client))
          (call-interactively #'agent-shell-anthropic-start-claude-code)
        (display-buffer current-client agent-shell-display-action)
        (switch-to-buffer-other-window current-client)
        (evil-insert-state))))

  ;; ;; tramp integration
  ;; ;; first, mimic `agent-shell--resolve-devcontainer-path'
  ;; (defun agent-shell--resolve-tramp-path (path)
  ;;   "TODO"
  ;;   (if-let* ((remote (file-remote-p default-directory)))
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
  :evil-bind
  ((:maps normal)
   ("SPC a a" . #'gatsby>claude-code-toggle)
   (:maps agent-shell-mode-map :states insert)
   ("RET" . #'comint-accumulate)
   ("M-RET" . #'comint-send-input)
   (:maps agent-shell-mode-map :states normal)
   ("q" . #'delete-window)
   ([remap kill-buffer-and-window] . #'delete-window)))

(provide 'gatsby>ai)
;;; gatsby>ai.el ends here
