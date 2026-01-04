;;; gatsby>ai.el --- ai code helper -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

;; TODO
;; there are a few things lacking
;; - remote support (https://github.com/xenodium/agent-shell/issues/122)
;; - completion (https://github.com/xenodium/agent-shell/issues/60)
;; for now let's use eca which provide better user-experience
;; (use-package agent-shell
;;   :ensure (:host github :repo "xenodium/agent-shell")
;;   :hook (agent-shell-mode . corfu-mode)
;;   :custom
;;   (agent-shell-anthropic-claude-environment
;;    (agent-shell-make-environment-variables
;;     "ANTHROPIC_BASE_URL"
;;     "https://openrouter.ai/api"
;;     "ANTHROPIC_AUTH_TOKEN"
;;     (gatsby>>get-ai-api-key)
;;     "ANTHROPIC_API_KEY"
;;     ""))
;;   (agent-shell-display-action
;;    '(display-buffer-in-side-window (side . right) (window-width . 0.25) (slot . 0)))
;;   (agent-shell-file-completion-enabled t))


(use-package eca
  :ensure (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el"))
  :hook (eca-chat-mode . corfu-mode)
  :custom
  (eca-rewrite-diff-tool 'simple-diff)
  (eca-chat-window-width 0.25)
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


;; (use-package aider
;;   :ensure (:host github :repo "tninja/aider.el")
;;   :custom
;;   (aider-args
;;    '("--model"
;;      "openrouter/"
;;      "--no-auto-commits"
;;      "--no-auto-accept-architect"))
;;   :init
;;   (add-to-list
;;    'display-buffer-alist
;;    '("^\\*aider:\\(.+?\\)\\*"
;;      (display-buffer-in-side-window)
;;      (side . right)
;;      (slot . 0)
;;      (window-width . 0.25)
;;      (preserve-size . (t . nil))))
;;   :config
;;   (defun gatsby>>get-ai-api-key ()
;;     "run passage to get the openai_api_key. Return nil if no key is found"
;;     (thread-first
;;      "direnv exec %s passage show openrouter-api"
;;      (format (expand-file-name gatsby>dotfiles-repo-location))
;;      (shell-command-to-string)
;;      (string-trim)
;;      (string-split "\n")
;;      (last)
;;      (car)))
;;   (defun gatsby>>aider-with-api (fn &rest args)
;;     (let ((aider-args
;;            `(,@aider-args "--api" ,(format "openrouter=%s" (gatsby>>get-ai-api-key)))))
;;       (apply fn args)))
;;   (advice-add #'aider-run-aider :around #'gatsby>>aider-with-api)
;;   ;; TODO:
;;   ;; create an org-mode based intermediate layer
;;   :evil-bind
;;   ((:maps normal)
;;    ("SPC a a" . #'aider-run-aider)
;;    ("`" . #'aider-transient-menu)
;;    (:maps (normal visual))
;;    ("SPC a i" . #'aider-implement-todo)
;;    ("SPC a q" . #'aider-ask-question)
;;    (:maps aider-comint-mode-map :states normal)
;;    ("SPC" . nil)
;;    ("q" . #'delete-window)
;;    (:maps aider-comint-mode-map :states insert)
;;    ("<return>" . #'comint-accumulate)
;;    ("M-<return>" . #'comint-send-input)))

;; (use-package gptel
;;   :ensure (:host github
;;            :repo "karthink/gptel")
;;   :custom
;;   (gptel-model "anthropic/claude-sonnet-4.5")
;;   (gptel-use-header-line nil)
;;   (gptel-cache t)
;;   (gptel-use-tools t)
;;   (gptel-display-buffer-action '((side . right)
;;                                  (slot . 0)
;;                                  (window-width . 0.25)
;;                                  (preserve-size . (t . nil))))
;;   (gptel-default-mode 'org-mode)
;;   (gptel-prompt-prefix-alist '((org-mode . "* ")))
;;   :hook
;;   (gptel-post-response . (lambda (&rest _)
;;                            (evil-normal-state)))
;;   :config
;;   (defun gatsby>>get-ai-api-key ()
;;     "run passage to get the openai_api_key. Return nil if no key is found"
;;     (thread-first "direnv exec %s passage show openrouter-api"
;;                   (format (expand-file-name gatsby>dotfiles-repo-location))
;;                   (shell-command-to-string)
;;                   (string-trim)
;;                   (string-split "\n")
;;                   (last)
;;                   (car)))
;;   (setq gptel-backend (gptel-make-openai "OpenRouter"
;;                         :host "openrouter.ai"
;;                         :endpoint "/api/v1/chat/completions"
;;                         :stream t
;;                         :key #'gatsby>>get-ai-api-key
;;                         :models '(anthropic/claude-sonnet-4.5)))
;;   (defun gatsby>>display-gptel-window (fn &rest args)
;;     (cl-letf (((symbol-function #'display-buffer)
;;                (lambda (buffer-or-name &rest _)
;;                  (display-buffer-in-side-window
;;                   buffer-or-name
;;                   '((side . right)
;;                     (slot . 0)
;;                     (window-width . 0.25))))))
;;       (let ((buffer (apply fn args)))
;;         (when buffer
;;           (with-current-buffer buffer
;;             (goto-char (point-max))
;;             (evil-insert-state))
;;           (when-let ((window (get-buffer-window buffer)))
;;             (select-window window)))
;;         buffer)))
;;   (advice-add #'gptel :around #'gatsby>>display-gptel-window)
;;   (gatsby>defcommand gatsby>gptel-clean-screen ()
;;     (let ((last-line (save-excursion
;;                        (goto-char (point-max))
;;                        (beginning-of-line)
;;                        (point))))
;;       (set-window-start (selected-window) last-line)
;;       (goto-char (point-max))))
;;   :evil-bind
;;   ((:maps (normal visual))
;;    ("SPC a" . nil)
;;    ("SPC a a" . #'gptel)
;;    ("`" . #'gptel-menu)
;;    (:maps gptel-mode-map)
;;    ("M-RET" . #'gptel-send)
;;    (:maps gptel-mode-map
;;     :states normal)
;;    ("q" . #'delete-window)
;;    (:maps gptel-mode-map
;;     :states (insert normal))
;;    ("C-c C-l" . #'gatsby>gptel-clean-screen)))

;; ;; coding agent
;; (use-package macher
;;   :ensure (:host github
;;            :repo "kmontag/macher")
;;   :config
;;   (macher-install)
;;   (add-to-list
;;    'display-buffer-alist
;;    '("\\*macher:.*\\*"
;;      (display-buffer-in-side-window)
;;      (side . right)
;;      (slot . 0)
;;      (window-width . 0.25)
;;      (preserve-size . (t . nil))))
;;   ;; TODO: add serena (https://github.com/oraios/serena) tools to macher
;;   :evil-bind
;;   ((:maps (normal visual))
;;    ("SPC a i" . #'macher-implement)
;;    ("SPC a q" . #'macher-discuss)
;;    ("SPC a c" . #'gatsby>macher-write-commit-message)
;;    (:maps diff-mode-map
;;     :states normal)
;;    ("r" . #'macher-revise)))

(provide 'gatsby>ai)
;;; gatsby>ai.el ends here
