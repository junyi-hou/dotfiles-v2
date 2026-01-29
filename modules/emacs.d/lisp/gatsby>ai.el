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
  (agent-shell-header-style 'text)
  (agent-shell-display-action
   '(display-buffer-in-side-window (side . bottom) (window-height . 0.3) (slot . 0)))
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
                  (file-equal-p default-directory project-root)))))))
      (if (or force-new (not current-client))
          (call-interactively #'agent-shell-anthropic-start-claude-code)
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

  ;; send a notification when there's outstanding tool permission asks
  ;; NOTE: only work in Mac OS
  ;; NOTE: you would also need to give emacs permission in the system setting
  (defun gatsby>agent-notify-when-permission-requested ()
    (when-let* ((tool-permission (alist-get :tool-calls agent-shell--state))
                (pending-permission
                 (thread-last
                  tool-permission
                  (mapcar
                   (lambda (t)
                     `(,(alist-get :kind (cdr t)) . ,(alist-get :status (cdr t)))))
                  (seq-filter
                   (lambda (s) (equal "pending" (cdr s))))
                  (mapcar (lambda (t) (car t)))))
                (group-by
                 (let ((counts '()))
                   (dolist (item pending-permission counts)
                     (let ((entry (assoc item counts)))
                       (if entry
                           ;; If item is already in alist, increment its count
                           (setcdr entry (1+ (cdr entry)))
                         ;; Otherwise, add a new entry with count 1
                         (push (cons item 1) counts)))))))
      ;; send notification
      (do-applescript
       (format "display notification %S with title %S sound name \"Glass\""
               "" "Agent Shell Permission Center"))))


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
   ("C-c C-c" . #'comint-interrupt-subjob)
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
