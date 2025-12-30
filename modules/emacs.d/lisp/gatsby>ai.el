;;; gatsby>ai.el --- ai code helper -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(use-package aider
  :ensure (:host github
           :repo "tninja/aider.el")
  :custom
  (aider-args '("--model" "openrouter/anthropic/claude-sonnet-4.5:floor"
                "--no-auto-commits" "--no-auto-accept-architect"))
  :init
  (add-to-list 'display-buffer-alist '("^\\*aider:\\(.+?\\)\\*"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (slot . 0)
                                       (window-width . 0.25)
                                       (preserve-size . (t . nil))))
  :config
  (defun gatsby>>get-ai-api-key ()
    "run passage to get the openai_api_key. Return nil if no key is found"
    (thread-first "direnv exec %s passage show openrouter-api"
                  (format (expand-file-name gatsby>dotfiles-repo-location))
                  (shell-command-to-string)
                  (string-trim)
                  (string-split "\n")
                  (last)
                  (car)))
  (defun gatsby>>aider-with-api (fn &rest args)
    (let ((aider-args `(,@aider-args "--api" ,(format "openrouter=%s"
                                               (gatsby>>get-ai-api-key)))))
      (apply fn args)))
  (advice-add #'aider-run-aider :around #'gatsby>>aider-with-api)
  ;; TODO:
  ;; create an org-mode based intermediate layer
  :evil-bind
  ((:maps normal)
   ("SPC a a" . #'aider-run-aider)
   ("`" . #'aider-transient-menu)
   (:maps (normal visual))
   ("SPC a i" . #'aider-implement-todo)
   ("SPC a q" . #'aider-ask-question)
   (:maps aider-comint-mode-map
    :states normal)
   ("SPC" . nil)
   ("q" . #'delete-window)
   (:maps aider-comint-mode-map
    :states insert)
   ("<return>" . #'comint-accumulate)
   ("M-<return>" . #'comint-send-input)))

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
