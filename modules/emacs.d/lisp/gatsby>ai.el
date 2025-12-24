;;; gatsby>ai.el --- ai code helper -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)


(use-package gptel
  :ensure (:host github :repo "karthink/gptel")
  :custom
  (gptel-model "anthropic/claude-sonnet-4.5")
  (gptel-use-header-line nil)
  (gptel-cache t)
  (gptel-use-tools t)
  (gptel-display-buffer-action '((side . right)
                                 (slot . 0)
                                 (window-width . 0.25)
                                 (preserve-size . (t . nil))))
  (gptel-default-mode 'org-mode)
  :hook
  (gptel-post-response . (lambda (&rest _) (evil-normal-state)))
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

  (setq gptel-backend (gptel-make-openai "OpenRouter"
                        :host "openrouter.ai"
                        :endpoint "/api/v1/chat/completions"
                        :stream t
                        :key #'gatsby>>get-ai-api-key
                        :models '(anthropic/claude-sonnet-4.5)))

  (defun gatsby>>display-gptel-window (fn &rest args)
    (cl-letf (((symbol-function #'display-buffer)
               (lambda (buffer-or-name &rest _)
                 (display-buffer-in-side-window
                  buffer-or-name
                  '((side . right)
                    (slot . 0)
                    (window-width . 0.25))))))
      (let ((buffer (apply fn args)))
        (when buffer
          (with-current-buffer buffer
            (goto-char (point-max))
            (evil-insert-state))
          (when-let ((window (get-buffer-window buffer)))
            (select-window window)))
        buffer)))

  (advice-add #'gptel :around #'gatsby>>display-gptel-window)

  (gatsby>defcommand gatsby>gptel-clean-screen ()
    (let ((last-line (save-excursion (goto-char (point-max)) (beginning-of-line) (point))))
      (set-window-start (selected-window) last-line)
      (goto-char (point-max))))

  :evil-bind
  ((:maps (normal visual))
   ("SPC a" . nil)
   ("SPC a a" . #'gptel)
   ("SPC a r" . #'gptel-rewrite)
   ("SPC a C-g" . #'gptel-abort)
   ("`" . #'gptel-menu)
   (:maps gptel-mode-map)
   ("M-RET" . #'gptel-send)
   (:maps gptel-mode-map :states normal)
   ("q" . #'delete-window)
   (:maps gptel-mode-map :states (insert normal))
   ("C-c C-l" . #'gatsby>gptel-clean-screen)))

;; coding agent
(use-package macher
  :ensure (:host github :repo "kmontag/macher")
  :config
  (macher-install)

  (add-to-list
   'display-buffer-alist
   '("\\*macher:.*\\*"
     (display-buffer-in-side-window)
     (side . right)
     (slot . 0)
     (window-width . 0.25)
     (preserve-size . (t . nil))))

  ;; TODO: add serena (https://github.com/oraios/serena) tools to macher
  ;; TODO: add commit writter

  :evil-bind
  ((:maps (normal visual))
   ("SPC a i" . #'macher-implement)
   ("SPC a q" . #'macher-discuss)
   (:maps diff-mode-map :states normal)
   ("a" . #'diff-apply-hunk)
   ("s" . #'diff-split-hunk)
   ("A" . #'diff-apply-buffer)
   ("q" . #'kill-buffer-and-window)
   ("r" . #'macher-revise)))


(provide 'gatsby>ai)
;;; gatsby>ai.el ends here
