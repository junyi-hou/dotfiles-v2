;;; gatsby>ai.el --- ai code helper -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(use-package aidermacs
  :ensure (:host github :repo "MatthewZMD/aidermacs")
	:custom
	(aidermacs-program (expand-file-name ".venv/bin/aider" gatsby>dotfiles-repo-location))
	(aidermacs-default-model "openrouter/openai/gpt-5-nano")
  :config
  (defun gatsby>>get-ai-api-key ()
    "run passage to get the openai_api_key. Return nil if no key is found"
		(let ((process-environment
					 (cons (format "PASSAGE_AGE=%s" (expand-file-name ".cargo/bin/rage" gatsby>dotfiles-repo-location))
								 process-environment))
					(output-buffer (generate-new-buffer "*cmd-output*")))
			(unwind-protect
					(when (eq 0 (call-process-shell-command
											 (format "%s show openrouter-api" (expand-file-name ".tools/bin/passage" gatsby>dotfiles-repo-location)) nil output-buffer nil))
						(prog1
								(with-current-buffer output-buffer
									(string-trim (buffer-string)))
							(kill-buffer output-buffer))))))

  (gatsby>defcommand gatsby>start-aider-session (subtree-only-p)
		(if-let ((api (format "openrouter=%s" (gatsby>>get-ai-api-key)))
						 (aidermacs-extra-args `(,@aidermacs-extra-args "--api-key" ,api))
						 (aidermacs-extra-args (if subtree-only-p `(,@aidermacs-extra-args "--subtree-only") aidermacs-extra-args)))
				(aidermacs-run)
			(user-error "Valid API keys not found")))

	;; until we have smerge backend
	(defun gatsby>>ediff-to-smerge (buffer1 buffer2)
    (let* ((content1 (with-current-buffer buffer1 (buffer-string)))
           (content2 (with-current-buffer buffer2 (buffer-string)))
					 (a (make-temp-file "diff-buffer1-"))
					 (b (make-temp-file "diff-buffer2-"))
					 (out (make-temp-file "diff-buffer-out-")))
      ;; Write contents to temp files for diff
      (unwind-protect
          (progn
            (with-temp-file a
              (insert content1))
            (with-temp-file b
              (insert content2))
            ;; Run diff with unified format and get output
            (let ((diff-output (shell-command-to-string (format "git merge-file -L %s -L B -L %s -p %s %s %s && cat %s" a b a out b out))))
              ;; Put diff output in second buffer
              (with-current-buffer buffer2
                (erase-buffer)
                (insert diff-output)
                (smerge-start-session)
                (message "Diff output inserted and smerge-mode enabled in buffer %S" 
                         buffer2))
              (gatsby>switch-to-buffer-new-window buffer2)))
        ;; Cleanup temp files
        (delete-file a)
        (delete-file b))))

	(advice-add #'aidermacs--show-ediff-for-file
              :around (defun gatsby>>aidermacs-use-smerge (oldfun &rest r)
                        (cl-letf (((symbol-function #'ediff-buffers) #'gatsby>>ediff-to-smerge))
                          (apply oldfun r))))

	;; always create new window in aidermacs:
	(advice-add #'aidermacs--show-file-selection-buffer :around
							(defun gatsby>>aidermacs-open-in-new-window (fn &rest args)
								(cl-letf (((symbol-function #'switch-to-buffer-other-window) #'gatsby>switch-to-buffer-new-window))
									(apply fn args))))

	(advice-add #'aidermacs-switch-to-buffer :around
							(defun gatsby>>aidermacs-open-in-side-bar (fn &rest args)
								(cl-letf* ((config '((side . right) (slot . 0) (window-width . 0.25) (preserve-size . (t . nil))))
													((symbol-function #'pop-to-buffer) (lambda (b) (display-buffer-in-side-window b config))))
									(apply fn args))))

	(advice-add #'aidermacs-switch-to-buffer :after
							(defun gatsby>>aidermacs-focus-and-insert (&rest _)
								(select-window (get-buffer-window (aidermacs-get-buffer-name)))
								(goto-char (point-max))
								(evil-insert-state)))

	;; allow non-vc project root
	;; can come in handy in large projects
	(advice-add #'aidermacs-project-root :override
							(lambda ()
								(or (project-root (project-current))
										(when buffer-file-name
											(file-name-directory buffer-file-name))
										default-directory)))

  :general
  (:keymaps '(normal motion) :prefix "SPC"
						"a" #'gatsby>start-aider-session)
  (:keymaps '(normal motion)
						"`" #'aidermacs-transient-menu)
	(:keymaps 'aidermacs-comint-mode-map :states 'normal
						"q" #'delete-window)
	(:keymaps 'aidermacs-comint-mode-map :states 'normal :prefix "SPC"
						"q" #'delete-window))

(provide 'gatsby>ai)
;;; gatsby>ai.el ends here
