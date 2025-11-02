;;; gatsby>python.el --- python specific config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(gatsby>use-internal-pacakge python
  :mode ("\\.py\\'" . python-ts-mode)
	:custom (python-indent-offset 4)
  :hook
  (python-ts-mode . gatsby>>python-set-indent-width)
  ;; (python-ts-mode . eglot-ensure)
  :init
  (defun gatsby>>python-set-indent-width (&rest _)
    (setq-local tab-width 4))

	;; tree-sitter
  (gatsby>install-treesitter-grammar
   'python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

	;; lsp
	(defconst gatsby>pyright-configfile-location (no-littering-expand-var-file-name "pyrightconfig"))
	(make-directory gatsby>pyright-configfile-location t)

	(gatsby>defcommand gatsby>start-pyright ()
		(let* ((root (directory-file-name (expand-file-name (project-root (project-current)))))
					 (config-root (file-name-concat gatsby>pyrefly-configfile-location (replace-regexp-in-string "/" "-" root)))
					 (config-file (file-name-concat config-root "pyrightconfig.json"))
					 (lsp-cmd
						`(,(expand-file-name ".venv/bin/basedpyright-langserver" gatsby>dotfiles-repo-location)
							"--stdio"
							))
					 (eglot-server-programs `((python-ts-mode . ,lsp-cmd))))
			(call-interactively #'eglot)))

	;; (gatsby>defcommand gatsby>edit-pyrefly-config ()
	;; 	(let* ((root (directory-file-name (expand-file-name (project-root (project-current)))))
	;; 				 (config-root (file-name-concat gatsby>pyrefly-configfile-location (replace-regexp-in-string "/" "-" root)))
	;; 				 (config-file (file-name-concat config-root "pyrefly.toml")))
	;; 		(unless (file-directory-p config-root)
	;; 			(make-directory config-root t))
	;; 		(unless (file-exists-p config-file)
	;; 			(with-temp-buffer
	;; 				(write-file config-file)))
	;; 		(find-file config-file)))

;; 	(gatsby>defcommand gatsby>start-pyright ()
;;     "Start pyright with project-specific configuration.
;; If `pyrightconfig.json` does not exist in the project root,
;; create one from the default template."
;;     (let* (;; use custom server setting
;; 					 (lspce-server-programs
;; 						`(("python"
;; 							 ,(expand-file-name ".venv/bin/basedpyright-langserver" user-emacs-directory)
;; 							 "--stdio"
;; 						   ,(gatsby>plist-to-hash-table-recursive gatsby>pyright-initialization-options)))))
;; 			(lspce-mode)))
	
  :config
  (gatsby>defcommand gatsby>python-generate-notebook (run)
    "Run the current script and produce a notebook file using `jupytext'.

   If the prefix argument RUN is non-nil, execute all cells to produce output."
    (let* ((file (buffer-file-name))
           (command (concat "jupytext --to ipynb " file)))
      (when run
        (setq command (s-join " " `(,command "--pipe-fmt" "ipynb" "--pipe" "'jupyter nbconvert --to ipynb --execute --allow-errors --stdin --stdout'"))))
      (compile command)))

  (defun gatsby>python-insert-delimiter (&optional args)
    "Insert a jupyter notebook code cell delimiter (# %%).

  If the prefix ARGS is non-nil, insert a markdown cell instead (# %% [markdown])"
    (interactive "P")
    (insert "\n# %%")
    (when args (insert " [markdown]"))
    (insert "\n")
    (when args
      (insert "\"\"\"\"\"\"") (backward-char 3)))

  (gatsby>defcommand gatsby>python-move-to-next-cell ()
    (let ((cell-regexp "^# %%\\(.\\)*\n"))
      (re-search-forward cell-regexp nil 'noerror)))

  (gatsby>defcommand gatsby>python-move-to-prev-cell ()
    (let ((cell-regexp "^# %%\\(.\\)*\n"))
      (re-search-backward cell-regexp nil 'noerror)))

	(gatsby>defcommand gatsby>python-eval-region-or-cell ()
		(if (region-active-p)
				(let ((b (region-beginning))
							(e (region-end)))
					(jupyter-eval-string (buffer-substring-no-properties b e))
					(evil-normal-state))
			(let* ((cell-regexp "^# %%\\(.\\)*\n")
						 (b (save-excursion (or (and (not from-top)
                                         (re-search-backward cell-regexp nil 'noerror))
                                    (point-min))))
						 (e (save-excursion (or (re-search-forward cell-regexp nil 'noerror) (point-max)))))
				(jupyter-eval-string (buffer-substring-no-properties b e)))))

  :general
  (:keymaps 'python-ts-mode-map :states '(insert normal)
						"M-RET" #'gatsby>python-insert-delimiter)

  (:keymaps 'python-ts-mode-map :states 'normal
						">" #'gatsby>python-move-to-next-cell
						"<" #'gatsby>python-move-to-prev-cell)

  (:keymaps 'python-ts-mode-map :states 'normal :prefix "SPC"
						"rr" #'gatsby>python-eval-region-or-cell
						"ro" #'gatsby>jupyter-start-or-switch-to-repl
						"rz" #'jupyter-repl-associate-buffer)

  (:states 'visual :keymaps 'python-ts-mode-map
					 "<" #'python-indent-shift-left
					 ">" #'python-indent-shift-right))

;; linting using ruff
;; (use-package flymake-ruff
;; 	:ensure (:host github :repo "erickgnavar/flymake-ruff")
;; 	:custom (flymake-ruff-program (expand-file-name ".venv/bin/ruff" user-emacs-directory))
;; 	:hook (eglot-managed-mode . flymake-ruff-load))

(provide 'gatsby>python)
;;; gatsby>python.el ends here
