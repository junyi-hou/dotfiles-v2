;;; gatsby>terminal.el --- terminal emulator -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(use-package xterm-color :ensure t)

(gatsby>use-internal-pacakge eshell
  :hook
  (eshell-first-time-mode . gatsby>eshell-setup)
  (eshell-first-time-mode . gatsby>eshell-setkey)
  (eshell-mode . corfu-mode)
  (eshell-mode . gatsby>eshell-change-buffer-title)
  (eshell-directory-change . gatsby>eshell-change-buffer-title)

  :init
  (defun gatsby>eshell-setup ()
    "Further setup eshell mode."
    (require 'esh-mode)
    (add-hook 'eshell-before-prompt-hook (lambda () (setq xterm-color-preserve-properties t)))
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
    (setq eshell-buffer-maximum-lines 12000)
    (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
    (setq eshell-list-files-after-cd t)
    (setq eshell-error-if-no-glob t
          eshell-glob-case-insensitive t)
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-scroll-to-bottom-on-input 'all)
    (setq eshell-scroll-show-maximum-output nil)
    (setq gatsby>eshell-history-cache-file (no-littering-expand-var-file-name "eshell-history.el"))
    (setq gatsby>eshell-history-item-counts 500))

  (defun gatsby>eshell-change-buffer-title ()
    "Change the title of eshell buffer to reflect $pwd."
    (rename-buffer (format "%s: %s" eshell-buffer-name (directory-file-name default-directory)) 'unique))

  (gatsby>defcommand gatsby>eshell-history ()
    "<C-r> in eshell."
    (let* ((eshell-bol (save-excursion (eshell-bol) (point)))
           (vertico-sort-function nil)
           (history (thread-last gatsby>eshell-history-cache-file
                                 gatsby>retrieve-or-save-item
                                 (mapcar (lambda (item) (string-join item " ")))
                                 ;; rank from the most recent to the
                                 nreverse))
           (command (completing-read
                     "History: "
                     history
                     nil nil
                     (buffer-substring eshell-bol (point)))))
      (delete-region eshell-bol (point))
      (insert command)))

  (defun gatsby>eshell-save-history (fn &rest args)
    "Actually save the CMD to history cache file."
    (gatsby>retrieve-or-save-item gatsby>eshell-history-cache-file
                                  (thread-last (eshell-get-old-input)
                                               substring-no-properties
                                               string-trim
                                               ((lambda (s) (string-split s " "))))
                                  gatsby>eshell-history-item-counts)
    (apply fn args))

  (advice-add #'eshell-send-input :around #'gatsby>eshell-save-history)

  (gatsby>defcommand eshell/cls (&rest _)
    "clean screen by scrolling all the way down."
    (let ((last-line (save-excursion (goto-char (point-max)) (beginning-of-line) (point))))
      (set-window-start (selected-window) last-line)
      (eshell-interrupt-process)))

  (defun gatsby>eshell-alias (executable &rest args)
    (when (executable-find executable (file-remote-p default-directory))
      (cl-letf (((symbol-function 'message) #'ignore))
        (eshell-command-result (format "%s %s" executable
                                       (thread-last args
                                                    (mapcar (lambda (it) (format "%s" it)))
                                                    ((lambda (it) (string-join it " ")))))))))

  (defun eshell/tree (&rest args)
    "Run tree using exa or tree if available."
    (cond
     ((executable-find "exa") (apply #'gatsby>eshell-alias `("exa" "--tree" ,@args)))
     ((executable-find "tree") (apply #'gatsby>eshell-alias `("tree" ,@args)))
     (t (user-error "Can't find `exa' or `tree'"))))

  (defun eshell/ls (&rest args)
    (if (executable-find "exa")
        (apply #'gatsby>eshell-alias `("exa" ,@args))
      (apply #'gatsby>eshell-alias `("ls" ,@args))))

  (defun eshell/ll (&rest _)
    (eshell/ls "-alh"))

  (defun eshell/ff (&rest files)
    (mapc #'find-file (flatten-tree files)))

  (defun eshell/ssh (&rest args)
    "Ignoring all flags passed to ssh and "
    (let* ((_args (thread-last args
                               (mapcar (lambda (i) (format "%s" i)))
                               (cl-remove-if (lambda (s) (string-prefix-p "-" s)))))
           (host (format "/ssh:%s:~" (car _args))))
      (if (> (length _args) 1)
          (user-error (format "eshell/ssh accepts only one argument in the form of user@hostname, got %s instead" _args))
        (apply #'eshell/cd `(,host)))))

  ;; FIXME: this does not work?
  (defun gatsby>eshell-cd (cd &rest args)
    "Make `eshell/cd' tramp-aware and project-friendly."
    (let* ((host (file-remote-p default-directory))
           ;; assuming remote is linux (which is almost always true)
           (home (format "%s/home/%s" host (file-remote-p default-directory 'user)))
           (root (when-let ((pr (project-current))) (project-root pr))))
      (cond ((and (not args) root (string= (file-truename root) (file-truename default-directory))) (funcall cd))
            ((and (not args) root) (funcall cd root))
            ((and (not args) host (file-exists-p home)) (funcall cd home))
            ((and (not args) host) (funcall cd (format "%s/" host)))
            (t (funcall cd args)))))

  (advice-add #'eshell/cd :around #'gatsby>eshell-cd)

  (with-eval-after-load 'evil
    (gatsby>defcommand gatsby>eshell-goto-last-prompt ()
      "Goto current prompt and continue editting."
      (goto-char (point-max))
      (evil-insert 1))

    (evil-define-operator eshell/evil-change (beg end type register yank-handler delete-func)
      "Like `evil-change' but will not delete/copy the prompt."
      (interactive "<R><x><y>")
      (save-restriction
        (narrow-to-region eshell-last-output-end (point-max))
        (evil-change (max beg (point-min))
                     (if (eq type 'line) (point-max) (min (or end (point-max)) (point-max)))
                     type register yank-handler delete-func)))

    (evil-define-operator eshell/evil-change-line (beg end type register yank-handler)
      "Change to end of line."
      :motion evil-end-of-line
      (interactive "<R><x><y>")
      (eshell/evil-change beg end type register yank-handler #'evil-delete-line))

    (evil-define-operator eshell/evil-delete (beg end type register yank-handler)
      "Like `evil-delete' but will not delete/copy the prompt."
      (interactive "<R><x><y>")
      (save-restriction
        (narrow-to-region eshell-last-output-end (point-max))
        (evil-delete (if beg (max beg (point-min)) (point-min))
                     (if (eq type 'line) (point-max) (min (or end (point-max)) (point-max)))
                     type register yank-handler)))

    (evil-define-operator eshell/evil-delete-line (_beg end type register yank-handler)
      "Change to end of line."
      :motion nil
      :keep-visual t
      (interactive "<R><x>")
      (eshell/evil-delete (point) end type register yank-handler)))

  (defun gatsby>eshell-setkey ()
    "Customize key in eshell-mode."
    (general-define-key
     :states '(normal visual motion)
     :keymaps 'eshell-mode-map
     "A" #'gatsby>eshell-goto-last-prompt
     "H" #'eshell-bol
     "c" #'eshell/evil-change
     "C" #'eshell/evil-change-line
     "d" #'eshell/evil-delete
     "D" #'eshell/evil-delete-line
     "<" #'eshell-previous-prompt
     ">" #'eshell-next-prompt
     "q" #'kill-buffer-and-window)

    (general-define-key
     :states '(normal visual motion)
     :keymaps 'eshell-mode-map
     :prefix "SPC"
     "q" #'kill-buffer-and-window)

    (general-define-key
     :states 'insert
     :keymaps 'eshell-mode-map
     "C-r" #'gatsby>eshell-history)

    (general-define-key
     :states '(normal visual motion emacs insert)
     :keymaps 'eshell-mode-map
     :prefix "C-c"
     "C-l" #'eshell/cls))

  :config
  (gatsby>defcommand gatsby>eshell-open-or-switch (home)
    "Open a new shell.
If the prefix argument (HOME) is not null, go to the home directory.
If there is a visible eshell window in the same PWD, switch to it instead of open a new one."
    (let* ((default-directory (if home (expand-file-name "~/") default-directory))
           (dir default-directory)
           (visible
            (thread-last (buffer-list)
                         (cl-remove-if-not (lambda (buf) (with-current-buffer buf
                                                           (and (string-equal major-mode "eshell-mode")
                                                                (file-equal-p dir default-directory)))))
                         (cl-some (lambda (buf) (get-buffer-window buf))))))
      (if visible
          (progn (select-window visible) (evil-insert-state))
        (split-window-below (- (/ (window-total-height) 3)))
        (other-window 1)
        (prog1
            (eshell 'Z)
          (evil-insert-state)))))
  :commands gatsby>eshell-open-or-switch)

(provide 'gatsby>terminal)
;;; gatsby>terminal.el ends here
