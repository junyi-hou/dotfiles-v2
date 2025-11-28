;;; gatsby>terminal.el --- terminal emulator -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'gatsby>>utility)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

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
    (with-eval-after-load 'evil
      (evil-define-key 'normal eshell-mode-map
        (kbd "A") #'gatsby>eshell-goto-last-prompt
        (kbd "H") #'eshell-bol
        (kbd "c") #'eshell/evil-change
        (kbd "C") #'eshell/evil-change-line
        (kbd "d") #'eshell/evil-delete
        (kbd "D") #'eshell/evil-delete-line
        (kbd "<") #'eshell-previous-prompt
        (kbd ">") #'eshell-next-prompt
        (kbd "q") #'kill-buffer-and-window
        (kbd "SPC q") #'kill-buffer-and-window)

      (evil-define-key 'insert 'eshell-mode-map
        (kbd "C-r") #'gatsby>eshell-history)

      (evil-define-key '(normal visual emacs insert) eshell-mode-map
        (kbd "C-c C-l") #'eshell/cls)))

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


;; as a backend for other programs
(use-package vterm
  :ensure (:host github :repo "akermu/emacs-libvterm")
  :hook (evil-mode . (lambda () (gatsby>>put-mode-to-evil-state 'vterm-mode 'insert)))
  :custom
  (vterm-shell "zsh")
  (vterm-buffer-name-string "*vterm*: %s")
  (vterm-max-scrollback 10000)
  :config

  ;; taken from `evil-collection'
  ;; https://github.com/emacs-evil/evil-collection/blob/master/modes/vterm/evil-collection-vterm.el
  (with-eval-after-load 'evil

    (evil-define-motion evil-collection-vterm-first-non-blank ()
      "Move the cursor to the first non-blank character after the prompt."
      :type exclusive
      (if (vterm-cursor-in-command-buffer-p (point))
          (vterm-beginning-of-line)
        (evil-first-non-blank)))

    (defun evil-collection-vterm-insert ()
      "Insert character before cursor."
      (interactive)
      (vterm-goto-char (point))
      (call-interactively #'evil-insert))

    (defun evil-collection-vterm-insert-line ()
      "Insert character at beginning of prompt."
      (interactive)
      (vterm-goto-char (vterm--get-prompt-point))
      (call-interactively #'evil-insert))

    (defun evil-collection-vterm-append ()
      "Append character after cursor."
      (interactive)
      (vterm-goto-char (1+ (point)))
      (call-interactively #'evil-insert))

    (defun evil-collection-vterm-append-line ()
      "Append character at end-of-line."
      (interactive)
      (vterm-goto-char (vterm--get-end-of-line))
      (call-interactively #'evil-insert))

    (defun evil-collection-vterm-paste-after (&optional arg)
      (interactive "P")
      (vterm-goto-char (+ 1 (point)))
      (call-interactively #'vterm-yank arg))

    (evil-define-operator evil-collection-vterm-delete (beg end type register yank-handler)
      "Modification of evil-delete to work in vterm buffer.
Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
      (interactive "<R><x><y>")
      (let* ((beg (max (or beg (point)) (vterm--get-prompt-point)))
             (end (min (or end beg) (vterm--get-end-of-line))))
        (unless register
          (let ((text (filter-buffer-substring beg end)))
            (unless (string-match-p "\n" text)
              ;; set the small delete register
              (evil-set-register ?- text))))
        (let ((evil-was-yanked-without-register nil))
          (evil-yank beg end type register yank-handler))
        (cond
         ((eq type 'block)
          (evil-apply-on-block #'vterm-delete-region beg end nil))
         ((and (eq type 'line)
               (= end (point-max))
               (or (= beg end)
                   (/= (char-before end) ?\n))
               (/= beg (point-min))
               (=  (char-before beg) ?\n))
          (vterm-delete-region (1- beg) end))
         (t
          (vterm-delete-region beg end)))
        ;; place cursor on beginning of line
        (when (and (called-interactively-p 'any)
                   (eq type 'line))
          (vterm-reset-cursor-point))))

    (evil-define-operator evil-collection-vterm-delete-backward-char (beg end type register)
      "Delete previous character."
      :motion evil-backward-char
      (interactive "<R><x>")
      (evil-collection-vterm-delete beg end type register))

    (evil-define-operator evil-collection-vterm-delete-char (beg end type register)
      "Delete current character."
      :motion evil-forward-char
      (interactive "<R><x>")
      (evil-collection-vterm-delete beg end type register))

    (evil-define-operator evil-collection-vterm-delete-line (beg end type register yank-handler)
      "Modification of evil-delete line to work in vterm bufer. Delete to end of line."
      :motion nil
      :keep-visual t
      (interactive "<R><x>")
      ;; act linewise in Visual state
      (let* ((beg (or beg (point)))
             (end (or end beg))
             (visual-line-mode (and evil-respect-visual-line-mode
                                    visual-line-mode))
             (line-end (if visual-line-mode
                           (save-excursion
                             (end-of-visual-line)
                             (point))
                         (line-end-position))))
        (when (evil-visual-state-p)
          (unless (memq type '(line screen-line block))
            (let ((range (evil-expand beg end
                                      (if visual-line-mode
                                          'screen-line
                                        'line))))
              (setq beg (evil-range-beginning range)
                    end (evil-range-end range)
                    type (evil-type range))))
          (evil-exit-visual-state))
        (cond
         ((eq type 'block)
          ;; equivalent to $d, i.e., we use the block-to-eol selection and
          ;; call `evil-collection-vterm-delete'. In this case we fake the call to
          ;; `evil-end-of-line' by setting `temporary-goal-column' and
          ;; `last-command' appropriately as `evil-end-of-line' would do.
          (let ((temporary-goal-column most-positive-fixnum)
                (last-command 'next-line))
            (evil-collection-vterm-delete beg end 'block register yank-handler)))
         ((memq type '(line screen-line))
          (evil-collection-vterm-delete beg end type register yank-handler))
         (t
          (evil-collection-vterm-delete beg line-end type register yank-handler)))))

    (evil-define-operator evil-collection-vterm-replace (beg end type register yank-handler)
      :motion evil-forward-char
      (interactive "<R>")
      (let ((replacement (make-string (- end beg) (read-char))))
        (evil-collection-vterm-delete beg end type register yank-handler)
        (vterm-insert replacement)
        (vterm-goto-char (1- end))))

    (evil-define-operator evil-collection-vterm-change (beg end type register yank-handler)
      (evil-collection-vterm-delete beg end type register yank-handler)
      (evil-collection-vterm-insert))

    (evil-define-operator evil-collection-vterm-change-line (beg end type register yank-handler)
      :motion evil-end-of-line-or-visual-line
      (evil-collection-vterm-delete-line beg end type register yank-handler)
      (evil-collection-vterm-insert))

    (evil-define-operator evil-collection-vterm-substitute (beg end type register)
      :motion evil-forward-char
      (interactive "<R><x>")
      (evil-collection-vterm-change beg end type register))

    (evil-define-operator evil-collection-vterm-substitute-line (beg end register yank-handler)
      :motion evil-line-or-visual-line
      :type line
      (interactive "<r><x>")
      (evil-collection-vterm-change beg end 'line register yank-handler))

    (evil-define-motion evil-collection-vterm-next-line (count)
      "Move the cursor COUNT lines down.
But don't allow the cursor to move bellow the last prompt line."
      :type line
      ;; This successfully prevents the `j' button from moving to an empty line
      ;; bellow the last prompt. However, it still can be bugged for example by
      ;; going to the one line above the last prompt and doing `10j'.
      (when (> (count-words (point) (point-max)) 0)
        (evil-next-line count)))
    )

  :evil-bind
  ((:maps vterm-mode-map :states insert)
    ("C-a" . #'vterm--self-insert)
    ("C-b" . #'vterm--self-insert)
    ("C-d" . #'vterm--self-insert)
    ("C-e" . #'vterm--self-insert)
    ("C-f" . #'vterm--self-insert)
    ("C-k" . #'vterm--self-insert)
    ("C-l" . #'vterm--self-insert)
    ("C-n" . #'vterm--self-insert)
    ("C-o" . #'vterm--self-insert)
    ("C-p" . #'vterm--self-insert)
    ("C-q" . #'vterm--self-insert)
    ("C-r" . #'vterm--self-insert)
    ("C-s" . #'vterm--self-insert)
    ("C-t" . #'vterm--self-insert)
    ("C-u" . #'vterm--self-insert)
    ("C-v" . #'vterm--self-insert)
    ("C-w" . #'vterm--self-insert)
    ("C-y" . #'vterm--self-insert)
    ("C-z" . #'vterm--self-insert)
    ("<delete>" . #'vterm-send-delete)

    (:maps vterm-mode-map :states (normal insert visual))
    ("C-c C-l" . #'vterm-clear)

    (:maps vterm-mode-map :states normal)
    ("<" . #'vterm-previous-prompt)
    (">" . #'vterm-next-prompt)
    ("p" . #'evil-collection-vterm-paste-after)
    ("P" . #'vterm-yank)
    ("a" . #'evil-collection-vterm-append)
    ("A" . #'evil-collection-vterm-append-line)
    ("d" . #'evil-collection-vterm-delete)
    ("D" . #'evil-collection-vterm-delete-line)
    ("x" . #'evil-collection-vterm-delete-char)
    ("X" . #'evil-collection-vterm-delete-backward-char)
    ("<return>" . #'vterm-send-return)
    ("^" . #'evil-collection-vterm-first-non-blank)
    ("i" . #'evil-collection-vterm-insert)
    ("I" . #'evil-collection-vterm-insert-line)
    ("u" . #'vterm-undo)
    ("r" . #'evil-collection-vterm-replace)
    ("c" . #'evil-collection-vterm-change)
    ("C" . #'evil-collection-vterm-change-line)
    ("s" . #'evil-collection-vterm-substitute)
    ("S" . #'evil-collection-vterm-substitute-line)
    ("j" . #'evil-collection-vterm-next-line)
    ("G" . #'vterm-reset-cursor-point)

    (:maps vterm-mode-map :states visual)
    ("d" . #'evil-collection-vterm-delete)
    ("x" . #'evil-collection-vterm-delete-backward-char)))

(provide 'gatsby>terminal)
;;; gatsby>terminal.el ends here
