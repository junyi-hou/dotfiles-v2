;;; gatsby>>utility.el --- macro & utility functions used throughout the configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cl-seq)
(require 'subr-x)
(require 'treesit)

(declare-function elpaca-write-lock-file "elpaca")
(declare-function elpaca--queued "elpaca")
(declare-function elpaca-get "elpaca")
(declare-function elpaca<-repo-dir "elpaca")
(declare-function elpaca<-recipe "elpaca")
(declare-function elpaca-rebuild "elpaca")
(declare-function elpaca-wait "elpaca")

(defvar gatsby>dotfiles-repo-location)

(defmacro gatsby>use-internal-package (name &rest args)
  "So I don't need to type `:ensure nil' every time."
  (declare (indent 1))
  `(use-package ,name
     :ensure
     nil
     ,@args))

(defmacro gatsby>defcommand (name args &rest body)
  "Define an interactive command with NAME, ARGS, and BODY.
Usage:
  (gatsby>defcommand foo () ...) -> (defun foo () (interactive) ...)
  (gatsby>defcommand foo (beg end) ...) -> (defun foo (beg end) (interactive \"r\") ...)
  (gatsby>defcommand foo (sth) ...) -> (defun foo (sth) (interactive \"P\") ...)
  (gatsby>defcommand foo (:x f) ...) -> (defun foo (x) (interactive (list f) ...))"
  (declare
   (doc-string 3) (indent defun)
   (debug
    (&define
     name lambda-list [&optional stringp] ("interactive" [&rest form]) def-body)))
  (let ((docstring
         (if (stringp (car body))
             (prog1 (car body)
               (setq body (cdr body)))
           ""))
        (interactive-form
         (pcase args
           (`(beg end) `(interactive "r"))
           (`(&rest ,_) `(interactive))
           (`(,_) `(interactive "P"))
           (`() `(interactive))
           ;; this is a keyword list
           ((and plist (guard (plistp plist)))
            (let ((keys '())
                  (values '()))
              ;; format: off
              (cl-loop
               for (k v) on plist by #'cddr do
               (push (intern (substring (symbol-name k) 1)) keys)
               (push v values))

              (setq args (nreverse keys))
              `(interactive (list ,@values))))
           ;; format: on
           (_ (user-error "[gatsby>defcommdn] malformed argument list")))))
    `(defun ,name ,args
       ,docstring
       ,interactive-form
       ,@body)))

;; replacing treesit-auto
(defun gatsby>install-treesitter-grammar (lang url &optional revision source-dir)
  "Install treesitter grammar for LANG at URL.
Optionally use REVISION and alternative SOURCE-DIR."
  (add-to-list 'treesit-language-source-alist `(,lang . (,url ,revision ,source-dir)))
  (unless (treesit-ready-p lang t)
    (treesit-install-language-grammar lang)))

;; persistent storage (across sessions)
(defun gatsby>retrieve-or-save-item (cache-file &optional new-item max-length)
  "Read or update the items saved in CACHE-FILE.

If the optional argument NEW-ITEM is not null, add it to the CACHE-FILE.
If the optional argument MAX-LENGTH is not null, check the total number of items in the CACHE-FILE and keep only up to that many entries.

Items are saved as a list.  Duplicated or nil items will be removed
before saving to the cache file."
  ;; first check CACHE-FILE
  (unless (and (file-exists-p cache-file) (file-writable-p cache-file))
    (shell-command (concat "touch " (shell-quote-argument cache-file))))

  (let ((items
         (with-temp-buffer
           (insert-file-contents cache-file)
           (when (> (length (buffer-string)) 0)
             (car (read-from-string (buffer-string))))))
        (inhibit-message t))
    ;; write
    (if new-item
        (with-temp-buffer
          (insert ";;; -*- coding: utf-8 -*-\n")
          (let ((print-length nil)
                (print-level nil))
            (pp
             (thread-last
              `(,@items
                ,(if (symbolp new-item)
                     (symbol-name new-item)
                   new-item))
              ((lambda (x) (cl-remove-duplicates x :test #'equal)))
              (cl-remove-if-not #'identity)
              ((lambda (n list)
                 (if n
                     (last list n)
                   list))
               max-length))
             (current-buffer)))
          ;; Don't use write-file; we don't want this buffer to visit it.
          (write-region (point-min) (point-max) cache-file))

      ;; read
      items)))

(defun gatsby>switch-to-buffer-new-window (buffer-or-name &optional norecord)
  "Switch to buffer, reusing existing window if visible, otherwise create new window.
If BUFFER-OR-NAME is already visible in a window, switch to that window.
Otherwise, always create a new window by splitting."
  (interactive (list
                (read-buffer "Switch to buffer in other window: "
                             (other-buffer (current-buffer))
                             nil)))
  (let* ((buffer (window-normalize-buffer-to-switch-to buffer-or-name))
         (window (get-buffer-window buffer 'visible)))
    (if window
        ;; Buffer is already visible, just select that window
        (select-window window norecord)
      ;; Buffer not visible, create new window
      (let ((new-window (funcall split-window-preferred-function)))
        (unless new-window
          ;; If sensible split failed, force a split
          (setq new-window (split-window nil nil 'right)))
        (select-window new-window norecord)
        (switch-to-buffer buffer norecord)))))

(defun gatsby>>put-mode-to-evil-state (modes state)
  "Putting MODES in evil STATE mode maps (e.g., `evil-normal-state-map').
should bind to `evil-mode-hook'"
  (let ((list (intern (format "evil-%s-state-modes" (symbol-name state))))
        (other-modes (cdr-safe modes)))
    (if other-modes
        (dolist (mode modes)
          (add-to-list list mode))
      (add-to-list list modes))))

;; secret management via `passage'
(gatsby>defcommand gatsby>retrieve-secret ()
  (let* ((default-directory gatsby>dotfiles-repo-location)
         (dir (getenv "PASSAGE_DIR"))
         (pass-path
          (thread-last
           (directory-files-recursively dir "\\.age$")
           (mapcar
            (lambda (f)
              (thread-first f (file-relative-name dir) (file-name-sans-extension))))
           (completing-read "Getting secret: ")))
         (secret
          (thread-last
           pass-path
           (format "direnv exec %s passage show %s" default-directory)
           (shell-command-to-string)
           ((lambda (l) (string-split l "\n" 'omit-nulls)))
           (last)
           (car))))
    (kill-new secret)
    (run-at-time 30 nil
                 (lambda (s)
                   (gui-set-selection 'CLIPBOARD "")
                   (setq kill-ring (delete s kill-ring))
                   (message "Secret cleaned"))
                 secret)
    (message "Secret %s copied to the clipboard..." pass-path)))

;; automatically update the elpaca lock file when exiting emacs
(defun gatsby>>update-elpaca-lock-file ()
  (let ((lock-file
         (expand-file-name
          (file-name-concat gatsby>dotfiles-repo-location
                            "modules/emacs.d/elpaca-lock.el"))))
    (elpaca-write-lock-file lock-file)))

(add-hook 'kill-emacs-hook #'gatsby>>update-elpaca-lock-file)


(defun gatsby>>run-process-with-callback (commands &optional buffer final-sentinel)
  "Run COMMANDS sequentially. The FINAL-SENTINEL is attached to the last process.
Return the final process ran."
  (when commands
    (let* ((command (car commands))
           (rest (cdr commands))
           (proc (apply #'start-process "git-task" buffer (car command) (cdr command))))
      (if (null rest)
          ;; If this is the last command, attach the custom sentinel
          (when final-sentinel
            (set-process-sentinel proc final-sentinel)
            proc)

        ;; Otherwise, use a sequence-manager sentinel to trigger the next step
        (set-process-sentinel
         proc
         (lambda (p event)
           (if (string= event "finished\n")
               (gatsby>>run-process-with-callback rest buffer final-sentinel)
             (message "Process failed: `%s' failed with %s"
                      (string-join (process-command p) " ")
                      event))))
        proc))))

(gatsby>defcommand gatsby>update-emacs-package ()
  "Go into the elpaca repo of an installed package, do git pull and elpaca rebuild."
  (let* ((queued (elpaca--queued))
         (packages (mapcar #'car queued))
         (package (completing-read "Update package: " packages nil t))
         (id (intern package))
         (e (elpaca-get id))
         (repo (elpaca<-repo-dir e))
         (branch (map-elt (elpaca<-recipe e) :branch)))
    (if (and repo (file-directory-p repo))
        (let* ((default-directory repo)
               (pkg-id id)
               (pkg-name package)
               (log-buffer (get-buffer-create (format "*elpaca-update-%s*" pkg-name))))

          (with-current-buffer log-buffer
            (let ((inhibit-read-only t))
              (erase-buffer)))

          (message "Updating %s in %s..." pkg-name repo)

          (unless branch
            (setq branch
                  (string-trim
                   (shell-command-to-string
                    "git remote show origin | grep \"HEAD branch\" | cut -d' ' -f5"))))

          (let* ((current-branch
                  (string-trim
                   (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
                 (commands
                  `(("git" "diff-index" "--quiet" "HEAD" "--")
                    ,@
                    (unless (string= current-branch branch)
                      `(("git" "checkout" ,branch)))
                    ("git" "pull"))))
            (gatsby>>run-process-with-callback
             commands
             log-buffer
             (lambda (_proc event)
               (when (string-match-p "finished" event)
                 (message "Git pull finished for %s. Rebuilding..." pkg-name)
                 (elpaca-rebuild pkg-id)
                 (elpaca-wait)
                 (gatsby>>update-elpaca-lock-file)
                 (message "Elpaca update and lock file update finished for %s" pkg-name)
                 (kill-buffer log-buffer))))))
      (error "Repository for %s not found" package))))

(gatsby>defcommand gatsby>sync-packages-to-lock-file ()
  "Checkout every installed elpaca package to the ref recorded in the lock file."
  (let* ((lock-file
          (expand-file-name
           (file-name-concat gatsby>dotfiles-repo-location
                             "modules/emacs.d/elpaca-lock.el")))
         (lock-entries
          (with-temp-buffer
            (insert-file-contents lock-file)
            (read (current-buffer))))
         (ref-table (make-hash-table :test #'eq)))
    (pcase-dolist (`(,pkg . ,props) lock-entries)
      (when-let* ((ref (plist-get (plist-get props :recipe) :ref)))
        (puthash pkg ref ref-table)))
    (dolist (pkg (hash-table-keys ref-table))
      (when-let* ((e (elpaca-get pkg))
                  (repo (elpaca<-repo-dir e))
                  ((file-directory-p repo))
                  (ref (gethash pkg ref-table)))
        (let ((default-directory repo))
          (gatsby>>run-process-with-callback `(("git" "checkout" ,ref))
                                             nil
                                             (lambda (_proc event)
                                               (if (string-match-p "finished" event)
                                                   (progn
                                                     (message
                                                      "Synced %s to %s, rebuilding..."
                                                      pkg ref)
                                                     (elpaca-rebuild pkg))
                                                 (message "Failed to sync %s: %s"
                                                          pkg
                                                          event)))))))))

(provide 'gatsby>>utility)
;;; gatsby>>utility.el ends here
