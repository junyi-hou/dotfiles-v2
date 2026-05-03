;;; gatsby>>utility.el --- macro & utility functions used throughout the configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'cl-seq)
(require 'subr-x)
(require 'map)
(require 'treesit)

(declare-function elpaca--queued "elpaca")
(declare-function elpaca-get "elpaca")
(declare-function elpaca-source-dir "elpaca")
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

;; secret management via `sops'
(defun gatsby>>passage-paths (data &optional prefix)
  "Return all leaf key paths in DATA as a list of slash-separated strings.
DATA is a parsed JSON alist from the sops-encrypted file; the top-level
`sops' metadata key is skipped."
  (let (paths)
    (map-do
     (lambda (key value)
       (unless (eq key 'sops)
         (let ((path
                (if prefix
                    (format "%s/%s" prefix key)
                  (format "%s" key))))
           (if (listp value)
               (setq paths (append paths (gatsby>>passage-paths value path)))
             (push path paths)))))
     data)
    paths))

(gatsby>defcommand gatsby>retrieve-secret
  (:secret-path
   (let* ((enc-file (expand-file-name "env.json.enc" gatsby>dotfiles-repo-location))
          (data (json-read-file enc-file))
          (paths (gatsby>>passage-paths data)))
     (completing-read "Getting secret: " paths)))
  (let ((secret
         (string-trim
          (shell-command-to-string
           (format "passage %s" (shell-quote-argument secret-path))))))
    (if (called-interactively-p 'interactive)
        (progn
          (kill-new secret)
          (run-at-time 30 nil
                       (lambda (s)
                         (gui-set-selection 'CLIPBOARD "")
                         (setq kill-ring (delete s kill-ring))
                         (message "Secret cleaned"))
                       secret)
          (message "Secret %s copied to the clipboard..." secret-path))
      secret)))

(gatsby>defcommand gatsby>edit-secret ()
  "Decrypt env.json.enc into a scratch buffer for editing.
Press C-c C-c to re-encrypt and save. Normal file saves are disabled."
  (unless (fboundp 'json-ts-mode)
    (user-error
     "json-ts-mode is unavailable (requires Emacs 29+ with tree-sitter JSON grammar)"))
  (let* ((enc-file (expand-file-name "env.json.enc" gatsby>dotfiles-repo-location))
         (decrypted
          (shell-command-to-string
           (format "sops --input-type json --output-type json -d %s"
                   (shell-quote-argument enc-file))))
         (buf (get-buffer-create "*edit-secret*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert decrypted)
      (json-pretty-print-buffer)
      (json-ts-mode)
      (setq-local buffer-file-name nil)
      (setq-local auto-save-default nil)
      (auto-save-mode -1)
      (setq-local make-backup-files nil)
      (setq-local buffer-offer-save nil)
      (let ((save-fn
             (lambda ()
               (interactive)
               (message "Encrypting the secrets...")
               (json-pretty-print-buffer)
               (let* ((content (buffer-string))
                      (default-directory gatsby>dotfiles-repo-location)
                      (out-buf (generate-new-buffer " *sops-enc-output*"))
                      (exit-code
                       (with-temp-buffer
                         (insert content)
                         (call-process-region (point-min) (point-max) "sops"
                                              nil
                                              out-buf
                                              nil
                                              "-e"
                                              "--input-type"
                                              "json"
                                              "--output-type"
                                              "json"
                                              "--config"
                                              (expand-file-name
                                               ".sops.yaml"
                                               gatsby>dotfiles-repo-location)
                                              "--filename-override"
                                              "env.json"
                                              "/dev/stdin"))))
                 (if (= exit-code 0)
                     (progn
                       (write-region
                        (with-current-buffer out-buf
                          (buffer-string))
                        nil enc-file)
                       (kill-buffer out-buf)
                       (kill-buffer buf)
                       (delete-window)
                       (message "Secrets saved to %s"
                                (file-name-nondirectory enc-file)))
                   (let ((err
                          (with-current-buffer out-buf
                            (buffer-string))))
                     (kill-buffer out-buf)
                     (user-error "sops encryption failed: %s" err))))))
            (cancel-fn
             (lambda ()
               (interactive)
               (kill-buffer-and-window)
               (message "Edit cancelled"))))
        (setq-local header-line-format
                    '(:eval
                      (let ((text "     C-c C-c  save & encrypt    C-c C-k  cancel"))
                        (propertize (concat
                                     text
                                     (make-string
                                      (max 0 (- (window-width) (length text))) ?\s))
                                    'face '(error :inverse-video t)))))
        (local-set-key (kbd "C-c C-c") save-fn)
        (local-set-key (kbd "C-c C-k") cancel-fn)
        (when (fboundp 'evil-local-set-key)
          (evil-local-set-key 'normal (kbd "C-c C-c") save-fn)
          (evil-local-set-key 'insert (kbd "C-c C-c") save-fn)
          (evil-local-set-key 'normal (kbd "C-c C-k") cancel-fn)
          (evil-local-set-key 'insert (kbd "C-c C-k") cancel-fn))
        ;; remap save-buffer and write-file: when buffer-file-name is nil,
        ;; save-buffer prompts via write-file before ever reaching write-file-functions
        (let ((no-save
               (lambda ()
                 (interactive)
                 (message "Use C-c C-c to save secrets"))))
          (local-set-key [remap save-buffer] no-save)
          (local-set-key [remap write-file] no-save)
          (local-set-key [remap evil-write] no-save))))
    (gatsby>switch-to-buffer-new-window buf)))

(defun gatsby>>run-process-with-callback (commands &optional buffer final-sentinel)
  "Run COMMANDS sequentially. The FINAL-SENTINEL is attached to the last process.
Return the final process ran."
  (when commands
    (let* ((command (car commands))
           (rest (cdr commands))
           (dir default-directory)
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
               (let ((default-directory dir))
                 (gatsby>>run-process-with-callback rest buffer final-sentinel))
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
         (repo (elpaca-source-dir e))
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
                 (message "Elpaca update finished for %s" pkg-name)
                 (kill-buffer log-buffer))))))
      (error "Repository for %s not found" package))))

(gatsby>defcommand gatsby>sync-packages-to-lock-file (all)
  "Update ALL installed packages (from `elpaca--queued') to their locked ref.
If the prefix arg ALL is not given, query the user for a package to update."
  (let* ((lock-entries
          (with-temp-buffer
            (insert-file-contents elpaca-lock-file)
            (read (current-buffer))))
         (ref-table (make-hash-table :test #'eq)))
    (unless all
      (let ((pkg
             (completing-read
              "Restore: "
              (thread-last lock-entries (mapcar #'car) (mapcar #'symbol-name)))))
        (setq lock-entries
              (list
               (seq-find
                (lambda (e) (equal (symbol-name (car e)) pkg)) lock-entries)))))
    (pcase-dolist (`(,pkg . ,props) lock-entries)
      (when-let* ((ref (map-nested-elt props '(:recipe :ref))))
        (puthash pkg ref ref-table)))
    (dolist (pkg (hash-table-keys ref-table))
      (when-let* ((e (elpaca-get pkg))
                  (repo (elpaca-source-dir e))
                  ((file-directory-p repo))
                  (ref (gethash pkg ref-table)))
        (let ((default-directory repo))
          (gatsby>>run-process-with-callback `(("git" "fetch") ("git" "checkout" ,ref))
                                             nil
                                             (lambda (_proc event)
                                               (if (string-match-p "finished" event)
                                                   (progn
                                                     (message
                                                      "Synced %s to %s, rebuilding..."
                                                      pkg ref)
                                                     (elpaca-rebuild pkg)
                                                     (elpaca-wait))
                                                 (message "Failed to sync %s: %s"
                                                          pkg
                                                          event)))))))))

(provide 'gatsby>>utility)
;;; gatsby>>utility.el ends here
