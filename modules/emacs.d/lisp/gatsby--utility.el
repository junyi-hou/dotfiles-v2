;;; gatsby--utility.el --- macro & utility functions used throughout the configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
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

(defgroup gatsby nil
  "Gatsby Emacs configuration options."
  :group 'emacs)

(defcustom gatsby>auto-sync-packages-to-lock-file t
  "If non-nil, automatically sync installed packages to `elpaca-lock-file'.
The sync happens asynchronously after `elpaca-after-init-hook' fires."
  :type 'boolean
  :group 'gatsby)

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

(defun gatsby>>run-process-with-callback (commands &optional buffer final-sentinel on-error)
  "Run COMMANDS sequentially. The FINAL-SENTINEL is attached to the last process.
ON-ERROR, if non-nil, is called with (PROC EVENT) when a non-final command
fails; otherwise a generic message is printed.  The chain always stops on
failure.  Return the final process ran."
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
                 (gatsby>>run-process-with-callback rest buffer final-sentinel on-error))
             (if on-error
                 (funcall on-error p event)
               (message "Process failed: `%s' failed with %s"
                        (string-join (process-command p) " ")
                        event)))))
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

(defun gatsby>>read-lock-refs ()
  "Return a hash table mapping package ids to their locked refs."
  (let ((table (make-hash-table :test #'eq)))
    (when (file-readable-p elpaca-lock-file)
      (pcase-dolist (`(,pkg . ,props)
                     (with-temp-buffer
                       (insert-file-contents elpaca-lock-file)
                       (read (current-buffer))))
        (when-let* ((ref (map-nested-elt props '(:recipe :ref))))
          (puthash pkg ref table))))
    table))

(defun gatsby>>package-current-ref (pkg callback)
  "Asynchronously get PKG's current git HEAD ref.
CALLBACK is called with (PKG REF) where REF is nil if unavailable."
  (if-let* ((e (elpaca-get pkg))
            (repo (elpaca-source-dir e))
            ((file-directory-p repo)))
    (let ((default-directory repo)
          (buf (generate-new-buffer " *elpaca-ref-check*")))
      (make-process
       :name (format "elpaca-ref-%s" pkg)
       :buffer buf
       :command '("git" "rev-parse" "HEAD")
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let ((ref
                  (when (and (= (process-exit-status proc) 0)
                             (buffer-live-p (process-buffer proc)))
                    (string-trim
                     (with-current-buffer (process-buffer proc)
                       (buffer-string))))))
             (when (buffer-live-p (process-buffer proc))
               (kill-buffer (process-buffer proc)))
             (funcall callback pkg ref))))))
    (funcall callback pkg nil)))

(defun gatsby>>check-packages-against-lock (&optional callback)
  "Asynchronously compare installed packages against `elpaca-lock-file'.
CALLBACK receives a list of (PKG LOCKED-REF CURRENT-REF) mismatches."
  (let* ((lock-refs (gatsby>>read-lock-refs))
         (pending (cl-remove-if-not #'elpaca-get (hash-table-keys lock-refs)))
         (mismatches nil))
    (cl-labels ((check-one
                 ()
                 (if pending
                     (let ((pkg (pop pending)))
                       (gatsby>>package-current-ref
                        pkg
                        (lambda (pkg current)
                          (let ((locked (gethash pkg lock-refs)))
                            (unless (and current locked (string= current locked))
                              (push (list pkg locked current) mismatches)))
                          (check-one))))
                   (when callback
                     (funcall callback (nreverse mismatches))))))
      (check-one))))

(defun gatsby>>sync-package-to-ref (pkg ref callback)
  "Checkout PKG to REF and rebuild it asynchronously.
CALLBACK is called with (PKG nil) on success, or (PKG ERROR-STRING) on
failure.  A failure in any step (missing repo, fetch, checkout) is
reported through CALLBACK and never throws."
  (if-let* ((e (elpaca-get pkg))
            (repo (elpaca-source-dir e))
            ((file-directory-p repo)))
      (let ((default-directory repo))
        (message "Syncing %s to locked ref %s..." pkg ref)
        (gatsby>>run-process-with-callback
         `(("git" "fetch") ("git" "checkout" ,ref))
         nil
         (lambda (_proc event)
           (if (string-match-p "finished" event)
               (progn
                 (elpaca-rebuild pkg)
                 (funcall callback pkg nil))
             (funcall callback
                      pkg
                      (format "`git checkout %s' %s" ref (string-trim event)))))
         (lambda (proc event)
           (funcall callback
                    pkg
                    (format "`%s' %s"
                            (string-join (process-command proc) " ")
                            (string-trim event))))))
    (funcall callback pkg "repository not found")))

(defun gatsby>>sync-packages-to-refs (pkg-refs &optional done-callback)
  "Sync each (PKG REF) pair in PKG-REFS to its locked ref, one at a time.
A failed package is reported and skipped; it never aborts the rest.
Print a summary of updated and failed packages at the end.
DONE-CALLBACK, if non-nil, is called with (UPDATED FAILED), each a list
of package symbols."
  (let ((updated nil)
        (failed nil))
    (cl-labels ((next
                 (items)
                 (if (null items)
                     (let* ((updated (nreverse updated))
                            (failed (nreverse failed))
                            (updated-str
                             (if updated
                                 (string-join
                                  (mapcar #'symbol-name updated) ", ")
                               "none"))
                            (failed-str
                             (if failed
                                 (string-join
                                  (mapcar #'symbol-name failed) ", ")
                               "none")))
                       (message "Package sync finished. Updated: %s. Failed: %s."
                                updated-str failed-str)
                       (when done-callback
                         (funcall done-callback updated failed)))
                   (pcase-let ((`(,pkg ,ref) (car items)))
                     (gatsby>>sync-package-to-ref
                      pkg ref
                      (lambda (pkg err)
                        (if err
                            (progn
                              (push pkg failed)
                              (message "Failed to sync %s: %s" pkg err))
                          (push pkg updated)
                          (message "Updated %s to %s" pkg ref))
                        (next (cdr items))))))))
      (next pkg-refs))))

(defun gatsby>>maybe-auto-sync-packages-to-lock-file ()
  "If enabled, asynchronously sync packages to their locked refs after init."
  (when gatsby>auto-sync-packages-to-lock-file
    (message "Checking installed packages against lock file...")
    (gatsby>>check-packages-against-lock
      (lambda (mismatches)
        (if mismatches
            (progn
              (message "Found %d package(s) out of sync with lock file"
                       (length mismatches))
              (gatsby>>sync-packages-to-refs
               (mapcar (lambda (m) (list (car m) (cadr m))) mismatches)))
          (message "All installed packages match lock file"))))))

(gatsby>defcommand gatsby>check-packages-against-lock ()
  "Asynchronously check installed packages against `elpaca-lock-file'.
Display mismatches in a temporary buffer."
  (message "Checking installed packages against lock file...")
  (gatsby>>check-packages-against-lock
   (lambda (mismatches)
     (if mismatches
         (let ((buf (get-buffer-create "*elpaca-lock-mismatches*")))
           (with-current-buffer buf
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert "Packages out of sync with lock file:\n\n")
               (pcase-dolist (`(,pkg ,locked ,current) mismatches)
                 (insert
                  (format "%s\n  locked:   %s\n  current:  %s\n\n"
                          pkg
                          (or locked "nil")
                          (or current "nil"))))
               (special-mode))
             (pop-to-buffer buf))
           (message "Found %d package(s) out of sync with lock file"
                    (length mismatches)))
       (message "All installed packages match lock file")))))

(gatsby>defcommand gatsby>sync-packages-to-lock-file (all)
  "Update installed packages to their locked refs.
If the prefix arg ALL is not given, query the user for a single package."
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
    (gatsby>>sync-packages-to-refs
     (mapcar (lambda (pkg) (list pkg (gethash pkg ref-table)))
             (hash-table-keys ref-table)))))

(provide 'gatsby--utility)
;;; gatsby--utility.el ends here
