;;; gatsby>>utility.el --- macro & utility functions used throughout the configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; =====================================================================
;; define new command without needing to add `interactive' over and over
;; =====================================================================

(eval-when-compile
  (require 'cl-lib)
  (require 'cl-seq)
  (require 'subr-x)
  (require 'treesit))

(defmacro gatsby>use-internal-pacakge (name &rest args)
  "So I don't need to type `:ensure nil' every time."
  (declare (indent 1))
  `(use-package ,name :ensure nil ,@args))

(defmacro gatsby>defcommand (name args &rest body)
  "Define an interactive command with NAME, ARGS, and BODY.
Usage:
  (gatsby>defcommand foo () ...) -> (defun foo () (interactive) ...)
  (gatsby>defcommand foo (beg end) ...) -> (defun foo (beg end) (interactive \"r\") ...)
  (gatsby>defcommand foo (sth) ...) -> (defun foo (sth) (interactive \"P\") ...)
  (gatsby>defcommand foo (:x f) ...) -> (defun foo (x) (interactive (list f) ...))
"
  (declare (doc-string 3)
           (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           ("interactive" [&rest form])
                           def-body)))
  (let ((docstring (if (stringp (car body)) (prog1 (car body) (setq body (cdr body))) ""))
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
              (cl-loop for (k v) on plist by #'cddr do
                       (push (intern (substring (symbol-name k) 1)) keys)
                       (push v values))
              (setq args (nreverse keys))
              `(interactive (list ,@values))))
           (_ (user-error "[gatsby>defcommdn] malformed argument list")))))
    `(defun ,name ,args
       ,docstring
       ,interactive-form
       ,@body)))

;; replacing treesit-auto
(defun gatsby>install-treesitter-grammar
    (lang url &optional revision source-dir)
  "Install treesitter grammar for LANG at URL.
Optionally use REVISION and alternative SOURCE-DIR."
  (add-to-list 'treesit-language-source-alist
               `(,lang . (,url ,revision ,source-dir)))
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

  (let ((items (with-temp-buffer
                 (insert-file-contents cache-file)
                 (when (> (length (buffer-string)) 0) (car (read-from-string (buffer-string))))))
        (inhibit-message t))
    ;; write
    (if new-item
        (with-temp-buffer
          (insert ";;; -*- coding: utf-8 -*-\n")
          (let ((print-length nil)
                (print-level nil))
            (pp (thread-last `(,@items ,new-item)
                             (cl-remove-duplicates)
                             (cl-remove-if-not #'identity)
                             ((lambda (n list) (last list n)) max-length))
                (current-buffer)))
          ;; Don't use write-file; we don't want this buffer to visit it.
          (write-region (point-min) (point-max) cache-file))

      ;; read
      items)))

(defun gatsby>switch-to-buffer-new-window (buffer-or-name &optional norecord)
  "Switch to buffer, reusing existing window if visible, otherwise create new window.
If BUFFER-OR-NAME is already visible in a window, switch to that window.
Otherwise, always create a new window by splitting."
  (interactive
   (list (read-buffer "Switch to buffer in other window: "
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

(defun gatsby>plist-to-hash-table-recursive (plist &optional test)
  "Recursively convert a property list PLIST to a hash table with string keys.

Nested plists within PLIST will be converted to nested hash tables.
This is useful for converting complex data structures like those
derived from JSON. Plist keys (which are typically symbols) are
converted to strings.

A value is treated as a nested plist if it is a non-empty list
with an even number of elements.

The optional argument TEST specifies the hash table's test function
(e.g., 'eq, 'eql, or 'equal). Defaults to 'equal."
  (when plist
    (let ((table (make-hash-table :test (or test #'equal))))
      (cl-loop for (key value) on plist by #'cddr do
               (let* ((processed-value
                       ;; Heuristic: Is the value a plist that should be converted?
                       (if (and (listp value)
                                (> (length value) 0)
                                (cl-evenp (length value)))
                           (gatsby>plist-to-hash-table-recursive value test)
                         value))
                      (key-string (substring (symbol-name key) 1)))
                 (puthash key-string processed-value table)))
      table)))

;; putting modes in evil-STATE-state-modes
;; should bind to `evil-mode-hook'
(defun gatsby>>put-mode-to-evil-state (modes state)
  (let ((list (intern (format "evil-%s-state-modes" (symbol-name state))))
        (other-modes (cdr-safe modes)))
    (if other-modes
        (dolist (mode modes)
          (add-to-list list mode))
      (add-to-list list modes))))

;; secret management via `passage'
;; (gatsby>defcommand gatsby>retrieve-secret ()
;;   (set-register)
;;   )

(provide 'gatsby>>utility)
;;; gatsby>>utility.el ends here
