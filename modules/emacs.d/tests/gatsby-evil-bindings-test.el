;;; gatsby-evil-bindings-test.el --- test that all evil-bound commands are defined  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defun gatsby-evil-bindings--extract-from-evil-bind (evil-bind-arg)
  "Return command symbols from EVIL-BIND-ARG, skipping config plists and nil bindings."
  (let (cmds)
    (dolist (item evil-bind-arg)
      (when (and (consp item)
                 (or (stringp (car item)) (vectorp (car item))))
        (let ((cmd (cdr item)))
          (cond
           ((and (consp cmd) (eq (car cmd) 'function))
            (push (cadr cmd) cmds))
           ((and (symbolp cmd) cmd)
            (push cmd cmds))))))
    (nreverse cmds)))

(defun gatsby-evil-bindings--collect-from-form (form)
  "Return (PKG-NAME CMD...) for a use-package FORM with :evil-bind, or nil."
  (when (and (listp form)
             (memq (car form) '(use-package gatsby>use-internal-package)))
    (let* ((pkg (cadr form))
           (rest (cddr form))
           cmds)
      (while rest
        (when (eq (car rest) :evil-bind)
          (setq cmds (append cmds (gatsby-evil-bindings--extract-from-evil-bind (cadr rest)))))
        (setq rest (cdr rest)))
      (when cmds (cons pkg cmds)))))

(defun gatsby-evil-bindings--collect-pkg-commands ()
  "Return list of (PKG-NAME CMD...) entries from :evil-bind blocks across all lisp/ files."
  (let* ((lisp-dir (file-name-concat gatsby>dotfiles-repo-location "modules/emacs.d/lisp"))
         result)
    (dolist (file (directory-files lisp-dir t "\\.el$"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (let ((entry (gatsby-evil-bindings--collect-from-form (read (current-buffer)))))
                (when entry (push entry result))))
          (end-of-file nil))))
    result))

(defun gatsby-evil-bindings--load-and-collect ()
  "Require each use-package symbol; return commands only from packages that loaded."
  (let ((pkg-cmds (gatsby-evil-bindings--collect-pkg-commands)))
    (delete-dups
     (apply #'append
            (mapcar
             (lambda (entry)
               (condition-case nil (require (car entry)) (error nil))
               (when (featurep (car entry)) (cdr entry)))
             pkg-cmds)))))

(ert-deftest gatsby-evil-bindings--all-commands-fboundp ()
  "Every command referenced in :evil-bind must be fboundp."
  (let* ((commands (gatsby-evil-bindings--load-and-collect))
         (unbound (cl-remove-if #'fboundp commands)))
    (should commands)
    (should-not unbound)))

(ert-deftest gatsby-evil-bindings--all-commands-interactive ()
  "Every fboundp command referenced in :evil-bind must be interactive (commandp)."
  (let* ((commands (gatsby-evil-bindings--load-and-collect))
         (non-interactive (cl-remove-if #'commandp (cl-remove-if-not #'fboundp commands))))
    (should commands)
    (should-not non-interactive)))

(defun gatsby-evil-bindings--parse-triples (evil-bind-arg)
  "Parse raw :evil-bind ARG into (MAP-SYM KEY-SEQ CMD) triples.
Delegates to `use-package-normalize/:evil-bind' for parsing.
Only returns direct bindings (nil :states); skips nil commands."
  (let* ((normalized (use-package-normalize/:evil-bind 'dummy :evil-bind (list evil-bind-arg)))
         (blocks (cl-remove-if-not #'identity
                                   (use-package-split-list-at-keys :block normalized)))
         triples)
    (dolist (block blocks)
      ;; block: ((quote STATES) MAP KEY1 CMD1 KEY2 CMD2 ...)
      (let ((states  (cadr (car block)))
            (map-sym (cadr block))
            (pairs   (cddr block)))
        (when (null states)
          (while pairs
            (let* ((key     (car pairs))
                   (raw-cmd (cadr pairs))
                   (cmd     (if (and (consp raw-cmd) (eq (car raw-cmd) 'function))
                                (cadr raw-cmd)
                              raw-cmd)))
              (when cmd
                (push (list map-sym key cmd) triples)))
            (setq pairs (cddr pairs))))))
    (nreverse triples)))

(defun gatsby-evil-bindings--collect-triples ()
  "Return all (MAP-SYM KEY-SEQ CMD-SYM) triples from :evil-bind blocks in lisp/ files."
  (let* ((lisp-dir (file-name-concat gatsby>dotfiles-repo-location "modules/emacs.d/lisp"))
         triples)
    (dolist (file (directory-files lisp-dir t "\\.el$"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (let ((form (read (current-buffer))))
                (when (and (listp form)
                           (memq (car form) '(use-package gatsby>use-internal-package)))
                  (let ((rest (cddr form)))
                    (while rest
                      (when (eq (car rest) :evil-bind)
                        (setq triples
                              (append triples
                                      (gatsby-evil-bindings--parse-triples (cadr rest)))))
                      (setq rest (cdr rest)))))))
          (end-of-file nil))))
    triples))

(ert-deftest gatsby-evil-bindings--all-bindings-in-place ()
  "Every direct :evil-bind key binding must be active in its target keymap."
  (let* ((triples (gatsby-evil-bindings--collect-triples))
         missing)
    (should triples)
    (dolist (triple triples)
      (pcase-let ((`(,map-sym ,key-seq ,cmd-sym) triple))
        (when (and (boundp map-sym)
                   (keymapp (symbol-value map-sym)))
          (let ((actual (lookup-key (symbol-value map-sym) key-seq)))
            (unless (eq actual cmd-sym)
              (push (list map-sym (key-description key-seq) cmd-sym actual)
                    missing))))))
    (should-not missing)))

(provide 'gatsby-evil-bindings-test)
;;; gatsby-evil-bindings-test.el ends here
