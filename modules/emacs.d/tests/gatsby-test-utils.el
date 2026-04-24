;;; gatsby-test-utils.el --- shared utilities for test files -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; Core walker

(defun gatsby-test-utils--for-each-use-package-form (callback)
  "Call CALLBACK for each use-package/gatsby>use-internal-package form in lisp/."
  (let ((lisp-dir (file-name-concat gatsby>dotfiles-repo-location "modules/emacs.d/lisp")))
    (dolist (file (directory-files lisp-dir t "\\.el$"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (let ((form (read (current-buffer))))
                (when (and (listp form)
                           (memq (car form) '(use-package gatsby>use-internal-package)))
                  (funcall callback form))))
          (end-of-file nil))))))

;;; Evil-bind utilities

(defun gatsby-test-utils--extract-evil-bind-commands (evil-bind-arg)
  "Return command symbols from EVIL-BIND-ARG, skipping config plists and nil bindings."
  (let (cmds)
    (dolist (item evil-bind-arg)
      (when (and (consp item)
                 (or (stringp (car item)) (vectorp (car item))))
        (let ((cmd (cdr item)))
          (cond
           ((and (consp cmd) (eq (car cmd) 'function)) (push (cadr cmd) cmds))
           ((and (symbolp cmd) cmd)                    (push cmd cmds))))))
    (nreverse cmds)))

(defun gatsby-test-utils--evil-bind-entry-from-form (form)
  "Return (PKG CMD...) for a use-package FORM with :evil-bind, or nil."
  (let ((rest (cddr form)) cmds)
    (while rest
      (when (eq (car rest) :evil-bind)
        (setq cmds (append cmds (gatsby-test-utils--extract-evil-bind-commands (cadr rest)))))
      (setq rest (cdr rest)))
    (when cmds (cons (cadr form) cmds))))

(defun gatsby-test-utils--collect-evil-bind-entries ()
  "Return list of (PKG CMD...) from :evil-bind blocks across all lisp/ files."
  (let (result)
    (gatsby-test-utils--for-each-use-package-form
     (lambda (form)
       (let ((entry (gatsby-test-utils--evil-bind-entry-from-form form)))
         (when entry (push entry result)))))
    (nreverse result)))

(defun gatsby-test-utils--load-and-collect-commands ()
  "Require each package; return commands only from packages that loaded."
  (delete-dups
   (apply #'append
          (mapcar (lambda (entry)
                    (condition-case nil (require (car entry)) (error nil))
                    (when (featurep (car entry)) (cdr entry)))
                  (gatsby-test-utils--collect-evil-bind-entries)))))

(defun gatsby-test-utils--parse-evil-bind-triples (evil-bind-arg)
  "Parse raw :evil-bind ARG into (MAP-SYM KEY-SEQ CMD) triples.
Only returns direct bindings (nil :states); skips nil commands."
  (let* ((normalized (use-package-normalize/:evil-bind 'dummy :evil-bind (list evil-bind-arg)))
         (blocks (cl-remove-if-not #'identity
                                   (use-package-split-list-at-keys :block normalized)))
         triples)
    (dolist (block blocks)
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
              (when cmd (push (list map-sym key cmd) triples)))
            (setq pairs (cddr pairs))))))
    (nreverse triples)))

(defun gatsby-test-utils--evil-bind-triples-from-form (form)
  "Return (MAP-SYM KEY-SEQ CMD) triples from FORM's :evil-bind blocks."
  (let ((rest (cddr form)) triples)
    (while rest
      (when (eq (car rest) :evil-bind)
        (setq triples (append triples (gatsby-test-utils--parse-evil-bind-triples (cadr rest)))))
      (setq rest (cdr rest)))
    triples))

(defun gatsby-test-utils--collect-evil-bind-triples ()
  "Return all (MAP-SYM KEY-SEQ CMD-SYM) triples from :evil-bind in lisp/ files."
  (let (triples)
    (gatsby-test-utils--for-each-use-package-form
     (lambda (form)
       (setq triples (append triples (gatsby-test-utils--evil-bind-triples-from-form form)))))
    triples))

;;; Treesitter utilities

(defun gatsby-test-utils--treesitter-langs-from-form (form)
  "Return list of lang symbols from gatsby>install-treesitter-grammar calls in FORM."
  (let (langs)
    (cl-labels ((walk (x)
                  (when (consp x)
                    (if (eq (car x) 'gatsby>install-treesitter-grammar)
                        (let ((lang-arg (cadr x)))
                          (when (and (consp lang-arg) (eq (car lang-arg) 'quote))
                            (push (cadr lang-arg) langs)))
                      (walk (car x))
                      (walk (cdr x))))))
      (walk (cddr form)))
    langs))

(defun gatsby-test-utils--collect-treesitter-langs ()
  "Return list of lang symbols declared via gatsby>install-treesitter-grammar."
  (let (langs)
    (gatsby-test-utils--for-each-use-package-form
     (lambda (form)
       (setq langs (append langs (gatsby-test-utils--treesitter-langs-from-form form)))))
    langs))

(provide 'gatsby-test-utils)
;;; gatsby-test-utils.el ends here
