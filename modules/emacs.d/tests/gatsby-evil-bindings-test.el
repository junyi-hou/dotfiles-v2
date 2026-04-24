;;; gatsby-evil-bindings-test.el --- test that all evil-bound commands are defined  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gatsby-test-utils)

(ert-deftest gatsby-evil-bindings--all-commands-fboundp ()
  "Every command referenced in :evil-bind must be fboundp."
  (let* ((commands (gatsby-test-utils--load-and-collect-commands))
         (unbound (cl-remove-if #'fboundp commands)))
    (should commands)
    (should-not unbound)))

(ert-deftest gatsby-evil-bindings--all-commands-interactive ()
  "Every fboundp command referenced in :evil-bind must be interactive (commandp)."
  (let* ((commands (gatsby-test-utils--load-and-collect-commands))
         (non-interactive (cl-remove-if #'commandp (cl-remove-if-not #'fboundp commands))))
    (should commands)
    (should-not non-interactive)))

(ert-deftest gatsby-evil-bindings--all-bindings-in-place ()
  "Every direct :evil-bind key binding must be active in its target keymap."
  (let* ((triples (gatsby-test-utils--collect-evil-bind-triples))
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
