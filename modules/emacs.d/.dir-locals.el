;;; Directory Local Variables -*- no-byte-compile: t -*-

((emacs-lisp-mode
  (eval . (setq-local elisp-flymake-byte-compile-load-path
                      (append '("." "./lisp" "../lisp")
                              (seq-filter #'file-directory-p
                                          (directory-files "~/.emacs.d/elpaca/builds" t "^[^.]")))))
  (elisp-autofmt-load-packages-local . ("use-package" "use-package-core" "gatsby>>utility" "evil-macros" "evil-common" "cl-lib" "cl-seq" "cl-macs")))
 (nil . ((gatsby>project-tests-command . ("make" "test-emacs")))))
