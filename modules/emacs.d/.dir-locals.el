;;; Directory Local Variables -*- no-byte-compile: t -*-

((emacs-lisp-mode
  (elisp-flymake-byte-compile-load-path . ("." "./lisp" "../lisp"))
  (elisp-autofmt-load-packages-local . ("use-package" "use-package-core" "gatsby>>utility" "evil-macros" "evil-common" "cl-lib" "cl-seq" "cl-macs")))
 (nil . ((gatsby>project-tests-command . ("make" "test-emacs")))))
