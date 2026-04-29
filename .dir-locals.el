;;; Directory Local Variables -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((gatsby>eglot-auto-format-before-save . t)
         (gatsby>project-tests-command . ("make" "test-emacs"))
         (gatsby>get-individual-test-function . (lambda ()
           (let ((choice (completing-read "Run tests: " '("emacs" "scripts") nil t)))
             (if (equal choice "emacs")
                 '("make" "test-emacs")
               '("make" "test-scripts"))))))))
