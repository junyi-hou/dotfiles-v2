emacs-lisp-mode
(defun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(new ";;; " (p (file-name-base (or (buffer-file-name) (buffer-name))) file) ".el --- " (p "summary" summary) " -*- lexical-binding: t -*-" n n
                ";;; Commentary:" n n

                ";;; Code:" n n

                (p "(message \"Hello, World\")") n n

                "(provide '" (s file) ")" n
                ";;; " (s file) ".el ends here")

markdown-ts-mode
(link (call-interactively #'markdown-insert-link))
(src (call-interactively #'markdown-insert-gfm-code-block))
