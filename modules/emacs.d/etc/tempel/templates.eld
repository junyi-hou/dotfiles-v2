;;; Template syntax
;; p Inserts an unnamed placeholder field.
;; n Inserts a newline.
;; > Indents with indent-according-to-mode.
;; r Inserts the currently active region. If no region is active, a placeholder field is inserted. If tempel-done-on-region is non-nil, the template is finished when you jump to the field (see also q).
;; r> Acts like r, but indent region.
;; n> Inserts a newline and indents.
;; & Insert newline unless there is only whitespace between line start and point.
;; % Insert newline unless there is only whitespace between point and line end.
;; o Like % but leaves the point before newline.
;; (s NAME) Inserts a named field.
;; (p PROMPT <NAME> <NOINSERT>) Insert an optionally named field with a prompt. The PROMPT is displayed directly in the buffer as default value. If NOINSERT is non-nil, no field is inserted. Then the minibuffer is used for prompting and the value is bound to NAME.
;; (r PROMPT <NAME> <NOINSERT>) Insert region or act like (p ...).
;; (r> PROMPT <NAME> <NOINSERT>) Act like (r ...), but indent region.
;; (p FORM <NAME> <NOINSERT>) Like p described above, but FORM is evaluated.
;; (FORM ...) Other Lisp forms are evaluated. Named fields are lexically bound.
;; q Like p, but the template is finished if the user jumps to the field (see also r).

fundamental-mode
(sep comment-start (make-string (length content) ?=) n
     comment-start (p "header" content) n
     comment-start (make-string (length content) ?=))

emacs-lisp-mode
(defun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(defcom "(gatsby>defcommand " p " (" p ")\n  \"" p "\"" n> r> ")")
(let "(let* (" p ")" n> r> ")")
(header ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
        " --- " p " -*- lexical-binding: t -*-" n
        ";;; Commentary:" n ";;; Code:" n n)
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
         " ends here" n)
(package (i header) r n n (i provide))


markdown-ts-mode
(link (call-interactively #'markdown-insert-link))
(src (call-interactively #'markdown-insert-gfm-code-block))

org-mode
(src "#+begin_src " q n r n "#+end_src")
