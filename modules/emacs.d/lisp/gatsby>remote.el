;;; gatsby>remote.el --- config for remote development via tramp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(gatsby>use-internal-package tramp
  :custom
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)
  (vc-handled-backends '(Git))
  (tramp-use-scp-direct-remote-copying t)
  (tramp-copy-size-limit (* 1024 1024))
  (tramp-chunksize 500)
  (tramp-ssh-controlmaster-options
   (concat
    "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
    "-o ControlMaster=auto -o ControlPersist=yes"))
  (tramp-use-connection-share t)
  (tramp-verbose 2)
  ;; enable envrc in tramp
  (envrc-remote t)

  :config
  (with-eval-after-load 'compile
    (remove-hook
     'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(use-package tramp-rpc
  :preface
  ;; TODO - remove manual dependency installation when upstream supports it
  (use-package msgpack
    :ensure (:host github :repo "xuchunyang/msgpack.el"))
  :ensure (:host github :repo "ArthurHeymans/emacs-tramp-rpc")
  :config

  (defun gatsby>>rpc-eshell/ssh (&rest args)
    "Use tramp-rpc for ssh."
    (let* ((_args
            (thread-last
             args
             (mapcar (lambda (i) (format "%s" i)))
             (cl-remove-if (lambda (s) (string-prefix-p "-" s)))))
           (host (format "/rpc:%s:~" (car _args))))
      (if (> (length _args) 1)
          (user-error
           (format
            "eshell/ssh accepts only one argument in the form of user@hostname, got %s instead"
            _args))
        (apply #'eshell/cd `(,host)))))

  (advice-add #'eshell/ssh :override #'gatsby>>rpc-eshell/ssh))

(provide 'gatsby>remote)
;;; gatsby>remote.el ends here
