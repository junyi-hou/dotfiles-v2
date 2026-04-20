;;; gatsby>remote.el --- config for remote development via tramp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package tramp
  :ensure (:type git :repo "https://git.savannah.gnu.org/git/tramp.git")
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

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  (with-eval-after-load 'compile
    (remove-hook
     'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

;; dependency for tramp-rpc
(use-package msgpack
  :ensure (:host github :repo "xuchunyang/msgpack.el"))

;; FIXME: HEAD has recursive require, use 7d96f4c
(use-package tramp-rpc
  :ensure (:host github :repo "ArthurHeymans/emacs-tramp-rpc")
  :after tramp
  :config
  (defun eshell/rpc (&rest args)
    "Connect to a remote host via tramp-rpc in eshell."
    (let* ((_args
            (thread-last
             args
             (mapcar (lambda (i) (format "%s" i)))
             (cl-remove-if (lambda (s) (string-prefix-p "-" s)))))
           (host (format "/rpc:%s:~" (car _args))))
      (if (> (length _args) 1)
          (user-error
           (format
            "eshell/rpc accepts only one argument in the form of user@hostname, got %s instead"
            _args))
        (apply #'eshell/cd `(,host))))))


(provide 'gatsby>remote)
;;; gatsby>remote.el ends here
