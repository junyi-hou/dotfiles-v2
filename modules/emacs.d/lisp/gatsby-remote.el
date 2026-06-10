;;; gatsby-remote.el --- config for remote development via tramp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package tramp
  :ensure (:type git :repo "https://git.savannah.gnu.org/git/tramp.git")
  :custom
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)
  (remote-file-name-inhibit-cache 10)
  (vc-handled-backends '(Git))
  (tramp-use-scp-direct-remote-copying t)
  (tramp-copy-size-limit (* 1024 1024))
  (tramp-chunksize 4096)
  (tramp-ssh-controlmaster-options
   (concat
    "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
    "-o ControlMaster=auto -o ControlPersist=yes"))
  (tramp-use-connection-share t)
  (tramp-verbose 2)
  ;; enable envrc in tramp
  (envrc-remote t)
  (enable-remote-dir-locals t)
  :config

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq vc-ignore-dir-regexp
        (rx (or (regexp tramp-file-name-regexp) (regexp vc-ignore-dir-regexp))))

  (with-eval-after-load 'compile
    (remove-hook
     'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(use-package tramp-hlo
  :ensure (:host github :repo "jsadusk/tramp-hlo")
  :config (tramp-hlo-setup))

;; dependency for tramp-rpc
(use-package msgpack
  :ensure (:host github :repo "xuchunyang/msgpack.el"))

(use-package tramp-rpc
  :ensure (:host github :repo "ArthurHeymans/emacs-tramp-rpc")
  :after tramp
  :custom (tramp-rpc-deploy-git-build-policy 'release)
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


(provide 'gatsby-remote)
;;; gatsby-remote.el ends here
