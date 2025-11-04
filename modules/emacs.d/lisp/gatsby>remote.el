;;; gatsby>remote.el --- config for remote development via tramp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(gatsby>use-internal-pacakge tramp
  :defer t
  :custom
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)
  (vc-handled-backends '(Git))
  (tramp-use-scp-direct-remote-copying t)
  (tramp-copy-size-limit (* 1024 1024))
  (tramp-verbose 2)
  ;; enable envrc in tramp
  (envrc-remote t)
  :init
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocal "scp")
   'remote-direct-async-process)

  :config
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))

  ;; cache
  (defun gatsby>>cache-remote (key cache orig-fn &rest args)
    "Cache the value in CACHE if KEY is a remoote path."
    (if (and key (file-remote-p key))
        (if-let ((current (assoc key (symbol-value cache))))
            (cdr current)
          (let ((current (apply orig-fn args)))
            (set cache (cons (cons key current) (symbol-value cache)))
            current))
      (apply orig-fn args)))

  (defvar gatsby>project-current-cache nil)
  (advice-add #'project-current :around
              (defun gatsby>>cache-project-currnet (fn &optional prompt directory)
                (gatsby>>cache-remote (or directory
                                          project-current-directory-override
                                          default-directory)
                                      'gatsby>project-current-cache fn prompt directory)))

  (defvar gatsby>vc-git-root-cache nil)
  (advice-add #'vc-git-root :around
              (defun gatsby>>cache-vc-git-root (fn file)
                (let ((value (gatsby>>cache-remote (file-name-directory file) 'gatsby>vc-git-root-cache fn file)))
                  ;; sometimes vc-git-root returns nil even when there is a root there
                  (when (null (cdr (car gatsby>vc-git-root-cache)))
                    (setq vc-git-root-cache (cdr gatsby>vc-git-root-cache)))
                  value))))

(provide 'gatsby>remote)
;;; gatsby>remote.el ends here
