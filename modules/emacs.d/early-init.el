;;; early-init.el --- things need to be done as early as possible -*- lexical-binding: t; -*-

;;; Commentary:
;; 1. GC optimization during initialization
;; 2. Use `elpaca' for package management
;; 3. Define custom modeline
;; 4. Define custom fringe

;;; Code:

;; GC optimization
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil)
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package (elpaca-use-package-mode))

;; mode-line
(defvar gatsby>right-mode-line '((:eval (gatsby>>mode-line-vc-info)) mode-name))

(defun gatsby>>format-right-mode-line ()
  (format-mode-line gatsby>right-mode-line))

(defun gatsby>>mode-line-vc-info ()
  (if (and vc-mode buffer-file-name)
      (let* ((backend (vc-backend buffer-file-name))
             (repo (file-name-nondirectory (directory-file-name (vc-root-dir))))
             (str (format "%s:%s "
                          repo
                          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))))
        (propertize (if (> (length str) 25)
                        (concat
                         (substring str 0 (- 25 3))
                         "...")
                      str)
                    'mouse-face 'mode-line-highlight))
    ""))

;; more buffer position percentage
(setq mode-line-percent-position '(-3 "%o"))

(setq-default mode-line-format
              (list " "
                    mode-line-mule-info
                    mode-line-modified
                    mode-line-remote " "
                    "%[%b%] "
                    mode-line-position
                    '(:eval (propertize
                             " " 'display
                             `((space :align-to (- (+ right right-fringe right-margin)
                                                   ,(+ 3 (string-width (gatsby>>format-right-mode-line))))))))
                    '(:eval (gatsby>>format-right-mode-line))))

;; fringe
(setq-default fringe-indicator-alist
              '((continuation nil nil)
                (truncation nil nil)
                (overlay-arrow . nil)
                (up . nil)
                (down . nil)
                (top nil nil)
                (bottom nil nil nil nil)
                (top-bottom nil nil nil nil)
                (empty-line . nil)
                (unknown . nil)))

(provide 'early-init)
;;; early-init.el ends here
