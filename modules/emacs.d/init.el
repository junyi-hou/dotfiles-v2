;;; init.el --- emacs config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set `gastby>dotfile-repo-location' variable by finding the symlinked dotfiles.
;; Only work if the config is installed correctly according to the instruction in
;; `README.md'.  Display a warning if cannot find the repo location, in which case some
;; functionalities will not be available
(let ((repo-location
       (locate-dominating-file
        (file-truename (expand-file-name "init.el" user-emacs-directory)) ".git")))
  (if repo-location
      (defconst gatsby>dotfiles-repo-location repo-location
        "The git root of the dotfile repo")
    (display-warning
     'Dotfiles
     "Unable to find dotfiles repo location. Did you install the configuration using `make install'?")))


(require 'gatsby>use-package)
(require 'gatsby>default)
(require 'gatsby>editing)
(require 'gatsby>terminal)
(require 'gatsby>project-management)

(require 'gatsby>lsp)
(require 'gatsby>repl)

(require 'gatsby>python)
(require 'gatsby>clojure)
(require 'gatsby>elisp)
(require 'gatsby>sh-scripts)
(require 'gatsby>config-files)
(require 'gatsby>text)

(require 'gatsby>ai)

(require 'gatsby>ui)
(require 'gatsby>remote)

;;; init.el ends here
