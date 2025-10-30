;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'gatsby>use-package)
(require 'gatsby>default)
(require 'gatsby>editing)
(require 'gatsby>terminal)
(require 'gatsby>project-management)

(require 'gatsby>lsp)

(require 'gatsby>python)
(require 'gatsby>elisp)
(require 'gatsby>config-files)
(require 'gatsby>text)

(require 'gatsby>ai)

(require 'gatsby>ui)
(require 'gatsby>remote)
