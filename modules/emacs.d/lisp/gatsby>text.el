;;; gatsby>text.el --- specific config for editing text files -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'gatsby>>utility)

(use-package markdown-ts-mode
  :ensure (:host github :repo "LionyxML/markdown-ts-mode")
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer t
  :init
  ;; treesitter
  (gatsby>install-treesitter-grammar
   'markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.4.1" "tree-sitter-markdown/src")
  (gatsby>install-treesitter-grammar 'markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.4.1" "tree-sitter-markdown-inline/src")

  ;; lsp
  
  )

(use-package typst-ts-mode
  :ensure (:host sourcehut :repo "meow_king/typst-ts-mode")
  :defer t
  :custom-face
  (typst-ts-markup-raw-blob-face ((t :inherit 'default)))
  (typst-ts-markup-rawspan-lang-face ((t :inherit 'default :weight bold)))
  :hook
  (typst-ts-mode . eglot-ensure)
  (typst-ts-mode . corfu-mode)
  (typst-ts-mode . display-line-numbers-mode)
  :init
  ;; tree-sitter
  (gatsby>install-treesitter-grammar
   'typst "https://github.com/uben0/tree-sitter-typst" "v0.11.0")

  ;; lsp
  (add-to-list 'eglot-server-programs `(typst-ts-mode ,(expand-file-name ".tools/bin/tinymist" gatsby>dotfiles-repo-location)))

  :config
  ;; automatically watch file when running `typst-ts-compile-and-preview'
  ;; let's watch only 1 file
  (defvar gatsby>>typst-watching-file nil)

  (defun gatsby>>typst-stop-watching (&rest _)
    (let ((current-file (buffer-file-name)))
     (when (and (file-equal-p current-file gatsby>>typst-watching-file)
               (process-live-p (get-buffer-process typst-ts-watch-process-buffer-name)))
      (typst-ts-watch-stop)
      (setq gatsby>>typst-watching-file nil))))

  (defun gatsby>>typst-start-watching (&rest _)
    (cl-letf (((symbol-function #'message) #'ignore)
              (current-file (buffer-file-name)))

      ;; turn off watching if we are watching other file
      (when (and gatsby>>typst-watching-file
                 (file-equal-p current-file gatsby>>typst-watching-file)
                 (y-or-n-p (format "Watch is running on %s, kill it now? " gatsby>>typst-watching-file)))
        (typst-ts-watch-stop)
        (setq gatsby>>typst-watching-file nil))

      ;; start watching if `gatsby>>typst-watching-file' is null
      (unless gatsby>>typst-watching-file
        (typst-ts-watch-start)
        (setq gatsby>>typst-watching-file (buffer-file-name))

        ;; setup stop watching hook
        (add-hook 'kill-buffer-hook #'gatsby>>typst-stop-watching nil t))))

  (advice-add #'typst-ts-compile-and-preview :before #'gatsby>>typst-start-watching)

  :evil-bind
  ((:maps typst-ts-mode-map :states normal)
   ("SPC r r" . #'typst-ts-compile-and-preview)
   ("SPC r o" . #'typst-ts-preview)))

(provide 'gatsby>text)
;;; gatsby>text.el ends here
