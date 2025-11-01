;;; gatsby>nix.el --- nix specific config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package nix-ts-mode
  :ensure (:host github :repo "nix-community/nix-ts-mode")
  :mode "\\.nix\\'")

(provide 'gatsby>nix)
;;; gatsby>nix.el ends here
