;;; gatsby-bazel.el --- bazel build file support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package bazel
  :ensure (:host github :repo "bazel-contrib/bazel.el")
  :mode
  ("BUILD\\'" . bazel-mode)
  ("BUILD\\.bazel\\'" . bazel-mode)
  ("WORKSPACE\\'" . bazel-mode)
  ("WORKSPACE\\.bazel\\'" . bazel-mode)
  ("\\.bzl\\'" . bazel-starlark-mode)
  :config
  ;; better monorepo support
  (add-to-list 'project-vc-extra-root-markers "BUILD.bazel"))

(provide 'gatsby-bazel)
;;; gatsby-bazel.el ends here
