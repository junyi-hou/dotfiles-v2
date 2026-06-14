;;; gatsby-bazel.el --- bazel build file support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package bazel
  :ensure (:host github :repo "bazel-contrib/bazel")
  :mode
  ("BUILD\\'" . bazel-mode)
  ("BUILD\\.bazel\\'" . bazel-mode)
  ("WORKSPACE\\'" . bazel-mode)
  ("WORKSPACE\\.bazel\\'" . bazel-mode)
  ("\\.bzl\\'" . bazel-starlark-mode))

(provide 'gatsby-bazel)
;;; gatsby-bazel.el ends here
