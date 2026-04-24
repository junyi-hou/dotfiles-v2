;;; gatsby-treesitter-test.el --- test that declared tree-sitter grammars are installed  -*- lexical-binding: t; -*-

(require 'ert)
(require 'treesit)
(require 'gatsby-test-utils)

(ert-deftest gatsby-treesitter--all-declared-grammars-installed ()
  "Every language declared via gatsby>install-treesitter-grammar must have its grammar installed."
  (let* ((langs (gatsby-test-utils--collect-treesitter-langs))
         (missing (cl-remove-if (lambda (lang) (treesit-ready-p lang t)) langs)))
    (should langs)
    (should-not missing)))

(provide 'gatsby-treesitter-test)
;;; gatsby-treesitter-test.el ends here
