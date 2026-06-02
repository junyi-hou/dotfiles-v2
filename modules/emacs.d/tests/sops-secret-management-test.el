;;; sops-secret-management-test.el --- tests for sops-secret-management.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'sops-secret-management)

;;; sops--passage-paths

(ert-deftest sops--passage-paths--empty-data ()
  (should (null (sops--passage-paths nil))))

(ert-deftest sops--passage-paths--flat-alist ()
  (let ((result (sops--passage-paths '((FOO . "bar") (BAZ . "qux")))))
    (should (= 2 (length result)))
    (should (member "FOO" result))
    (should (member "BAZ" result))))

(ert-deftest sops--passage-paths--skips-sops-key ()
  (let ((result (sops--passage-paths '((FOO . "bar") (sops . "metadata")))))
    (should (= 1 (length result)))
    (should (member "FOO" result))
    (should-not (member "sops" result))))

(ert-deftest sops--passage-paths--nested-alist ()
  (let ((result (sops--passage-paths '((env . ((FOO . "bar") (BAZ . "qux")))))))
    (should (= 2 (length result)))
    (should (member "env/FOO" result))
    (should (member "env/BAZ" result))))

(ert-deftest sops--passage-paths--deeply-nested ()
  (let ((result (sops--passage-paths '((a . ((b . ((c . "val")))))))))
    (should (= 1 (length result)))
    (should (member "a/b/c" result))))

(ert-deftest sops--passage-paths--with-prefix ()
  (let ((result (sops--passage-paths '((KEY . "val")) "base")))
    (should (= 1 (length result)))
    (should (member "base/KEY" result))))

(ert-deftest sops--passage-paths--nested-prefix-is-slash-separated ()
  (let ((result (sops--passage-paths '((group . ((item . "v")))) "root")))
    (should (= 1 (length result)))
    (should (member "root/group/item" result))))

(ert-deftest sops--passage-paths--mixed-flat-and-nested ()
  (let ((result (sops--passage-paths '((top . "v1") (env . ((FOO . "v2")))))))
    (should (= 2 (length result)))
    (should (member "top" result))
    (should (member "env/FOO" result))))

(ert-deftest sops--passage-paths--sops-skipped-at-nested-level ()
  "sops key is only skipped at the level where eq comparison is made."
  (let ((result (sops--passage-paths '((data . ((sops . "nested") (KEY . "v")))))))
    ;; sops nested inside another key: the outer eq check passes for 'data,
    ;; then recurses; inside, 'sops is skipped.
    (should (= 1 (length result)))
    (should (member "data/KEY" result))
    (should-not (member "data/sops" result))))

(provide 'sops-secret-management-test)
;;; sops-secret-management-test.el ends here
