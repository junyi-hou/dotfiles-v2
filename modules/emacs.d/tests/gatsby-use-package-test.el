;;; gatsby-use-package-test.el --- tests for gatsby>use-package.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'gatsby>use-package)

;; Pure function tests

(ert-deftest gatsby>>evil-bind-config-p--valid-config ()
  "Test detection of valid :evil-bind config."
  (should (gatsby>>evil-bind-config-p '(:maps normal)))
  (should (gatsby>>evil-bind-config-p '(:maps (normal insert))))
  (should (gatsby>>evil-bind-config-p '(:states normal))))

(ert-deftest gatsby>>evil-bind-config-p--invalid-config ()
  "Test rejection of invalid :evil-bind config."
  (should-not (gatsby>>evil-bind-config-p '(:invalid-key value)))
  (should-not (gatsby>>evil-bind-config-p '())))

(ert-deftest gatsby>>ensure-list--nil ()
  "Test conversion of nil to empty list."
  (should (equal (gatsby>>ensure-list nil) '())))

(ert-deftest gatsby>>ensure-list--symbol ()
  "Test conversion of symbol to list."
  (should (equal (gatsby>>ensure-list 'foo) '(foo))))

(ert-deftest gatsby>>ensure-list--list ()
  "Test that list is returned unchanged."
  (should (equal (gatsby>>ensure-list '(a b c)) '(a b c))))

(ert-deftest gatsby>>maybe-evil-state-to-keymap--known-state ()
  "Test conversion of known evil states to keymap names."
  (should (equal (gatsby>>maybe-evil-state-to-keymap 'normal) 'evil-normal-state-map))
  (should (equal (gatsby>>maybe-evil-state-to-keymap 'insert) 'evil-insert-state-map))
  (should (equal (gatsby>>maybe-evil-state-to-keymap 'visual) 'evil-visual-state-map)))

(ert-deftest gatsby>>maybe-evil-state-to-keymap--unknown-symbol ()
  "Test that unknown symbols are returned unchanged."
  (should (equal (gatsby>>maybe-evil-state-to-keymap 'my-keymap) 'my-keymap))
  (should (equal (gatsby>>maybe-evil-state-to-keymap 'foo-map) 'foo-map)))

(ert-deftest use-package-normalize/:evil-bind--simple-bindings ()
  "Test use-package-normalize/:evil-bind with simple bindings."
  (let ((result (use-package-normalize/:evil-bind 'test-pkg :evil-bind '((("C-c" . #'foo-cmd))))))
    (should (eq (car result) :block))
    (should (eq (caddr result) 'global-map))))

(ert-deftest use-package-normalize/:evil-bind--with-config ()
  "Test use-package-normalize/:evil-bind with explicit config."
  (let ((result (use-package-normalize/:evil-bind 'test-pkg :evil-bind '(((:maps foo-map) ("C-c" . #'foo-cmd))))))
    (should (eq (car result) :block))
    (should (eq (caddr result) 'foo-map))))

(ert-deftest use-package-normalize/:evil-bind--multiple-blocks ()
  "Test use-package-normalize/:evil-bind with multiple binding blocks."
  (let ((result (use-package-normalize/:evil-bind 'test-pkg :evil-bind
                  '((("C-c" . #'cmd1) (:maps bar-map) ("C-d" . #'cmd2))))))
    ;; Should have two blocks - first with global-map, second with bar-map
    (should (eq (car result) :block))
    (should (eq (caddr result) 'global-map))
    (should (member :block (cdr result)))
    (should (member 'bar-map result))))

(ert-deftest use-package-normalize/:evil-bind--multiple-blocks-maps-only ()
  "Test with multiple blocks each having only :maps configuration."
  (let ((result (use-package-normalize/:evil-bind 'test-pkg :evil-bind
                  '(((:maps foo-map) ("C-c" . #'cmd1) (:maps bar-map) ("C-d" . #'cmd2))))))
    (should (eq (car result) :block))
    (should (eq (caddr result) 'foo-map))
    (should (member :block (cdr result)))
    (should (member 'bar-map result))
    (should (member '(function cmd1) result))
    (should (member '(function cmd2) result))))

(ert-deftest use-package-normalize/:evil-bind--multiple-blocks-states-only ()
  "Test with multiple blocks each having only :states configuration."
  (let ((result (use-package-normalize/:evil-bind 'test-pkg :evil-bind
                  '(((:states normal) ("C-c" . #'cmd1) (:states visual) ("C-d" . #'cmd2))))))
    (should (eq (car result) :block))
    (should (eq (caddr result) 'evil-normal-state-map))
    (should (member :block (cdr result)))
    (should (member 'evil-visual-state-map result))
    (should (member '(function cmd1) result))
    (should (member '(function cmd2) result))))

(ert-deftest use-package-normalize/:evil-bind--multiple-blocks-mixed-config ()
  "Test with multiple blocks mixing :maps, :states, and both."
  (let ((result (use-package-normalize/:evil-bind 'test-pkg :evil-bind
                  '(((:maps foo-map) ("C-c" . #'cmd1) (:states normal) ("C-d" . #'cmd2) (:maps bar-map :states visual) ("C-e" . #'cmd3))))))
    (should (eq (car result) :block))
    (should (member 'foo-map result))
    (should (member 'evil-normal-state-map result))
    (should (member 'bar-map result))
    (should (member '(function cmd1) result))
    (should (member '(function cmd2) result))
    (should (member '(function cmd3) result))))

(ert-deftest use-package-handler/:evil-bind--generates-code ()
  "Test that use-package-handler/:evil-bind generates expected s-expressions."
  (let* ((normalized-args (use-package-normalize/:evil-bind 'my-pkg :evil-bind '(((:maps foo-map) ("C" . #'c-cmd) (:maps (normal insert)) ("P" . #'p-cmd) ("S" . #'s-cmd) (:maps bar-map :states insert) ("T" . #'t-cmd)))))
         (result (use-package-handler/:evil-bind 'my-pkg :evil-bind normalized-args '() nil)))
    (should (equal (car result) '(with-eval-after-load 'evil
                                   (evil-define-key 'nil foo-map "C" (function c-cmd))
                                   (evil-define-key 'nil evil-normal-state-map "P" (function p-cmd) "S" (function s-cmd))
                                   (evil-define-key 'nil evil-insert-state-map "P" (function p-cmd) "S" (function s-cmd))
                                   (evil-define-key '(insert) bar-map "T" (function t-cmd)))))))

(ert-deftest use-package-autoloads/:evil-bind--simple-binding ()
  "Test use-package-autoloads/:evil-bind with a single command."
  (let* ((normalized-args (use-package-normalize/:evil-bind 'my-pkg :evil-bind '(((:maps foo-map) ("C-c" . #'my-cmd)))))
         (result (use-package-autoloads/:evil-bind 'my-pkg :evil-bind normalized-args)))
    (should (equal result '((my-cmd . command))))))

(ert-deftest use-package-autoloads/:evil-bind--multiple-commands ()
  "Test extraction of multiple commands."
  (let* ((normalized-args (use-package-normalize/:evil-bind 'my-pkg :evil-bind '(((:maps foo-map) ("C-c" . #'cmd1) ("C-d" . #'cmd2) ("C-e" . #'cmd3)))))
         (result (use-package-autoloads/:evil-bind 'my-pkg :evil-bind normalized-args)))
    (should (= 3 (length result)))
    (should (member '(cmd1 . command) result))
    (should (member '(cmd2 . command) result))
    (should (member '(cmd3 . command) result))))

(ert-deftest use-package-autoloads/:evil-bind--duplicate-commands ()
  "Test that duplicate commands are removed."
  (let* ((normalized-args (use-package-normalize/:evil-bind 'my-pkg :evil-bind '(((:maps foo-map) ("C-c" . #'same-cmd) ("C-d" . #'same-cmd) ("C-e" . #'other-cmd)))))
         (result (use-package-autoloads/:evil-bind 'my-pkg :evil-bind normalized-args)))
    (should (= 2 (length result)))
    (should (member '(other-cmd . command) result))
    (should (member '(same-cmd . command) result))))

(ert-deftest use-package-autoloads/:evil-bind--multiple-maps ()
  "Test extraction from multiple maps."
  (let* ((normalized-args (use-package-normalize/:evil-bind 'my-pkg :evil-bind '(((:maps foo-map) ("C" . #'cmd1) (:maps bar-map) ("D" . #'cmd2)))))
         (result (use-package-autoloads/:evil-bind 'my-pkg :evil-bind normalized-args)))
    (should (= 2 (length result)))
    (should (member '(cmd1 . command) result))
    (should (member '(cmd2 . command) result))))

(ert-deftest use-package-autoloads/:evil-bind--with-states ()
  "Test extraction from blocks with :states."
  (let* ((normalized-args (use-package-normalize/:evil-bind 'my-pkg :evil-bind '(((:states (normal visual)) ("C-c" . #'cmd1) ("C-d" . #'cmd2)))))
         (result (use-package-autoloads/:evil-bind 'my-pkg :evil-bind normalized-args)))
    (should (= 2 (length result)))
    (should (member '(cmd1 . command) result))
    (should (member '(cmd2 . command) result))))

(ert-deftest use-package-autoloads/:evil-bind--mixed-maps-and-states ()
  "Test extraction from blocks mixing :maps and :states."
  (let* ((normalized-args (use-package-normalize/:evil-bind 'my-pkg :evil-bind '(((:maps foo-map) ("C" . #'cmd1) (:states normal) ("D" . #'cmd2) (:maps bar-map :states visual) ("E" . #'cmd3)))))
         (result (use-package-autoloads/:evil-bind 'my-pkg :evil-bind normalized-args)))
    (should (= 3 (length result)))
    (should (member '(cmd1 . command) result))
    (should (member '(cmd2 . command) result))
    (should (member '(cmd3 . command) result))))

(ert-deftest use-package-autoloads/:evil-bind--empty-args ()
  "Test with no bindings."
  (let ((result (use-package-autoloads/:evil-bind 'my-pkg :evil-bind '())))
    (should (null result))))

(ert-deftest use-package-normalize/:evil-bind--nil-command ()
  "Test that (\"key\" . nil) bindings are normalized correctly."
  (let ((result (use-package-normalize/:evil-bind 'test-pkg :evil-bind '((("C-c" . nil))))))
    (should (eq (car result) :block))
    (should (eq (caddr result) 'global-map))
    (should (member nil result))))

(ert-deftest use-package-normalize/:evil-bind--nil-command-with-map ()
  "Test that (\"key\" . nil) bindings are normalized correctly with explicit map."
  (let ((result (use-package-normalize/:evil-bind 'test-pkg :evil-bind '(((:maps foo-map) ("C-c" . nil))))))
    (should (eq (car result) :block))
    (should (eq (caddr result) 'foo-map))
    (should (member nil result))))

(ert-deftest use-package-handler/:evil-bind--nil-command ()
  "Test that (\"key\" . nil) generates (evil-define-key ... key nil)."
  (let* ((normalized-args (use-package-normalize/:evil-bind 'my-pkg :evil-bind '(((:maps foo-map) ("C-c" . nil)))))
         (result (use-package-handler/:evil-bind 'my-pkg :evil-bind normalized-args '() nil)))
    (should (equal (car result) `(with-eval-after-load 'evil
                                   (evil-define-key 'nil foo-map ,(kbd "C-c") nil))))))

(ert-deftest use-package-autoloads/:evil-bind--nil-command-excluded ()
  "Test that nil commands are excluded from autoloads."
  (let* ((normalized-args (use-package-normalize/:evil-bind 'my-pkg :evil-bind '(((:maps foo-map) ("C-c" . nil) ("C-d" . #'real-cmd)))))
         (result (use-package-autoloads/:evil-bind 'my-pkg :evil-bind normalized-args)))
    (should (= 1 (length result)))
    (should (equal result '((real-cmd . command))))))

(provide 'gatsby-use-package-test)
;;; gatsby-use-package-test.el ends here
