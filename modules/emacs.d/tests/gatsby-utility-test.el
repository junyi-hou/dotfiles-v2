;;; gatsby-utility-test.el --- tests for gatsby>>utility.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'gatsby>>utility)

;; Macro expansion tests

(ert-deftest gatsby>use-internal-package--basic-expansion ()
  "Test that gatsby>use-internal-package expands to use-package with :ensure nil."
  (let ((expanded (macroexpand-1 '(gatsby>use-internal-package foo))))
    (should (eq (car expanded) 'use-package))
    (should (eq (cadr expanded) 'foo))
    (should (member :ensure expanded))
    (should (member nil expanded))))

(ert-deftest gatsby>use-internal-package--with-keywords ()
  "Test gatsby>use-internal-package preserves other keywords."
  (let ((expanded (macroexpand-1 '(gatsby>use-internal-package foo :hook (bar . baz)))))
    (should (eq (car expanded) 'use-package))
    (should (member :hook expanded))
    (should (member :ensure expanded))))

(ert-deftest gatsby>defcommand--no-args ()
  "Test gatsby>defcommand with no arguments expands to (interactive)."
  (let ((expanded (macroexpand-1 '(gatsby>defcommand foo () "doc" (message "test")))))
    (should (eq (car expanded) 'defun))
    (should (eq (cadr expanded) 'foo))
    (should (stringp (cadddr expanded)))
    (should (member '(interactive) expanded))))

(ert-deftest gatsby>defcommand--region-args ()
  "Test gatsby>defcommand with (beg end) args expands to (interactive \"r\")."
  (let ((expanded (macroexpand-1 '(gatsby>defcommand foo (beg end) (message "test")))))
    (should (eq (car expanded) 'defun))
    (should (equal (caddr expanded) '(beg end)))
    (should (member '(interactive "r") expanded))))

(ert-deftest gatsby>defcommand--single-arg ()
  "Test gatsby>defcommand with single arg expands to (interactive \"P\")."
  (let ((expanded (macroexpand-1 '(gatsby>defcommand foo (x) (message "test")))))
    (should (eq (car expanded) 'defun))
    (should (equal (caddr expanded) '(x)))
    (should (member '(interactive "P") expanded))))

(ert-deftest gatsby>defcommand--rest-args ()
  "Test gatsby>defcommand with &rest args expands to (interactive)."
  (let ((expanded (macroexpand-1 '(gatsby>defcommand foo (&rest args) (message "test")))))
    (should (eq (car expanded) 'defun))
    (should (member '(interactive) expanded))))

(ert-deftest gatsby>defcommand--keyword-args ()
  "Test gatsby>defcommand with keyword args creates proper interactive form."
  (let ((expanded (macroexpand-1 '(gatsby>defcommand foo (:x 42) (message "test")))))
    (should (eq (car expanded) 'defun))
    (should (eq (cadr expanded) 'foo))
    (should (equal (caddr expanded) '(x)))
    (should (member '(interactive (list 42)) expanded))))

(ert-deftest gatsby>defcommand--docstring ()
  "Test gatsby>defcommand preserves docstring."
  (let ((expanded (macroexpand-1 '(gatsby>defcommand foo () "my docstring" (message "test")))))
    (should (eq (car expanded) 'defun))
    (should (equal (cadddr expanded) "my docstring"))))

;; Functional tests using real filesystem

(ert-deftest gatsby>retrieve-or-save-item--read-existing-file ()
  "Test reading an existing cache file."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert ";;; -*- coding: utf-8 -*-\n")
            (pp '(item1 item2) (current-buffer))
            (write-region (point-min) (point-max) cache-file))
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (equal result '(item1 item2)))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--save-to-empty-file ()
  "Test saving a new item to an empty cache file."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          (gatsby>retrieve-or-save-item cache-file 'new-item)
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (equal result '("new-item")))))
      (delete-file cache-file))))


(ert-deftest gatsby>retrieve-or-save-item--write-format ()
  "Test that saving creates proper format with header comment."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          (gatsby>retrieve-or-save-item cache-file 'test-item)
          (with-temp-buffer
            (insert-file-contents cache-file)
            (let ((content (buffer-string)))
              (should (string-match-p ";;; .*coding: utf-8" content))
              (should (string-match-p "\"test-item\"" content)))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--preserves-item-type ()
  "Test that saving symbols converts them to strings."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          (gatsby>retrieve-or-save-item cache-file 'my-symbol)
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (equal result '("my-symbol")))
            (should (stringp (car result)))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--deduplicates-with-equal ()
  "Test that deduplication uses equal, not eq."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          ;; Save duplicate string items
          (gatsby>retrieve-or-save-item cache-file "item1")
          (gatsby>retrieve-or-save-item cache-file "item1")
          (gatsby>retrieve-or-save-item cache-file "item2")
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (= (length result) 2))
            (should (equal result '("item1" "item2")))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--deduplicates-symbols ()
  "Test that symbol deduplication works (converts to strings)."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          (gatsby>retrieve-or-save-item cache-file 'sym1)
          (gatsby>retrieve-or-save-item cache-file 'sym1)
          (gatsby>retrieve-or-save-item cache-file 'sym2)
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (= (length result) 2))
            (should (equal result '("sym1" "sym2")))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--max-length-limits-items ()
  "Test that max-length parameter limits the number of stored items."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          ;; Save multiple items with max-length 3
          (gatsby>retrieve-or-save-item cache-file 'a)
          (gatsby>retrieve-or-save-item cache-file 'b)
          (gatsby>retrieve-or-save-item cache-file 'c)
          (gatsby>retrieve-or-save-item cache-file 'd 3)
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (= (length result) 3))
            (should (member "d" result))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--null-max-length-keeps-all ()
  "Test that null max-length keeps all items."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          ;; Save multiple items with no max-length limit
          (gatsby>retrieve-or-save-item cache-file 'a)
          (gatsby>retrieve-or-save-item cache-file 'b)
          (gatsby>retrieve-or-save-item cache-file 'c)
          (gatsby>retrieve-or-save-item cache-file 'd)
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (= (length result) 4))
            (should (equal result '("a" "b" "c" "d")))))
      (delete-file cache-file))))

(ert-deftest gatsby>retrieve-or-save-item--max-length-one ()
  "Test max-length of 1 keeps only the most recent item."
  (let ((cache-file (make-temp-file "cache" nil ".el")))
    (unwind-protect
        (progn
          (gatsby>retrieve-or-save-item cache-file 'a)
          (gatsby>retrieve-or-save-item cache-file 'b 1)
          (let ((result (gatsby>retrieve-or-save-item cache-file)))
            (should (= (length result) 1))
            (should (equal result '("b")))))
      (delete-file cache-file))))

;; gatsby>switch-to-buffer-new-window tests

(ert-deftest gatsby>switch-to-buffer-new-window--switches-to-visible-buffer ()
  "Test switching to buffer that is already visible reuses the window."
  (let ((test-buffer (get-buffer-create "*test-buffer-visible*"))
        (initial-config (current-window-configuration)))
    (unwind-protect
        (progn
          ;; Set up: create two windows, display test-buffer in one
          (delete-other-windows)
          (split-window)
          (switch-to-buffer test-buffer)
          (let ((initial-window-count (length (window-list))))
            ;; Switch to the other window and call function
            (select-window (next-window))
            (gatsby>switch-to-buffer-new-window test-buffer)
            ;; Should reuse existing window, not create new one
            (should (= (length (window-list)) initial-window-count))
            (should (eq (current-buffer) test-buffer))))
      (set-window-configuration initial-config)
      (kill-buffer test-buffer))))

(ert-deftest gatsby>switch-to-buffer-new-window--creates-window-for-invisible-buffer ()
  "Test switching to invisible buffer creates a new window."
  (let ((test-buffer (get-buffer-create "*test-buffer-invisible*"))
        (initial-config (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (let ((initial-window-count (length (window-list))))
            (gatsby>switch-to-buffer-new-window test-buffer)
            ;; Should create new window
            (should (> (length (window-list)) initial-window-count))
            (should (eq (current-buffer) test-buffer))))
      (set-window-configuration initial-config)
      (kill-buffer test-buffer))))

(ert-deftest gatsby>switch-to-buffer-new-window--displays-correct-buffer ()
  "Test that the correct buffer is displayed after switching."
  (let ((test-buffer (get-buffer-create "*test-display-buffer*"))
        (initial-config (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (gatsby>switch-to-buffer-new-window test-buffer)
          (should (eq (current-buffer) test-buffer))
          (should (eq (window-buffer (selected-window)) test-buffer)))
      (set-window-configuration initial-config)
      (kill-buffer test-buffer))))

(ert-deftest gatsby>switch-to-buffer-new-window--multiple-windows ()
  "Test switching with multiple windows where buffer is visible in non-current window."
  (let ((test-buffer (get-buffer-create "*test-multi-window*"))
        (initial-config (current-window-configuration)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (split-window)
          (split-window)
          ;; Display test buffer in the second window
          (select-window (next-window))
          (switch-to-buffer test-buffer)
          (let ((target-window (selected-window)))
            ;; Move to first window
            (select-window (next-window))
            (gatsby>switch-to-buffer-new-window test-buffer)
            ;; Should have switched to the window where buffer is visible
            (should (eq (selected-window) target-window))
            (should (eq (current-buffer) test-buffer))))
      (set-window-configuration initial-config)
      (kill-buffer test-buffer))))

(provide 'gatsby-utility-test)
;;; gatsby-utility-test.el ends here
