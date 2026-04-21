;;; gatsby-default-test.el --- tests for gatsby>default.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'gatsby>default)

(ert-deftest gatsby>split-window-sensibly--wide-window ()
  "Test horizontal split when window is wider than 2*fill-column."
  (let ((fill-column 80))
    (cl-letf (((symbol-function 'window-width) (lambda (&optional _) 200))
              ((symbol-function 'split-window-right) (lambda (&optional _) 'split-result)))
      (let ((result (gatsby>split-window-sensibly)))
        (should (equal result 'split-result))))))

(ert-deftest gatsby>split-window-sensibly--narrow-window ()
  "Test vertical split when window is narrower than 2*fill-column."
  (let ((fill-column 80))
    (cl-letf (((symbol-function 'window-width) (lambda (&optional _) 150))
              ((symbol-function 'split-window-below) (lambda (&optional _) 'split-result)))
      (let ((result (gatsby>split-window-sensibly)))
        (should (equal result 'split-result))))))

(ert-deftest gatsby>>compilation-sentinel--non-zero-exit-calls-display-buffer ()
  "Test that display-buffer is called on non-zero exit status."
  (let ((display-buffer-called nil))
    (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) (setq display-buffer-called t))))
      (let ((mock-process (make-process :name "test" :buffer "*test*" :command '("true"))))
        (cl-letf (((symbol-function 'process-exit-status) (lambda (_) 1))
                  ((symbol-function 'process-buffer) (lambda (_) (get-buffer "*test*"))))
          (gatsby>>compilation-sentinel mock-process nil)
          (should display-buffer-called))))))

(ert-deftest gatsby>>compilation-sentinel--zero-exit-no-display ()
  "Test that display-buffer is not called on zero exit status."
  (let ((display-buffer-called nil))
    (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) (setq display-buffer-called t))))
      (let ((mock-process (make-process :name "test" :buffer "*test*" :command '("true"))))
        (cl-letf (((symbol-function 'process-exit-status) (lambda (_) 0))
                  ((symbol-function 'process-buffer) (lambda (_) (get-buffer "*test*"))))
          (gatsby>>compilation-sentinel mock-process nil)
          (should-not display-buffer-called))))))

(ert-deftest gatsby>>newline--non-comment-line ()
  "Non-comment line: call newline-fun without inserting extra text."
  (with-temp-buffer
    (insert "hello world")
    (let ((called nil)
          (comment-start-skip nil))
      (gatsby>>newline (lambda (&rest _) (setq called t)))
      (should called)
      (should (string= "hello world" (buffer-string))))))

(ert-deftest gatsby>>newline--comment-line-first-newline ()
  "Comment line, first newline: call newline-fun then insert comment prefix."
  (with-temp-buffer
    (insert ";; some text")
    (let ((comment-start-skip ";+\\s-*")
          (last-command 'other))
      (gatsby>>newline (lambda (&rest _) (insert "\n")))
      (should (string= ";; some text\n;; " (buffer-string))))))

(ert-deftest gatsby>>newline--comment-line-second-newline ()
  "Comment line, second newline: delete comment prefix, keep leading whitespace."
  (with-temp-buffer
    (insert ";; some text\n;; ")
    (goto-char (point-max))
    (let ((comment-start-skip ";+\\s-*")
          (last-command 'newline))
      (gatsby>>newline (lambda (&rest _) (insert "\n")))
      (should (string= ";; some text\n" (buffer-string))))))

(ert-deftest gatsby>>newline--comment-line-second-newline-with-indent ()
  "Second newline on indented comment line: keep leading whitespace only."
  (with-temp-buffer
    (insert "  ;; some text\n  ;; ")
    (goto-char (point-max))
    (let ((comment-start-skip ";+\\s-*")
          (last-command 'newline))
      (gatsby>>newline (lambda (&rest _) (insert "\n")))
      (should (string= "  ;; some text\n  " (buffer-string))))))

(ert-deftest gatsby>>balance-windows-ignore-side--no-side-windows ()
  "When there are no side windows, call orig-fun directly."
  (let ((orig-called nil))
    (cl-letf (((symbol-function 'window-list) (lambda () '(win1 win2)))
              ((symbol-function 'window-parameter) (lambda (_win _param) nil)))
      (gatsby>>balance-windows-ignore-side (lambda (&rest _) (setq orig-called t)))
      (should orig-called))))

(ert-deftest gatsby>>balance-windows-ignore-side--side-windows-deleted-before-balance ()
  "Side windows are deleted before orig-fun is called."
  (let ((deleted-windows nil)
        (orig-called nil)
        (side-win 'side-win)
        (normal-win 'normal-win))
    (cl-letf (((symbol-function 'window-list) (lambda () (list side-win normal-win)))
              ((symbol-function 'window-parameter)
               (lambda (win param)
                 (when (and (eq win side-win) (eq param 'window-side)) 'bottom)))
              ((symbol-function 'window-buffer) (lambda (_) (get-buffer-create "*test-side*")))
              ((symbol-function 'delete-window) (lambda (w) (push w deleted-windows)))
              ((symbol-function 'display-buffer-in-side-window) (lambda (_buf _alist) normal-win))
              ((symbol-function 'set-window-dedicated-p) #'ignore)
              ((symbol-function 'window-resize) #'ignore)
              ((symbol-function 'window-width) (lambda (_) 20))
              ((symbol-function 'window-height) (lambda (_) 10)))
      (gatsby>>balance-windows-ignore-side (lambda (&rest _) (setq orig-called t)))
      (should orig-called)
      (should (member side-win deleted-windows)))))

(ert-deftest gatsby>>balance-windows-ignore-side--side-windows-restored-after-balance ()
  "Side windows are restored after orig-fun completes."
  (let ((restored-buffers nil)
        (side-win 'side-win)
        (normal-win 'normal-win)
        (test-buf (get-buffer-create "*test-side-restore*")))
    (cl-letf (((symbol-function 'window-list) (lambda () (list side-win normal-win)))
              ((symbol-function 'window-parameter)
               (lambda (win param)
                 (cond
                  ((and (eq win side-win) (eq param 'window-side)) 'right)
                  ((and (eq win side-win) (eq param 'window-slot)) 0)
                  (t nil))))
              ((symbol-function 'window-buffer) (lambda (_) test-buf))
              ((symbol-function 'delete-window) #'ignore)
              ((symbol-function 'display-buffer-in-side-window)
               (lambda (buf _alist) (push buf restored-buffers) normal-win))
              ((symbol-function 'set-window-dedicated-p) #'ignore)
              ((symbol-function 'window-resize) #'ignore)
              ((symbol-function 'window-width) (lambda (_) 30))
              ((symbol-function 'window-height) (lambda (_) 10)))
      (gatsby>>balance-windows-ignore-side #'ignore)
      (should (member test-buf restored-buffers)))))

(ert-deftest gatsby>>balance-windows-ignore-side--side-windows-restored-on-error ()
  "Side windows are restored even if orig-fun signals an error."
  (let ((restored-buffers nil)
        (side-win 'side-win)
        (normal-win 'normal-win)
        (test-buf (get-buffer-create "*test-side-error*")))
    (cl-letf (((symbol-function 'window-list) (lambda () (list side-win normal-win)))
              ((symbol-function 'window-parameter)
               (lambda (win param)
                 (cond
                  ((and (eq win side-win) (eq param 'window-side)) 'left)
                  ((and (eq win side-win) (eq param 'window-slot)) 0)
                  (t nil))))
              ((symbol-function 'window-buffer) (lambda (_) test-buf))
              ((symbol-function 'delete-window) #'ignore)
              ((symbol-function 'display-buffer-in-side-window)
               (lambda (buf _alist) (push buf restored-buffers) normal-win))
              ((symbol-function 'set-window-dedicated-p) #'ignore)
              ((symbol-function 'window-resize) #'ignore)
              ((symbol-function 'window-width) (lambda (_) 20))
              ((symbol-function 'window-height) (lambda (_) 10)))
      (ignore-errors
        (gatsby>>balance-windows-ignore-side (lambda (&rest _) (error "test error"))))
      (should (member test-buf restored-buffers)))))

(provide 'gatsby-default-test)
;;; gatsby-default-test.el ends here
