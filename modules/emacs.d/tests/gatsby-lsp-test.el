;;; gatsby-lsp-test.el --- tests for gatsby>lsp.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'gatsby>lsp)

(defmacro gatsby>>with-corfu-state (index last-cmd &rest body)
  "Run BODY with `corfu--index' set to INDEX and `last-command' to LAST-CMD.
Stubs `corfu-insert', `corfu-expand', and `corfu--goto' to record calls."
  (declare (indent 2))
  `(let ((corfu--index ,index)
         (last-command ,last-cmd)
         calls)
     (cl-letf (((symbol-function 'corfu-insert)
                (lambda () (push 'corfu-insert calls)))
               ((symbol-function 'corfu-expand)
                (lambda () (push 'corfu-expand calls)))
               ((symbol-function 'corfu--goto)
                (lambda (n) (push (list 'corfu--goto n) calls))))
       ,@body
       (nreverse calls))))

(ert-deftest gatsby>corfu-complete--expands-on-first-call ()
  "Calls `corfu-expand' when last-command is unrelated."
  (let ((result (gatsby>>with-corfu-state 0 'self-insert-command
                  (gatsby>corfu-complete))))
    (should (equal result '(corfu-expand)))))

(ert-deftest gatsby>corfu-complete--inserts-when-last-was-self ()
  "Calls `corfu-insert' when last-command is `gatsby>corfu-complete'."
  (let ((result (gatsby>>with-corfu-state 0 'gatsby>corfu-complete
                  (gatsby>corfu-complete))))
    (should (equal result '(corfu-insert)))))

(ert-deftest gatsby>corfu-complete--inserts-when-last-was-corfu-complete ()
  "Calls `corfu-insert' when last-command is `corfu-complete'."
  (let ((result (gatsby>>with-corfu-state 0 'corfu-complete
                  (gatsby>corfu-complete))))
    (should (equal result '(corfu-insert)))))

(ert-deftest gatsby>corfu-complete--inserts-when-last-was-corfu-next ()
  "Calls `corfu-insert' when last-command is `corfu-next'."
  (let ((result (gatsby>>with-corfu-state 0 'corfu-next
                  (gatsby>corfu-complete))))
    (should (equal result '(corfu-insert)))))

(ert-deftest gatsby>corfu-complete--inserts-when-last-was-corfu-previous ()
  "Calls `corfu-insert' when last-command is `corfu-previous'."
  (let ((result (gatsby>>with-corfu-state 0 'corfu-previous
                  (gatsby>corfu-complete))))
    (should (equal result '(corfu-insert)))))

(ert-deftest gatsby>corfu-complete--goto-0-when-index-negative ()
  "Calls `corfu--goto 0' before `corfu-insert' when index is negative."
  (let ((result (gatsby>>with-corfu-state -1 'corfu-next
                  (gatsby>corfu-complete))))
    (should (equal result '((corfu--goto 0) corfu-insert)))))

(ert-deftest gatsby>corfu-complete--no-goto-when-index-zero ()
  "Skips `corfu--goto' when index is already 0."
  (let ((result (gatsby>>with-corfu-state 0 'corfu-next
                  (gatsby>corfu-complete))))
    (should (equal result '(corfu-insert)))))

(ert-deftest gatsby>corfu-complete--no-goto-when-index-positive ()
  "Skips `corfu--goto' when a candidate is already selected."
  (let ((result (gatsby>>with-corfu-state 2 'corfu-next
                  (gatsby>corfu-complete))))
    (should (equal result '(corfu-insert)))))

(defmacro gatsby>>with-eldoc-transiant-env (&rest body)
  "Run BODY with a fresh posframe buffer and `eldoc-mouse--hide-posframe' stubbed.
Cleans up the buffer and any active transient map afterward."
  (declare (indent 0))
  `(let ((buf (get-buffer-create eldoc-mouse-posframe-buffer-name)))
     (unwind-protect
         (cl-letf (((symbol-function 'eldoc-mouse--hide-posframe) #'ignore))
           ,@body)
       (with-current-buffer buf
         (when (functionp gatsby>>eldoc-restore-keymap-fn)
           (funcall gatsby>>eldoc-restore-keymap-fn))
         (setq-local gatsby>>eldoc-restore-keymap-fn nil))
       (kill-buffer buf))))

(ert-deftest gatsby>>eldoc-enable-transiant-map--sets-restore-fn ()
  "Sets `gatsby>>eldoc-restore-keymap-fn' to a function in the posframe buffer."
  (gatsby>>with-eldoc-transiant-env
    (gatsby>>eldoc-enable-transiant-map)
    (with-current-buffer (get-buffer eldoc-mouse-posframe-buffer-name)
      (should (functionp gatsby>>eldoc-restore-keymap-fn)))))

(ert-deftest gatsby>>eldoc-enable-transiant-map--activates-map ()
  "Transient map is in `overriding-terminal-local-map' after enabling."
  (gatsby>>with-eldoc-transiant-env
    (gatsby>>eldoc-enable-transiant-map)
    (should (memq gatsby>eldoc-transiant-map overriding-terminal-local-map))))

(ert-deftest gatsby>>eldoc-disable-transiant-map--deactivates-map ()
  "Transient map is removed from `overriding-terminal-local-map' after disabling."
  (gatsby>>with-eldoc-transiant-env
    (gatsby>>eldoc-enable-transiant-map)
    (gatsby>>eldoc-disable-transiant-map)
    (should-not (memq gatsby>eldoc-transiant-map overriding-terminal-local-map))))

(ert-deftest gatsby>>eldoc-disable-transiant-map--clears-restore-fn ()
  "Clears `gatsby>>eldoc-restore-keymap-fn' after disabling."
  (gatsby>>with-eldoc-transiant-env
    (gatsby>>eldoc-enable-transiant-map)
    (gatsby>>eldoc-disable-transiant-map)
    (with-current-buffer (get-buffer eldoc-mouse-posframe-buffer-name)
      (should-not gatsby>>eldoc-restore-keymap-fn))))

(provide 'gatsby-lsp-test)
;;; gatsby-lsp-test.el ends here
