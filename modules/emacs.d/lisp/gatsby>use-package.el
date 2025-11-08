;;; gatsby>use-package.el --- use-package extension -*- lexical-binding: t; -*-

;;; Commentary:
;; Create a new use-package keywords: evil-bind. Example useage:
;; (use-package foo
;;   :evil-bind
;;   (
;;    ;; without any keywords: bind to global keymaps
;;    ("C-c C-k" . foo-command-1)
;;    ;; bindings after this will bind to maps and states
;;    (:maps foo-mode-map :states (normal visual))
;;    ("f" . #'foo-command-2)
;;    ("g" . #'foo-command-3)
;;    ;; if maps are not given, induce it from states (evil-STATE-state-map)
;;    (:states (normal visual))
;;    ("w" . #'foo-command-5)
;;    ;; use state name in maps does the same:
;;    ;; equivalent to above
;;    (:maps (normal visual))
;;    ("w" . #'foo-command-5)
;;    ;; automatically deal with prefix
;;    ("SPC f f" . #'foo-command-4)
;;    ("SPC f c" . #'foo-command-4)
;;   )
;; )


;;; Code:
(eval-when-compile
  (require 'use-package-core)
  (require 'cl-lib))

(defconst gatsby>>evil-states '(normal motion visual emacs insert operator replace)
  "A list of valid states in evil.")

(defconst gatsby>>evil-states-maps (mapcar (lambda (state) (intern (format "evil-%s-state-map" state))) gatsby>>evil-states)
  "A list of valid states in evil.")

(defun gatsby>>maybe-evil-state-to-keymap (map)
  "If MAP is an evil state, convert it to the corresponding keymap."
  (let ((evil-states gatsby>>evil-states))
    (if (memq map evil-states)
        (intern (format "evil-%s-state-map" (symbol-name map)))
      map)))

(defun gatsby>>evil-bind-config-p (item)
  "Return true if `item' is a plist with one or both of the following keys: `:maps' & `:states'"
  (pcase item
  ((or `(:maps ,_)
       `(:states ,_)
       `(:maps ,_ :states ,_)
       `(:states ,_ :maps ,_))
   t)
  (_ nil)))

(defun gatsby>>ensure-list (var)
  "I only need to care about three possibilities: nil, a symbol of a list of symbols"
  (pcase var
    ((pred listp) var)
    (_ `(,var))))

(defun gatsby>>normalize-block (block)
  (let* ((config (car block)))

    (unless (gatsby>>evil-bind-config-p config)
      ;; no config block found,use default
      (setq block `((:maps global-map) ,@(car block))
            config '(:maps global-map)))

    (let ((maps (gatsby>>ensure-list (plist-get config :maps)))
          (states (gatsby>>ensure-list (plist-get config :states))))

      ;; handle short-handed maps
      (if maps
          (setq maps (mapcar #'gatsby>>maybe-evil-state-to-keymap maps))
        (setq maps (mapcar #'gatsby>>maybe-evil-state-to-keymap states)
              states nil))

      (thread-last maps
                   (mapcar
                    (lambda (map)
                      `(:block
                        (quote ,states)
                        ,map
                        ,@(apply #'append
                                 (mapcar
                                  (lambda (cons) `(,(let ((key (car cons))) (if (stringp key) (kbd key) key)) ,(cdr cons)))
                                  (cdr block))))))
                   (cl-reduce #'append)))))

(defun use-package-normalize/:evil-bind (name keyword args)
  (thread-last args
               car
               (use-package-split-when #'gatsby>>evil-bind-config-p)
               (mapcar #'gatsby>>normalize-block)
               (cl-reduce #'append)))

(defun use-package-handler/:evil-bind (name _keyword args rest state)
  (use-package-concat
   (use-package-process-keywords name rest state)
   `((with-eval-after-load 'evil
       ,@(mapcar #'(lambda (block) `(evil-define-key ,@block))
                 (thread-last args
                              (use-package-split-list-at-keys :block)
                              (cl-remove-if-not #'identity)))))))

(defalias 'use-package-autoloads/:evil-bind 'use-package-autoloads-mode)

(setq use-package-keywords (cl-loop for item in use-package-keywords
                                    if (eq item :bind-keymap*)
                                    collect :bind-keymap* and collect :evil-bind
                                    else
                                    ;; don't add duplicates
                                    unless (eq item :evil-bind)
                                    collect item))

(provide 'gatsby>use-package)
;;; gatsby>use-package.el ends here
