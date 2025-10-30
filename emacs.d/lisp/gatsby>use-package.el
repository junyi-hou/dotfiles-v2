;;; gatsby>use-package.el --- use-package extension -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package general
  :demand t
  :ensure (:host github :repo "noctuid/general.el" :wait t))

;; TODO: replace general.el with in-house and simpler :evil-bind
;; blocker: need to handle prefix map like SPC-o-s

;; (eval-when-compile
;;   (require 'use-package-core)
;;   (require 'cl-lib))

;; (defconst gatsby>>evil-states '(normal motion visual emacs insert operator replace)
;;   "A list of valid states in evil.")


;; (defconst gatsby>>evil-states-maps (mapcar (lambda (state) (intern (format "evil-%s-state-map" state))) gatsby>>evil-states)
;;   "A list of valid states in evil.")

;; (defun gatsby>>maybe-evil-state-to-keymap (map)
;;   "If MAP is an evil state, convert it to the corresponding keymap."
;;   (let ((evil-states gatsby>>evil-states))
;;     (if (memq map evil-states)
;;         (intern (format "evil-%s-state-map" (symbol-name map)))
;;       map)))


;; (defun use-package-normalize/:evil-bind (_name _keyword args)
;;   "Turns evil states into the corresponding maps."
;;   args)

;; (defun gatsby>>combine (&rest lists)
;;   "Return combinations of LISTS.
;; (gatsby>>combine '(1 2) '(a b)) => '((1 a) (1 b) (2 a) (2 b))"
;;   (if (null lists)
;;       (list nil)
;;     (cl-mapcan (lambda (item)
;;                  (mapcar (lambda (rest)
;;                            (cons item rest))
;;                          (apply #'gatsby>>combine (cdr lists))))
;;                (car lists))))


;; (defun gatsby>>process-evil-bind (items current-map current-state current-prefix results)
;;   "Bind commands to keys in the states defined in ITEMS into RESULTS to CURRENT-MAP in CURRENT-STATE if not null."
;;   (pcase items
;;     ;; terminal state: return results
;;     (`() (nreverse results))
;;     ;; if :map presents, change `current-map' to map and reset `current-state'
;;     (`(:map ,map . ,rest) (gatsby>>process-evil-bind rest map nil nil results))
;;     ;; if :state presents, update the states
;;     (`(:state ,state . ,rest) (gatsby>>process-evil-bind rest current-map state nil results))
;;     ;; if :prefix presents, update the prefix
;;     (`(:prefix ,prefix . ,rest) (gatsby>>process-evil-bind rest current-map current-state prefix results))
;;     ;; if not, does the actual key binding
;;     (`((,key . ,cmd) . ,rest)
;;      (let* ((key (kbd key))
;;             (forms '(1))
;;             (results (append forms results)))
;;        (gatsby>>process-evil-bind rest current-map current-state results)))
;;     ;; error
;;     (_ (user-error "Invalid key binding in :evil-bind: %S" (car items)))))

;; (defun use-package-handler/:evil-bind (name _keyword args rest state)
;;   "Handle :evil-bind keyword for use-package."
;;   ;; default map is global map
;;   (let* ((forms (gatsby>>process-evil-bind args 'global-map nil nil)))
;;     (use-package-handler/:config name :config forms rest state)))

;; (add-to-list 'use-package-keywords :evil-bind t)

(provide 'gatsby>use-package)
;;; gatsby>use-package.el ends here
