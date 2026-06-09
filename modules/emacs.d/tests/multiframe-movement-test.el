;;; multiframe-movement-test.el --- tests for multiframe-movement.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'multiframe-movement)

;;; Mock helpers
;;
;; Each test defines a small set of fake "frames" as symbols and uses
;; cl-letf to redirect the frame primitive calls used by each function.

(defmacro mmov-test--with-frames (frame-specs &rest body)
  "Execute BODY with frame primitives mocked according to FRAME-SPECS.
FRAME-SPECS is an alist of (FRAME-SYM . (x y w h visible parent)).
`selected-frame' returns the first frame in the list.
`frame-list' returns all frames."
  (declare (indent 1))
  (let ((specs (cl-gensym "specs")))
    `(let ((,specs ,frame-specs))
       (cl-letf
           (((symbol-function 'selected-frame)
             (lambda () (caar ,specs)))
            ((symbol-function 'frame-list)
             (lambda () (mapcar #'car ,specs)))
            ((symbol-function 'frame-position)
             (lambda (f)
               (let ((s (cdr (assq f ,specs))))
                 (cons (nth 0 s) (nth 1 s)))))
            ((symbol-function 'frame-pixel-width)
             (lambda (f)
               (nth 2 (cdr (assq f ,specs)))))
            ((symbol-function 'frame-pixel-height)
             (lambda (f)
               (nth 3 (cdr (assq f ,specs)))))
            ((symbol-function 'frame-visible-p)
             (lambda (f)
               (nth 4 (cdr (assq f ,specs)))))
            ((symbol-function 'frame-parameter)
             (lambda (f param)
               (when (eq param 'parent-frame)
                 (nth 5 (cdr (assq f ,specs)))))))
         ,@body))))

;;; multiframe-movement--frame-entry-window

(ert-deftest multiframe-movement--frame-entry-window--direction-to-window ()
  "Each direction maps to the correct (col, row) via window-at."
  (let ((frame-w 80) (frame-h 25))
    (cl-letf (((symbol-function 'frame-width)  (lambda (_f) frame-w))
              ((symbol-function 'frame-height) (lambda (_f) frame-h))
              ((symbol-function 'window-at)
               (lambda (col row &optional _frame)
                 (cons col row))))
      (let ((f 'mock-frame))
        ;; right  → top-left corner: (0, 0)
        (should (equal '(0 . 0)  (multiframe-movement--frame-entry-window f 'right)))
        ;; left   → top-right corner: (max-col=79, 0)
        (should (equal '(79 . 0) (multiframe-movement--frame-entry-window f 'left)))
        ;; down   → top-left corner: (0, 0)
        (should (equal '(0 . 0)  (multiframe-movement--frame-entry-window f 'down)))
        ;; up     → bottom-left corner: (0, max-row=23)
        (should (equal '(0 . 23) (multiframe-movement--frame-entry-window f 'up)))))))

;;; multiframe-movement--next-frame

(ert-deftest multiframe-movement--next-frame--finds-frame-to-the-right ()
  ;; cur at (400,0) 400×300; right-frame at (800,0) — touching, non-overlapping
  (mmov-test--with-frames `((cur        . (400 0 400 300 t nil))
                             (right-frame . (800 0 400 300 t nil))
                             (left-frame  . (0   0 400 300 t nil)))
    (should (eq 'right-frame (multiframe-movement--next-frame 'right)))))

(ert-deftest multiframe-movement--next-frame--finds-frame-to-the-left ()
  (mmov-test--with-frames `((cur        . (400 0 400 300 t nil))
                             (right-frame . (800 0 400 300 t nil))
                             (left-frame  . (0   0 400 300 t nil)))
    (should (eq 'left-frame (multiframe-movement--next-frame 'left)))))

(ert-deftest multiframe-movement--next-frame--nil-when-no-frame-in-direction ()
  ;; Only one frame; no neighbors up or down.
  (mmov-test--with-frames `((cur . (400 0 400 300 t nil)))
    (should (null (multiframe-movement--next-frame 'up)))
    (should (null (multiframe-movement--next-frame 'down)))))

(ert-deftest multiframe-movement--next-frame--picks-nearest-of-two-right ()
  ;; Two frames to the right; should pick the closer one.
  (mmov-test--with-frames `((cur        . (0    0 400 300 t nil))
                             (near-right  . (400  0 400 300 t nil))
                             (far-right   . (1200 0 400 300 t nil)))
    (should (eq 'near-right (multiframe-movement--next-frame 'right)))))

(ert-deftest multiframe-movement--next-frame--skips-child-frames ()
  (mmov-test--with-frames `((cur         . (0   0 400 300 t nil))
                             (child-right  . (400 0 400 300 t some-parent))
                             (normal-right . (800 0 400 300 t nil)))
    ;; child-right is at 400 which is nearer, but excluded because it has a parent
    (should (eq 'normal-right (multiframe-movement--next-frame 'right)))))


(provide 'multiframe-movement-test)
;;; multiframe-movement-test.el ends here
