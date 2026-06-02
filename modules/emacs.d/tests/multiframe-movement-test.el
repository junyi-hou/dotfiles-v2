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

;;; multiframe-movement--frame-distance-sq

(ert-deftest multiframe-movement--frame-distance-sq--zero-at-center ()
  "Distance from the frame's own center is 0."
  (mmov-test--with-frames `((f . (0 0 100 100 t nil)))
    ;; center = (50, 50)
    (should (= 0 (multiframe-movement--frame-distance-sq 'f 50 50)))))

(ert-deftest multiframe-movement--frame-distance-sq--3-4-5-triangle ()
  "3-4-5 right triangle from center gives squared distance 25."
  (mmov-test--with-frames `((f . (0 0 100 100 t nil)))
    ;; center = (50, 50); point 3 left and 4 up = (47, 46)
    (should (= 25 (multiframe-movement--frame-distance-sq 'f 47 46)))))

(ert-deftest multiframe-movement--frame-distance-sq--offset-origin ()
  "Frame not at pixel origin: center is computed from position + half-size."
  (mmov-test--with-frames `((f . (200 100 400 300 t nil)))
    ;; center = (200 + 200, 100 + 150) = (400, 250)
    (should (= 0 (multiframe-movement--frame-distance-sq 'f 400 250)))
    (should (= 100 (multiframe-movement--frame-distance-sq 'f 390 250)))))

;;; multiframe-movement--visible-frames

(ert-deftest multiframe-movement--visible-frames--excludes-invisible ()
  (mmov-test--with-frames `((f1 . (0 0 400 300 t   nil))
                             (f2 . (0 0 400 300 nil nil))
                             (f3 . (0 0 400 300 t   nil)))
    (let ((result (multiframe-movement--visible-frames)))
      (should (= 2 (length result)))
      (should (member 'f1 result))
      (should-not (member 'f2 result))
      (should (member 'f3 result)))))

(ert-deftest multiframe-movement--visible-frames--excludes-child-frames ()
  (mmov-test--with-frames `((f1 . (0 0 400 300 t nil))
                             (f2 . (0 0 400 300 t some-parent)))
    (let ((result (multiframe-movement--visible-frames)))
      (should (equal result '(f1))))))

(ert-deftest multiframe-movement--visible-frames--empty-when-all-invisible ()
  (mmov-test--with-frames `((f1 . (0 0 400 300 nil nil))
                             (f2 . (0 0 400 300 nil nil)))
    (should (null (multiframe-movement--visible-frames)))))

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

;;; multiframe-movement--extreme-frames

(ert-deftest multiframe-movement--extreme-frames--rightmost-single ()
  (mmov-test--with-frames `((f1 . (0   0 400 300 t nil))
                             (f2 . (400 0 400 300 t nil))
                             (f3 . (800 0 400 300 t nil)))
    (should (equal (multiframe-movement--extreme-frames 'right) '(f3)))))

(ert-deftest multiframe-movement--extreme-frames--leftmost-single ()
  (mmov-test--with-frames `((f1 . (0   0 400 300 t nil))
                             (f2 . (400 0 400 300 t nil))
                             (f3 . (800 0 400 300 t nil)))
    (should (equal (multiframe-movement--extreme-frames 'left) '(f1)))))

(ert-deftest multiframe-movement--extreme-frames--multiple-at-same-extreme ()
  ;; f2 and f3 share the rightmost x-coordinate.
  (mmov-test--with-frames `((f1 . (0   0 400 300 t nil))
                             (f2 . (800 0 400 300 t nil))
                             (f3 . (800 0 400 300 t nil)))
    (let ((result (multiframe-movement--extreme-frames 'right)))
      (should (= 2 (length result)))
      (should (member 'f2 result))
      (should (member 'f3 result)))))

;;; multiframe-movement--best-frame-for-side

(ert-deftest multiframe-movement--best-frame-for-side--single-candidate ()
  (mmov-test--with-frames `((f1 . (0   0 400 300 t nil))
                             (f2 . (800 0 400 300 t nil)))
    (should (eq 'f2 (multiframe-movement--best-frame-for-side 'right)))
    (should (eq 'f1 (multiframe-movement--best-frame-for-side 'left)))))

(ert-deftest multiframe-movement--best-frame-for-side--picks-closest-to-point ()
  ;; f1 and f2 are tied at x=0 (leftmost); point is near f1's center.
  ;; f1 center: (200, 150); f2 center: (200, 550); point at (200, 100)
  ;; → f1 is closer.
  (mmov-test--with-frames `((cur . (800 0   400 300 t nil))
                             (f1  . (0   0   400 300 t nil))
                             (f2  . (0   400 400 300 t nil)))
    (cl-letf (((symbol-function 'window-absolute-pixel-position)
               (lambda () '(200 . 100))))
      (should (eq 'f1 (multiframe-movement--best-frame-for-side 'left))))))

(ert-deftest multiframe-movement--best-frame-for-side--prefers-current-frame-when-candidate ()
  ;; f1 (current) and f2 are tied at x=0 (leftmost); point is near f2.
  ;; f1 center (200,150); f2 center (200,550); point at (200,500) → f2 closer.
  ;; But f1 is the current frame, so it wins regardless.
  (mmov-test--with-frames `((f1  . (0   0   400 300 t nil))
                             (f2  . (0   400 400 300 t nil))
                             (far . (800 0   400 300 t nil)))
    (cl-letf (((symbol-function 'window-absolute-pixel-position)
               (lambda () '(200 . 500))))
      (should (eq 'f1 (multiframe-movement--best-frame-for-side 'left))))))

(ert-deftest multiframe-movement--best-frame-for-side--falls-back-to-point-when-current-not-candidate ()
  ;; cur is at x=800 (not leftmost); f1 and f2 tied at x=0.
  ;; Point at (200,100) is closer to f1 center (200,150) than f2 center (200,550).
  (mmov-test--with-frames `((cur . (800 0   400 300 t nil))
                             (f1  . (0   0   400 300 t nil))
                             (f2  . (0   400 400 300 t nil)))
    (cl-letf (((symbol-function 'window-absolute-pixel-position)
               (lambda () '(200 . 100))))
      (should (eq 'f1 (multiframe-movement--best-frame-for-side 'left))))))

(ert-deftest multiframe-movement--best-frame-for-side--nil-when-no-candidates ()
  ;; Single frame — no extreme in any direction except its own side.
  ;; With only one frame, extreme-frames returns it for every direction,
  ;; so best-frame-for-side always returns it.  Test an unsupported side.
  (mmov-test--with-frames `((f1 . (0 0 400 300 t nil)))
    (should (null (multiframe-movement--best-frame-for-side 'center)))))

;;; multiframe-movement-side-window-mode

(ert-deftest multiframe-movement-side-window-mode--enable-adds-advice ()
  (multiframe-movement-side-window-mode -1)
  (unwind-protect
      (progn
        (multiframe-movement-side-window-mode 1)
        (should (advice-member-p #'multiframe-movement--side-window-advice
                                 'display-buffer-in-side-window)))
    (multiframe-movement-side-window-mode -1)))

(ert-deftest multiframe-movement-side-window-mode--disable-removes-advice ()
  (multiframe-movement-side-window-mode 1)
  (multiframe-movement-side-window-mode -1)
  (should-not (advice-member-p #'multiframe-movement--side-window-advice
                               'display-buffer-in-side-window)))

(ert-deftest multiframe-movement-side-window-mode--toggle-is-idempotent ()
  (multiframe-movement-side-window-mode -1)
  (multiframe-movement-side-window-mode -1)
  (should-not (advice-member-p #'multiframe-movement--side-window-advice
                               'display-buffer-in-side-window)))

(provide 'multiframe-movement-test)
;;; multiframe-movement-test.el ends here
