;;; multiframe-movement.el --- window movement across frames -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'windmove)

(defun multiframe-movement--frame-entry-window (frame direction)
  "Return the window to focus when entering FRAME from DIRECTION."
  (let ((max-col (1- (frame-width frame)))
        (max-row (- (frame-height frame) 2)))
    (pcase direction
      ('right (window-at 0       0       frame))
      ('left  (window-at max-col 0       frame))
      ('down  (window-at 0       0       frame))
      ('up    (window-at 0       max-row frame)))))

(defun multiframe-movement--next-frame (direction)
  "Return the nearest non-overlapping frame in DIRECTION from the selected frame, or nil."
  (let* ((cur (selected-frame))
         (pos (frame-position cur))
         (cur-x (car pos))
         (cur-y (cdr pos))
         (cur-w (frame-pixel-width cur))
         (cur-h (frame-pixel-height cur)))
    (seq-reduce
     (lambda (best frame)
       (let* ((fpos (frame-position frame))
              (fx (car fpos))
              (fy (cdr fpos))
              (fw (frame-pixel-width frame))
              (fh (frame-pixel-height frame)))
         (pcase direction
           ('right
            (if (and (>= fx (+ cur-x cur-w))
                     (or (null best) (< fx (car (frame-position best)))))
                frame best))
           ('left
            (if (and (<= (+ fx fw) cur-x)
                     (or (null best) (> (+ fx fw)
                                        (+ (car (frame-position best))
                                           (frame-pixel-width best)))))
                frame best))
           ('down
            (if (and (>= fy (+ cur-y cur-h))
                     (or (null best) (< fy (cdr (frame-position best)))))
                frame best))
           ('up
            (if (and (<= (+ fy fh) cur-y)
                     (or (null best) (> (+ fy fh)
                                        (+ (cdr (frame-position best))
                                           (frame-pixel-height best)))))
                frame best)))))
     (seq-filter (lambda (f) (and (not (eq f cur))
                                   (null (frame-parameter f 'parent-frame))))
                  (frame-list))
     nil)))

(defun multiframe-movement--move (direction)
  "Move to the next window in DIRECTION, crossing frames if at the edge."
  (if (windmove-find-other-window direction)
      (pcase direction
        ('right (windmove-right))
        ('left  (windmove-left))
        ('up    (windmove-up))
        ('down  (windmove-down)))
    (unless (and (eq direction 'down) (active-minibuffer-window))
      (let ((next (multiframe-movement--next-frame direction)))
        (when next
          (select-frame-set-input-focus next)
          (select-window (multiframe-movement--frame-entry-window next direction)))))))

(defun multiframe-movement-right ()
  "Move right, crossing to the leftmost window of the next frame if at the rightmost window."
  (interactive)
  (multiframe-movement--move 'right))

(defun multiframe-movement-left ()
  "Move left, crossing to the rightmost window of the next frame if at the leftmost window."
  (interactive)
  (multiframe-movement--move 'left))

(defun multiframe-movement-up ()
  "Move up, crossing to the bottom window of the next frame if at the topmost window."
  (interactive)
  (multiframe-movement--move 'up))

(defun multiframe-movement-down ()
  "Move down, crossing to the top window of the next frame if at the bottommost window."
  (interactive)
  (multiframe-movement--move 'down))

(defun multiframe-movement-open-frame-on-empty-monitor ()
  "Open a maximized frame on a monitor that has no Emacs frame.
Does nothing if there is only one monitor or all monitors have frames."
  (interactive)
  (let* ((monitors (display-monitor-attributes-list))
         (empty (and (> (length monitors) 1)
                     (seq-find (lambda (m)
                                 (not (seq-some #'frame-visible-p (alist-get 'frames m))))
                               monitors))))
    (if empty
        (let* ((wa (alist-get 'workarea empty))
               (x (nth 0 wa))
               (y (nth 1 wa))
               (frame (make-frame `((left . (+ ,x)) (top . (+ ,y)) (visibility . nil)))))
          (make-frame-visible frame)
          (set-frame-parameter frame 'fullscreen 'maximized)
          (select-frame-set-input-focus frame))
      (message "multiframe-movement: no empty monitor found"))))

(provide 'multiframe-movement)
;;; multiframe-movement.el ends here
