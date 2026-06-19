;;; jupyter-kitty-graphics.el --- kitty-graphics image display for emacs-jupyter -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Bridges emacs-jupyter and kitty-graphics so that Jupyter plots
;; (image/png, image/jpeg, image/svg+xml) render inline in terminal
;; Emacs via the Kitty graphics protocol or Sixel.
;;
;; Enable `jupyter-kitty-graphics-mode' to activate.
;; Requires both `kitty-graphics' and `jupyter-mime' to be loaded.

;;; Code:

(require 'kitty-graphics nil t)
(require 'jupyter-mime nil t)

(declare-function kitty-gfx-display-image "kitty-graphics")
(declare-function kitty-gfx-remove-images "kitty-graphics")
(defvar kitty-graphics-mode)
(defvar kitty-gfx--active-backend)
(defvar kitty-gfx--cell-pixel-width)
(defvar kitty-gfx--cell-pixel-height)

;; ── Minor mode ────────────────────────────────────────────────────

(defvar jupyter-kitty-gfx--saved-mime-types nil
  "Original value of `jupyter-nongraphic-mime-types' before modification.")

;;;###autoload
(define-minor-mode jupyter-kitty-graphics-mode
  "Toggle kitty-graphics image display in Jupyter REPL buffers.

When enabled, image/png, image/jpeg, and image/svg+xml MIME types
are added to `jupyter-nongraphic-mime-types' so that Jupyter tries
to render them.  The actual rendering is delegated to
`kitty-gfx-display-image'."
  :global t
  (if jupyter-kitty-graphics-mode
      (progn
        (setq jupyter-kitty-gfx--saved-mime-types jupyter-nongraphic-mime-types)
        (with-no-warnings
          (setq jupyter-nongraphic-mime-types
                '(:application/vnd.jupyter.widget-view+json
                  :text/html :text/markdown
                  :image/svg+xml :image/jpeg :image/png
                  :text/plain))))
    (when jupyter-kitty-gfx--saved-mime-types
      (with-no-warnings
        (setq jupyter-nongraphic-mime-types jupyter-kitty-gfx--saved-mime-types))
      (setq jupyter-kitty-gfx--saved-mime-types nil))))

;; ── Image insertion ──────────────────────────────────────────────

(defun jupyter-kitty-gfx--metadata-dims (metadata)
  "Extract :width and :height from METADATA plist, convert to cell cols/rows.
Returns a cons (MAX-COLS . MAX-ROWS), each nil when not specified."
  (cl-destructuring-bind (&key width height &allow-other-keys) metadata
    (let* ((cw (or kitty-gfx--cell-pixel-width 8))
           (ch (or kitty-gfx--cell-pixel-height 16)))
      (cons (when (and width (> width 0))
              (max 1 (ceiling (/ (float width) cw))))
            (when (and height (> height 0))
              (max 1 (ceiling (/ (float height) ch))))))))

(defun jupyter-kitty-gfx--insert-image (data extension &optional metadata)
  "Write raw bytes DATA to a temp file with EXTENSION, display via kitty-graphics.
DATA is raw image bytes (not base64-encoded).
METADATA is a plist optionally containing :width and :height in pixels,
used to set the display size (e.g. from matplotlib figsize)."
  (let* ((file (make-temp-file "jupyter-kitty-" nil (concat "." extension)))
         (dims (jupyter-kitty-gfx--metadata-dims metadata)))
    ;; Write through a unibyte buffer for binary-exact output.
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert data)
      (let ((coding-system-for-write 'no-conversion))
        (write-region nil nil file nil 'silent)))
    (condition-case err
        (let ((beg (point))
              (end (progn (insert "\n") (point))))
          (let ((ov (kitty-gfx-display-image file beg end (car dims) (cdr dims))))
            (when ov
              (overlay-put ov 'kitty-gfx-delete-file file))))
      (error
       (message "jupyter-kitty-gfx: %s" (error-message-string err))
       (insert "\n")))))

;; `:around' methods intercept before the primary methods (which have
;; `&context' guards on `image-type-available-p' — that returns t even
;; in terminal Emacs when libpng/libjpeg are compiled in, so the
;; existing primary methods match and produce invisible GUI images).

(cl-defmethod jupyter-insert :around ((_mime (eql :image/png)) data
                                      &optional metadata)
  (if (and jupyter-kitty-graphics-mode
           kitty-graphics-mode
           kitty-gfx--active-backend
           (> (length data) 0))
      (progn (jupyter-kitty-gfx--insert-image (base64-decode-string data) "png" metadata) t)
    (cl-call-next-method)))

(cl-defmethod jupyter-insert :around ((_mime (eql :image/jpeg)) data
                                      &optional metadata)
  (if (and jupyter-kitty-graphics-mode
           kitty-graphics-mode
           kitty-gfx--active-backend
           (> (length data) 0))
      (progn (jupyter-kitty-gfx--insert-image (base64-decode-string data) "jpg" metadata) t)
    (cl-call-next-method)))

(cl-defmethod jupyter-insert :around ((_mime (eql :image/svg+xml)) data
                                      &optional metadata)
  (if (and jupyter-kitty-graphics-mode
           kitty-graphics-mode
           kitty-gfx--active-backend
           (> (length data) 0))
      (progn (jupyter-kitty-gfx--insert-image data "svg" metadata) t)
    (cl-call-next-method)))

;; ── Cell clearing ──────────────────────────────────────────────────

(with-eval-after-load 'jupyter-repl
  (defun jupyter-kitty-gfx--clear-cells-advice (&rest _)
    (when kitty-graphics-mode
      (kitty-gfx-remove-images)))
  (advice-add 'jupyter-repl-clear-cells :before
              #'jupyter-kitty-gfx--clear-cells-advice))

(provide 'jupyter-kitty-graphics)
;;; jupyter-kitty-graphics.el ends here
