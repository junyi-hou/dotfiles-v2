;;; side-by-side-diff.el --- Side-by-side unified diff viewer -*- lexical-binding: t; -*-

;;; Commentary:

;; Display unified diffs side-by-side in two synchronized buffers.
;; The raw diff is piped through `delta' which supplies syntax
;; highlighting, per-line diff colours, intra-line word highlights,
;; and a line-number gutter.  The gutter is parsed to find the +/-
;; marker (driving left/right placement) and then stripped before
;; rendering.  ANSI escapes in delta's output are converted to text
;; properties via `ansi-color-apply'.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'outline)
(require 'ansi-color)

(declare-function magit-current-section "magit-section")
(declare-function magit-section-lineage "magit-section")
(declare-function agent-shell-diff-accept-all "agent-shell-diff")
(declare-function agent-shell-diff-reject-all "agent-shell-diff")
(declare-function agent-shell-diff-open-file "agent-shell-diff")
(declare-function evil-visual-line "evil")

(defvar agent-shell-diff-mode-map)
(defvar ssdf-agent-shell-mode)

;;;; Customization

(defgroup side-by-side-diff nil
  "Side-by-side unified diff viewer."
  :group 'tools
  :prefix "ssdf-")

(defcustom ssdf-default-context 3
  "Default context lines shown around each hunk."
  :type 'natnum
  :group 'side-by-side-diff)

(defcustom ssdf-delta-program "delta"
  "Path to the delta executable.
Delta is a hard requirement: `ssdf-display-diff' errors out if it
cannot be found on `exec-path'."
  :type 'string
  :group 'side-by-side-diff)

(defcustom ssdf-delta-extra-args nil
  "Extra command-line arguments appended to every delta invocation."
  :type '(repeat string)
  :group 'side-by-side-diff)


;;;; Faces

(defface ssdf-padding
  '((t :inherit magit-diff-context))
  "Background for empty padding lines used to keep sides aligned."
  :group 'side-by-side-diff)

(defface ssdf-file-heading
  '((t :inherit magit-diff-file-heading))
  "Background stripe for file heading lines."
  :group 'side-by-side-diff)

(defface ssdf-file-heading-highlight
  '((t :inherit magit-diff-file-heading-highlight))
  "Background stripe for the currently focused file heading line."
  :group 'side-by-side-diff)

(defface ssdf-file-name
  '((t :inherit magit-diff-file-heading :weight bold :height 1.1))
  "Filename text within the file heading."
  :group 'side-by-side-diff)

(defface ssdf-hunk-heading
  '((t :inherit magit-diff-hunk-heading))
  "Background stripe for hunk header lines."
  :group 'side-by-side-diff)

(defface ssdf-hunk-heading-highlight
  '((t :inherit magit-diff-hunk-heading-highlight))
  "Background stripe for the currently focused hunk header line."
  :group 'side-by-side-diff)

(defface ssdf-hunk-marker
  '((t :inherit magit-diff-hunk-heading :weight bold))
  "Face for @@ markers and === decorators."
  :group 'side-by-side-diff)

(defface ssdf-hunk-old-range
  '((t :inherit magit-diff-removed :weight bold))
  "Face for the -old,count range in hunk headers."
  :group 'side-by-side-diff)

(defface ssdf-hunk-new-range
  '((t :inherit magit-diff-added :weight bold))
  "Face for the +new,count range in hunk headers."
  :group 'side-by-side-diff)

(defface ssdf-hunk-func
  '((t :inherit (magit-diff-hunk-heading font-lock-function-name-face) :slant italic))
  "Face for the optional function-context text after @@ in hunk headers."
  :group 'side-by-side-diff)

(defface ssdf-dimmed
  '((t :inherit magit-dimmed))
  "Face applied over non-current hunks to dim them."
  :group 'side-by-side-diff)


;;;; Global state

(defconst ssdf--left-name  "*ssdf-left*")
(defconst ssdf--right-name "*ssdf-right*")

(defvar ssdf--window-config nil
  "Window configuration saved before opening the side-by-side view.")

;;;; Buffer-local state

(defvar-local ssdf--peer nil
  "The peer buffer (the other side of the diff).")

(defvar-local ssdf--context ssdf-default-context
  "Number of context lines currently displayed.")

(defvar-local ssdf--source-fn nil
  "Function (CONTEXT-LINES -> DIFF-STRING) to regenerate the diff.
Nil when the diff cannot be regenerated (e.g. static diff-mode buffer).")

(defvar ssdf--syncing nil
  "Non-nil while syncing peer position, to prevent recursion.")

(defvar-local ssdf--dim-overlays nil
  "Overlays covering non-current hunk lines.")

(defvar-local ssdf--mirror-overlay nil
  "Overlay in the peer buffer mirroring this buffer's selection.")

(defvar-local ssdf--agent-shell-diff-buffer nil
  "Agent-shell diff buffer backing this side-by-side display.")

(defvar-local ssdf--agent-shell-raw-diff nil
  "Raw unified diff generated for an agent-shell diff buffer.")


;;;; Data structure

(cl-defstruct (ssdf--hunk (:constructor ssdf--hunk-create) (:copier nil))
  file header old-start new-start lines)

;;;; Delta integration

(defun ssdf--run-delta (diff-text)
  "Pipe DIFF-TEXT through delta and return its ANSI-coloured output."
  (unless (executable-find ssdf-delta-program)
    (user-error "Delta executable %S not found on PATH" ssdf-delta-program))
  (with-temp-buffer
    (insert diff-text)
    (apply #'call-process-region
           (point-min) (point-max) ssdf-delta-program t t nil
           (append (list "--paging=never"
                         "--true-color=always"
                         "--no-gitconfig"
                         "--file-style=raw"
                         "--hunk-header-style=raw"
                         "--keep-plus-minus-markers"
                         "--line-numbers")
                   ssdf-delta-extra-args))
    (buffer-string)))

(defun ssdf--parse-delta-line (line)
  "Parse one content LINE from delta output.
Return (TYPE . PROPERTIZED-CONTENT) or nil if LINE is not a content line.
The leading line-number gutter is stripped; ANSI escapes in the content
become text properties via `ansi-color-apply'."
  (let ((colored (ansi-color-apply line)))
    (when (string-match "│\\([-+ ]\\)\\(.*\\)$" colored)
      (let ((marker  (aref (match-string 1 colored) 0))
            (content (match-string 2 colored)))
        (pcase marker
          (?-  (cons 'removed content))
          (?+  (cons 'added   content))
          (?\s (cons 'context content)))))))

(defun ssdf--parse-delta (raw-diff)
  "Run delta on RAW-DIFF and return a list of `ssdf--hunk' structs."
  (let* ((delta-out (ssdf--run-delta raw-diff))
         (lines     (split-string delta-out "\n"))
         result file header old-start new-start pending)
    (cl-flet ((flush ()
                (when header
                  (push (ssdf--hunk-create
                         :file file :header header
                         :old-start old-start :new-start new-start
                         :lines (nreverse pending))
                        result)
                  (setq header nil pending nil))))
      (dolist (line lines)
        (let ((plain (ansi-color-filter-apply line)))
          (cond
           ((string-empty-p plain))
           ((string-match "^diff --git a/.+ b/\\(.+\\)" plain)
            (flush)
            (setq file (match-string 1 plain)))
           ((string-match "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)" plain)
            (flush)
            (setq header    (string-trim-right plain "[ \t│]+")
                  old-start (string-to-number (match-string 1 plain))
                  new-start (string-to-number (match-string 2 plain))))
           ;; Skip delta decoration: "\ No newline" markers and box-drawing rules.
           ((string-prefix-p "\\" plain))
           ((<= #x2500 (aref plain 0) #x257F))
           ;; Content line: delta's --line-numbers gutter always contains │.
           ((string-match "│" plain)
            (when header
              (when-let* ((entry (ssdf--parse-delta-line line)))
                (push entry pending))))
           ;; Anything else is a file name emitted by delta's --file-style=raw
           ;; (just the bare path, no "diff --git" prefix).  Flush any open hunk.
           (t
            (flush)
            (setq file (string-trim plain))))))
      (flush))
    (nreverse result)))

;;;; Alignment

(defun ssdf--align (lines)
  "Align LINES into (LEFT . RIGHT) for side-by-side display.
Each element is (TYPE . CONTENT) where TYPE is context/removed/added/padding.
Consecutive removed/added runs are paired index-by-index; the shorter
side is padded so context lines stay vertically aligned."
  (let (left right rm-acc add-acc)
    (cl-flet ((flush ()
                (let* ((r (nreverse rm-acc))
                       (a (nreverse add-acc))
                       (n (max (length r) (length a))))
                  (dotimes (i n)
                    (let ((rl (nth i r)) (al (nth i a)))
                      (push (cons (if rl 'removed 'padding) (or rl "")) left)
                      (push (cons (if al 'added   'padding) (or al "")) right)))
                  (setq rm-acc nil add-acc nil))))
      (dolist (cell lines)
        (pcase (car cell)
          ('context (flush)
                    (push cell left)
                    (push cell right))
          ('removed (push (cdr cell) rm-acc))
          ('added   (push (cdr cell) add-acc))))
      (flush))
    (cons (nreverse left) (nreverse right))))

;;;; Rendering

(defun ssdf--propertize-file-heading (filename)
  "Return a propertized file-heading string for FILENAME."
  (propertize
   (concat "\n"
           (propertize "===" 'face 'ssdf-hunk-marker)
           " "
           (propertize filename 'face 'ssdf-file-name)
           " "
           (propertize "===\n" 'face 'ssdf-hunk-marker))
   'face 'ssdf-file-heading))

(defun ssdf--propertize-hunk-header (header)
  "Return a propertized hunk-header string for HEADER."
  (propertize
   (if (string-match
        "^\\(@@ \\)\\(-[0-9,]+\\) \\(\\+[0-9,]+\\)\\( @@\\)\\(.*\\)$"
        header)
       (concat
        (propertize (match-string 1 header) 'face 'ssdf-hunk-marker)
        (propertize (match-string 2 header) 'face 'ssdf-hunk-old-range)
        " "
        (propertize (match-string 3 header) 'face 'ssdf-hunk-new-range)
        (propertize (match-string 4 header) 'face 'ssdf-hunk-marker)
        (propertize (match-string 5 header) 'face 'ssdf-hunk-func)
        "\n")
     (concat header "\n"))
   'face 'ssdf-hunk-heading))

(defun ssdf--insert-line (buf type text)
  "Insert TEXT plus a newline into BUF; apply padding face when TYPE is padding.
The TYPE (context/removed/added/padding) is recorded as the `'ssdf-type'
text property on the inserted line so that patch construction can
recover it later."
  (with-current-buffer buf
    (let ((inhibit-read-only t)
          (start (point)))
      (insert text "\n")
      (put-text-property start (point) 'ssdf-type type)
      (when (eq type 'padding)
        (add-face-text-property start (point) 'ssdf-padding nil)))))

(defun ssdf--render (hunks left-buf right-buf)
  "Fill LEFT-BUF and RIGHT-BUF with the side-by-side rendering of HUNKS."
  (let (cur-file)
    (dolist (hunk hunks)
      (unless (equal (ssdf--hunk-file hunk) cur-file)
        (setq cur-file (ssdf--hunk-file hunk))
        (let ((heading (ssdf--propertize-file-heading cur-file)))
          (dolist (buf (list left-buf right-buf))
            (with-current-buffer buf
              (let ((inhibit-read-only t)) (insert heading))))))
      (let* ((aligned     (ssdf--align (ssdf--hunk-lines hunk)))
             (hunk-header (ssdf--propertize-hunk-header (ssdf--hunk-header hunk)))
             (old-start   (ssdf--hunk-old-start hunk))
             (new-start   (ssdf--hunk-new-start hunk))
             (h-file      cur-file))
        (dolist (buf (list left-buf right-buf))
          (with-current-buffer buf
            (let ((inhibit-read-only t)
                  (start (point)))
              (insert hunk-header)
              (put-text-property start (point) 'ssdf-old-start old-start)
              (put-text-property start (point) 'ssdf-new-start new-start)
              (put-text-property start (point) 'ssdf-file      h-file))))
        (cl-loop for (ltype . ltext) in (car aligned)
                 for (rtype . rtext) in (cdr aligned)
                 do (ssdf--insert-line left-buf  ltype ltext)
                    (ssdf--insert-line right-buf rtype rtext))))))

;;;; Dimming

(defun ssdf--hunk-bounds ()
  "Return (START . END) of the hunk block containing point, or nil."
  (save-excursion
    (beginning-of-line)
    (let ((hstart (if (looking-at "^@@ ")
                      (point)
                    (and (re-search-backward "^@@ " nil t) (point)))))
      (when hstart
        (goto-char hstart)
        (forward-line 1)
        (let ((hend (if (re-search-forward "^\\(@@ \\|=== \\)" nil t)
                        (match-beginning 0)
                      (point-max))))
          (cons hstart hend))))))

(defun ssdf--apply-dimming (buf)
  "Refresh dim overlays in BUF based on its current point."
  (with-current-buffer buf
    (mapc #'delete-overlay ssdf--dim-overlays)
    (setq ssdf--dim-overlays nil)
    (when-let* ((bounds (ssdf--hunk-bounds))
                (hstart (car bounds))
                (hend   (cdr bounds)))
      (when (> hstart (point-min))
        (let ((ov (make-overlay (point-min) hstart)))
          (overlay-put ov 'face 'ssdf-dimmed)
          (push ov ssdf--dim-overlays)))
      (when (< hend (point-max))
        (let ((ov (make-overlay hend (point-max))))
          (overlay-put ov 'face 'ssdf-dimmed)
          (push ov ssdf--dim-overlays)))
      ;; Heading overlays sit at priority 1 so they show through the
      ;; priority-0 dimming overlay covering the rest of the buffer.
      (let ((cur-file-pos (save-excursion
                            (goto-char hstart)
                            (when (re-search-backward "^=== " nil t)
                              (line-beginning-position)))))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\(@@ \\|=== \\)" nil t)
            (let* ((lstart  (line-beginning-position))
                   (lend    (line-beginning-position 2))
                   (is-hunk (string= (match-string 1) "@@ "))
                   (face    (cond
                             ((and is-hunk (= lstart hstart))
                              'ssdf-hunk-heading-highlight)
                             ((and (not is-hunk) cur-file-pos (= lstart cur-file-pos))
                              'ssdf-file-heading-highlight)
                             (is-hunk
                              'ssdf-hunk-heading)
                             (t
                              'ssdf-file-heading)))
                   (ov      (make-overlay lstart lend)))
              (overlay-put ov 'face face)
              (overlay-put ov 'priority 1)
              (push ov ssdf--dim-overlays))))))))

(defun ssdf--update-dimming ()
  "Refresh dim overlays in both ssdf buffers."
  (ssdf--apply-dimming (current-buffer))
  (when (buffer-live-p ssdf--peer)
    (ssdf--apply-dimming ssdf--peer)))


;;;; Scroll synchronization

(defun ssdf--sync ()
  "Sync point and window-start of peer buffer to match the current buffer."
  (when (and (not ssdf--syncing)
             ssdf--peer
             (buffer-live-p ssdf--peer))
    (let ((ssdf--syncing t)
          (point-line (line-number-at-pos))
          (start-line (line-number-at-pos (window-start))))
      (dolist (win (get-buffer-window-list ssdf--peer nil t))
        (with-selected-window win
          (goto-char (point-min))
          (forward-line (1- point-line))
          (let ((start (save-excursion
                         (goto-char (point-min))
                         (forward-line (1- start-line))
                         (point))))
            (set-window-start win start t)))))))

;;;; Visual-line selection

(defvar ssdf-visual-line-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap evil-visual-char] #'evil-visual-line)
    map)
  "Remap evil's char-wise visual selection to line-wise selection.")

;;;###autoload
(define-minor-mode ssdf-visual-line-mode
  "Use line-wise visual selection in `ssdf-mode' buffers.
Remaps `evil-visual-char' to `evil-visual-line' so that starting a
visual selection covers whole diff lines, which mirrors cleanly to
the peer buffer."
  :keymap ssdf-visual-line-mode-map)

;;;; Selection mirroring

(defun ssdf--mirror-selection ()
  "Mirror this buffer's active region to the peer buffer via an overlay.
When a region is active (e.g. under evil visual selection), a `region'
overlay covering the same line range is placed in the peer.  The
overlay is moved on every cursor line change so the peer tracks the
selection live.  When no region is active, the peer overlay is removed."
  (when (and ssdf--peer
             (buffer-live-p ssdf--peer)
             (not ssdf--syncing))
    (let ((mp (mark t)))
      (if (and mark-active mp)
          (let* ((ml (line-number-at-pos mp))
                 (pl (line-number-at-pos (point)))
                 (lo (min ml pl))
                 (hi (max ml pl)))
            (with-current-buffer ssdf--peer
              (let ((start (save-excursion
                             (goto-char (point-min))
                             (forward-line (1- lo))
                             (point)))
                    (end (save-excursion
                           (goto-char (point-min))
                           (forward-line hi)
                           (point))))
                (if (and ssdf--mirror-overlay
                         (overlay-buffer ssdf--mirror-overlay))
                    (move-overlay ssdf--mirror-overlay start end)
                  (setq ssdf--mirror-overlay (make-overlay start end))
                  (overlay-put ssdf--mirror-overlay 'face 'region)
                  (overlay-put ssdf--mirror-overlay 'priority 2)))))
        (with-current-buffer ssdf--peer
          (when (and ssdf--mirror-overlay
                     (overlay-buffer ssdf--mirror-overlay))
            (delete-overlay ssdf--mirror-overlay)
            (setq ssdf--mirror-overlay nil)))))))

;;;; Staging

(defun ssdf--linetype-content (buf row)
  "Return (TYPE . CONTENT) for line ROW (1-indexed) in BUF."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- row))
      (let ((pos (point)))
        (cons (or (get-text-property pos 'ssdf-type) 'padding)
              (buffer-substring-no-properties pos (line-end-position)))))))

(defun ssdf--row-patch-lines (left-buf right-buf row)
  "Return list of patch lines for the row at buffer ROW (1-indexed).
Walks LEFT-BUF and RIGHT-BUF in parallel since they are aligned."
  (let* ((l (ssdf--linetype-content left-buf  row))
         (r (ssdf--linetype-content right-buf row))
         (ltype    (car l))
         (lcontent (cdr l))
         (rtype    (car r))
         (rcontent (cdr r)))
    (cond
     ((and (eq ltype 'context) (eq rtype 'context))
      (list (concat " "  lcontent)))
     ((and (eq ltype 'removed)  (eq rtype 'added))
      (list (concat "-" lcontent) (concat "+" rcontent)))
     ((and (eq ltype 'removed)  (eq rtype 'padding))
      (list (concat "-" lcontent)))
     ((and (eq ltype 'padding)  (eq rtype 'added))
      (list (concat "+" rcontent)))
     (t nil))))

(defun ssdf--row-line-offset (left-buf right-buf row)
  "Return (OLD-DELTA . NEW-DELTA) for ROW.
Indicates how many old/new file lines this row contributes to offsets."
  (let ((ltype (car (ssdf--linetype-content left-buf  row)))
        (rtype (car (ssdf--linetype-content right-buf row))))
    (cons (if (memq ltype '(context removed)) 1 0)
          (if (memq rtype '(context added))   1 0))))

(defun ssdf--hunk-row-range (header-pos)
  "Return (FIRST-CONTENT-ROW . LAST-CONTENT-ROW) for the hunk whose
header (the `@@ ...' line) starts at HEADER-POS."
  (save-excursion
    (goto-char header-pos)
    (forward-line 1)
    (let ((first (line-number-at-pos)))
      (if (re-search-forward "^\\(@@ \\|=== \\)" nil t)
          (cons first (1- (line-number-at-pos (match-beginning 0))))
        (save-excursion
          (goto-char (point-max))
          (when (bolp) (forward-line -1))
          (cons first (line-number-at-pos)))))))

(defun ssdf--current-hunk-header-pos ()
  "Return the position of the `@@ ' hunk header containing point, or nil."
  (save-excursion
    (beginning-of-line)
    (or (and (looking-at "^@@ ") (point))
        (and (re-search-backward "^@@ " nil t) (point)))))

(defun ssdf--refresh-diff ()
  "Re-render the diff using the saved source function.
If the refreshed diff is empty (all changes staged), close the view."
  (let* ((left-buf (get-buffer ssdf--left-name))
         (source-fn (and (buffer-live-p left-buf)
                         (buffer-local-value 'ssdf--source-fn left-buf)))
         (ctx (and (buffer-live-p left-buf)
                   (buffer-local-value 'ssdf--context left-buf))))
    (unless source-fn
      (user-error "Cannot refresh: diff source unavailable"))
    (let ((diff-text (funcall source-fn (or ctx ssdf-default-context))))
      (if (or (null diff-text)
              (string-empty-p (string-trim diff-text)))
          (ssdf-quit)
        (ssdf-display-diff diff-text
                           :context (or ctx ssdf-default-context)
                           :source-fn source-fn)))))

(defun ssdf--hunk-index-in-file (header-pos)
  "Return the 1-based index of HEADER-POS among the `@@' headers of its file."
  (save-excursion
    (goto-char header-pos)
    (let* ((file-start (save-excursion
                         (if (re-search-backward "^=== " nil t)
                             (line-end-position)
                           (point-min))))
           (bound (save-excursion
                    (goto-char header-pos)
                    (line-end-position)))
           (count 0))
      (save-excursion
        (goto-char file-start)
        (while (re-search-forward "^@@ " bound t)
          (cl-incf count)))
      count)))

(defun ssdf--goto-file-hunk (file-name hunk-index)
  "Move point to the HUNK-INDEX (1-based) `@@' header within FILE-NAME."
  (ssdf--goto-file-heading file-name)
  (let ((remaining hunk-index)
        (found nil))
    (while (and (> remaining 0)
                (re-search-forward "^@@ " nil t))
      (setq found (match-beginning 0))
      (cl-decf remaining))
    (when found (goto-char found))))

(defun ssdf-stage ()
  "Stage the hunk at point, or the active region, then refresh the diff.
Uses `git apply --cached --recount' so partial hunks stage correctly.
After staging, repositions to the same hunk index within the file;
if the staged hunk was the last one, the next remaining hunk is
landed on instead."
  (interactive)
  (unless ssdf--peer
    (user-error "Not in a ssdf buffer"))
  (let* ((left-buf    (get-buffer ssdf--left-name))
         (right-buf   (get-buffer ssdf--right-name))
         (header-pos  (ssdf--current-hunk-header-pos))
         (file-name   (and header-pos (ssdf--current-file-name))))
    (unless (and header-pos file-name)
      (user-error "No hunk at point"))
    (let ((saved-hunk-index (ssdf--hunk-index-in-file header-pos)))
      (let* ((base-old   (or (get-text-property header-pos 'ssdf-old-start) 1))
             (base-new   (or (get-text-property header-pos 'ssdf-new-start) 1))
             (hunk-range (ssdf--hunk-row-range header-pos))
             (region-rows
              (if mark-active
                  (let ((beg (line-number-at-pos (region-beginning)))
                        (end (line-number-at-pos (region-end))))
                    (cons (max beg (car hunk-range))
                          (min end (cdr hunk-range))))
                hunk-range))
             (start-row  (car region-rows))
             (hunk-start (car hunk-range))
             (offsets    (cl-loop for row from hunk-start to (1- start-row)
                                  for off = (ssdf--row-line-offset left-buf right-buf row)
                                  sum (car off) into od
                                  sum (cdr off) into nd
                                  finally return (cons od nd)))
             (patch-old  (+ base-old (car offsets)))
             (patch-new  (+ base-new (cdr offsets)))
             (patch-lines (cl-loop for row from start-row to (cdr region-rows)
                                   append (ssdf--row-patch-lines left-buf right-buf row))))
        (unless patch-lines
          (user-error "Nothing to stage in the selected range"))
        (let ((patch (concat "diff --git a/" file-name " b/" file-name "\n"
                             "--- a/" file-name "\n"
                             "+++ b/" file-name "\n"
                             "@@ -" (number-to-string patch-old) " +"
                             (number-to-string patch-new) " @@\n"
                             (mapconcat #'identity patch-lines "\n")
                             "\n")))
          (let ((status
                 (with-temp-buffer
                   (insert patch)
                   (call-process-region (point-min) (point-max)
                                        "git" nil t nil
                                        "apply" "--cached" "--recount" "-"))))
            (unless (eq status 0)
              (user-error "git apply --cached failed")))))
      (ssdf--refresh-diff)
      (when (get-buffer ssdf--left-name)
        (with-current-buffer (get-buffer ssdf--left-name)
          (ssdf--goto-file-hunk file-name saved-hunk-index))))))

;;;; Mode

(defvar ssdf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n")   #'ssdf-next-hunk)
    (define-key map (kbd "p")   #'ssdf-prev-hunk)
    (define-key map (kbd "N")   #'ssdf-next-file)
    (define-key map (kbd "P")   #'ssdf-prev-file)
    (define-key map (kbd "]")   #'ssdf-next-hunk)
    (define-key map (kbd "[")   #'ssdf-prev-hunk)
    (define-key map (kbd "}")   #'ssdf-next-file)
    (define-key map (kbd "{")   #'ssdf-prev-file)
    (define-key map (kbd "+")   #'ssdf-increase-context)
    (define-key map (kbd "-")   #'ssdf-decrease-context)
    (define-key map (kbd "s")   #'ssdf-stage)
    (define-key map (kbd "q")   #'ssdf-quit)
    map)
  "Keymap for `ssdf-mode'.")

(define-derived-mode ssdf-mode special-mode "SSDF"
  "Major mode for side-by-side diff viewing."
  :group 'side-by-side-diff
  (setq-local truncate-lines t
              buffer-read-only t
              outline-regexp "^=== "
              outline-level (lambda () 1))
  (visual-line-mode -1)
  (outline-minor-mode 1)
  (ssdf-visual-line-mode 1)
  (add-hook 'post-command-hook        #'ssdf--sync           nil t)
  (add-hook 'post-command-hook        #'ssdf--update-dimming t   t)
  (add-hook 'post-command-hook        #'ssdf--mirror-selection t   t)
  (when (featurep 'consult)
    (add-hook 'consult-after-jump-hook #'ssdf--sync nil t)))


;;;; Navigation

(defun ssdf-next-hunk ()
  "Move to the next hunk header."
  (interactive)
  (when-let* ((pos (save-excursion (end-of-line)
                                   (re-search-forward "^@@ " nil t))))
    (goto-char pos)
    (beginning-of-line)))

(defun ssdf-prev-hunk ()
  "Move to the previous hunk header."
  (interactive)
  (beginning-of-line)
  (when-let* ((pos (re-search-backward "^@@ " nil t)))
    (goto-char pos)))

(defun ssdf-next-file ()
  "Move to the next file heading."
  (interactive)
  (when-let* ((pos (save-excursion (end-of-line)
                                   (re-search-forward "^=== " nil t))))
    (goto-char pos)
    (beginning-of-line)))

(defun ssdf-prev-file ()
  "Move to the previous file heading."
  (interactive)
  (beginning-of-line)
  (when-let* ((pos (re-search-backward "^=== " nil t)))
    (goto-char pos)))

;;;; Context adjustment

(defun ssdf-increase-context (arg)
  "Increase context lines by ARG steps of 3 (default 1 step)."
  (interactive "p")
  (ssdf--adjust-context (* (or arg 1) 3)))

(defun ssdf-decrease-context (arg)
  "Decrease context lines by ARG steps of 3 (default 1 step)."
  (interactive "p")
  (ssdf--adjust-context (- (* (or arg 1) 3))))

(defun ssdf--current-file-name ()
  "Return the file name from the nearest `=== file ===' heading at or above point."
  (save-excursion
    (beginning-of-line)
    (or (re-search-backward "^=== \\(.+\\) ===" nil t)
        (re-search-forward  "^=== \\(.+\\) ===" nil t))
    (match-string 1)))

(defun ssdf--goto-file-heading (file-name)
  "Move point to the start of the `=== FILE-NAME ===' heading and recenter."
  (goto-char (point-min))
  (when (re-search-forward
         (concat "^=== " (regexp-quote file-name) " ===") nil t)
    (goto-char (line-beginning-position))
    (when (get-buffer-window (current-buffer)) (recenter 0))))

(defun ssdf--adjust-context (delta)
  "Change context lines by DELTA and refresh."
  (let* ((file-name (ssdf--current-file-name))
         (source-fn (or ssdf--source-fn
                        (and (buffer-live-p ssdf--peer)
                             (buffer-local-value 'ssdf--source-fn ssdf--peer))))
         (new-ctx (max 0 (+ (or ssdf--context ssdf-default-context) delta))))
    (unless source-fn
      (user-error "Cannot adjust context: diff source unavailable"))
    (ssdf-display-diff (funcall source-fn new-ctx)
                       :context new-ctx
                       :source-fn source-fn)
    (when file-name
      (ssdf--goto-file-heading file-name))))

;;;; Quit

(defun ssdf-quit ()
  "Close the side-by-side diff and restore the previous window layout."
  (interactive)
  (dolist (w (seq-filter (lambda (w) (window-parameter w 'window-side))
                         (window-list nil 'no-minibuf)))
    (condition-case nil (delete-window w) (error nil)))
  (when ssdf--window-config
    (set-window-configuration ssdf--window-config)
    (setq ssdf--window-config nil))
  (dolist (name (list ssdf--left-name ssdf--right-name))
    (when-let* ((buf (get-buffer name)))
      (kill-buffer buf))))

;;;; Main entry point

;;;###autoload
(cl-defun ssdf-display-diff (diff-text &key context source-fn)
  "Display DIFF-TEXT as a side-by-side diff in two windows.

DIFF-TEXT is raw unified diff output; it is piped through `delta'
to obtain syntax highlighting and per-line diff colours.

CONTEXT is the context-line count encoded in DIFF-TEXT (informational).
SOURCE-FN is a function (CONTEXT-LINES -> DIFF-STRING) enabling live
context adjustment via `ssdf-increase-context' / `ssdf-decrease-context'."
  (let ((hunks (ssdf--parse-delta diff-text)))
    (unless hunks
      (user-error "No diff hunks found"))
    ;; Preserve the original window layout unless we are refreshing
    ;; from within an existing ssdf session (context adjustment).
    (unless (memq (current-buffer)
                  (delq nil (list (get-buffer ssdf--left-name)
                                  (get-buffer ssdf--right-name))))
      (setq ssdf--window-config (current-window-configuration)))
    (let ((left-buf  (get-buffer-create ssdf--left-name))
          (right-buf (get-buffer-create ssdf--right-name))
          (ctx (or context ssdf-default-context)))
      (dolist (buf (list left-buf right-buf))
        (with-current-buffer buf
          (let ((inhibit-read-only t)) (erase-buffer))
          (setq ssdf--mirror-overlay nil
                ssdf--dim-overlays nil)
          (ssdf-mode)))
      (ssdf--render hunks left-buf right-buf)
      (with-current-buffer left-buf
        (setq ssdf--peer right-buf
              ssdf--context ctx
              ssdf--source-fn source-fn)
        (goto-char (point-min)))
      (with-current-buffer right-buf
        (setq ssdf--peer left-buf
              ssdf--context ctx
              ssdf--source-fn source-fn)
        (goto-char (point-min)))
      ;; Lay out windows: collapse non-side windows to one, split [left | right]
      (let* ((non-side (seq-filter
                        (lambda (w) (not (window-parameter w 'window-side)))
                        (window-list nil 'no-minibuf)))
             (keep (or (and (not (window-parameter (selected-window) 'window-side))
                            (selected-window))
                       (car non-side))))
        (dolist (w non-side)
          (unless (eq w keep)
            (condition-case nil (delete-window w) (error nil))))
        (when keep (select-window keep))
        (switch-to-buffer left-buf)
        (set-window-buffer (split-window-right) right-buf)))))

;;;; Source-specific entry points

;;;###autoload
(defun ssdf-from-diff-buffer ()
  "Open side-by-side view for the current `diff-mode' buffer."
  (interactive)
  (unless (derived-mode-p 'diff-mode)
    (user-error "Not in a diff-mode buffer"))
  (ssdf-display-diff (buffer-substring-no-properties (point-min) (point-max))))

;;;; Agent-shell integration

(defun ssdf--agent-shell-diff-label (file prefix)
  "Return a diff label for FILE with PREFIX."
  (concat prefix (or file "file")))

(defun ssdf--agent-shell-diff-text (old new file)
  "Return a git-shaped unified diff between OLD and NEW for FILE."
  (let* ((suffix (when-let* ((ext (and file (file-name-extension file))))
                   (concat "." ext)))
         (old-file (make-temp-file "ssdf-old" nil suffix))
         (new-file (make-temp-file "ssdf-new" nil suffix))
         (display-file (or file "file")))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert old))
          (with-temp-file new-file (insert new))
          (with-temp-buffer
            (let ((status (process-file
                           "diff" nil t nil
                           "-u"
                           "-L" (ssdf--agent-shell-diff-label display-file "a/")
                           "-L" (ssdf--agent-shell-diff-label display-file "b/")
                           old-file new-file)))
              (unless (memq status '(0 1))
                (user-error "diff failed with status %s" status)))
            (concat "diff --git "
                    (ssdf--agent-shell-diff-label display-file "a/")
                    " "
                    (ssdf--agent-shell-diff-label display-file "b/")
                    "\n"
                    (buffer-string))))
      (ignore-errors (delete-file old-file))
      (ignore-errors (delete-file new-file)))))

(defun ssdf--agent-shell-insert-diff (old new file buf)
  "Insert an agent-shell diff into BUF and cache its SSDF input.
This replaces `agent-shell-diff--insert-diff' while
`ssdf-agent-shell-mode' is enabled."
  (let ((diff-text (ssdf--agent-shell-diff-text old new file)))
    (with-current-buffer buf
      (setq-local ssdf--agent-shell-raw-diff diff-text)
      ;; `agent-shell-diff' removes the first and last lines after insertion,
      ;; matching the command/status lines inserted by `diff-no-select'.
      (insert (format "Diff command: diff -u %s\n" (or file "file")))
      (insert (replace-regexp-in-string "\\`diff --git [^\n]*\n" "" diff-text))
      (insert "Diff finished.\n"))))

(defun ssdf--agent-shell-run (command)
  "Run agent-shell diff COMMAND in the backing diff buffer."
  (let ((buf ssdf--agent-shell-diff-buffer))
    (unless (buffer-live-p buf)
      (user-error "Agent-shell diff buffer is no longer live"))
    (with-current-buffer buf
      (call-interactively command))
    (unless (buffer-live-p buf)
      (ssdf-quit))))

(defun ssdf-agent-shell-accept-all ()
  "Accept the backing agent-shell diff and close the side-by-side view."
  (interactive)
  (ssdf--agent-shell-run #'agent-shell-diff-accept-all))

(defun ssdf-agent-shell-reject-all ()
  "Reject the backing agent-shell diff and close the side-by-side view."
  (interactive)
  (ssdf--agent-shell-run #'agent-shell-diff-reject-all))

(defun ssdf-agent-shell-open-file ()
  "Open the file associated with the backing agent-shell diff."
  (interactive)
  (ssdf--agent-shell-run #'agent-shell-diff-open-file))

(defun ssdf--agent-shell-mode-map ()
  "Return a local SSDF keymap derived from `agent-shell-diff-mode-map'."
  (let ((map (copy-keymap agent-shell-diff-mode-map)))
    (substitute-key-definition
     #'agent-shell-diff-accept-all #'ssdf-agent-shell-accept-all map)
    (substitute-key-definition
     #'agent-shell-diff-reject-all #'ssdf-agent-shell-reject-all map)
    (substitute-key-definition
     #'agent-shell-diff-open-file #'ssdf-agent-shell-open-file map)
    (substitute-key-definition 'diff-hunk-next #'ssdf-next-hunk map)
    (substitute-key-definition 'diff-hunk-prev #'ssdf-prev-hunk map)
    (substitute-key-definition #'kill-current-buffer #'ssdf-quit map)
    (substitute-key-definition #'kill-buffer-and-window #'ssdf-quit map)
    (set-keymap-parent map ssdf-mode-map)
    map))

(defun ssdf--agent-shell-setup-buffer (buf diff-buffer)
  "Configure SSDF BUF to proxy commands to DIFF-BUFFER."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq-local ssdf--agent-shell-diff-buffer diff-buffer
                  header-line-format
                  (and (buffer-live-p diff-buffer)
                       (buffer-local-value 'header-line-format diff-buffer)))
      (use-local-map (ssdf--agent-shell-mode-map)))))

(defun ssdf--agent-shell-display (diff-buffer)
  "Display DIFF-BUFFER with SSDF when it has cached agent-shell diff text."
  (when (buffer-live-p diff-buffer)
    (when-let* ((diff-text (buffer-local-value 'ssdf--agent-shell-raw-diff
                                               diff-buffer)))
      (ssdf-display-diff diff-text)
      (ssdf--agent-shell-setup-buffer (get-buffer ssdf--left-name) diff-buffer)
      (ssdf--agent-shell-setup-buffer (get-buffer ssdf--right-name) diff-buffer))))

(defun ssdf--agent-shell-diff-around (orig &rest args)
  "Display `agent-shell-diff' results with SSDF after ORIG handles setup."
  (let ((window-config (current-window-configuration))
        (diff-buffer (apply orig args)))
    (when (and ssdf-agent-shell-mode
               (buffer-live-p diff-buffer)
               (buffer-local-value 'ssdf--agent-shell-raw-diff diff-buffer))
      (condition-case err
          (progn
            (set-window-configuration window-config)
            (ssdf--agent-shell-display diff-buffer))
        (error
         (message "ssdf-agent-shell: %s" (error-message-string err))
         (pop-to-buffer diff-buffer))))
    diff-buffer))

(defun ssdf--agent-shell-enable ()
  "Enable SSDF advice for agent-shell diffs."
  (unless (advice-member-p
           #'ssdf--agent-shell-insert-diff 'agent-shell-diff--insert-diff)
    (advice-add
     'agent-shell-diff--insert-diff
     :override #'ssdf--agent-shell-insert-diff))
  (unless (advice-member-p #'ssdf--agent-shell-diff-around 'agent-shell-diff)
    (advice-add
     'agent-shell-diff
     :around #'ssdf--agent-shell-diff-around)))

(defun ssdf--agent-shell-disable ()
  "Disable SSDF advice for agent-shell diffs."
  (advice-remove 'agent-shell-diff--insert-diff #'ssdf--agent-shell-insert-diff)
  (advice-remove 'agent-shell-diff #'ssdf--agent-shell-diff-around))

;;;###autoload
(define-minor-mode ssdf-agent-shell-mode
  "Display `agent-shell-diff' buffers as side-by-side SSDF views."
  :global t
  :group 'side-by-side-diff
  (if ssdf-agent-shell-mode
      (with-eval-after-load 'agent-shell-diff
        (when ssdf-agent-shell-mode
          (ssdf--agent-shell-enable)))
    (when (featurep 'agent-shell-diff)
      (ssdf--agent-shell-disable))))

(defun ssdf--magit-staged-p ()
  "Return non-nil if the current magit context refers to staged changes.
In `magit-status-mode' this checks the section at point; in
`magit-diff-mode' it checks `magit-buffer-typearg' and `magit-buffer-diff-args'."
  (cond
   ((derived-mode-p 'magit-status-mode)
    (cl-loop for s = (magit-current-section) then (oref s parent)
             while s
             thereis (eq (oref s type) 'staged)))
   (t
    (let ((typearg  (bound-and-true-p magit-buffer-typearg))
          (diffargs (bound-and-true-p magit-buffer-diff-args)))
      (or (equal typearg "--staged")
          (equal typearg "--cached")
          (and diffargs (or (member "--staged" diffargs)
                            (member "--cached" diffargs))))))))

(defun ssdf--git (&rest args)
  "Run git with ARGS in `default-directory' and return stdout."
  (with-temp-buffer
    (apply #'process-file "git" nil t nil args)
    (buffer-string)))

;;;###autoload
(defun ssdf-from-magit ()
  "Open side-by-side view for the current magit diff buffer."
  (interactive)
  (pcase major-mode
    ('magit-revision-mode
     (let* ((rev (bound-and-true-p magit-buffer-revision))
            (source-fn (lambda (ctx)
                         (ssdf--git "show" (format "-U%d" ctx) "--format=" rev))))
       (unless rev (user-error "Cannot determine revision"))
       (ssdf-display-diff (funcall source-fn ssdf-default-context)
                          :context ssdf-default-context
                          :source-fn source-fn)))
    ('magit-stash-mode
     (let* ((rev (bound-and-true-p magit-buffer-revision))
            (source-fn (lambda (ctx)
                         (ssdf--git "diff" (format "-U%d" ctx)
                                    (concat rev "^1") rev))))
       (unless rev (user-error "Cannot determine revision"))
       (ssdf-display-diff (funcall source-fn ssdf-default-context)
                          :context ssdf-default-context
                          :source-fn source-fn)))
    ('magit-diff-mode
     (let* ((stage-arg (if (ssdf--magit-staged-p) '("--staged") nil))
              (source-fn (lambda (ctx)
                           (apply #'ssdf--git "diff" (format "-U%d" ctx) stage-arg))))
         (ssdf-display-diff (funcall source-fn ssdf-default-context)
                            :context ssdf-default-context
                            :source-fn source-fn)))
    ('magit-status-mode
     (let* ((section (magit-current-section))
            (lineage (magit-section-lineage section)))
       (pcase lineage
         (`(commit . ,_)
          (let* ((rev (oref section value))
                 (source-fn (lambda (ctx)
                              (ssdf--git "show" (format "-U%d" ctx) "--format=" rev))))
            (ssdf-display-diff (funcall source-fn ssdf-default-context)
                               :context ssdf-default-context
                               :source-fn source-fn)))
         (`(stash . ,_)
          (let* ((rev (oref section value))
                 (source-fn (lambda (ctx)
                              (ssdf--git "diff" (format "-U%d" ctx)
                                         (concat rev "^1") rev))))
            (ssdf-display-diff (funcall source-fn ssdf-default-context)
                               :context ssdf-default-context
                               :source-fn source-fn)))
         (`(hunk file staged . ,_)
          (let* ((file (oref (oref section parent) value))
                 (source-fn (lambda (ctx)
                              (ssdf--git "diff" (format "-U%d" ctx)
                                         "--staged" "--" file))))
            (ssdf-display-diff (funcall source-fn ssdf-default-context)
                               :context ssdf-default-context
                               :source-fn source-fn)))
         (`(hunk file unstaged . ,_)
          (let* ((file (oref (oref section parent) value))
                 (source-fn (lambda (ctx)
                              (ssdf--git "diff" (format "-U%d" ctx) "--" file))))
            (ssdf-display-diff (funcall source-fn ssdf-default-context)
                               :context ssdf-default-context
                               :source-fn source-fn)))
         (`(file staged . ,_)
          (let* ((file (oref section value))
                 (source-fn (lambda (ctx)
                              (ssdf--git "diff" (format "-U%d" ctx)
                                         "--staged" "--" file))))
            (ssdf-display-diff (funcall source-fn ssdf-default-context)
                               :context ssdf-default-context
                               :source-fn source-fn)))
         (`(file unstaged . ,_)
          (let* ((file (oref section value))
                 (source-fn (lambda (ctx)
                              (ssdf--git "diff" (format "-U%d" ctx) "--" file))))
            (ssdf-display-diff (funcall source-fn ssdf-default-context)
                               :context ssdf-default-context
                               :source-fn source-fn)))
         (`(staged . ,_)
          (let ((source-fn (lambda (ctx)
                             (ssdf--git "diff" (format "-U%d" ctx) "--staged"))))
            (ssdf-display-diff (funcall source-fn ssdf-default-context)
                               :context ssdf-default-context
                               :source-fn source-fn)))
         (`(unstaged . ,_)
          (let ((source-fn (lambda (ctx)
                             (ssdf--git "diff" (format "-U%d" ctx)))))
            (ssdf-display-diff (funcall source-fn ssdf-default-context)
                               :context ssdf-default-context
                               :source-fn source-fn)))
         (_ (user-error "Unsupported section at point (lineage: %s)" lineage)))))
    (_ (user-error "Not in a magit diff buffer (got %s)" major-mode))))


(provide 'side-by-side-diff)
;;; side-by-side-diff.el ends here
