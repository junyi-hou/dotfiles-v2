;;; side-by-side-diff.el --- Side-by-side unified diff viewer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'outline)

;;;; Customization

(defgroup side-by-side-diff nil
  "Side-by-side unified diff viewer."
  :group 'tools
  :prefix "ssdf-")

(defcustom ssdf-default-context 3
  "Default context lines shown around each hunk."
  :type 'natnum
  :group 'side-by-side-diff)


;;;; Faces

(defface ssdf-removed
  '((t :inherit magit-diff-removed))
  "Background for removed lines (left buffer)."
  :group 'side-by-side-diff)

(defface ssdf-added
  '((t :inherit magit-diff-added))
  "Background for added lines (right buffer)."
  :group 'side-by-side-diff)

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


(defface ssdf-removed-word
  '((t :inherit magit-diff-removed-highlight))
  "Face for intra-line removed word spans."
  :group 'side-by-side-diff)

(defface ssdf-added-word
  '((t :inherit magit-diff-added-highlight))
  "Face for intra-line added word spans."
  :group 'side-by-side-diff)

;;;; Global state

(defconst ssdf--left-name    "*ssdf-left*")
(defconst ssdf--right-name   "*ssdf-right*")

(defvar ssdf--window-config nil
  "Window configuration saved before opening the side-by-side view.")

;;;; Buffer-local state

(defvar-local ssdf--peer nil
  "The peer buffer (the other side of the diff).")

(defvar-local ssdf--context ssdf-default-context
  "Number of context lines currently displayed.")

(defvar-local ssdf--source-fn nil
  "Function (context-lines -> diff-string) to regenerate the diff.
Nil when the diff cannot be regenerated (e.g. static diff-mode buffer).")

(defvar ssdf--syncing nil
  "Non-nil while syncing peer position, to prevent recursion.")

(defvar-local ssdf--dim-overlays nil
  "Overlays covering non-current hunk lines.")


(defvar-local ssdf--parser nil
  "Parser function (diff-text -> hunks) used for this session.
Stored so context adjustment can regenerate with the same parser.")

;;;; Parsing

(cl-defstruct (ssdf--hunk (:constructor ssdf--hunk-create) (:copier nil))
  file header old-start new-start lines)

(defun ssdf--parse (diff-text)
  "Return list of `ssdf--hunk' structs parsed from unified DIFF-TEXT."
  (let (result file header old new pending)
    (cl-flet ((flush ()
                (when header
                  (push (ssdf--hunk-create
                         :file file :header header
                         :old-start old :new-start new
                         :lines (nreverse pending))
                        result)
                  (setq header nil pending nil))))
      (dolist (line (split-string diff-text "\n"))
        (cond
         ((string-match "^diff --git a/.+ b/\\(.+\\)" line)
          (flush)
          (setq file (match-string 1 line)))
         ((string-match "^@@ -\\([0-9]+\\)[^+]* \\+\\([0-9]+\\)" line)
          (flush)
          (setq header line
                old (string-to-number (match-string 1 line))
                new (string-to-number (match-string 2 line))))
         ((and header (not (string-empty-p line)))
          (let ((ch (aref line 0)))
            (cond ((= ch ?-) (push (cons 'removed (substring line 1)) pending))
                  ((= ch ?+) (push (cons 'added   (substring line 1)) pending))
                  ((= ch ? ) (push (cons 'context (substring line 1)) pending)))))))
      (flush))
    (nreverse result)))

;;;; Word-diff support

(defun ssdf--propertize-word-spans (text side)
  "Return TEXT with word-diff markers converted to Emacs text properties.
SIDE is `left' or `right'.
For `left': highlight [-...-] content with `ssdf-removed-word', drop {+...+}.
For `right': highlight {+...+} content with `ssdf-added-word', drop [-...-]."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "\\[\\-\\(.*?\\)\\-\\]" nil t)
      (replace-match
       (if (eq side 'left)
           (propertize (match-string 1) 'face 'ssdf-removed-word)
         "")
       t t))
    (goto-char (point-min))
    (while (re-search-forward "{\\+\\(.*?\\)\\+}" nil t)
      (replace-match
       (if (eq side 'right)
           (propertize (match-string 1) 'face 'ssdf-added-word)
         "")
       t t))
    (buffer-string)))

(defun ssdf--parse-word-diff (diff-text)
  "Return list of `ssdf--hunk' structs parsed from --word-diff=plain DIFF-TEXT.
Lines with both [-...-] and {+...+} markers produce a `word-changed' entry
whose cdr is (left-propertized . right-propertized).
Lines wrapped entirely in [-...-] or {+...+} produce `removed'/`added' entries."
  (let (result file header old new pending)
    (cl-flet ((flush ()
                (when header
                  (push (ssdf--hunk-create
                         :file file :header header
                         :old-start old :new-start new
                         :lines (nreverse pending))
                        result)
                  (setq header nil pending nil))))
      (dolist (line (split-string diff-text "\n"))
        (cond
         ((string-match "^diff --git a/.+ b/\\(.+\\)" line)
          (flush)
          (setq file (match-string 1 line)))
         ((string-match "^@@ -\\([0-9]+\\)[^+]* \\+\\([0-9]+\\)" line)
          (flush)
          (setq header line
                old (string-to-number (match-string 1 line))
                new (string-to-number (match-string 2 line))))
         ((and header (not (string-empty-p line)))
          (cond
           ((string-prefix-p "[-" line)
            (push (cons 'removed
                        (substring line 2 (- (length line) 2)))
                  pending))
           ((string-prefix-p "{+" line)
            (push (cons 'added
                        (substring line 2 (- (length line) 2)))
                  pending))
           ((= (aref line 0) ? )
            (let ((content (substring line 1)))
              (if (string-match-p "\\[\\-\\|{\\+" content)
                  (push (cons 'word-changed
                              (cons (ssdf--propertize-word-spans content 'left)
                                    (ssdf--propertize-word-spans content 'right)))
                        pending)
                (push (cons 'context content) pending))))))))
      (flush))
    (nreverse result)))

;;;; Alignment

(defun ssdf--align (lines)
  "Align hunk LINES into (left-tagged . right-tagged) for side-by-side display.
Each element is (type . text) where type is context/removed/added/padding.
Runs of removed/added are padded so both sides have equal line counts."
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
          ('context     (flush)
                        (push (cons 'context (cdr cell)) left)
                        (push (cons 'context (cdr cell)) right))
          ('removed     (push (cdr cell) rm-acc))
          ('added       (push (cdr cell) add-acc))
          ('word-changed
           (flush)
           (push (cons 'word-changed (cadr cell)) left)
           (push (cons 'word-changed (cddr cell)) right))))
      (flush))
    (cons (nreverse left) (nreverse right))))

;;;; Rendering

(defun ssdf--line-face (type side)
  "Return face for line TYPE on SIDE (\\='left or \\='right), or nil."
  (pcase type
    ('removed      (when (eq side 'left)  'ssdf-removed))
    ('added        (when (eq side 'right) 'ssdf-added))
    ('word-changed (if   (eq side 'left)  'ssdf-removed 'ssdf-added))
    ('padding      'ssdf-padding)
    (_ nil)))

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

(defun ssdf--render (hunks left-buf right-buf)
  "Fill LEFT-BUF and RIGHT-BUF with the side-by-side rendering of HUNKS."
  (let (cur-file)
    (dolist (hunk hunks)
      (unless (equal (ssdf--hunk-file hunk) cur-file)
        (setq cur-file (ssdf--hunk-file hunk))
        (let ((heading (ssdf--propertize-file-heading cur-file)))
          (dolist (buf (list left-buf right-buf))
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (insert heading))))))
      (let* ((aligned     (ssdf--align (ssdf--hunk-lines hunk)))
             (left-lines  (car aligned))
             (right-lines (cdr aligned))
             (hunk-header (ssdf--propertize-hunk-header (ssdf--hunk-header hunk))))
        (dolist (buf (list left-buf right-buf))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (insert hunk-header))))
        (cl-loop for (ltype . ltext) in left-lines
                 for (rtype . rtext) in right-lines
                 do
                 (with-current-buffer left-buf
                   (let ((inhibit-read-only t)
                         (face (ssdf--line-face ltype 'left)))
                     (let ((s (concat ltext "\n")))
                       (when face (add-face-text-property 0 (length s) face t s))
                       (insert s))))
                 (with-current-buffer right-buf
                   (let ((inhibit-read-only t)
                         (face (ssdf--line-face rtype 'right)))
                     (let ((s (concat rtext "\n")))
                       (when face (add-face-text-property 0 (length s) face t s))
                       (insert s)))))))))


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
      ;; Apply per-heading faces at priority 1 so they stand out from ssdf-dimmed
      ;; (priority 0). Current hunk/file get -highlight variants; others get the
      ;; normal variants, preventing them from being uniformly dimmed.
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
    (define-key map (kbd "=")   #'ssdf-increase-context)
    (define-key map (kbd "-")   #'ssdf-decrease-context)
    (define-key map (kbd "TAB") #'ssdf-next-file)
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
  (add-hook 'post-command-hook        #'ssdf--sync           nil t)
  (add-hook 'post-command-hook        #'ssdf--update-dimming t   t)
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

(defun ssdf--adjust-context (delta)
  "Change context lines by DELTA and refresh."
  (let ((source-fn (or ssdf--source-fn
                       (and (buffer-live-p ssdf--peer)
                            (buffer-local-value 'ssdf--source-fn ssdf--peer))))
        (parser    (or ssdf--parser
                       (and (buffer-live-p ssdf--peer)
                            (buffer-local-value 'ssdf--parser ssdf--peer))
                       #'ssdf--parse))
        (new-ctx   (max 0 (+ (or ssdf--context ssdf-default-context) delta))))
    (unless source-fn
      (user-error "Cannot adjust context: diff source unavailable"))
    (ssdf-display-diff (funcall source-fn new-ctx)
                       :context new-ctx
                       :source-fn source-fn
                       :parser parser)))

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
(cl-defun ssdf-display-diff (diff-text &key context source-fn (parser #'ssdf--parse))
  "Display DIFF-TEXT as a side-by-side diff in two windows.

CONTEXT is the context-line count encoded in DIFF-TEXT (informational).
SOURCE-FN is a function (context-lines -> diff-string) enabling live
context adjustment via `ssdf-increase-context' / `ssdf-decrease-context'.
PARSER is the function used to parse DIFF-TEXT (default `ssdf--parse').
Pass `ssdf--parse-word-diff' when DIFF-TEXT was produced with --word-diff=plain."
  (let ((hunks (funcall parser diff-text)))
    (unless hunks
      (user-error "No diff hunks found"))
    ;; Preserve the original window layout unless we are refreshing from
    ;; within an existing ssdf session (context adjustment).
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
          (ssdf-mode)))
      (ssdf--render hunks left-buf right-buf)
      (with-current-buffer left-buf
        (setq ssdf--peer right-buf
              ssdf--context ctx
              ssdf--source-fn source-fn
              ssdf--parser parser)
        (goto-char (point-min)))
      (with-current-buffer right-buf
        (setq ssdf--peer left-buf
              ssdf--context ctx
              ssdf--source-fn source-fn
              ssdf--parser parser)
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

(defun ssdf--magit-staged-p ()
  "Return non-nil if the current magit context refers to staged changes.
In `magit-status-mode' this checks the section at point; in `magit-diff-mode'
it checks `magit-buffer-typearg' and `magit-buffer-diff-args'."
  (cond
   ((derived-mode-p 'magit-status-mode)
    (magit-section-match '(staged * *)))
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
  (unless (featurep 'magit)
    (user-error "Magit is not loaded"))
  (pcase major-mode
    ('magit-revision-mode
     (let ((rev (bound-and-true-p magit-buffer-revision)))
       (unless rev (user-error "Cannot determine revision"))
       (ssdf-display-diff
        (ssdf--git "show" (format "-U%d" ssdf-default-context) "--word-diff=plain" "--format=" rev)
        :context ssdf-default-context
        :parser #'ssdf--parse-word-diff
        :source-fn (lambda (ctx)
                     (ssdf--git "show" (format "-U%d" ctx) "--word-diff=plain" "--format=" rev)))))

    ('magit-stash-mode
     (let ((rev (bound-and-true-p magit-buffer-revision)))
       (unless rev (user-error "Cannot determine revision"))
       (ssdf-display-diff
        (ssdf--git "diff" (format "-U%d" ssdf-default-context) "--word-diff=plain"
                   (concat rev "^1") rev)
        :context ssdf-default-context
        :parser #'ssdf--parse-word-diff
        :source-fn (lambda (ctx)
                     (ssdf--git "diff" (format "-U%d" ctx) "--word-diff=plain"
                                (concat rev "^1") rev)))))

    ((or 'magit-status-mode 'magit-diff-mode)
     (let* ((staged (ssdf--magit-staged-p))
            (extra  (if staged '("--staged") nil))
            (source-fn (lambda (ctx)
                         (apply #'ssdf--git "diff"
                                (format "-U%d" ctx) "--word-diff=plain" extra))))
       (ssdf-display-diff
        (funcall source-fn ssdf-default-context)
        :context ssdf-default-context
        :parser #'ssdf--parse-word-diff
        :source-fn source-fn)))

    (_ (user-error "Not in a magit diff buffer (got %s)" major-mode))))

;;;; Evil integration

(declare-function evil-set-initial-state  "evil-core"    (mode state))
(declare-function evil-define-key*        "evil-core"    (state keymap &rest bindings))
(declare-function magit-current-section   "magit-section" ())
(declare-function magit-section-match     "magit-section" (condition &optional section))

(with-eval-after-load 'evil
  (evil-set-initial-state 'ssdf-mode 'motion)
  (evil-define-key* 'motion ssdf-mode-map
    "n"         #'ssdf-next-hunk
    "p"         #'ssdf-prev-hunk
    "N"         #'ssdf-next-file
    "P"         #'ssdf-prev-file
    "]"         #'ssdf-next-hunk
    "["         #'ssdf-prev-hunk
    "}"         #'ssdf-next-file
    "{"         #'ssdf-prev-file
    "+"         #'ssdf-increase-context
    "="         #'ssdf-increase-context
    "-"         #'ssdf-decrease-context
    "q"         #'ssdf-quit))

(provide 'side-by-side-diff)
;;; side-by-side-diff.el ends here
