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
(require 'outline)
(require 'ansi-color)

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
            (setq header    plain
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
  "Insert TEXT plus a newline into BUF; apply padding face when TYPE is padding."
  (with-current-buffer buf
    (let ((inhibit-read-only t)
          (start (point)))
      (insert text "\n")
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
             (hunk-header (ssdf--propertize-hunk-header (ssdf--hunk-header hunk))))
        (dolist (buf (list left-buf right-buf))
          (with-current-buffer buf
            (let ((inhibit-read-only t)) (insert hunk-header))))
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
        (new-ctx (max 0 (+ (or ssdf--context ssdf-default-context) delta))))
    (unless source-fn
      (user-error "Cannot adjust context: diff source unavailable"))
    (ssdf-display-diff (funcall source-fn new-ctx)
                       :context new-ctx
                       :source-fn source-fn)))

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
  (unless (featurep 'magit)
    (user-error "Magit is not loaded"))
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

    ((or 'magit-status-mode 'magit-diff-mode)
     (if (and (derived-mode-p 'magit-status-mode)
              (magit-section-match 'stash))
         (let* ((rev (oref (magit-current-section) value))
                (source-fn (lambda (ctx)
                             (ssdf--git "diff" (format "-U%d" ctx)
                                        (concat rev "^1") rev))))
           (ssdf-display-diff (funcall source-fn ssdf-default-context)
                              :context ssdf-default-context
                              :source-fn source-fn))
       (let* ((stage-arg (if (ssdf--magit-staged-p) '("--staged") nil))
              (source-fn (lambda (ctx)
                           (apply #'ssdf--git "diff" (format "-U%d" ctx) stage-arg))))
         (ssdf-display-diff (funcall source-fn ssdf-default-context)
                            :context ssdf-default-context
                            :source-fn source-fn))))

    (_ (user-error "Not in a magit diff buffer (got %s)" major-mode))))

;;;; Evil integration

(declare-function evil-set-initial-state  "evil-core"     (mode state))
(declare-function evil-define-key*        "evil-core"     (state keymap &rest bindings))
(declare-function magit-current-section   "magit-section" ())
(declare-function magit-section-match     "magit-section" (condition &optional section))

(with-eval-after-load 'evil
  (evil-set-initial-state 'ssdf-mode 'motion)
  (evil-define-key* 'motion ssdf-mode-map
    "n" #'ssdf-next-hunk
    "p" #'ssdf-prev-hunk
    "N" #'ssdf-next-file
    "P" #'ssdf-prev-file
    "]" #'ssdf-next-hunk
    "[" #'ssdf-prev-hunk
    "}" #'ssdf-next-file
    "{" #'ssdf-prev-file
    "+" #'ssdf-increase-context
    "=" #'ssdf-increase-context
    "-" #'ssdf-decrease-context
    "q" #'ssdf-quit))

(provide 'side-by-side-diff)
;;; side-by-side-diff.el ends here
