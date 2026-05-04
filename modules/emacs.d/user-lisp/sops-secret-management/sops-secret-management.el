;;; sops-secret-management.el --- Secret management via sops and passage -*- lexical-binding: t; -*-
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Code:

(require 'json)
(require 'map)
(require 'treesit)

(declare-function gatsby>switch-to-buffer-new-window "gatsby>>utility")
(defvar gatsby>dotfiles-repo-location)

(defgroup sops-secret-management nil
  "Secret management via sops and passage."
  :group 'tools
  :prefix "sops-")

(defcustom sops-enc-file "env.json.enc"
  "Name of the sops-encrypted secrets file, relative to `gatsby>dotfiles-repo-location'."
  :type 'string
  :group 'sops-secret-management)

;; secret retrieval via passage

(defun sops--passage-paths (data &optional prefix)
  "Return all leaf key paths in DATA as a list of slash-separated strings.
DATA is a parsed JSON alist; the top-level `sops' metadata key is skipped."
  (let (paths)
    (map-do
     (lambda (key value)
       (unless (eq key 'sops)
         (let ((path (if prefix (format "%s/%s" prefix key) (format "%s" key))))
           (if (listp value)
               (setq paths (append paths (sops--passage-paths value path)))
             (push path paths)))))
     data)
    paths))

(defun sops--local-public-key ()
  "Return the age public key from ~/.config/age/key."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "~/.config/age/key"))
    (goto-char (point-min))
    (if (re-search-forward "^# public key: \\(.+\\)$" nil t)
        (match-string 1)
      (user-error "No public key found in ~/.config/age/key"))))

;;;###autoload
(defun sops-retrieve-secret (secret-path)
  "Retrieve the secret at SECRET-PATH via passage and copy it to the clipboard.
The clipboard is cleared after 30 seconds."
  (interactive
   (let* ((enc-file (expand-file-name sops-enc-file gatsby>dotfiles-repo-location)))
     (unless (file-exists-p enc-file)
       (user-error "Encrypted secrets not found: %s" enc-file))
     (let* ((data (json-read-file enc-file))
            (paths (sops--passage-paths data)))
       (list (completing-read "Getting secret: " paths)))))
  (let ((secret (string-trim (shell-command-to-string
                              (format "passage %s" (shell-quote-argument secret-path))))))
    (if (called-interactively-p 'interactive)
        (progn
          (kill-new secret)
          (run-at-time 30 nil
                       (lambda (s)
                         (gui-set-selection 'CLIPBOARD "")
                         (setq kill-ring (delete s kill-ring))
                         (message "Secret cleaned"))
                       secret)
          (message "Secret %s copied to the clipboard..." secret-path))
      secret)))

;; sops-secret-ts-mode

(defvar-local sops-secret--enc-file nil
  "Path to the sops-encrypted file backing the current secret buffer.")

(defun sops-secret-save ()
  "Encrypt and save the current secret buffer back to its enc-file."
  (interactive)
  (let* ((buf (current-buffer))
         (enc-file sops-secret--enc-file))
    (message "Encrypting the secrets...")
    (json-pretty-print-buffer)
    (let* ((content (buffer-string))
           (default-directory gatsby>dotfiles-repo-location)
           (out-buf (generate-new-buffer " *sops-enc-output*"))
           (exit-code
            (with-temp-buffer
              (insert content)
              (call-process-region (point-min) (point-max) "sops"
                                   nil out-buf nil
                                   "-e"
                                   "--input-type" "json"
                                   "--output-type" "json"
                                   "--age" (sops--local-public-key)
                                   "/dev/stdin"))))
      (if (= exit-code 0)
          (progn
            (write-region (with-current-buffer out-buf (buffer-string)) nil enc-file)
            (kill-buffer out-buf)
            (kill-buffer buf)
            (delete-window)
            (message "Secrets saved to %s" (file-name-nondirectory enc-file)))
        (let ((err (with-current-buffer out-buf (buffer-string))))
          (kill-buffer out-buf)
          (user-error "sops encryption failed: %s" err))))))

(defun sops-secret-cancel ()
  "Discard the current secret buffer without saving."
  (interactive)
  (kill-buffer-and-window)
  (message "Edit cancelled"))

(defun sops--no-save ()
  (interactive)
  (message "Use C-c C-c to save secrets"))

(defvar sops-secret-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'sops-secret-save)
    (define-key map (kbd "C-c C-k") #'sops-secret-cancel)
    (define-key map [remap save-buffer] #'sops--no-save)
    (define-key map [remap write-file]  #'sops--no-save)
    (define-key map [remap evil-write]  #'sops--no-save)
    map)
  "Keymap for `sops-secret-ts-mode'.")

(define-derived-mode sops-secret-ts-mode json-ts-mode "Sops-Secret"
  "Major mode for editing SOPS-encrypted JSON secrets in a scratch buffer."
  :group 'sops-secret-management
  (setq-local buffer-file-name nil
              auto-save-default nil
              make-backup-files nil
              buffer-offer-save nil
              header-line-format
              '(:eval
                (let ((text "     C-c C-c  save & encrypt    C-c C-k  cancel"))
                  (propertize (concat text
                                      (make-string
                                       (max 0 (- (window-width) (length text))) ?\s))
                              'face '(error :inverse-video t)))))
  (auto-save-mode -1))

;;;###autoload
(defun sops-edit-secret ()
  "Decrypt the sops enc-file into a scratch buffer for editing.
Press C-c C-c to re-encrypt and save.  Normal file saves are disabled."
  (interactive)
  (unless (treesit-ready-p 'json t)
    (user-error "Tree-sitter JSON grammar unavailable (requires Emacs 29+ with grammar installed)"))
  (let* ((enc-file (expand-file-name sops-enc-file gatsby>dotfiles-repo-location))
         (_ (unless (file-exists-p enc-file)
              (user-error "Encrypted secrets not found: %s" enc-file)))
         (decrypted (shell-command-to-string
                     (format "sops --input-type json --output-type json -d %s"
                             (shell-quote-argument enc-file))))
         (buf (get-buffer-create "*edit-secret*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert decrypted)
      (json-pretty-print-buffer)
      (sops-secret-ts-mode)
      (setq-local sops-secret--enc-file enc-file))
    (gatsby>switch-to-buffer-new-window buf)))

(provide 'sops-secret-management)
;;; sops-secret-management.el ends here
