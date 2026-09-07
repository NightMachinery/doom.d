;;; ~/doom.d/night-file.el -*- lexical-binding: t; -*-

(defun night/make-buffer-executable ()
  (interactive)
  (shell-command
   (concat "chmod u+x " (shell-quote-argument (buffer-file-name)))))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;;;
(defun night/change-file-extension (&optional ext)
      (interactive)
      (let* (
             ;;; 
             ;; (new-extension (read-from-minibuffer "Type the new extension including the dot (.): "))
             ;; (new-file-name (concat (file-name-sans-extension buffer-file-name) new-extension))
             ;;;
             (new-extension (or ext
                                (ivy-read "(Also saves the current buffer!) Type the new extension (without the dot): " '("org" "md"))))
             (new-file-name (concat (file-name-sans-extension buffer-file-name) "." new-extension))
             (filename (buffer-file-name)))
        (save-buffer)
        (rename-file filename new-file-name t)
        (rename-buffer (concat (file-name-sans-extension (buffer-name)) new-extension))
        (set-visited-file-name new-file-name)
        (set-buffer-modified-p nil)
        (message (concat "File renamed to " new-file-name))))

(defun night/extension-set-to-org ()
  (interactive)
  (night/change-file-extension "org"))
;;;
(defun night/mkdir-for-file (filename)
  "Create the directory for FILENAME if it does not exist."
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename))))
;;;
(defcustom night/encrypted-file-patterns
  '("\\.(gpg|asc|pgp|age)\\Z")
  "PCRE patterns naming files that Emacs decrypts into the buffer.

Matched by `night/file-encrypted-p', which also consults
`epa-file-name-regexp', so this list never has to restate the epa
configuration -- it is here for the formats epa does not own, such as
age."
  :type '(repeat string)
  :group 'night)

(defun night/file-path-candidates (file)
  "Return the paths FILE should be matched against: itself and its truename.

A symlink whose own name looks innocent can point into a tree that is
not, and the reverse happens just as often, so a path policy that looks
at only one of the two is trivially side-stepped."
  (when file
    (let ((expanded (expand-file-name file)))
      (delete-dups (list expanded (file-truename expanded))))))

(defun night/file-encrypted-p (file)
  "Return non-nil if FILE is one whose plaintext only ever lives in a buffer.

Both FILE and its truename are tested, against
`night/encrypted-file-patterns' and against `epa-file-name-regexp'."
  (let ((case-fold-search nil))
    (cl-some
     (lambda (path)
       (or (cl-some (lambda (pattern)
                      ;; A pattern that will not compile is skipped rather than
                      ;; treated as a match: `epa-file-name-regexp' below still
                      ;; catches the ordinary case, whereas calling every file
                      ;; encrypted would have `night/close-fileless-buffers'
                      ;; kill the session.  `night/fim-path-policy' makes the
                      ;; opposite choice, because there the cost is reversed.
                      (when-let ((regexp (night/pcre-to-regexp pattern)))
                        (string-match-p regexp path)))
                    night/encrypted-file-patterns)
           (and (bound-and-true-p epa-file-name-regexp)
                (string-match-p epa-file-name-regexp path))))
     (night/file-path-candidates file))))
(comment
 (night/file-encrypted-p "/tmp/a.org.gpg")
 (night/file-encrypted-p "/tmp/a.org"))
;;;
