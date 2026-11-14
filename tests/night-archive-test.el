;;; tests/night-archive-test.el -*- lexical-binding: t; -*-
(setq native-comp-enable-subr-trampolines nil)
(require 'ert)
(require 'org)
(require 'org-archive)

(unless (fboundp '--map)
  (defmacro --map (form list)
    `(mapcar (lambda (it) ,form) ,list)))

(unless (fboundp '-each)
  (defun -each (list fn)
    (mapc fn list)))

(unless (fboundp 's-slice-at)
  (defun s-slice-at (regexp s)
    (let ((pos 0)
          (res nil))
      (while (string-match regexp s (if (> pos 0) (1+ pos) 0))
        (let ((idx (match-beginning 0)))
          (push (substring s pos idx) res)
          (setq pos idx)))
      (push (substring s pos) res)
      (nreverse res))))

(load (expand-file-name "../autoload/org/night-archive.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro night/test-with-org-archive-files (&rest body)
  "Run BODY with temporary source, archive, and unrelated Org files."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "night-archive-test-" t))
          (src-file (expand-file-name "notes.org" dir))
          (arch-file (expand-file-name "notes.org_archive" dir))
          (other-file (expand-file-name "other.org" dir))
          (org-archive-location "%s_archive::")
          (org-archive-subtree-save-file-p t)
          (org-tags-column 0)
          src-buf arch-buf other-buf)
     (unwind-protect
         (progn
           (with-temp-file src-file
             (insert "* Parent :work:\n** Child task\nBody text\n"))
           (with-temp-file other-file
             (insert "* Unrelated\n"))
           (setq src-buf (find-file-noselect src-file))
           (setq other-buf (find-file-noselect other-file))
           (with-current-buffer other-buf
             (org-mode)
             (goto-char (point-max))
             (insert "unsaved edit\n"))
           (with-current-buffer src-buf
             (org-mode)
             (goto-char (point-min))
             (re-search-forward "^\\*\\* Child task")
             ,@body))
       (dolist (buf (list src-buf arch-buf other-buf
                          (find-buffer-visiting arch-file)))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory dir t))))

(ert-deftest night/org-archive-save-mode-auto-saves-source-and-archive-only ()
  "Default `auto' saves source and archive buffers without prompting or touching others."
  (night/test-with-org-archive-files
    (let ((night/org-archive-save-mode 'auto)
          (prompted nil))
      (cl-letf (((symbol-function 'save-some-buffers)
                 (lambda (&rest _) (setq prompted t))))
        (org-archive-subtree-default))
      (setq arch-buf (find-buffer-visiting arch-file))
      (should-not prompted)
      (should (buffer-live-p arch-buf))
      (should-not (buffer-modified-p src-buf))
      (should-not (buffer-modified-p arch-buf))
      (should (buffer-modified-p other-buf))
      (with-current-buffer arch-buf
        (goto-char (point-min))
        (should (re-search-forward "^\\* Parent :work:$" nil t))
        (should (re-search-forward "^\\*\\* Child task$" nil t))))))

(ert-deftest night/org-archive-save-mode-ask-scopes-to-archive-pair ()
  "`ask' passes `save-some-buffers' a predicate matching only source and archive."
  (night/test-with-org-archive-files
    (let ((night/org-archive-save-mode 'ask)
          (captured-pred nil)
          (captured-arg 'unset))
      (cl-letf (((symbol-function 'save-some-buffers)
                 (lambda (&optional arg pred)
                   (setq captured-arg arg
                         captured-pred pred))))
        (org-archive-subtree-default))
      (setq arch-buf (find-buffer-visiting arch-file))
      (should (null captured-arg))
      (should (functionp captured-pred))
      (should (with-current-buffer src-buf (funcall captured-pred)))
      (should (with-current-buffer arch-buf (funcall captured-pred)))
      (should-not (with-current-buffer other-buf (funcall captured-pred)))
      (should (buffer-modified-p src-buf))
      (should (buffer-modified-p arch-buf)))))

(ert-deftest night/org-archive-save-mode-ask-all-calls-unfiltered-save-some-buffers ()
  "`ask-all' preserves calling `save-some-buffers' with no filter."
  (night/test-with-org-archive-files
    (let ((night/org-archive-save-mode 'ask-all)
          (called-with 'unset))
      (cl-letf (((symbol-function 'save-some-buffers)
                 (lambda (&rest args) (setq called-with args))))
        (org-archive-subtree-default))
      (should (null called-with)))))

(ert-deftest night/org-archive-save-mode-nil-leaves-both-unsaved ()
  "nil leaves both source and archive buffers modified despite `org-archive-subtree-save-file-p'."
  (night/test-with-org-archive-files
    (let ((night/org-archive-save-mode nil))
      (org-archive-subtree-default)
      (setq arch-buf (find-buffer-visiting arch-file))
      (should (buffer-modified-p src-buf))
      (should (buffer-modified-p arch-buf)))))

(ert-deftest night/org-toggle-retired-tag-insert-and-detect ()
  "Insert `@retired' at start of heading title and remove it from anywhere in title."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (org-priority-highest 0)
        (org-priority-lowest 9)
        (org-tags-column 0))
    (dolist (pair '(("* Heading\nBody line\n" . "* @retired Heading\nBody line\n")
                    ("** TODO [#1] Heading :work:\n" . "** TODO [#1] @retired Heading :work:\n")
                    ("* \n" . "* @retired\n")
                    ("* TODO\n" . "* TODO @retired\n")
                    ("* :work:\n" . "* @retired :work:\n")
                    ("* @retired Heading\n" . "* Heading\n")
                    ("* Heading @retired middle\n" . "* Heading middle\n")
                    ("* Heading @retired\n" . "* Heading\n")
                    ("* @retired @retired Heading\n" . "* Heading\n")
                    ("* @retired\n" . "* \n")
                    ("* @retired :work:\n" . "* :work:\n")
                    ("* Heading @retired-v2\n" . "* @retired Heading @retired-v2\n")
                    ("* Heading foo@retired\n" . "* @retired Heading foo@retired\n")
                    ("* Heading @RETIRED\n" . "* @retired Heading @RETIRED\n")))
      (with-temp-buffer
        (org-mode)
        (insert (car pair))
        (goto-char (point-max))
        (forward-line -1)
        (night/org-toggle-retired-tag)
        (should (equal (buffer-string) (cdr pair)))))))

(ert-deftest night/org-toggle-retired-tag-works-from-subtree-body ()
  "Toggling from inside the subtree body updates the enclosing heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nSome body text\n")
    (goto-char (point-max))
    (should (eq (night/org-toggle-retired-tag) t))
    (should (equal (buffer-string) "* @retired Heading\nSome body text\n"))
    (should (eq (night/org-toggle-retired-tag) nil))
    (should (equal (buffer-string) "* Heading\nSome body text\n"))))
