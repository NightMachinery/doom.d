;;; autoload/night-ntag.el -*- lexical-binding: t; -*-

(defvar ntag-sep "..")

(defun night/ntag-create (tag)
  (concat  ntag-sep tag ntag-sep))
(defalias 'ntag #'night/ntag-create)
(defalias 'night/ntag #'night/ntag-create)
(comment
 (ntag "hi"))

;;;
(defun night/buffer-tag-p (tag &optional buffer)
  "Return non-nil if the file name associated with BUFFER (or the current buffer)
contains the TAG, processed by `night/ntag`.

TAG is the string tag to check for (e.g., \"org-playlist\").
BUFFER can be either a buffer or a string representing a file name.
Defaults to the current buffer if BUFFER is nil."
  ;; Ensure tag is a string, otherwise night/ntag might error later
  (unless (stringp tag)
    (error "TAG argument must be a string, got %S" tag))

  (let ((bfn (cond
              ((stringp buffer) buffer) ; If buffer is already a filename string
              ((bufferp buffer) (buffer-file-name buffer)) ; If buffer is a buffer object
              ((null buffer) (buffer-file-name (current-buffer))) ; Default to current buffer
              (t (error "BUFFER argument must be a string, buffer, or nil, got %S" buffer))))) ; Handle invalid buffer arg
    ;; Check if bfn is non-nil (i.e., associated with a file)
    ;; and if the filename contains the processed tag.
    (and bfn (s-contains? (night/ntag tag) bfn))))

(defun night/buffer-playlistp (&optional buffer)
  "Return non-nil if the file name associated with BUFFER
contains the tag 'org-playlist'."
  (night/buffer-tag-p "org-playlist" buffer))

(defun night/buffer-no-night-directives-p (&optional buffer)
  "Return non-nil if the file name associated with BUFFER
contains the tag 'org-playlist'."
  (night/buffer-tag-p "no-directives" buffer))

(cl-defun night/search-random-instance-smart (&optional arg)
  "Jump to a random instance of a search pattern with smart behavior for playlists.

If the current buffer is recognized as a playlist (see `night/buffer-playlistp')
and no prefix argument (C-u) is provided, the search pattern \"\\<@\\(great|good\\)\"
is used. Otherwise, the current Evil search pattern is used.

ARG is the interactive prefix argument."
  (interactive "P")
  (if (and (not arg) (night/buffer-playlistp))
      (night/search-random-instance
       :pattern "\\<@\\(great\\|good\\|horror\\|nostalgia\\|female/soft\\|upbeat\\|roman\\|humming\\)"
       ;; \\|pain
       ;; It's best not to weight painful songs more. They already have a good number of good/great tags.
       )
    (night/search-random-instance)))
;;;
