;;; autoload/night-ntag.el -*- lexical-binding: t; -*-

(defvar ntag-sep "..")

(defun night/ntag-create (tag)
  (concat  ntag-sep tag ntag-sep))
(defalias 'ntag #'night/ntag-create)
(defalias 'night/ntag #'night/ntag-create)
(comment
 (ntag "hi"))

;;;
(defun night/buffer-playlistp (&optional buffer)
  "Return non-nil if the file name associated with BUFFER (or the current buffer)
contains the tag 'org-playlist'.

BUFFER can be either a buffer or a string representing a file name.
Defaults to the current buffer if BUFFER is nil."
  (let ((bfn (cond
              ((stringp buffer) buffer)
              ((bufferp buffer) (buffer-file-name buffer))
              (t (buffer-file-name (current-buffer))))))
    (and bfn (s-contains? (night/ntag "org-playlist") bfn))))

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
