;;; ~/doom.d/night-buffer.el -*- lexical-binding: t; -*-
;;;
(defun night/force-kill-current-buffer ()
  (interactive)
  ;; [[https://emacs.stackexchange.com/questions/59348/force-kill-a-buffer][force kill a buffer? - Emacs Stack Exchange]]
  ;; You can use [help:with-current-buffer] together with this to force kill any buffer.
  (let (kill-buffer-hook kill-buffer-query-functions)
    (kill-buffer)))
;;;
(defun night/buffer-reopen ()
  (interactive)
  (if-let (filename (or
                     buffer-file-name))
      (progn
        (kill-current-buffer)
        (find-file-existing filename))
    (error "Couldn't find filename in current buffer")))
;;;
(defun night/diff-buffers (buffer-A buffer-B)
  "Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B."
  (interactive
   (list (read-buffer "buffer1: " (current-buffer))
         (read-buffer "buffer2: " (current-buffer))))
  ;; ediff might not be loaded, I don't know how it is loaded, but playing around with its commands does load it ...
  ;; (ediff-buffers-internal buffer-A buffer-B nil nil 'ediff-buffers)
  (ediff-buffers buffer-A buffer-B))

(defun night/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or
                     buffer-file-name
                     (and (bound-and-true-p list-buffers-directory)
                          list-buffers-directory)))
      (progn
        (kill-new filename)
        (message "Yanked: %s" filename))
    (error "Couldn't find filename in current buffer")))

(map! :leader "fy" #'night/yank-buffer-filename) ; overrides doom's version
