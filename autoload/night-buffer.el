;;; ~/doom.d/night-buffer.el -*- lexical-binding: t; -*-

(defun night/diff-buffers (buffer-A buffer-B)
  "Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B."
  (interactive
   (list (read-buffer "buffer1: " (current-buffer))
         (read-buffer "buffer2: " (current-buffer))))
  (ediff-buffers-internal buffer-A buffer-B nil nil 'ediff-buffers))
