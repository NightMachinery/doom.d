;;; autoload/night-plaintext-cells.el -*- lexical-binding: t; -*-
;;;
(defun night/get-comment-char ()
  "Get the comment character for the current major mode."
  (cond
   (comment-start
    (substring comment-start 0 1))
   (t
    "#"
    ;; (error "No comment character defined for this mode")
    )))

(defun night/h-make-cell-boundary-regexp (num-chars)
  "Create a regexp to match cell boundaries with NUM-CHARS comment characters."
  (let* ((comment-char (night/get-comment-char)))
    (concat
     "^"
     "\\s-*"
     (regexp-quote (make-string num-chars (string-to-char comment-char))))))

(cl-defun night/h-move-to-plaintext-cell (&key direction (num-chars 3))
  "Move to the plaintext cell boundary in the specified DIRECTION.
If NUM-CHARS is provided, use that many comment characters to mark the boundary."
  (let* ((boundary-regexp (night/h-make-cell-boundary-regexp num-chars))

         (search-fn (if (eq direction 'next) 're-search-forward 're-search-backward))
         (move-line (if (eq direction 'next) 1 -1)))
    (move-beginning-of-line nil)
    (when (looking-at-p boundary-regexp)
      (forward-line move-line))
    (funcall search-fn boundary-regexp nil t)))

(defun night/previous-plaintext-cell (&optional num-chars)
  "Move to the previous plaintext cell boundary.
If NUM-CHARS is provided, use that many comment characters to mark the boundary."
  (interactive "P")
  (night/h-move-to-plaintext-cell :direction 'previous :num-chars (or num-chars 3)))

(defun night/next-plaintext-cell (&optional num-chars)
  "Move to the next plaintext cell boundary.
If NUM-CHARS is provided, use that many comment characters to mark the boundary."
  (interactive "P")
  (night/h-move-to-plaintext-cell :direction 'next :num-chars (or num-chars 3)))

(map! :map (python-mode-map emacs-lisp-mode-map)
      "C-<up>" #'night/previous-plaintext-cell
      "C-<down>" #'night/next-plaintext-cell)
;;;
