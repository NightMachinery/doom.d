;;; night-util.el ---                                -*- lexical-binding: t; -*-
(defun night/range (start &optional end step)
  (unless end
    (setq end start
      start 0))
  (number-sequence start (1- end) step))

(comment
 (night/range 10)
 (night/range 5 10)
 (night/range 5 10 3))

(defun night/empty-str-to-nil (str)
  ;; @duplicateCode/02a88ee7dd8a3c6ecfcdf7f1fc2f3827
  (when (and str (not (string= str "")))
    str))
;;;
(defun night/messages-get ()
  (with-current-buffer "*Messages*"
    ;; (print! "%s")
    (buffer-substring-no-properties (point-min) (point-max))))
;;;
(defun night/arrN (lst)
  (let ((res "")
        (sep ""))
    (dolist (elem lst res)
      (setq res (concat res sep (format! "%S" elem)))
      (setq sep "
"))
    res))
(comment
 (night/arrN  (list 1 2 "3")))
;;;
(provide 'night-util)
;;; night-util.el ends here
