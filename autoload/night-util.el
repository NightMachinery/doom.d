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
  (when (and str (not (string= str "")))
    str))
;;;
(defun night/messages-get ()
  (with-current-buffer "*Messages*"
    ;; (print! "%s")
    (buffer-substring-no-properties (point-min) (point-max))))
;;;
(provide 'night-util)
;;; night-util.el ends here
