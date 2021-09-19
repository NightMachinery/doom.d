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
;;;
(provide 'night-util)
;;; night-util.el ends here
