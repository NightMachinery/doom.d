;;; autoload/night-debugging.el -*- lexical-binding: t; -*-

(defun night/message-and-return (msg x)
  (progn
    (when msg
      (message msg x))
    x))
;;;
(defun night/busy-spin (&optional duration)
  "Busy spin for DURATION seconds."
  (interactive "nDuration (seconds): ")
  (let ((end-time (time-add (current-time) (seconds-to-time (or duration 3)))))
    (while (time-less-p (current-time) end-time)
      ;; Do nothing except consume CPU cycles.
      )))
(comment
 (night/busy-spin 30))
;;;
