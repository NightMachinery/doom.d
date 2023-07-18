;;; autoload/night-debugging.el -*- lexical-binding: t; -*-

(defun night/message-and-return (msg x)
  (progn
    (when msg
      (message msg x))
    x))
