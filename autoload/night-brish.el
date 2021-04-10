;;; autoload/night-brish.el -*- lexical-binding: t; -*-

(defun night/brishz (&rest args)
  (interactive
   (let ((cmd (read-string "Command: " nil 'night-brishz-history)))
     (list "eval" cmd)
     )
   )
  (apply #'call-process "brishzq.zsh" nil nil nil args)
  )
