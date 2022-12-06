;;; autoload/night-navigation-jump.el -*- lexical-binding: t; -*-

(after! better-jumper
  (cl-defun night/jump-set (&key pos buffer)
    (let
        ((jump-set #'better-jumper-set-jump))
        (cond
         (buffer
          (with-current-buffer buffer
            (funcall jump-set pos)))
         (t (funcall jump-set pos))))))
