;;; autoload/night-color.el -*- lexical-binding: t; -*-

(defun night/color-name-to-rgb256 (name)
  (let ((rgb (color-name-to-rgb name)))
    (mapcar (lambda (value) (round (* value 255))) rgb)))

(comment
 (night/color-name-to-rgb256 "white")
 (night/color-name-to-rgb256 "black")
 (night/color-name-to-rgb256 "gray")
 (night/color-name-to-rgb256 "purple4"))
