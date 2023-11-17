;;; autoload/night-color.el -*- lexical-binding: t; -*-

(defun night/color-name-to-rgb256 (name)
  (let ((rgb (color-name-to-rgb name)))
    (mapcar (lambda (value) (round (* value 255))) rgb)))

(comment
 (night/color-name-to-rgb256 "white")
 (night/color-name-to-rgb256 "black")
 (night/color-name-to-rgb256 "gray")
 (night/color-name-to-rgb256 "purple4"))

(defun night/counsel-colors-rgb (&optional use-floats)
  "Show a list of all W3C web colors for use in CSS.

You can insert or kill the name or hexadecimal RGB value of the
selected color."
  (interactive "P")
  (let* ((colors (counsel-colors--web-alist))
         (counsel--colors-format
          (format "%%-%ds %%s %%s"
                  (apply #'max 0 (mapcar #'string-width colors)))))
    (ivy-read "Web color: " colors
              :require-match t
              :history 'counsel-colors-web-history
              :action (lambda (name)
                        (let* (
                               (rgb
                                (cond
                                 (use-floats (color-name-to-rgb name))
                                 (t (night/color-name-to-rgb256 name))))
                               (text (mapconcat #'number-to-string rgb ", ")))
                          (night/pbcopy text)
                          (message "copied: %s" text)))
              :caller 'counsel-colors-web)))
