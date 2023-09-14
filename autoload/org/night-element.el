;;; autoload/org/night-element.el -*- lexical-binding: t; -*-

(defun night/org-value-at-point ()
  (interactive)
  (let ((value (org-element-property :value (org-element-at-point))))
    (when (and (called-interactively-p 'interactive) value)
      (kill-new value))
    value))
