;;; autoload/night-python.el -*- lexical-binding: t; -*-

(remove-hook 'python-mode-hook #'poetry-tracking-mode)
(remove-hook 'python-mode-hook #'doom-modeline-env-setup-python)
(remove-hook 'python-mode-hook #'doom--enable-+web-django-mode-in-python-mode-h)
;;;
(setq night/python-outline-regexp
      "^#*[[:space:]]*\\*+ ")

(defun night/python-startup ()
  (interactive)
  (outshine-mode)

  (comment
   ;; not needed as of yet
   (make-local-variable 'outline-heading-alist)
   (setq outline-heading-alist
         '(("# *" . 1)
           ("# **" . 2)
           ("# ***" . 3)
           ("# ****" . 4)
           ("# *****" . 5)
           ("# ******" . 6)
           ("# *******" . 7)
           ("# ********" . 8))))
  (comment
   (make-local-variable 'outline-regexp)
   (setq outline-regexp night/python-outline-regexp)
   ;; @conflict [help:+fold/open] doesn't work when outshine-mode is enabled and we have set our custom outline-regexp.
   ))
(add-hook 'python-mode-hook #'night/python-startup)
;;;
