;;; autoload/night-scroll.el -*- lexical-binding: t; -*-

(defun night/screen-center-ni (&rest args)
  ;; somehow the interactive version doesn't work with advice-add
  (night/screen-center))

(defun night/screen-center (&rest args)
  (interactive)
  (ignore-errors (recenter nil)))

(defun night/scroll-halfpage-down ()
  (interactive)
  ;; (scroll-down 4)
  (call-interactively #'evil-scroll-up)

  (night/screen-center))

(defun night/scroll-halfpage-up ()
  (interactive)
  ;; (scroll-up 4)
  (call-interactively #'evil-scroll-down)

  (night/screen-center))

(defun night/scroll-down ()
  (interactive)
  (scroll-down 4))

(defun night/scroll-up ()
  (interactive)
  (scroll-up 4))

(defun night/scroll-right ()
  (interactive)
  (scroll-right 4))

(defun night/scroll-left ()
  (interactive)
  (scroll-left 4))

;;;
(provide 'night-scroll)
