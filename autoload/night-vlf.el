;;; autoload/night-vlf.el -*- lexical-binding: t; -*-

(require 'vlf-setup)
;;;
(defun night/disable-font-lock ()
  (font-lock-mode -1)
  (setq-local font-lock-keywords nil))

(defun night/so-long ()
  "Disable font-lock and flycheck. See also `night/so-long-strong`."
  (interactive)
  (night/disable-font-lock)
  (night/disable-flycheck)
  ;; (linum-mode -1)
  )

(defun night/so-long-strong ()
  "A wrapper for `so-long`"
  (interactive)
  (so-long)
  (hl-line-mode)
)
;;;
