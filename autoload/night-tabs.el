;;; autoload/night-tabs.el -*- lexical-binding: t; -*-

(defun night/tab-close ()
  (interactive)
  (tab-close)
  (when (= (length (tab-bar-tabs)) 1)
    (tab-bar-mode -1)))

(defun night/tab-close-others ()
  (interactive)
  (tab-close-other)
  (tab-bar-mode -1))
;;;
(setq display-buffer-base-action '(nil))
;; (setq display-buffer-base-action '(display-buffer-in-tab)) ; https://emacs.stackexchange.com/questions/61677/make-display-buffer-open-buffer-in-new-tab
;; seems useless?
;;;
