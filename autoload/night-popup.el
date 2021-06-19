;;; night-popup.el ---                               -*- lexical-binding: t; -*-


(set-popup-rule! "^\\*sly-desc.*\\*$" :ignore nil :height 0.6)

(set-popup-rule! "^\\*lispy.*\\*$" :ignore nil :height 0.5)

(set-popup-rule! "^\\*brishz.*\\*$" :ignore nil :height 0.5)

(set-popup-rule! "^.*poporg.*$" :ignore nil :height 0.5)

(set-popup-rule! "^\\*vc-diff*\\*$" :ignore nil :height 0.5)

(set-popup-rule! "^\\*night-html-viewer\\*$" :ignore nil :height 0.7)
;;;
(setq night/fullscreen-popups nil)
(defun night/fullscreen-popups-disable ()
  (interactive)
  (set-popup-rule! "^\\*eww.*\\*$" :ignore nil :height 0.4)
  (setq night/fullscreen-popups nil))

(defun night/fullscreen-popups-enable ()
  (interactive)
  (set-popup-rule! "^\\*eww.*\\*$" :ignore t)
  ;; the *eww* buffer is still easily quitable with =q=

  (setq night/fullscreen-popups t))
;;;
