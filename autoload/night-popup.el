;;; night-popup.el ---                               -*- lexical-binding: t; -*-
;; @warn Beware =:ttl= killing process buffers!
;;;
(set-popup-rule! "^\\*Flymake.*\\*$" :height 0.5 :ttl nil)

(set-popup-rule! "^\\*eldoc.*\\*$" :ttl 60 :width 0.3 :side 'right)
;; (set-popup-rule! "^\\*eldoc.*\\*$" :ttl 60 :height 0.3)

(set-popup-rule! "^\\*Help.*\\*$"  :height 0.5 :select t)

(set-popup-rule! "^\\*brishz.*\\*$" :ignore nil :height 0.5 :ttl nil)

;; doesn't work, @idk why
;; (set-popup-rule! "^\\*RE.Builder.*\\*$" :ignore nil :height 0.3)

(set-popup-rule! "^.*poporg.*$" :ignore nil :height 0.5)

(set-popup-rule! "^\\*vc-diff*\\*$" :ignore nil :height 0.5 :ttl nil)

(set-popup-rule! "^\\*night-html-viewer\\*$" :ignore nil :height 0.7)

(after! (jupyter)
  (set-popup-rule! "^\\*jupyter.*\\*$" :ignore nil :height 0.5 :select nil :ttl nil))
(after! (racket-mode)
  (set-popup-rule! "^\\*Racket REPL.*\\*$" :ignore nil :height 0.5 :select nil :ttl nil))
(after! (eglot)
  (set-popup-rule! "^\\*eglot.*\\*$" :ignore nil :height 0.5 :select t :ttl nil))
(after! (julia-repl)
  (set-popup-rule! "^\\*julia.*\\*$" :ignore nil :height 0.5 :ttl nil)
  )
(after! (lispy)
  (set-popup-rule! "^\\*lispy.*\\*$" :ignore nil :height 0.5 :select t :ttl nil))
(after! (cider)
  (set-popup-rule! "^\\*cider.*\\*$" :ignore nil :height 0.5 :select t :ttl nil))
(after! (sly)
  (set-popup-rule! "^\\*sly.*repl.*\\*$" :ignore nil :height 0.6 :select t :ttl nil)
  (set-popup-rule! "^\\*sly-desc.*\\*$" :ignore nil :height 0.6)
  (set-popup-rule! "^\\*sly-apropos.*\\*$" :ignore nil :height 0.6 :select t))
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
