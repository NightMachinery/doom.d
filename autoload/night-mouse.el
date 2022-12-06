;;; ~/doom.d/autoload/night-mouse.el -*- lexical-binding: t; -*-
;;; mac-port's smooth scrolling is the best one, but it still sucks.

(after! (night-scroll)
  (xterm-mouse-mode 1) ;; doom already does this I think

;;;
  (defun night/horizontal-scroll-enable-locally ()
    (interactive)
    (setq-local mouse-wheel-flip-direction t)
    (setq-local mouse-wheel-tilt-scroll t))
  (defun night/horizontal-scroll-disable-locally ()
    (interactive)
    (setq-local mouse-wheel-flip-direction nil)
    (setq-local mouse-wheel-tilt-scroll nil))
  (defun night/horizontal-scroll-enable ()
   ;; [jalali:1401/09/09/21:18] I disabled the horizontal scrolling, as it made using the trackpad to scroll normally difficult.
    (interactive)
    (setq mouse-wheel-flip-direction t)
    (setq mouse-wheel-tilt-scroll t)
    (comment
     (global-set-key (kbd "<wheel-right>") #'night/scroll-left)
     (global-set-key (kbd "<wheel-left>") #'night/scroll-right)))
  (defun night/horizontal-scroll-disable ()
   ;; [jalali:1401/09/09/21:18] I disabled the horizontal scrolling, as it made using the trackpad to scroll normally difficult.
    (interactive)
    (setq mouse-wheel-flip-direction nil)
    (setq mouse-wheel-tilt-scroll nil)
    (comment
     (global-set-key (kbd "<wheel-right>") nil)
     (global-set-key (kbd "<wheel-left>") nil)))

;;;

  (global-set-key (kbd "<mouse-5>") #'night/scroll-up)
  (global-set-key (kbd "<mouse-4>") #'night/scroll-down)

  (setq
   ;; If the frame contains multiple windows, scroll the one under the cursor
   ;; instead of the one that currently has keyboard focus.
   mouse-wheel-follow-mouse 't
   ;; Completely disable mouse wheel acceleration to avoid speeding away.
   mouse-wheel-progressive-speed nil
   ;; This is an alist mapping the modifier key to the amount to scroll when
   ;; the wheel is moved with the modifier key depressed.
   mouse-wheel-scroll-amount '(4 ((shift) . 1) ((control) . 6)))

  (comment (when (display-graphic-p)
             (setq mac-mouse-wheel-smooth-scroll t))) ; makes scrolling slow

  (comment 
   (setq scroll-step 1
         scroll-margin 0
         scroll-conservatively 100000))
  )
