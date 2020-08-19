;;; ~/doom.d/autoload/night-mouse.el -*- lexical-binding: t; -*-
;;; mac-port's smooth scrolling is the best one, but it still sucks.

(progn
  (xterm-mouse-mode 1) ;; doom already does this I think
  (global-set-key (kbd "<wheel-right>") '(lambda ()
                                           (interactive)
                                           (scroll-left 4)))
  (global-set-key (kbd "<wheel-left>") '(lambda ()
                                          (interactive)
                                          (scroll-right 4)))

  (global-set-key (kbd "<mouse-5>") '(lambda ()
                                       (interactive)
                                       (scroll-up 4)))
  (global-set-key (kbd "<mouse-4>") '(lambda ()
                                       (interactive)
                                       (scroll-down 4)))

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
