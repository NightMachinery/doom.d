;;; ~/doom.d/autoload/night-mouse.el -*- lexical-binding: t; -*-

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
