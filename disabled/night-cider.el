;;; ~/doom.d/night-cider.el -*- lexical-binding: t; -*-


(eval-after-load 'cider
  '(progn
     (add-hook 'cider--debug-mode-hook (lambda ()
                                         (if (bound-and-true-p cider--debug-mode)
                                             (turn-off-evil-snipe-mode)
                                           (turn-on-evil-snipe-mode))))
  (add-hook 'cider-connections-buffer-mode-hook (lambda ()
                                                  (evil-insert-state 1)))

     (defun my-cider-eval-paragraph ()
       (interactive)
       (save-excursion
         (mark-paragraph)
         (command-execute 'cider-eval-region)))
     (defun my-cider-eval-line ()
       (interactive)
       (save-mark-and-excursion
         (my-select-current-line)
         (command-execute 'cider-eval-region)))
     (cider-add-to-alist 'cider-jack-in-dependencies
                         "org.tcrawley/dynapath" "0.2.5")
     (cider-add-to-alist 'cider-jack-in-dependencies
                         "com.cemerick/pomegranate" "0.4.0"))
  (evil-define-key 'normal clojure-mode-map (kbd "\\ e l") #'my-cider-eval-line)
  )
