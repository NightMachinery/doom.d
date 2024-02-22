;;; autoload/night-consult.el -*- lexical-binding: t; -*-

(after! (ivy night-ivy night-orderless)
  (require 'vertico)
  (vertico-mode)
;;;
  (map! :map vertico-map
        :g "M-<down>" #'vertico-scroll-up
        :g "M-<up>" #'vertico-scroll-down
        )
;;;
  (dolist (fn
           '(consult-line
             consult-yank-pop
             consult-yank-from-kill-ring))
    (add-to-list 'ivy-completing-read-handlers-alist
                 `(,fn . completing-read-default)))
;;;
  )
