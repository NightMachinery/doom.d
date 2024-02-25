;;; autoload/night-embark.el -*- lexical-binding: t; -*-

(require 'embark)
(after! (ivy embark)
  (define-key ivy-minibuffer-map (kbd "<deletechar>") #'embark-act)
  (map!
   ;; :map ivy-minibuffer-map
   "M-z" #'embark-act
   "<f12>" #'embark-act
   ))
