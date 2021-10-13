;;; autoload/night-embark.el -*- lexical-binding: t; -*-

(require 'embark)
(after! (ivy embark)
  (define-key ivy-minibuffer-map (kbd "<deletechar>") 'embark-act))
