;;; autoload/night-whitespace.el -*- lexical-binding: t; -*-

(after! ws-butler
  (add-to-list 'ws-butler-global-exempt-modes 'org-mode))
