;;; autoload/night-marginalia.el -*- lexical-binding: t; -*-

(require 'marginalia)

(marginalia-mode)
;; (marginalia-mode -1)

(map! :map (completion-list-mode-map minibuffer-local-map)
 :g
 "M-a" #'marginalia-cycle)

;; `marginalia-annotator-registry'
(dolist (key '(file buffer))
  (comment
   (setq marginalia-annotator-registry
         (assoc-delete-all key marginalia-annotator-registry)))
  (progn
   (let ((entry (assoc key marginalia-annotator-registry)))
     (when entry
       (setcdr entry (cons 'builtin
                           (remq 'builtin (cdr entry))))))))
