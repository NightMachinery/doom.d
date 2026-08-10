;;; autoload/night-marginalia.el -*- lexical-binding: t; -*-

(require 'marginalia)

(marginalia-mode)
;; (marginalia-mode -1)

(map! :map (completion-list-mode-map minibuffer-local-map)
 :g
 "M-a" #'marginalia-cycle)

;; `marginalia-annotators'
;;
;; @warn Use `marginalia-annotators', not `marginalia-annotator-registry'.
;; The latter has been merely an obsolete alias since marginalia 2.0
;; (`define-obsolete-variable-alias' in marginalia.el), and current versions
;; have dropped it, so referring to it fails with
;;   void-variable marginalia-annotator-registry
;; on any host that installed marginalia recently. The new name exists in both
;; old and new versions, so this is safe on every machine.
(dolist (key '(file buffer))
  (comment
   (setq marginalia-annotators
         (assoc-delete-all key marginalia-annotators)))
  (progn
   (let ((entry (assoc key marginalia-annotators)))
     (when entry
       (setcdr entry (cons 'builtin
                           (remq 'builtin (cdr entry))))))))
