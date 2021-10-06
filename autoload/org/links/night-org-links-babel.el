;;; night-org-links-babel.el ---                     -*- lexical-binding: t; -*-
;;;
;; https://emacs.stackexchange.com/questions/50985/can-org-mode-link-to-a-babel-block?rq=1
;;
;; [[id:03ca90e6-a445-4487-9e19-ef527bc37763][orgmode/links:=babel-run=]]
(after! (org ol)
(org-link-set-parameters
 "babel-run"
 :follow #'org-babel-ref-resolve))
;;;
