;;; night-discourse.el ---                           -*- lexical-binding: t; -*-
;;;
;; (require 'nndiscourse)
(after! (nndiscourse)              ;; @toGarbageCollect/1401/1
  ;; (custom-set-variables '(gnus-select-method
  ;;                         '(nndiscourse "discourse.doomemacs.org" (nndiscourse-scheme "https"))))
  (custom-set-variables '(gnus-select-method
                          '(nndiscourse "discourse.julialang.org" (nndiscourse-scheme "https")))))

;;;
;; (provide 'night-discourse)
;;; night-discourse.el ends here
