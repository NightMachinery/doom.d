;;; night-ui.el ---                                  -*- lexical-binding: t; -*-

(defface special-comment '((t (:foreground "#a12a2a"))) "Red")

(font-lock-add-keywords
 nil '(("\\(@\\S*\\)" 1 'special-comment t)))
;; jas (@wsw)
;;
