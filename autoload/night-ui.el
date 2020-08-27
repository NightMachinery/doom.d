(when (display-graphic-p)
  (doom-adjust-font-size 2)
  )

;; red: #a12a2a
(defface special-comment '((t (:foreground "#3437eb" :weight bold))) "This is just a doc string")

(defun night/highlight-atsign ()
  (interactive)
  (font-lock-add-keywords
   ;; [:punct:]
   nil '(("\\B\\(@[^][[:space:](){};,\n]+\\)" 1 'special-comment t))))
;;; tests
;; jas (@wsw aws) @hi+ maddah_ali@sharif.edu hi@gmail.com
;;a@a
;;@hi
;;;
