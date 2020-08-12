
;; red: #a12a2a
(defface special-comment '((t (:foreground "#3437eb" :weight bold))) "This is just a doc string")

(defun night/highlight-atsign ()
  (interactive)
  (font-lock-add-keywords
   nil '(("\\B\\(@[^[:space:][:punct:]\n]+\\)" 1 'special-comment t))))
;;; tests
;; jas (@wsw aws) hi@gmail.com
;;a@a
;;@hi
;;;
(defun night/generic-hook-fn ()
  (interactive)
  ;; (flyspell-mode-off)
  (night/highlight-atsign)  ; @bug This hook might get called multiple times on a single buffer, and that can cause pollution.
  )
(add-hook 'after-change-major-mode-hook #'night/generic-hook-fn)
