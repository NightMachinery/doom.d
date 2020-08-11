
;; red: #a12a2a
(defface special-comment '((t (:foreground "#3437eb" :weight bold))) "This is just a doc string")

(defun night/highlight-atsign ()
  (interactive)
  (font-lock-add-keywords
   nil '(("\\(@[^[:space:][:punct:]\n]+\\)" 1 'special-comment t))))
(defun night/generic-hook-fn ()
  (interactive)
  ;; (flyspell-mode-off)
  (night/highlight-atsign))
(add-hook 'after-change-major-mode-hook #'night/generic-hook-fn)
;; jas (@wsw aws)
;;
