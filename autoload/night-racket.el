;;; autoload/night-racket.el -*- lexical-binding: t; -*-

(after! (racket-mode)
  (comment
   (defun night/racket-startup ()
     )
   (add-hook 'racket-mode-hook #'night/racket-startup))
;;;
  (comment
   (defun night/racket-send-buffer (&optional buffer)
     (interactive)
     (with-current-buffer buffer
       (racket-send-region (point-min) (point-max)))))

(defun night/racket-send-buffer ()
  (interactive)
  (racket--send-region-to-repl
   (point-min) (point-max)))
;;;
  (map! :map racket-mode-map
        :localleader
        "ee" #'racket-eval-last-sexp

        "sb" #'night/racket-send-buffer
        )
;;;
  )
