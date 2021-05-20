;;; night-eww.el ---                                 -*- lexical-binding: t; -*-

;;;
(defun night/eww-startup ()
  (interactive)
;;;
  ;; there is a raceCondition here, and this code works randomly:
  ;; (dolist (ix '(1 2 3))
  ;;   (call-interactively #'hlt-toggle-link-highlighting))
;;;
  (let
      ((cb (current-buffer)))
    (run-with-timer 0.5 nil #'(lambda ()
                                (with-current-buffer cb (dolist (ix '(1 2))
                                                          ;; (z fsay (i ix))
                                                          (call-interactively #'hlt-toggle-link-highlighting))))))
  )
(add-hook 'eww-after-render-hook #'night/eww-startup)
;;;
(map! :map eww-mode-map
      :n "o" #'link-hint-open-link)
