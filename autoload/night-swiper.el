;;; autoload/night-swiper.el -*- lexical-binding: t; -*-

(after! (ivy swiper)
  (defun night/advice-with-wrap-disabled (orig-fn &rest args)
    (let ((wrap-orig (not truncate-lines)))
      (night/wrap-soft-disable)
      (unwind-protect
          (apply orig-fn args)
        (if wrap-orig
            (night/wrap-soft-enable)))))

  ;; `swiper' gets way too slow when it tries to jump the source buffer to the middle of a long soft-wrapped line
  (advice-add 'swiper-isearch :around #'night/advice-with-wrap-disabled)
  )
