;;; night-minor-modes.el ---                         -*- lexical-binding: t; -*-
;;;
(defun night/minor-modes-disable-all ()
  (interactive)
  (mapc
   (lambda (mode-symbol)
     (when (functionp mode-symbol)
       ;; some symbols are functions which aren't normal mode functions
       (ignore-errors
         (funcall mode-symbol -1))))
   minor-mode-list))
;;;
(provide 'night-minor-modes)
;;; night-minor-modes.el ends here
