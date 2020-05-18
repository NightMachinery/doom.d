;;; ~/doom.d/night-gui.el -*- lexical-binding: t; -*-

(setq frame-resize-pixelwise t)
(setq frame-alpha-lower-limit 0)
(defun night/invisible-til-keypress  (&optional frame)
  (interactive)
  (let* ((invisible-alpha 0) (current-alpha (or (car (frame-parameter frame 'alpha))
                                                100)))
    (set-frame-parameter frame
                         'alpha
                         (cons invisible-alpha invisible-alpha))
    (read-event)
    (set-frame-parameter frame
                         'alpha
                         (cons current-alpha current-alpha))))

;;;
(night/set-leader-keys "z i" #'night/invisible-til-keypress)
