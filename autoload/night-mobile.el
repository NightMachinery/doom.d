;;; night-mobile.el -*- lexical-binding: t; -*-

(defvar night/h-mobile-overlays (make-hash-table :test #'eq)
  "Line-number suppression overlays indexed by their window.")

(defun night/h-mobile-reconcile (&optional _window)
  "Synchronize mobile overlays before any window is redisplayed."
  (let ((wanted (make-hash-table :test #'eq)))
    (dolist (frame (frame-list))
      (when (and (not (display-graphic-p frame))
                 (frame-parameter frame 'night/mobile))
        (dolist (window (window-list frame 'no-minibuffer))
          (with-current-buffer (window-buffer window)
            (let ((start (point-min))
                  (end (save-excursion
                         (goto-char (point-max))
                         (min (line-beginning-position)
                              (1- (point-max))))))
              ;; Never cover EOB: Emacs 29's lookup there ignores `window'.
              (when (< start end)
                (puthash window t wanted)
                (let ((overlay (gethash window night/h-mobile-overlays)))
                  (unless (and overlay (overlay-buffer overlay))
                    (setq overlay (make-overlay start end))
                    (overlay-put overlay 'window window)
                    (overlay-put overlay 'display-line-numbers-disable t)
                    (puthash window overlay night/h-mobile-overlays))
                  (unless (and (eq (overlay-buffer overlay) (current-buffer))
                               (= (overlay-start overlay) start)
                               (= (overlay-end overlay) end))
                    (move-overlay overlay start end (current-buffer))))))))))
    (maphash (lambda (window overlay)
               (unless (gethash window wanted)
                 (delete-overlay overlay)
                 (remhash window night/h-mobile-overlays)))
             night/h-mobile-overlays)))

(define-minor-mode night/mobile-line-numbers-mode
  "Hide line numbers except on the final line in marked terminal frames."
  :global t
  (cond
   (night/mobile-line-numbers-mode
    (add-hook 'pre-redisplay-functions #'night/h-mobile-reconcile)
    (night/h-mobile-reconcile))
   (t
    (remove-hook 'pre-redisplay-functions #'night/h-mobile-reconcile)
    (maphash (lambda (_window overlay) (delete-overlay overlay))
             night/h-mobile-overlays)
    (clrhash night/h-mobile-overlays)))
  (force-window-update t))

(defun night/mobile-frame-toggle ()
  "Toggle mobile line-number suppression for the selected terminal frame."
  (interactive)
  (when (display-graphic-p)
    (user-error "Mobile display is only available in terminal frames"))
  (set-frame-parameter nil 'night/mobile
                       (not (frame-parameter nil 'night/mobile)))
  (when night/mobile-line-numbers-mode
    (night/h-mobile-reconcile))
  (force-window-update t))

(night/mobile-line-numbers-mode 1)

(provide 'night-mobile)
