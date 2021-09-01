;;; autoload/night-scroll.el -*- lexical-binding: t; -*-

(defun night/screen-center-ni (&rest args)
  ;; somehow the interactive version doesn't work with advice-add
  (night/screen-center))

(defun night/screen-center (&rest args)
  (interactive)
  (ignore-errors (recenter nil)))

(defun night/halfpage-step-get ()
  5
  ;; setting this too high can cause scrolls to lose unseen content, as images with big heights can snap to the top of the screen and thus hide the lines above them.
  ;; Unfortunately, even with low numbers, this bug might still occur.
  )

(defun night/scroll-halfpage-down ()
  (interactive)
  (cond
   ((and (display-graphic-p) (equalp major-mode 'org-mode) org-inline-image-overlays)
    ;; (scroll-down 4)
;;;
    (let ((i 0)
          (case-fold-search t))
      (while (and (< i (night/halfpage-step-get))
                  (or (= i 0) (not (save-excursion
                                     (beginning-of-line 1)
                                     (search-forward-regexp "\\[\\[?[^][]*\\.png\\]\\]" (line-end-position) t)))))
        (evil-previous-line 1)
        (setq i (+ i 1))))
;;;
    ;; (evil-previous-line 4)
;;;
    ;; (evil-scroll-up 0)
;;;
    (let ((current-line (line-number-at-pos (point)))
          (start-line (line-number-at-pos (window-start)))
          (last-line (line-number-at-pos (window-end))))
      (message "current-line: %s; start-line: %s; last-line: %s" current-line start-line last-line)
      (cond
       ((>= (+ 1 current-line)
            last-line)
        (ignore-errors (recenter -1)))
       ((<= (+ 0 current-line)
            start-line)
        (ignore-errors (recenter 0)))
       (t (night/screen-center))))

    ;; (ignore-errors (recenter 0))
    )
   (t (evil-scroll-up 0
                      ;; If the scroll count is zero the command scrolls half the screen.
                      )
      (night/screen-center))))

(defun night/scroll-halfpage-up ()
  (interactive)
;;;
  (cond
   ((and (display-graphic-p) (equalp major-mode 'org-mode) org-inline-image-overlays)
    ;; (scroll-up 4) ;; gets stuck when images are present
;;;
    ;; we can stop at the first line where we see '[[file:sth.png]]', so that the scrolling snaps to images
    (let ((i 0)
          (case-fold-search t))
      (while (and (< i (night/halfpage-step-get))
                  (or (= i 0) (not (save-excursion
                                     (beginning-of-line 1)
                                     (search-forward-regexp "\\[\\[?[^][]*\\.png\\]\\]" (line-end-position) t)))))
        (evil-next-line 1)
        (setq i (+ i 1))))
;;;
    ;; (evil-scroll-down 0)
    ;; using 4 caused even more buggy behavior when scrolling images
;;;
    (let ((current-line (line-number-at-pos (point)))
          (start-line (line-number-at-pos (window-start)))
          (last-line (line-number-at-pos (window-end))))
      (message "current-line: %s; start-line: %s; last-line: %s" current-line start-line last-line)
      (cond
       ((>= (+ 1 current-line)
            last-line)
        (ignore-errors (recenter -1)))
       ((<= (+ 0 current-line)
            start-line)
        (ignore-errors (recenter 0)))
       (t (night/screen-center))))
    ;; recentering can cause the nasty behavior where the cursor is at just below the modeline and invisible, but not recentering can cause losing unseen content between scrolls

    ;; (ignore-errors (recenter 0)) ;; loses unseen content easily
    )
   (t (evil-scroll-down 0
                        ;; If the scroll count is zero the command scrolls half the screen.
                        )
      (night/screen-center))))

(defun night/scroll-down ()
  (interactive)
  (scroll-down 4))

(defun night/scroll-up ()
  (interactive)
  (scroll-up 4))

(defun night/scroll-right ()
  (interactive)
  (scroll-right 4))

(defun night/scroll-left ()
  (interactive)
  (scroll-left 4))

;;;
(provide 'night-scroll)
