;;; autoload/night-scroll.el -*- lexical-binding: t; -*-

(when (display-graphic-p)
  (require 'iscroll)
  (pixel-scroll-precision-mode)
  (setq scroll-margin 7))
;;;
(setq next-screen-context-lines 4)
;;;
(defun night/screen-center-ni (&rest args)
  ;; somehow the interactive version doesn't work with advice-add
  (cond
   ((and
     (boundp 'org-tree-slide-mode)
     org-tree-slide-mode)
    (let*
        ((org-tree-slide-skip-outline-level
          (or
           night/org-tree-slide-skip-outline-level-for-going-back
           org-tree-slide-skip-outline-level)))
      (when (not (org-at-heading-p))
        (org-tree-slide--outline-previous-heading))
      (night/screen-top)
      (org-tree-slide--display-tree-with-narrow)))
   (t (night/screen-center))))

(defun night/screen-top (&rest args)
  (interactive)
  (ignore-errors
    (call-interactively #'evil-scroll-line-to-top)))

(defun night/screen-bottom (&rest args)
  (interactive)
  (ignore-errors
    (call-interactively #'evil-scroll-line-to-bottom)))

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
  (night/scroll-halfpage-up t))

(defun night/scroll-halfpage-up (&optional down?)
  (interactive)
;;;
  (cl-block nil
    (let ((image-line-p nil) (scroll-bug nil))
      (cond
       ((and (display-graphic-p) (equalp major-mode 'org-mode) org-inline-image-overlays)
        (cond
         (t
          (let ((step (* 3 (night/halfpage-step-get))
                      ;; long images can stil cause lost content, but reducing the step doesn't help much with this
                      ;; if we use =(iscroll-previous-line step)= instead of doing a loop on =(iscroll-previous-line 1)=, we don't seem to lose content, but then we sometimes get stuck
                      ))
            (let ((current-line (line-number-at-pos (point)))
                  ;; https://emacs.stackexchange.com/questions/14920/how-to-determine-the-line-number-of-the-first-visible-line-of-a-window
                  (start-line (line-number-at-pos (window-start)))
                  (last-line (line-number-at-pos (window-end))))
              ;; (message "current-line: %s; start-line: %s; last-line: %s" current-line start-line last-line)
              (cond
               ((or (<= (+ 0 current-line)
                        start-line) (>= (+
                                         (cond
                                          (image-line-p 0)
                                          (t 0))
                                         current-line)
                        last-line))
                ;; (message "scroll bug detected")
                (setq scroll-bug t)
                (cond
                 (down?
                  (recenter -1))
                 (t
                  (recenter 0)))

                ;; (cl-return-from nil)
                )
               ))
            (cond ((not scroll-bug) (cond
                      (down?
                       (iscroll-previous-line step))
                      (t
                       (iscroll-next-line step))))

                  (t (let ((i 0)
                             (case-fold-search t))
                         (while (and (< i step)
                                     ;; (not image-line-p)
                                     )
                           (cond
                            (down?
                             (iscroll-previous-line 1))
                            (t
                             (iscroll-next-line 1)))
                           (when (save-excursion
                                   (beginning-of-line 1)
                                   (search-forward-regexp "\\[\\[?[^][]*\\.png\\]\\]" (line-end-position) t))
                             (setq image-line-p t))
                           (setq i (+ i 1))))))


            ))
         (t
          ;; (scroll-up 4) ;; gets stuck when images are present
;;;
          ;; we can stop at the first line where we see '[[file:sth.png]]', so that the scrolling snaps to images
          (let ((i 0)
                (case-fold-search t))
            (while (and (< i (night/halfpage-step-get))
                        (not image-line-p))
              (cond
               (down? (evil-previous-line 1))
               (t (evil-next-line 1)))
              (when (save-excursion
                      (beginning-of-line 1)
                      (search-forward-regexp "\\[\\[?[^][]*\\.png\\]\\]" (line-end-position) t))
                (setq image-line-p t))
              (setq i (+ i 1))))
;;;
          ;; (evil-scroll-down 0)
          ;; using 4 caused even more buggy behavior when scrolling images
;;;
          (let ((current-line (line-number-at-pos (point)))
                ;; https://emacs.stackexchange.com/questions/14920/how-to-determine-the-line-number-of-the-first-visible-line-of-a-window
                (start-line (line-number-at-pos (window-start)))
                (last-line (line-number-at-pos (window-end))))
            ;; (message "current-line: %s; start-line: %s; last-line: %s" current-line start-line last-line)
            (cond
             ((>= (+
                   (cond
                    (image-line-p 1)
                    (t 0))
                   current-line)
                  last-line)
              (ignore-errors (recenter -1)))
             ((<= (+ 0 current-line)
                  start-line)
              (ignore-errors (recenter 0)))
             (t (night/screen-center))))

          ;; recentering can cause the nasty behavior where the cursor is at just below the modeline and invisible, but not recentering can cause losing unseen content between scrolls
          ;; (ignore-errors (recenter 0)) ;; loses unseen content easily
          )))
       (t (let* ((screen-lines (window-body-height))
                (next-screen-context-lines
                 (min
                  (max 0 (- screen-lines 5))
                  11)))
              (cond
               (down? (scroll-down))
               (t (scroll-up))))
          ;; If the scroll count is zero the command scrolls half the screen.
          )
       (t (cond
           ;; @upstreamBug These evil scroll commands sometimes fail in the visual mode.
           (down? (evil-scroll-up 0))
           (t (evil-scroll-down 0)))
          ;; If the scroll count is zero the command scrolls half the screen.
          )))))

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
(defun night/scroll-precision-up ()
  (interactive)
  (pixel-scroll-precision-scroll-up 150))

(defun night/scroll-precision-down ()
  (interactive)
  (pixel-scroll-precision-scroll-down 150))
;;;
(provide 'night-scroll)
