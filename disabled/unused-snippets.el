;;; ~/doom.d/unused-snippets.el -*- lexical-binding: t; -*-
(defun my-ivy-switch-file-search ()
  "Switch to counsel-file-jump, preserving current input."
  (interactive)
  (let ((input (ivy--input)))
    (ivy-quit-and-run (counsel-file-jump))))

(define-key ivy-minibuffer-map (kbd "M-s r") 'my-ivy-switch-file-search)
;;;
  (setq company-global-modes '(not tuareg-mode org-mode)) ;  python-mode

(spacemacs/set-leader-keys "oi" '(lambda () (interactive "")
                                   (progn  (save-buffer)
                                           (do-applescript "tell application \"IntelliJ IDEA 2018.1\" to activate"))))
(setq-default evil-escape-delay 0.3)
  (fset 'evil-visual-update-x-selection 'ignore)

;;;
(when (and (spacemacs/system-is-mac) (display-graphic-p))
  ;; Disable pixel-by-pixel scrolling, since it's extremely choppy.
  (setq mac-mouse-wheel-smooth-scroll nil))

;; Keyboard smooth scrolling: Prevent the awkward "snap to re-center" when
;; the text cursor moves off-screen. Instead, only scroll the minimum amount
;; necessary to show the new line. (A number of 101+ disables re-centering.)
(setq scroll-conservatively 101)

;; Optimize mouse wheel scrolling for smooth-scrolling trackpad use.
;; Trackpads send a lot more scroll events than regular mouse wheels,
;; so the scroll amount and acceleration must be tuned to smooth it out.
(setq
 ;; If the frame contains multiple windows, scroll the one under the cursor
 ;; instead of the one that currently has keyboard focus.
 mouse-wheel-follow-mouse 't
 ;; Completely disable mouse wheel acceleration to avoid speeding away.
 mouse-wheel-progressive-speed nil
 ;; The most important setting of all! Make each scroll-event move 2 lines at
 ;; a time (instead of 5 at default). Simply hold down shift to move twice as
 ;; fast, or hold down control to move 3x as fast. Perfect for trackpads.
 mouse-wheel-scroll-amount '(2 ((shift) . 4) ((control) . 6)))
;;;
;; (set-face-attribute hl-line-face nil :foreground 'unspecified) ;;
;;;
