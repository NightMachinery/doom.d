;;; autoload/night-company-box.el -*- lexical-binding: t; -*-

(after! company

  (when (and
         ;; nil ;; @outdatedComment company-box suddenly became buggy and caused weird visual effects like flashing to black. Since it used to work well, do try it later again.
         (display-graphic-p)) ;; when GUI
    (require 'company-box)
    (setq company-box-backends-colors
          '(
            (company-capf . (:selected (:background "light steel blue" :foreground "black")))
            (company-dabbrev . (:selected (:background "light steel blue" :foreground "black")))
            (company-dabbrev-code . (:selected (:background "light steel blue" :foreground "black")))
            (company-files . (:selected (:background "light steel blue" :foreground "black")))
            (company-ispell . (:selected (:background "light steel blue" :foreground "black")))
            (company-yasnippet . (:all "lime green" :selected (:background "lime green" :foreground "black"))))))

  ;;; monkey patching company-box to wrap the doc buffers: (https://github.com/sebastiencs/company-box/issues/165)
  (defun company-box-doc--set-frame-position (frame)
    (-let* ((box-position (frame-position (company-box--get-frame)))
            (box-width (frame-pixel-width (company-box--get-frame)))
            (window (frame-root-window frame))
            (frame-resize-pixelwise t)

            ;; ((width . height) (window-text-pixel-size window nil nil 10000 10000))
            ((width . height) (window-text-pixel-size window nil nil (- (frame-outer-width) (+ 20 (+ box-width (nth 0 box-position)))) (- (frame-outer-height) 20))) ;; @monkeyPatched

            (bottom (+ company-box--bottom (window-pixel-top) (frame-border-width)))
            (x (+ (car box-position) box-width (/ (frame-char-width) 2)))
            (y (cdr box-position))
            (y (if (> (+ y height 20) bottom)
                   (- y (- (+ y height) bottom) 20)
                 y))
            (space-right (- (frame-pixel-width) x))
            (space-left (car box-position))
            (x (or (let ((border (* (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0)
                                    2)))
                     (and (> width space-right)
                          (> space-left (+ width border (/ (frame-char-width) 2)))
                          (- (car box-position) width border (/ (frame-char-width) 2))))
                   x)))
      (set-frame-position frame (max x 0) (max y 0))
      (set-frame-size frame width height t))))

(defun company-box-doc--make-buffer (object)
  (let* ((buffer-list-update-hook nil)
         (inhibit-modification-hooks t)
         (string (cond ((stringp object) object)
                       ((bufferp object) (with-current-buffer object (buffer-string))))))
    (when (and string (> (length (string-trim string)) 0))
      (with-current-buffer (company-box--get-buffer "doc")
        (erase-buffer)
        (insert string)
        (setq mode-line-format nil
              display-line-numbers nil
              header-line-format nil
              show-trailing-whitespace nil
              cursor-in-non-selected-windows nil)

        ;;; @monkeyPatched
        ;; (adaptive-wrap-prefix-mode t)
        (toggle-truncate-lines -1)
        ;;;

        (current-buffer)))))
