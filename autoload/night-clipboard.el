;;; ~/doom.d/night-clipboard.el -*- lexical-binding: t; -*-

(defun ns-yank-image-at-point-as-image ()
  "Yank the image at point to the X11 clipboard as image/png."
  (interactive)
  (let ((image (get-text-property (point) 'display)))
    (if (eq (car image) 'image)
        (let ((data (plist-get (cdr image) ':data))
              (file (plist-get (cdr image) ':file)))
          (cond (data
                 (with-temp-buffer
                   (insert data)
                   (call-shell-region
                    (point-min) (point-max)
                    "impbcopy"))) ;; http://www.alecjacobson.com/weblog/?p=3816 Linux x11: https://emacs.stackexchange.com/questions/41016/how-can-i-yank-images-from-emacs?noredirect=1#comment64407_41016
                (file
                 (if (file-exists-p file)
                     (start-process
                      "_" nil "impbcopy"  (file-truename file))))
                (t
                 (message "The image seems to be malformed."))))
      (message "Point is not at an image."))))


;;;
(defun night/paste-yank-html ()
  (interactive)
  (insert-for-yank (z html2org)))
;;;
(after! (org evil-org) (map!
                        :map evil-org-mode-map
                        :nv "P" #'night/paste-yank-html))       ;; @futureCron @tradeoff Is this upgrade worth the slowdown in 'P'? @update I limited it to only org, where it should be worth it.
(night/set-leader-keys "y y" #'night/paste-yank-html)

(night/set-leader-keys "z c i" #'ns-yank-image-at-point-as-image)
