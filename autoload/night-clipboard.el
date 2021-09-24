;;; ~/doom.d/night-clipboard.el -*- lexical-binding: t; -*-
;;;
(defun night/h-kill-skip-whitespace (orig-fn string &optional rest)
  "an advice around `kill-new' to skip whitespace-only kills. @warn This can break some assumptions."
  (let* (
         (string-raw (substring-no-properties string))
         (space-p
          (not (string-match-p "[^ \t\n\r]" string-raw))
          ;; (or (equalp string-raw "")
          ;;     (string-match-p "^\\(\s\\|\n\\)+$" string-raw) ;; '^', '$' are treated per line, so this won't work
          ;;     )
          ))
    ;; (message "space-p: %s, isSpace: %s, string-raw: %s, string-cat: %s" space-p (zb isSpace (i string-raw)) string-raw (z reval-withstdin (i string-raw) cat -vte))
    (cond
     ((or
       ;; t ;; disable this modification entirely
       (not space-p))
      (apply orig-fn string rest))
     (t
      ;; (message "skipped whitespace kill: %s" string-raw)
      ;; (message "skipped whitespace kill")

      ;; imitating the return value of `kill-new'
      0))))
(advice-add 'kill-new :around #'night/h-kill-skip-whitespace)
;;;
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
;;;
(defun night/pbadd-current ()
  (interactive)
  (z pbadd (buffer-file-name)))
;;;
