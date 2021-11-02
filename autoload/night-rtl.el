;;; autoload/night-rtl.el -*- lexical-binding: t; -*-

(setq night/persian-font "B Nazanin") ;; "IranNastaliq"
;; (setq night/persian-font "Courier New")

(setq default-input-method "farsi-isiri-9147")

;;;
(setq visual-order-cursor-movement nil) ;; @redundant
(map!
 ;; Makes the arrow keys consistent between the insert and normal state
 ;; Without this, [help:left-char], [help:right-char] would have been used in the insert mode.
 :gnvio
 "<left>" #'evil-backward-char
 :gnvio
 "<right>" #'evil-forward-char)
;;;

(defun night/enable-bidirectional ()
  (interactive)


  (when (fboundp 'set-fontset-font)
    ;;  this function isn't defined in builds of Emacs that aren't compiled with GUI support.
    (set-fontset-font
     "fontset-default"
     ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Modifying-Fontsets.html
     ;; https://unicodemap.org/search.asp?search=%D9%84%DA%AF%DA%86%D9%BE%DA%98
     ;; https://en.wikipedia.org/wiki/Persian_alphabet
     ;; `describe-char`
     ;; (cons (decode-char 'ucs #x0600) (decode-char 'ucs #
     ;; arabic (includes Persian) range
     'arabic
     night/persian-font
     ))
  (setq face-font-rescale-alist `((,night/persian-font . 1.50)))
  (setq bidi-paragraph-direction 'nil))

(add-hook 'org-mode-hook 'night/enable-bidirectional)
(add-hook 'markdown-mode-hook 'night/enable-bidirectional)
