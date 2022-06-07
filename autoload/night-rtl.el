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
 "<right>" #'evil-forward-char
;;;
;;;
 ;; This bug also happens with =evil-previous-line=, so I am reverting these. But it might be that different contexts are buggy depending on which command is used. Update: I think the bug happens with logical movement in both commands. (Visual movement with =next-line= works fine, it seems.)
 ;; This bug does not exist in =emacs -q= with =(setq bidi-paragraph-direction 'nil)=.
 ;; Test it with: [[id:4d81b59d-5b5c-4a69-9791-92d6c71e78d2][TA/gen:Logic circuits]]

 ;; :g
 ;; ;; :nviog
 ;; "<up>" #'previous-line ;; We can't remap these in the global mode, or =ivy= breaks
 ;; :g
 ;; ;; :nviog
 ;; "<down>" #'next-line

 ;; :nvio
 ;; "<up>" #'evil-previous-line ;; #'evil-previous-visual-line or #'previous-line can get buggy on some lines (this happens on LTR lines as well)
 ;; :nvio
 ;; "<down>" #'evil-next-line ;; #'evil-next-visual-line
;;;
 )
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
