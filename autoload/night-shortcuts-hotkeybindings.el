;;;
(defun night/tmp-buffer ()
  (interactive)
  (find-file "~/tmp/tmp.txt"))

(night/set-leader-keys " z t" #'night/tmp-buffer)
;;;
(global-set-key (kbd "H-C-M-e") 'insert-char)
(night/set-leader-keys "z s" 'save-some-buffers)
(night/set-leader-keys "z w" 'fixup-whitespace)
(global-set-key (kbd "M-DEL") 'doom/backward-kill-to-bol-and-indent)
;;;
(global-set-key (kbd "C-a") 'evil-beginning-of-line)
(global-set-key (kbd "C-e") 'evil-end-of-line)
;;; 
