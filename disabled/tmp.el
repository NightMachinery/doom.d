;;; tmp.el -*- lexical-binding: t; -*-
;;;
(signal 'error (list "you're attempting to eval tmp.el; aborted"))
(z bello)
;;;
(setq glyphless-char-display (make-display-table))
;;;
(setq *night/org-babel-remote* :f1)
(setq *night/org-babel-remote* nil)
(setq *night/org-babel-remote* :remote_notebook_server)
;;;
(add-to-list 'tramp-remote-path "/home/ubuntu/miniconda3/bin/")

(setq tramp-default-remote-shell "/bin/my.sh")
(add-to-list 'tramp-remote-process-environment "PYTHONPATH='/home/ubuntu/miniconda3/lib/python3.9/site-packages'")

(dired "/jpy:51.178.215.202#2390:/")
;;;
(format "hi %s" "ji")
(s-join "\n" (list "hi" "jo"))
;;;
(map!
 ;; Makes the arrow keys consistent between the insert and normal state
 ;; Without this, [help:left-char], [help:right-char] would have been used in the insert mode.
 :gnvio
 "<left>" #'evil-backward-char
 :gnvio
 "<right>" #'evil-forward-char
 ;;;
 :g
 "<up>" #'previous-line                 ;; We can't remap these in the global mode, or =ivy= breaks
 :g
 "<down>" #'next-line
 :nvio
 "<up>" #'previous-line ;; #'evil-previous-visual-line or #'previous-line can get buggy on some lines (this happens on LTR lines as well)
 :nvio
 "<down>" #'next-line ;; #'evil-next-visual-line
 )
;;;
(advice-add #'consult--read
            :around
            (lambda (&rest app)
              (let ((completing-read-function #'completing-read-default))
                (apply app))))
;;;
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose
        eldoc-echo-area-use-multiline-p 7)
;;;
;;;
(org-roam-buffer-visit-thing)
