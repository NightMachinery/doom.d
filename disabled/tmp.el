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
(defun night/bell-link ()
  ;; (night/brishz "awaysh" "tts-glados1-cached" "link, inserted")
  )
;;;
(map!
 :ivong
 [tab] nil)
;;;
;;;
(progn
  (setq hi 1)
  :hey)

(setq org-html--id-attr-prefix "JOKE_")
;;;
(setq org-link-frame-setup
 '((vm . vm-visit-folder-other-frame)
   (vm-imap . vm-visit-imap-folder-other-frame)
   (gnus . org-gnus-no-new-news)
   ;; (file . find-file-other-window)
   (file . find-file)
   (wl . wl-other-frame)
   ;; (id . org-id-open)
   ))
;;
(markerp (point))
;;
(z arger "hi a" h j)
(defun insert-my-name () (insert "insert-my-name"))

(setq
 scroll-conservatively 101
 maximum-scroll-margin 0.25
 scroll-margin 0
 scroll-step 0)
(setq  scroll-margin 7)
;;;
(setq-local scroll-preserve-screen-position 'always)
;;
(map! :map pdf-view-mode-map
      :localleader "m"
      (lambda ()
        "Saves the current position on the pdf to jump to later with <C-f2>."
        (interactive)
        (setf my-bookmark (pdf-view-bookmark-make-record))))

(map! :map pdf-view-mode-map
      :localleader "j"
      (lambda ()
        "Loads the position saved by <C-f1>."
        (interactive)
        (pdf-view-bookmark-jump my-bookmark)))
;;;
(map! :i "M-<backspace>" #'night/kill-whitespace-or-word-backward)
;;;
(night/pbcopy "hij")
(night/search-dir :dir (getenv "DOOMDIR") :query "" :args "--glob * ")
;;;
(fboundp 'nil)
