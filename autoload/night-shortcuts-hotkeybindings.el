;;;
;; See ~/doom.d/autoload/night-unimpaired.el
;;;
(defun night/tmp-buffer ()
  (interactive)
  (find-file "~/tmp/tmp.txt"))

(night/set-leader-keys "z t" #'night/tmp-buffer)
;;;
(global-set-key (kbd "H-C-M-e") 'insert-char)
(night/set-leader-keys "z s" 'save-some-buffers)
;;;
;; http://ergoemacs.org/emacs/emacs_shrink_whitespace.html
;; (night/set-leader-keys "z w" 'fixup-whitespace)
;; (global-set-key (kbd "M-DEL") 'doom/backward-kill-to-bol-and-indent)
(global-set-key (kbd "M-DEL") 'fixup-whitespace)
;; the mac GUI uses backspace for the del key :|
(map! :nvig                             ; this is necessary as evil binds they key in its insert state
      "M-<backspace>" #'fixup-whitespace)
;; (global-set-key (kbd "M-DEL") 'cycle-spacing) ; cycles between original indent, just one space, and no space.
; Use C-DEL to delete with more control
;;;
(global-set-key (kbd "C-a") 'evil-beginning-of-line)
(global-set-key (kbd "C-e") 'evil-end-of-line)
;;;
(map! :n
      "J" #'counsel-dash-at-point)      ; originally joined the two lines.
(map! :leader
            "f r" #'night/fzf-recentf
            "t d" #'tab-close
            "t D" #'tab-close-other
      )
