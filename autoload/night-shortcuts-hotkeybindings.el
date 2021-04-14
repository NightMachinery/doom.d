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
(after! evil
  (evil-define-motion evil-beginning-of-line-or-visual-line (count)
    "Move the cursor to the first character of the current screen
line if `visual-line-mode' is active and
`evil-respect-visual-line-mode' is non-nil.  If COUNT is given,
move COUNT - 1 screen lines forward first."
    :type inclusive
    (if (and (fboundp 'beginning-of-visual-line)
             evil-respect-visual-line-mode
             visual-line-mode)
        (beginning-of-visual-line count)
      (evil-beginning-of-line)))

  ;; @bug https://github.com/emacs-evil/evil/pull/1440
  ;; when that bug gets resolved, we can bind these to evil-end-of-line directly if we so wish
  ;; note that =g 0=, =g j, etc already act on physical lines
  (global-set-key (kbd "C-a") #'evil-first-non-blank)
  (global-set-key (kbd "C-e") #'evil-end-of-line-or-visual-line)
  ;; We might need to set these via map! :ng as well
;;;
  (map! :n
        "J" #'counsel-dash-at-point     ; originally joined the two lines.
        :nvig
        "C-s" #'save-buffer
        :nvig
        "C-a" #'evil-first-non-blank ;; #'evil-beginning-of-line-or-visual-line
        :nvig
        "C-e" #'evil-end-of-line-or-visual-line ; @alt: doom/forward-to-last-non-comment-or-eol
        (:prefix "g"
         :n
         "s l" #'link-hint-open-link)
        :n
        "g s 9" #'night/avy-goto-opening-paren
        :n
        "g s 0" #'night/avy-goto-closing-paren
        :n
        "g s s" #'avy-goto-char
        :n
        "g s SPC" #'avy-goto-char-2 ; check its default binding if you want to unbind this
;;;
        :nv
        "0" #'evil-beginning-of-line-or-visual-line
        :nv
        "$" #'evil-end-of-line-or-visual-line
        :nv
        "j" #'evil-next-line
        :nv
        "k" #'evil-previous-line)
  (map! :leader
        "f r" #'night/fzf-recentf
        "t d" #'tab-close
        "t D" #'night/close-other-tabs)
  )
