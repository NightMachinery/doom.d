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
(map!
 :nvig
 "C-z" #'evil-undo
 :n
 "U" #'evil-redo
 )
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
        :nvoig
        "C-a" #'evil-first-non-blank ;; #'evil-beginning-of-line-or-visual-line
        :nvoig
        "C-e" #'evil-end-of-line-or-visual-line ; @alt: doom/forward-to-last-non-comment-or-eol
        (:prefix "g"
         :n
         "s l" #'link-hint-open-link)
        :nvo
        "g s 9" #'night/avy-goto-opening-paren
        :nvo
        "g s 0" #'night/avy-goto-closing-paren
        :nvo
        "g s s" #'avy-goto-char
        :nvo
        "g s SPC" #'avy-goto-char-2 ; check its default binding if you want to unbind this
;;;
        :nvo
        "0" #'evil-beginning-of-line-or-visual-line
        :nvo
        "$" #'evil-end-of-line-or-visual-line
        :nvo                ; the operator hotkeys are also set in night-evil.el
        "j" #'evil-next-line
        :nvo
        "k" #'evil-previous-line)
  (map! :leader
        "f r" #'night/fzf-recentf
        "t d" #'night/tab-close
        "t D" #'night/tab-close-others)

  (map! :map profiler-report-mode-map
        :nvig "TAB" #'profiler-report-toggle-entry
        :nvig "<tab>" #'profiler-report-toggle-entry
        )
  )
