;;;
;; See ~/doom.d/autoload/night-unimpaired.el
;;;
;; * [[https://www.masteringemacs.org/article/mastering-key-bindings-emacs][Keymap Lookup Order]]
;; Emacs will look for keys in a certain order, and that order I have described below. Keep in mind that only /active/ keymaps are used, and that the order is top-to-bottom; the first “match” is used, subject to criteria that we don't care about.

;; 1. =overriding-terminal-local-map= for terminal-specific key binds.
;; 2. =overriding-local-map= for keys that should override all other local keymaps. Be VERY careful if you use this! It replaces all keymaps.
;; 3. /Keymap char property at point/ for keymaps that are local to the character point is at. This is used for stuff like fields in yasnippet and the customize dialog.
;; 4. =emulation-mode-map-alists= for advanced multi-mode keymap management
;; 5. =minor-mode-overriding-map-alist= for overriding the keymaps used by minor modes in major modes.
;; 6. =minor-mode-map-alist= is exactly like the /overriding/ version above, but the preferred means of specifying the keymaps for minor modes.
;; 7. /Keymap text property at point/ is like the one above for char properties but is for text properties only.
;; 8. =current-local-map= for keymaps defined in the buffers' current local map
;; 9. =current-global-map= is the last place Emacs will look for key binds and it is for the global ones.

;; All you care about is that /minor mode/ keys come before /local/ keys, and they in turn are checked before /global/ keys.
;;
;; See also =doom-lookup-key=
;; ------
(after! (general scrollback-mode)
  (setq night-priority-map-alist
        (cons t scrollback-mode-map)
        )
  ;; (setq emulation-mode-map-alists
  ;;     (cons 'general-maps-alist
  ;;           (remq 'general-maps-alist emulation-mode-map-alists)))
  (add-to-list 'emulation-mode-map-alists 'night-priority-map-alist) ;; @broken still gets overridden by, e.g., evil-snipe
  )
;;;
(defun night/helpful-key (key-sequence)
  (interactive
   (list (read-key-sequence "Press key: ")))
  (let ((sym (key-binding key-sequence)))
    (cond
     ((keymapp sym)
      (describe-keymap sym))
     (t
      (funcall #'helpful-key key-sequence)))))
;;;
(defun night/bookmarks-open-tmp ()
  (interactive)
  (find-file "~/tmp/tmp.org"))

(defun night/bookmarks-open-chatgpt ()
  (interactive)
  (find-file (concat
              (getenv "nightNotesPrivate")
              "/subjects/ML/chatlog/chatgpt/megalog/gen.org")))

(defun night/bookmarks-open-latex-ocr ()
  (interactive)
  (find-file (concat
              (getenv "nightNotesPrivate")
              "/subjects/ML/chatlog/chatgpt/GPT4V/LaTeX OCR/1.org")))

(night/set-leader-keys "z t" #'night/bookmarks-open-tmp)
(night/set-leader-keys "z z" #'night/bookmarks-open-chatgpt)
(night/set-leader-keys "z l" #'night/bookmarks-open-latex-ocr)
;;;
(cl-defun night/save-buffer
    (&key (force-normal-state t))
  (interactive)
  (save-buffer)
  (when force-normal-state
    (evil-normal-state)))
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
  (unbind-key "C-<wheel-down>" global-map) ;; bound to various functions of [help:text-scale-adjust]
  (unbind-key "C-<wheel-up>" global-map)
;;;
  (map! :n
        "J" #'counsel-dash-at-point     ; originally joined the two lines.
        :nvig
        "C-s" #'night/save-buffer
        :nv "P" #'night/paste-yank-html
        ;; :nv "P" #'evil-paste-before
        :nv "p" #'evil-paste-before
        :nvig
        "s-," #'night/pns
        :nvoig
        "<next>" #'evil-avy-goto-char
        :nvoig
        "S-<up>" #'night/scroll-halfpage-down ;; scroll commands use inverted terminology
        :nvoig
        "S-<down>" #'night/scroll-halfpage-up
        :nvoig
        "s-<up>" #'night/scroll-precision-up ;; scroll commands use inverted terminology
        :nvoig
        "s-<down>" #'night/scroll-precision-down
        :nvoig
        "C-a" #'evil-first-non-blank ;; #'evil-beginning-of-line-or-visual-line
        :nvoig
        "C-e" #'evil-end-of-line-or-visual-line ; @alt: doom/forward-to-last-non-comment-or-eol
        (:prefix "g"
         :n
         "s l" #'link-hint-open-link
         :n
         "l" #'org-open-at-point-global)
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

        :nvo
        "j" #'evil-next-line
        :nvio
        "<down>" #'evil-next-line
        ;; We can't remap these in the global mode, or =ivy= breaks

        :nvio
        "<up>" #'evil-previous-line
        :nvo
        "k" #'evil-previous-line

        :nvo
        "] \\" #'night/repeat-command
        :nvo
        "] x" #'er/expand-region
        )
  (setq expand-region-contract-fast-key "z")
  (setq expand-region-reset-fast-key "c")

  (map! :leader
        "b D" #'night/force-kill-current-buffer
        "b R" #'night/buffer-reopen
        ;; "f r" #'night/fzf-recentf ;; somewhat slow
        "f r" #'counsel-recentf
        ;; "f r" #'helm-recentf ;; loses keys while it is not still open, and isn't fuzzy

        "y o" #'poporg-dwim

        "x" nil ;;  @doom unbinds doom/open-scratch-buffer
        "x s" #'doom/open-scratch-buffer
        "x d" #'fixup-whitespace

        "t d" #'night/tab-close
        "t D" #'night/tab-close-others

        "s h h" #'hlt-highlight-regexp-region
        "s h n" #'hlt-next-highlight
        "s h N" #'hlt-previous-highlight
        "s h p" #'hlt-previous-highlight
        "s h f" #'night/hlt-counsel-face

        "s d" ;; overrides @doom's default search pwd function
        #'night/search-dir
        ;; #'night/search-dir-limited
        "s D" #'night/search-doom
        "s a" #'night/agsi

        "s m" #'counsel-evil-marks

        "n L" #'org-insert-link-global
        )

  (map! :map profiler-report-mode-map
        :nvig "TAB" #'profiler-report-toggle-entry
        :nvig "<tab>" #'profiler-report-toggle-entry
        )

  (map! :map prog-mode-map
        :g "M-p" #'flymake-goto-prev-error
        ;; @seeAlso `+default/diagnostics', `flymake-show-buffer-diagnostics'
        ;; @seeAlso `previous-error'
        :g "M-n" #'flymake-goto-next-error
        )
  (map!
   :g
   "C-S-<up>" #'scroll-other-window-down
   ;; "C-M-<up>" #'scroll-other-window-down
   :g
   "C-S-<down>" #'scroll-other-window
   ;; "C-M-<down>" #'scroll-other-window

   :map (minibuffer-mode-map
         ivy-minibuffer-map
         vertico-map
         evil-ex-completion-map)
   :nvig
   "s-," #'night/pns

   :g
   "C-<up>" #'scroll-other-window-down
   ;; "C-M-<up>" #'scroll-other-window-down
   :g
   "C-<down>" #'scroll-other-window
   )
  (after! (org)
    (map!
     :map org-mode-map
     :g
     "C-S-<down>" #'scroll-other-window
     :g
     "C-S-<up>" #'scroll-other-window-down))
  )
