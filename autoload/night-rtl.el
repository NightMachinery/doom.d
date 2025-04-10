;;; autoload/night-rtl.el -*- lexical-binding: t; -*-

(setq night/persian-font "B Nazanin") ;; "IranNastaliq"
;; (setq night/persian-font "Courier New")

(setq default-input-method "farsi-isiri-9147")
;;;
(defun night/insert-persian-half-space ()
  "Insert a Persian half-space (zero-width non-joiner) character."
  (interactive)
  (insert-char ?\u200C))

(defun night/insert-shift-space ()
  (interactive)
  (cond
   ((string= current-input-method "farsi-isiri-9147")
    (night/insert-persian-half-space))
   (t
    ;; (insert " ")
    (insert-char ?\s)
    ;; It'd be best if we could do shift+space from the original map, e.g., [help:org-self-insert-command], [help:lispy-space], but I don't know how to do that.
    ;; It still doesn't matter much, as we never use shift+space in non-Persian buffers anyway.
    )))

(map!
 :i
 "S-SPC" #'night/insert-shift-space
 )

;; [help:+default-minibuffer-maps]
(map! :map
      (
       minibuffer-local-map
       ivy-minibuffer-map
       ;; minibuffer-mode-map
       ;; read-expression-map
       ;; evil-ex-map
       ;; evil-ex-search-keymap
       evil-ex-completion-map)
      ;; ivy-minibuffer-map
      :g
      "S-SPC"
      #'night/insert-shift-space
      ;; "i"
      ;; #'night/bello
      )
;;;
(setq visual-order-cursor-movement nil) ;; @redundant
(map!
 ;; Makes the arrow keys consistent between the insert and normal state
 ;; Without this, [help:left-char], [help:right-char] would have been used in the insert mode.
 :gnvio
 "<left>" #'evil-backward-char
 :gnvio
 "<right>" #'evil-forward-char
 )
;;;

(defun night/enable-bidirectional ()    ;; night/enable-rtl
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
  ;;;
  (setq face-font-rescale-alist `((,night/persian-font . 1.30)))
  (setq bidi-paragraph-direction 'nil)
  (setq bidi-paragraph-separate-re "^"
        bidi-paragraph-start-re "^"))

(defun night/enable-bidirectional-extras ()
  (interactive)
  ;;;
  (setq-local line-move-ignore-invisible nil)
  ;; [[id:ff0eb77c-aa38-4cb1-8633-c7d255e479a3][Scrolling Bug]]
  ;;;
  )

(defun night/h-rtl-enter ()
  (interactive)
  (activate-input-method "farsi-isiri-9147")
  (insert "\n"))

(defun night/h-latex-after-save ()
  (interactive)
  ;; (z-async t h-seminar-compile-emacs)
  ;; @done Delete the above line, we no longer need it.
  )
(defun night/latex-rtl-opinionated ()
  (interactive)
  (map! :map 'local
        :i "\\" (lambda () (interactive)
                  (activate-input-method nil)
                  (insert "\\"))
        :i "<return>" #'night/h-rtl-enter
        :i "RET" #'night/h-rtl-enter
        )
;;;
  (add-hook 'after-save-hook
            #'night/h-latex-after-save
            nil t)
;;;
  (setq-local company-backends
              ;; company-dabbrev doesn't seem to find dabbrev completions from other buffers, unlike normal dabbrev. I tried it with everything else disabled, too, for both dabbrev and dabbrev-code.
              '(
                company-dabbrev
                company-dabbrev-code
                (
                 company-reftex-labels company-reftex-citations
                 (+latex-symbols-company-backend company-auctex-macros company-auctex-environments)
                 company-capf company-files
                 company-yasnippet
                 )
                ))
  (setq-local company-idle-delay 0)
  (setq-local company-frontends
              '(
                ;; company-pseudo-tooltip-frontend
                company-preview-frontend
                ;; This preview frontend didn't show anything for me ...
                ))
  (setq-local company-minimum-prefix-length 3)
  )
(add-hook 'text-mode-hook 'night/enable-bidirectional)
(add-hook 'org-mode-hook 'night/enable-bidirectional)
(add-hook 'markdown-mode-hook 'night/enable-bidirectional)
(add-hook 'LaTeX-mode-hook 'night/enable-bidirectional)
(add-hook 'LaTeX-mode-hook 'night/enable-bidirectional-extras)
;;;
(defun night/evil-line-move-with-bidi-workaround (count)
  "Move the cursor COUNT lines, considering the bidirectional paragraph direction."
  ;; This function works around a bug where we can't scroll.
  ;; [[id:ff0eb77c-aa38-4cb1-8633-c7d255e479a3][Scrolling Bug]]
  (let ((line-move-visual nil))
    ;; `line-move-visual': `evil-previous-line' is specifically for moving logical lines. There is `evil-previous-visual-line' for visual lines.
    (cond
     ((eq (current-bidi-paragraph-direction) 'right-to-left)
      (let ((line-move-ignore-invisible nil))
        (evil-line-move count)))
     (t
      (evil-line-move count)))))

(evil-define-motion night/evil-previous-line (count)
  "Move the cursor COUNT lines up, working around a bug that makes scrolling not work."

  :type line
  (night/evil-line-move-with-bidi-workaround (- (or count 1))))

(evil-define-motion night/evil-next-line (count)
  "Move the cursor COUNT lines down, working around a bug that makes scrolling not work."

  :type line
  (night/evil-line-move-with-bidi-workaround (or count 1)))

(advice-add 'evil-next-line :override #'night/evil-next-line)
(advice-add 'evil-previous-line :override #'night/evil-previous-line)
;;;
