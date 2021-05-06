(when (display-graphic-p)
  (doom-adjust-font-size 2)
  )

;;;
;; How to make the highlights re-use the original background color? https://github.com/tarsius/hl-todo/issues/60 ; did not work:
;; :background "unspecified"
;; :inherit t
(defface special-comment '((t (:inherit t :foreground "#3437eb" :weight bold))) "This is just a doc string")

;;;

;; white-green background: #e9f2eb
(defface zsh-macro '((t (:foreground "#1adb51" :background "#e9f1f2" :weight bold))) "This is just a doc string")

(defun night/highlight-atsign ()
  "DEPRECATED: We now customize hl-todo instead."
  (interactive)
  (progn (font-lock-add-keywords
            ;; [:punct:]
            nil '(("\\B\\(@[^][[:space:](){};,\n\"=]+\\)" 1 'special-comment t)))))

(defun night/highlight-atsign-zsh ()
  (interactive)
  (progn (font-lock-add-keywords 'sh-mode
             '(("\\B\\(@[^][[:space:](){};,\n\"=]+\\)" 1 'zsh-macro t)))))

(defun night/highlight-org ()
  (interactive)
  (progn
    ;; (font-lock-add-keywords 'org-mode
    ;;                         '(("^.*\\B@great\\B.*$" 1 'zsh-macro t)))
    (font-lock-add-keywords 'org-mode
                            '(("^.*\\(@great\\|:great:\\).*$" . 'zsh-macro)))
    )
  )

(night/highlight-atsign-zsh)
(night/highlight-org)
;;; tests
;; jas (@wsw aws) @hi+ @hi? maddah_ali@sharif.edu hi@gmail.com TODO @ XXXX @ja
;;a@a
;;@hi j
;;;
(after! hl-todo
  ;; The syntax class of the characters at either end has to be `w' (which means word) in `hl-todo--syntax-table'.
  (modify-syntax-entry ?@ "w" hl-todo--syntax-table)
  (modify-syntax-entry ?+ "w" hl-todo--syntax-table)
  (modify-syntax-entry ?? "w" hl-todo--syntax-table)
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(
          ;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold)
          ("@[^][[:space:](){};,\n\"=]+" special-comment)
          ))
  )
