(when (display-graphic-p)
  (doom-adjust-font-size 2)
  )

;; red: #a12a2a
(defface special-comment '((t (:foreground "#3437eb" :weight bold))) "This is just a doc string")

(defun night/highlight-atsign ()
  "DEPRECATED: We now customize hl-todo instead."
  (interactive)
  (progn (font-lock-add-keywords
            ;; [:punct:]
            nil '(("\\B\\(@[^][[:space:](){};,\n\"=]+\\)" 1 'special-comment t)))))
;;; tests
;; jas (@wsw aws) @hi+ maddah_ali@sharif.edu hi@gmail.com TODO @ XXXX @ja
;;a@a
;;@hi j
;;;
(after! hl-todo
  ;; The syntax class of the characters at either end has to be `w' (which means word) in `hl-todo--syntax-table'.
  (modify-syntax-entry ?@ "w" hl-todo--syntax-table)
  (modify-syntax-entry ?+ "w" hl-todo--syntax-table)
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
