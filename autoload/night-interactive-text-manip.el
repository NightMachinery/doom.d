;;; autoload/night-interactive-text-manip.el -*- lexical-binding: t; -*-

(defun night/dwim-backward-kill-word ()
  "DWIM kill characters backward until encountering the beginning of a
word or non-word."
  (interactive)
  (if (thing-at-point 'word) (backward-kill-word 1)
    (let* ((orig-point              (point))
           (orig-line               (line-number-at-pos))
           (backward-word-point     (progn (backward-word) (point)))
           (backward-non-word-point (progn (goto-char orig-point) (night/backward-non-word) (point)))
           (min-point               (max backward-word-point backward-non-word-point)))

      (if (< (line-number-at-pos min-point) orig-line) (progn (goto-char min-point) (end-of-line) (delete-horizontal-space))
        (delete-region min-point orig-point)
        (goto-char min-point))
      )))

(defun night/backward-non-word ()
  "Move backward until encountering the beginning of a non-word."
  (interactive)
  (search-backward-regexp "[^a-zA-Z0-9\s\n]")
  (while (looking-at "[^a-zA-Z0-9\s\n]")
    (backward-char))
  (forward-char))
;;;
(defvar night/syntax-table1
  (let ((st (make-syntax-table)))
    ;; ` default = punctuation
    ;; ' default = punctuation
    ;; , default = punctuation
    ;; ; default = punctuation
    (modify-syntax-entry ?{ "." st)  ;; { = punctuation
    (modify-syntax-entry ?} "." st)  ;; } = punctuation
    (modify-syntax-entry ?\" "." st) ;; " = punctuation
    (modify-syntax-entry ?\\ "_" st) ;; \ = symbol
    (modify-syntax-entry ?\$ "_" st) ;; $ = symbol
    (modify-syntax-entry ?\% "_" st) ;; % = symbol
    st)
  "Syntax table used while executing custom movement functions.")

(defun night/delete-word-or-whitespace-usually-forwards (&optional arg)
"http://stackoverflow.com/a/20456861/2112489"
(interactive "P")
  (with-syntax-table night/syntax-table1
    (let* (
        beg
        end
        (word-regexp "\\sw")
        (punctuation-regexp "\\s.")
        (symbol-regexp "\\s_\\|\\s(\\|\\s)"))
      (cond
        ;; Condition # 1
        ;; right of cursor = word or punctuation or symbol
        ((or
            (save-excursion (< 0 (skip-syntax-forward "w")))
            (save-excursion (< 0 (skip-syntax-forward ".")))
            (save-excursion (< 0 (skip-syntax-forward "_()"))))
          ;; Condition #1 -- Step 1 of 2
          (cond
            ;; right of cursor = word
            ((save-excursion (< 0 (skip-syntax-forward "w")))
              (skip-syntax-forward "w")
              (setq end (point))
              (while (looking-back word-regexp)
                (backward-char))
              (setq beg (point))
              (delete-region beg end))
            ;; right of cursor = punctuation
            ((save-excursion (< 0 (skip-syntax-forward ".")))
              (skip-syntax-forward ".")
              (setq end (point))
              (while (looking-back punctuation-regexp)
                (backward-char))
              (setq beg (point))
              (delete-region beg end))
            ;; right of cursor = symbol
            ((save-excursion (< 0 (skip-syntax-forward "_()")))
              (skip-syntax-forward "_()")
              (setq end (point))
              (while (looking-back symbol-regexp)
                (backward-char))
              (setq beg (point))
              (delete-region beg end)))
          ;; Condition #1 -- Step 2 of 2
          (cond
            ;; right of cursor = whitespace
            ;; left of cursor = not word / not symbol / not punctuation = whitespace or bol
            ((and
                (save-excursion (< 0 (skip-chars-forward "\s\t")))
                (not (save-excursion (> 0 (skip-syntax-backward "w"))))
                (not (save-excursion (> 0 (skip-syntax-backward "."))))
                (not (save-excursion (> 0 (skip-syntax-backward "_()")))))
              (setq beg (point))
              (skip-chars-forward "\s\t")
              (setq end (point))
              (delete-region beg end))
            ;; right of cursor = whitespace
            ;; left of cursor = word or symbol or punctuation
            ((and
                (save-excursion (< 0 (skip-chars-forward "\s\t")))
                (or
                  (save-excursion (> 0 (skip-syntax-backward "w")))
                  (save-excursion (> 0 (skip-syntax-backward ".")))
                  (save-excursion (> 0 (skip-syntax-backward "_()")))))
              (fixup-whitespace))))
        ;; Condition # 2
        ;; right of cursor = whitespace
        ;; left of cursor = bol | left of cursor = whitespace | right of cursor = whitespace + eol
        ((and
            (save-excursion (< 0 (skip-chars-forward "\s\t")))
            (or
              (bolp)
              (save-excursion (> 0 (skip-chars-backward "\s\t")))
              (save-excursion (< 0 (skip-chars-forward "\s\t")) (eolp))))
          (setq beg (point))
          (skip-chars-forward "\s\t")
          (setq end (point))
          (delete-region beg end))
        ;; Condition # 3
        ;; right of cursor = whitespace or eol
        ;; left of cursor = word or symbol or punctuation
        ;; not bol + word or symbol or punctuation
        ;; not bol + whitespace + word or symbol or punctuation
        ((and
            (or (save-excursion (< 0 (skip-chars-forward "\s\t"))) (eolp))
            (or
              (save-excursion (> 0 (skip-syntax-backward "w")))
              (save-excursion (> 0 (skip-syntax-backward ".")))
              (save-excursion (> 0 (skip-syntax-backward "_()"))))
            (not (save-excursion (> 0 (skip-syntax-backward "w")) (bolp)))
            (not (save-excursion (> 0 (skip-syntax-backward ".")) (bolp)))
            (not (save-excursion (> 0 (skip-syntax-backward "_()")) (bolp)))
            (not (save-excursion (and (> 0 (skip-syntax-backward "w")) (> 0 (skip-chars-backward "\s\t")) (bolp))))
            (not (save-excursion (and (> 0 (skip-syntax-backward ".")) (> 0 (skip-chars-backward "\s\t")) (bolp))))
            (not (save-excursion (and (> 0 (skip-syntax-backward "_()")) (> 0 (skip-chars-backward "\s\t")) (bolp)))))
          (setq end (point))
          (cond
            ((save-excursion (> 0 (skip-syntax-backward "w")))
              (while (looking-back word-regexp)
                (backward-char)))
            ((save-excursion (> 0 (skip-syntax-backward ".")))
              (while (looking-back punctuation-regexp)
                (backward-char)))
            ((save-excursion (> 0 (skip-syntax-backward "_()")))
              (while (looking-back symbol-regexp)
                (backward-char))))
          (setq beg (point))
          (when (save-excursion (> 0 (skip-chars-backward "\s\t")))
            (skip-chars-backward "\s\t")
            (setq beg (point)))
          (delete-region beg end)
          (skip-chars-forward "\s\t"))
        ;; Condition # 4
        ;; not bol = eol
        ;; left of cursor = bol + word or symbol or punctuation | bol + whitespace + word or symbol or punctuation
        ((and
            (not (and (bolp) (eolp)))
            (or
              (save-excursion (> 0 (skip-syntax-backward "w")) (bolp))
              (save-excursion (> 0 (skip-syntax-backward ".")) (bolp))
              (save-excursion (> 0 (skip-syntax-backward "_()")) (bolp))
              (save-excursion (and (> 0 (skip-syntax-backward "w")) (> 0 (skip-chars-backward "\s\t")) (bolp)))
              (save-excursion (and (> 0 (skip-syntax-backward ".")) (> 0 (skip-chars-backward "\s\t")) (bolp)))
              (save-excursion (and (> 0 (skip-syntax-backward "_()")) (> 0 (skip-chars-backward "\s\t")) (bolp)))))
          (skip-chars-forward "\s\t")
          (setq end (point))
          (setq beg (point-at-bol))
          (delete-region beg end))
        ;; Condition # 5
        ;; point = eol
        ;; not an empty line
        ;; whitespace to the left of eol
        ((and
            (not (and (bolp) (eolp)))
            (eolp)
            (save-excursion (> 0 (skip-chars-backward "\s\t"))))
          (setq end (point))
          (skip-chars-backward "\s\t")
          (setq beg (point))
          (delete-region beg end))
        ;; Condition # 6
        ;; point = not eob
        ;; point = bolp and eolp
        ;; universal argument = C-u = '(4)
        ((and
            (not (eobp))
            (and (bolp) (eolp))
            (equal arg '(4)))
          (delete-forward-char 1))) )))
;;;
;; (defun night/kill-whitespace-or-word-forward ()
;;   (interactive)
;;   (if (looking-at "[ \t\n]")
;;       (let ((p (point)))
;;         (re-search-forward "[^ \t\n]" nil :no-error)
;;         (backward-char)
;;         (kill-region p (point)))
;;     (kill-word 1)))

(defun night/kill-whitespace-or-word-backward ()
  (interactive)
  (let  ((p (point))
         (c (char-to-string (char-before)))
         )
    (if (member c '("\t"  " "))
        (progn
          (re-search-backward "[^ \t]" nil :no-error)
          (forward-char)
          (kill-region p (point)))

      (if (equalp c "\n")
          (delete-char -1)
        (night/dwim-backward-kill-word)
        ;; (progn
        ;;   (re-search-backward "[ \t\n]" nil :no-error)
        ;;   (forward-char)
        ;;   (kill-region p (point)))
        ))))
;;;
(defun night/backward-symbol (&optional count)
  (interactive)
  (forward-symbol (* -1 (or count 1))))
;;;
(map!
 :nviog
 ;; "C-w" #'night/dwim-backward-kill-word
 ;; "M-DEL" #'night/dwim-backward-kill-word
 "M-DEL" #'night/kill-whitespace-or-word-backward
 "M-<backspace>" #'night/kill-whitespace-or-word-backward
 ;; "M-DEL" #'night/delete-word-or-whitespace
 ;; "M-<backspace>" #'fixup-whitespace

 ;; "M-<left>" #'night/backward-symbol
 ;; "M-<right>" #'forward-symbol

 "M-<left>" #'evil-backward-WORD-end
 "M-<right>" #'evil-forward-WORD-end

 ;; "M-<left>" #'evil-backward-word-begin
 ;; "M-<right>" #'evil-forward-word-end
 )
