;;; autoload/night-kitty.el -*- lexical-binding: t; -*-
;;;
(defun night/kitty-p ()
  (let ((kitty-window-id (getenv "KITTY_WINDOW_ID")))
    (and kitty-window-id
         (not (string= kitty-window-id "")))))
;;;
(when (night/kitty-p)
  ;; https://github.com/kovidgoyal/kitty/issues/4094
  (cond
   (t
    ;; This will display SOFT HYPHEN as the ASCII dash character -, but with a special typeface that will make it stand out.
    (or standard-display-table
        (setq standard-display-table (make-display-table)))
    (aset standard-display-table
          173 (vector (make-glyph-code ?- 'escape-glyph))))
   (t
    ;; hides the SOFT HYPHEN character completely
    (set-char-table-range glyphless-char-display
                          (char-from-name "SOFT HYPHEN") 'zero-width)))

  (global-auto-composition-mode -1))
;;;
