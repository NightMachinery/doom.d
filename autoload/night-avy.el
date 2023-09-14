;;; autoload/night-avy.el -*- lexical-binding: t; -*-

(after! avy

  (defun night/avy-goto-opening-paren ()
    (interactive)
    (avy-jump "[[{(]"))
  (defun night/avy-goto-closing-paren ()
    (interactive)
    (avy-jump "[])}]"))
;;;
  (defun night/avy-goto-org-header ()
    (interactive)
    ;; https://github.com/abo-abo/avy/issues/320
    (let ((avy-text ""))
      (when (avy-jump "^\*+ ")
        ;; (when avy-text (forward-char (length avy-text)))
        )))
  ;;;
  (defun night/avy-copy-from-char-to-char ()
    "Copy text between two characters using avy."
    ;; [[https://github.com/abo-abo/avy/issues/367][{FR} Select two arbitrary characters and copy the content between them · Issue #367 · abo-abo/avy]]
  ;;;
    (interactive)
    ;; Get the starting position
    (save-excursion
      (let ((start-char (read-char "Start char: "))
            start-pos end-pos)
        (setq start-pos (avy-process (avy--regex-candidates (string start-char))))
        ;; If a starting position is selected
        (when start-pos
          (let ((end-char (read-char "End char: ")))
            (setq end-pos (avy-process (avy--regex-candidates (string end-char))))
            ;; If an ending position is selected, copy the text
            (when end-pos
              ;; (message "start: %s, end: %s" start-pos end-pos)
              ;; start: (3417 . 3418), end: (3419 . 3420)
              (let ((start-pos (car start-pos))
                    (end-pos (cdr end-pos)))
                (kill-ring-save
                 start-pos
                 end-pos)
                (nav-flash-show start-pos end-pos 'urgent-face 1))))))))
  (map!
   :nvo
   "g y" #'night/avy-copy-from-char-to-char)
  (map!
   :map magit-mode-map
   :nvo
   "g y" #'night/avy-copy-from-char-to-char)
  )
