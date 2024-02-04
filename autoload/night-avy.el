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
      (when-let ((start-char (night/read-char-or-cancel "Start char: ")))
        ;; (message "start-char: %s" start-char)
        (let (start-pos end-pos)
          (setq start-pos (avy-process (avy--regex-candidates (regexp-quote (string start-char)))))
          ;; If a starting position is selected
          (when (and
                 start-pos
                 (listp start-pos))
            (when-let ((end-char (night/read-char-or-cancel "End char: ")))
              (setq end-pos (avy-process (avy--regex-candidates (regexp-quote (string end-char)))))
              ;; If an ending position is selected, copy the text
              (when (and
                     end-pos
                     (listp end-pos))
                ;; (message "start: %s, end: %s" start-pos end-pos)
                ;; start: (3417 . 3418), end: (3419 . 3420)
                (let ((start-pos (car start-pos))
                      (end-pos (cdr end-pos)))
                  (kill-ring-save
                   start-pos
                   end-pos)
                  (nav-flash-show start-pos end-pos 'urgent-face 1)))))))))
  (map!
   :nvo
   "g y" #'night/avy-copy-from-char-to-char
   :leader
   "y a" #'night/avy-copy-from-char-to-char)
  (after! magit
    (map!
     :map magit-mode-map
     :nvo
     "g y" #'night/avy-copy-from-char-to-char)
    ;; [[https://github.com/emacs-evil/evil-collection/issues/754][{Q} How do I add a new hotkey? · Issue #754 · emacs-evil/evil-collection]]
    )
  )
