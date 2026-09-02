;;; night-snippets/fundamental-mode/.yas-setup.el -*- lexical-binding: t; -*-
;;;
;;; Loaded by `yas-reload-all' alongside the snippets in this directory.
;;; Because yasnippet appends `fundamental-mode' to every mode's table list,
;;; these helpers (and these snippets) are available in every buffer.

(defun night/yas-snippet-body (name)
  "Return the raw body of the snippet named NAME.
The result is what `yas-expand-snippet' accepts, so backquoted elisp in
the body is evaluated at expansion time, not at lookup time."
  (yas--template-content (yas-lookup-snippet name)))

(defun night/yas-snippet-string (name)
  "Render the snippet named NAME to a plain string.
Fields are flattened to their default text, so this is only meaningful
for snippets that have none."
  (let ((body (night/yas-snippet-body name)))
    (with-temp-buffer
      ;; `yas-expand-snippet' refuses to run without a set-up `yas-minor-mode',
      ;; even on a body that has no fields.
      (yas-minor-mode 1)
      (yas-expand-snippet body)
      (buffer-substring-no-properties (point-min) (point-max)))))
;;;
;;; One-shot shrink.
;;;
;;; Insert something, and let the very next keypress -- if it is a backspace --
;;; swap it for a shorter variant.  Any other key behaves completely normally
;;; and disarms the mechanism.  Nothing here is global and nothing survives the
;;; moment of expansion, so text already written is never affected.
;;;
;;; `set-transient-map' with a nil keep-pred is precisely this: "Normally, MAP
;;; is used only once, to look up the very next key."  It installs into
;;; `overriding-terminal-local-map', consulted ahead of evil's maps and ahead of
;;; the `keymap' char property that `yas-keymap' rides on, so it wins over both.
;;;
;;; Note there is deliberately no `on-exit' cleanup of the marker: `subr.el'
;;; calls `on-exit' from `pre-command-hook', which runs *before* the bound
;;; command, so clearing the marker there would break the deletion it is about
;;; to perform.  The marker is unreachable once the map pops, and is collected.
;;;
;;; Only text behind point can be shrunk, since the region runs from the start
;;; marker to point.

(defvar-local night/yas--shrink-start nil
  "Marker at the start of the text a shrinkable insertion just wrote.")

(defvar-local night/yas--shrink-variants nil
  "Variants still available to `night/yas-shrink-last-insertion'.")

(defun night/yas--emit (variant)
  "Insert VARIANT at point.
A string is inserted as-is.  A function is called instead, and may
insert whatever it likes, including a snippet expansion with fields."
  (if (functionp variant)
      (funcall variant)
    (insert variant)))

(defun night/yas--arm-shrink (start variants)
  "Let the next keypress replace the insertion at START with the first of VARIANTS."
  (setq night/yas--shrink-start start
        night/yas--shrink-variants variants)
  (when variants
    (let ((map (make-sparse-keymap)))
      ;; Both spellings are needed and are not duplicates: a GUI looks up the
      ;; raw `backspace' event first, and only translates it to DEL when that
      ;; is unbound.  (kbd "DEL") and [?\C-?] *are* the same key.
      (define-key map (kbd "DEL") #'night/yas-shrink-last-insertion)
      (define-key map [backspace] #'night/yas-shrink-last-insertion)
      (set-transient-map map))))

(defun night/yas-insert-shrinkable (&rest variants)
  "Insert the first of VARIANTS; the next backspace swaps in the one after it.
Repeated backspaces walk further down the list.  A variant is a string,
or a function that inserts at point; later variants are only evaluated
if they are actually reached."
  (undo-boundary)
  (let ((start (copy-marker (point))))
    (night/yas--emit (car variants))
    (night/yas--arm-shrink start (cdr variants))))

(defun night/yas-shrink-last-insertion ()
  "Replace the text just inserted with its next variant.
Armed for exactly one keypress by `night/yas-insert-shrinkable'."
  (interactive)
  (let ((start night/yas--shrink-start)
        (rest night/yas--shrink-variants))
    (when (and start rest (eq (marker-buffer start) (current-buffer)))
      (delete-region start (point))
      (night/yas--emit (car rest))
      (night/yas--arm-shrink start (cdr rest)))))
;;;
;;; Call sites.

(defun night/yas-expand-model-tag (tag)
  "Insert `@TAG' followed by today's date.
The next backspace drops the date and the space before it, leaving the
bare tag."
  (night/yas-insert-shrinkable
   (concat "@" tag " " (night/yas-snippet-string "timee"))
   (concat "@" tag)))

(defun night/yas-jalali-strings ()
  "Return a cons of the jalali stamp with and without the time of day.
`z' shells out, so both forms are built from a single `datej' call
rather than one per variant."
  (let ((day (z datej)))
    (cons (format "[jalali:%s/%s]" day (format-time-string "%H:%M"))
          (format "[jalali:%s]" day))))
