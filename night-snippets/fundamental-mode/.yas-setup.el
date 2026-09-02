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

(defun night/yas-expand-model-tag (tag)
  "Expand `@TAG', a space, and the `timee' snippet at point.
Used by the model-tag command snippets (gpt, fb, op, ...), so the date
format lives only in `timee' and the layout lives only here."
  (yas-expand-snippet
   (concat "@" tag " " (night/yas-snippet-body "timee"))))
