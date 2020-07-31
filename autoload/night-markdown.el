(defun night/markdown-follow-wiki-link-at-point (&optional arg)
  "Find Wiki Link at point.
With prefix argument ARG, open the file in other window.
See `markdown-wiki-link-p' and `markdown-follow-wiki-link'."
  (interactive "P")
  (if (markdown-wiki-link-p)
      ;; (markdown-follow-wiki-link (markdown-wiki-link-link) arg)
      ;; (print (markdown-wiki-link-link) arg)
      (find-file (markdown-wiki-link-link))
    (user-error "Point is not at a Wiki Link")))
