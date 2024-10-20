;;; autoload/org/night-org-headings.el -*- lexical-binding: t; -*-

(after! (org)
  (defun night/org-get-outline-heading-path ()
  "Return the outline path of the current Org heading.

When called interactively, display the path in the minibuffer."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let ((path (org-get-outline-path t t)))
      (when path
        (let ((formatted-path (org-format-outline-path path)))
          (when (called-interactively-p 'interactive)
            (message "%s" formatted-path))
          formatted-path))))))

(after! (org org-sticky-header)
  (setq org-sticky-header-full-path 'reversed)
  )
