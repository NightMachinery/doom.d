;;; autoload/org/night-toc.el -*- lexical-binding: t; -*-

(after! org
  (if (require 'toc-org nil t)
      (progn
        (add-hook 'org-mode-hook 'toc-org-mode)
        ;; enable in markdown, too
        (after! markdown (add-hook 'markdown-mode-hook 'toc-org-mode)
          (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)))
    (warn "toc-org not found"))
;;;
  (require 'org-make-toc)
  (setq org-make-toc-insert-custom-ids t)
  ;; When non-nil and using the default org-make-toc-link-type-fn to generate GitHub-compatible links, automatically sets the "CUSTOM_ID" property for each entry to their name. This will allow links to also work in org-mode in Emacs.
  ;; Note that the links are still inserted as =#header_name=.

  (defun night/org-make-toc--link-entry-id (pos)
    "Return text for entry at POS converted to a ID link."
    (-when-let* ((title (org-link-display-format (org-entry-get pos "ITEM")))
                 (target (org-id-get pos 'create))
                 (filename (if org-make-toc-filename-prefix
                               (file-name-nondirectory (buffer-file-name))
                             "")))
      (org-link-make-string (concat "id:" target)
                            (org-make-toc--visible-text title))))
  (setq org-make-toc-link-type-fn #'night/org-make-toc--link-entry-id)
;;;
  )
