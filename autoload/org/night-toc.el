;;; autoload/org/night-toc.el -*- lexical-binding: t; -*-

(after! org
  (if (require 'toc-org nil t)
      (progn
        (add-hook 'org-mode-hook 'toc-org-mode)
        ;; enable in markdown, too
        (after! markdown (add-hook 'markdown-mode-hook 'toc-org-mode)
          (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)))
    (warn "toc-org not found")))
