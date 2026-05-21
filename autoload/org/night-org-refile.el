;;; autoload/org/night-org-refile.el -*- lexical-binding: t; -*-

(after! (org org-refile)
  ;;;
  (setq org-reverse-note-order t)
  ;; refile as first child of target
  ;;;
  (setq org-refile-targets
        '((nil :maxlevel . 1)))

  )
