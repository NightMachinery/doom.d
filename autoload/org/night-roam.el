;;; autoload/org/night-roam.el -*- lexical-binding: t; -*-

(after! (org evil-org evil org-roam)
  (setq org-roam-directory (getenv "nightNotes"))
  (setq +org-roam-open-buffer-on-find-file nil)
  )
