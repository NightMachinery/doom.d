;;; autoload/org/night-agenda.el -*- lexical-binding: t; -*-

(after! (org)
  (setq org-agenda-files (list (concat (getenv "nightNotes") "/private/agenda"))))
