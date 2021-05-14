;;; autoload/org/night-roam.el -*- lexical-binding: t; -*-

(after! (org evil-org evil org-roam)
  ;; (setq org-roam-directory (getenv "nightNotes"))
  ;; @furureCron0 disable me if perf issues continue

  (setq +org-roam-open-buffer-on-find-file nil)
  (setq org-roam-db-update-idle-seconds 600))
