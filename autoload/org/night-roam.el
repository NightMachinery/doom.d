;;; autoload/org/night-roam.el -*- lexical-binding: t; -*-

(after! (org evil-org evil org-roam)
  (cond
   ((zb isServer) (setq org-roam-directory (concat (getenv "HOME") "/org-roam/")))
   (nil (setq org-roam-directory (getenv "nightNotes"))
        ;; disabled because of performance issues
        )
   )
  (z mkdir -p (i org-roam-directory))

  (setq +org-roam-open-buffer-on-find-file nil)
  (setq org-roam-db-update-idle-seconds 600)

  (provide 'night-roam)
  )
