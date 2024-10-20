;;; autoload/org/babel/night-babel-edit.el -*- lexical-binding: t; -*-

(after! (org-src)
  ;;;
  ;; [[https://discourse.doomemacs.org/t/how-to-open-org-src-buffers-in-the-same-window/2808/2][How to open org-src buffers in the same window? - User Support / Org - Doom Emacs Discourse]]
  (set-popup-rule! "^\\*Org Src" :ignore t)
  (setq org-src-window-setup
        ;; 'plain
        'current-window
        )
  ;;;

  (advice-add 'evil-org-edit-src-exit :override #'org-edit-src-exit)

  (map! :map (org-edit-src-exit)
;;;
        ;; These two get overriden by global hotkeys ...
        :nvig
        "C-<return>" #'org-edit-src-exit
        :nvig
        "C-RET" #'org-edit-src-exit
;;;
        :n
        "Z Z" #'org-edit-src-exit
        ))
