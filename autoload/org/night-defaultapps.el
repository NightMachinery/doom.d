;;; autoload/org/night-defaultapps.el -*- lexical-binding: t; -*-

(after! org (setq org-file-apps '((remote . emacs)
                       (auto-mode . emacs)
                       (directory . emacs)
                       ;; ("\\.mm\\'" . default)
                       ;; ("\\.x?html?\\'" . default)
                       ("pdf" . default)
                       (t . emacs)
                       )))
;;
