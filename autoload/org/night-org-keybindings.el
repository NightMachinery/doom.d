;;; autoload/org/night-org-keybindings.el -*- lexical-binding: t; -*-

(after! org
  (map! :map org-mode-map
        :localleader
        :nvi "lp" #'night/org-paste-clipboard-image)
  (setq org-startup-folded 'overview) ; @upstreambug https://github.com/hlissner/doom-emacs/issues/3693
  (map!
   :map evil-org-mode-map
   :n
   ;; "TAB" 'org-cycle
   ;; "<S-tab>" 'org-force-cycle-archived ; is overrided ...
   "TAB" 'org-force-cycle-archived
   ;; (setq org-cycle-open-archived-trees t)  ;; https://emacs.stackexchange.com/questions/64067/expand-an-archived-subtree-with-just-tab/
   :i
   "M-S-<left>" #'org-promote-subtree   ; already bound in normal mode
   :localleader
   "sr" #'avy-org-refile-as-child
   ))
