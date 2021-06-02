;;; autoload/org/night-org-keybindings.el -*- lexical-binding: t; -*-

(after! (org evil-org evil)
;;;
  (defun night/insert-zero-width-space ()
    (interactive)
    (insert "\u200b"))
;;;
  (defun night/org-shiftright ()
    (interactive)
    (cond
     ((and (not (eq org-support-shift-select 'always))
           (org-at-heading-p))

      (org-next-visible-heading 1)
      t)

     (t nil)))

  (defun night/org-shiftleft ()
    (interactive)
    (cond
     ((and (not (eq org-support-shift-select 'always))
           (org-at-heading-p))

      (org-next-visible-heading -1)
      t)

     (t nil)))


  (add-hook 'org-shiftleft-hook #'night/org-shiftleft)
  (add-hook 'org-shiftright-hook #'night/org-shiftright)
;;;
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

   :nvo
   "g8" #'night/avy-goto-org-header

   :nvo
   "{" #'org-previous-visible-heading

   :nvo
   "}" #'org-next-visible-heading

   :nvo "C-<up>" #'org-backward-heading-same-level
   :nvo "C-<down>" #'org-forward-heading-same-level

   :i
   "M-S-<left>" #'org-promote-subtree   ; already bound in normal mode
   :localleader
   "rs" #'avy-org-refile-as-child
   ;; "sr" #'avy-org-refile-as-child
   "rf" #'+org/refile-to-file
   "rF" #'night/org-refile-to-new-file
   ))
