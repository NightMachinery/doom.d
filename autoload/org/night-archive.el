;;; autoload/org/night-archive.el -*- lexical-binding: t; -*-

;;;
(setq org-startup-folded 'overview)     ; @upstreambug https://github.com/hlissner/doom-emacs/issues/3693
(map!
 :map evil-org-mode-map
 :n
 ;; "TAB" 'org-cycle
 ;; "<S-tab>" 'org-force-cycle-archived ; is overrided ...
 "TAB" 'org-force-cycle-archived
 )
;; (setq org-cycle-open-archived-trees t)  ;; https://emacs.stackexchange.com/questions/64067/expand-an-archived-subtree-with-just-tab/
;;;
(defun night/org-archive-done-tasks ()
  ;; https://stackoverflow.com/questions/6997387/how-to-archive-all-the-done-tasks-using-a-single-command
  ;; @alt https://emacs.stackexchange.com/a/21952/18737
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file)
  ;; change 'tree to 'file to operate over the entire file, and viceversa to operate only on current subtree
  )
