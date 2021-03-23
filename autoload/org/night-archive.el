;;; autoload/org/night-archive.el -*- lexical-binding: t; -*-

(defun night/org-archive-done-tasks ()
  ;; https://stackoverflow.com/questions/6997387/how-to-archive-all-the-done-tasks-using-a-single-command
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file)
  ;; change 'tree to 'file to operate over the entire file, and viceversa to operate only on current subtree
  )
