;;; ~/doom.d/autoload/night-search.el -*- lexical-binding: t; -*-

(after! rg
  (rg-enable-menu))

(defun night/search-dir (arg)
  "Conduct a text search in files under the given folder."
  (let ((default-directory arg))
    (call-interactively
     (cond ((featurep! :completion ivy)  #'+ivy/project-search-from-cwd)
           ((featurep! :completion helm) #'+helm/project-search-from-cwd)
           (#'rgrep)))))

(defun night/search-notes ()
  (interactive)
  (night/search-dir (getenv "nightNotes")))
