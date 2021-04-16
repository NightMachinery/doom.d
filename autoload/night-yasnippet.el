;;; autoload/night-yasnippet.el -*- lexical-binding: t; -*-

(after! yasnippet
  ;; (add-to-list 'yas-snippet-dirs (concat (getenv "DOOMDIR") "/" "night-snippets/"))
  (setq yas-snippet-dirs (list (concat (getenv "DOOMDIR") "/" "night-snippets/")))
  ;; having too many snippets is bad. We can use the function =+snippets/find-for-current-mode= to see what is available from where.
  ;; Elements appearing earlier in the list override later elements'
  ;; snippets.
  ;; The first directory is taken as the default for storing snippet's
  ;; created with yas-new-snippet.
  ;; Snippets named `__` are file templates.
  ;; These two were the defaults that we just reset:
  (add-to-list 'yas-snippet-dirs '+file-templates-dir 'append #'eq)
  (add-to-list 'yas-snippet-dirs 'doom-snippets-dir 'append #'eq)

  (yas-reload-all)
  )
