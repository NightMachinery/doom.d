;;; autoload/night-yasnippet.el -*- lexical-binding: t; -*-

(after! yasnippet
  (setq +snippets-dir (concat (getenv "DOOMDIR") "/" "night-snippets/"))
  (setq yas-snippet-dirs (list +snippets-dir))
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
;;;
  (setq yas-key-syntaxes ;; @userConfig
        (list #'yas-try-key-from-whitespace
              "w_.()" "w_." "w_"))
;;;
  (map!
   :map (yas-minor-mode-map)
   :ig
   "TAB" yas-maybe-expand
   :ig
   "<tab>" yas-maybe-expand
   :ig
   "\t" yas-maybe-expand
   :ig
   [?\t] yas-maybe-expand)
;;;
  )
