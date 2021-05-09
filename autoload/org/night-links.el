;;; autoload/org/night-links.el -*- lexical-binding: t; -*-

(after! org
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (defun night/org-description-formatter (link desc)
    (let* (
           (file
            (if (s-starts-with? "id:" link t)
                (car (org-id-find (s-chop-prefix "id:" link)))
              link))
           (tail
            (concat
             (f-filename (f-dirname file))
             "/"
             (f-filename file))))
      (message "%s" (concat link ", " desc ", " tail ", " file))
      (cond
       ((equalp desc tail) desc)
       (desc (concat tail ":" (or
                               (cadr (s-match ".*:\\(.*\\)" desc))
                               desc)))
       (tail tail)
       (t link)
       )))

  (setq org-link-make-description-function #'night/org-description-formatter)

  (after! org-super-links
    (setq org-super-links-backlink-prefix nil)
    (setq org-super-links-link-prefix nil)
    (setq org-super-links-link-postfix nil)
    (setq org-super-links-default-description-formatter #'night/org-description-formatter)

    (map!
     :map org-mode-map
     :localleader
     ;; overrides the default link bindings
     "ls" #'org-super-links-store-link
     "lS" #'org-super-links-insert-link
     "ld" #'org-super-links-delete-link
     ;; "la" #'org-super-links-quick-insert-drawer-link
     )
    ))
