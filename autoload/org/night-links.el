;;; autoload/org/night-links.el -*- lexical-binding: t; -*-

(after! org
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (defun night/org-description-formatter (link desc)
    (let* (
           (file
            (if (s-starts-with? "id:" link t)
                (car (org-id-find (s-chop-prefix "id:" link)))
              link))
           (parent
            (f-filename (f-dirname file))
            )
           (parent (or
                    (cadr (s-match "^[^/]*:\\(.*\\)" parent))
                    parent))            ;;  we can run this on file, as well, or `HOME:.xsh` links won't get their prefix stripped. But I think that's okay.
           (tail
            (cond
             ((or (equalp "." parent) (equalp "" parent))
              file
              )
             (t (concat
                 parent
                 "/"
                 (f-filename file))))))
      (message "%s" (concat link ", " desc ", " tail ", " file))
      (cond
       ((equalp desc tail) desc)
       (desc (concat tail ":" (or
                               (cadr (s-match ".*:\\(.*\\)" desc))
                               desc)))
       (tail tail)
       (t link)
       )))

  ;; (cadr (s-match "^[^/]*:\\(.*\\)" "aJK:lol/as"))

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
    )
;;;
  (setcdr org-link-abbrev-alist
          `(
            ("HOME" . ,(concat (getenv "HOME") "/"))
            ("DOOMDIR" . ,(concat (getenv "DOOMDIR") "/"))
            ("NIGHTDIR" . ,(concat (getenv "NIGHTDIR") "/"))
            ("cellar" . ,(concat (getenv "cellar") "/"))
            ("nightNotes" . ,(concat (getenv "nightNotes") "/"))
            ("orgdir" . ,(concat  org-directory "/"))))
  (add-to-list 'org-modules 'org-protocol)
  ;; https://orgmode.org/manual/Link-Abbreviations.html
  ;; If you need special abbreviations just for a single Org buffer, you can define them in the file with:
  ;; #+LINK: google    http://www.google.com/search?q=%s


  (defun org-nightNotes-complete-link ()
    (concat "nightNotes:" (night/browse-notes))
    )
  ;; @bug setting these completion functions causes these link types to appear twice in the =\ l l= list
  (org-link-set-parameters "nightNotes" :complete #'org-nightNotes-complete-link)

  (defun org-NIGHTDIR-complete-link ()
    (concat "NIGHTDIR:" (night/browse-NIGHTDIR))
    )
  (org-link-set-parameters "NIGHTDIR" :complete #'org-NIGHTDIR-complete-link)

  (defun org-DOOMDIR-complete-link ()
    (concat "DOOMDIR:" (night/browse-DOOMDIR))
    )
  (org-link-set-parameters "DOOMDIR" :complete #'org-DOOMDIR-complete-link)

  (defun org-HOME-complete-link ()
    (concat "HOME:" (night/browse-HOME))
    )
  (org-link-set-parameters "HOME" :complete #'org-HOME-complete-link)
;;;
  )
