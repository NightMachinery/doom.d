;;; autoload/org/night-noter.el -*- lexical-binding: t; -*-

;;;
;; org-noter-set-notes-window-location (vertical vs horizontal)
;; org-noter-set-doc-split-fraction
;; org-noter-insert-note-no-questions
;;;
(after! org-noter
  (map! :map org-noter-doc-mode-map
        :nvi "i" #'org-noter-insert-note
        :localleader
        :nvi "g" #'org-noter-sync-current-page-or-chapter)


  (map! :map org-noter-notes-mode-map
        :localleader
        :nvi "gd" #'org-noter-sync-current-note)
  )
