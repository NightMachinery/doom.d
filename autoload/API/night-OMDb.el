;;; autoload/API/night-OMDb.el -*- lexical-binding: t; -*-

(require 'orgmdb)
(after! orgmdb
  (setq orgmdb-omdb-apikey (getenv "OMDB_API"))
  (dolist (i
           ;; the list will be added in reverse, obviously
           ;; metascore is the same as metacritic
           ;; [[https://github.com/isamert/orgmdb.el/issues/6][isamert/orgmdb.el#6 {FR} Add `writers` property]]
           '(imdb-id plot actors awards tomatometer metacritic imdb-rating rated country type year genre runtime director title))
    (add-to-list 'orgmdb-fill-property-list i))
  ;; (setq orgmdb-fill-property-list nil)

  (advice-add #'orgmdb-fill-movie-properties :after #'org-fold-show-entry)
;;;
  ;; (defun night/insert-orgmdb-link ()
  ;;   (interactive)
  ;;   (night/unt :description nil)
  ;;   ;; (org-up-heading-safe)
  ;;   (call-interactively #'orgmdb-fill-movie-properties))

  ;; (map! :map org-mode-map
  ;;       :localleader
  ;;       "lm" #'night/insert-orgmdb-link)
;;;
  (defun night/org-imdb-fill-maybe ()
    "Check if the current line contains an IMDB link and fill movie properties if it does."
    (interactive)
    (save-excursion                     ; Preserve point position
      (let ((current-line (thing-at-point 'line t)))
        ;; Check if the current line contains an IMDB link
        (when (string-match "https://www.imdb.com/title/tt\\([0-9]+\\)/" current-line)

          ;; Call orgmdb-fill-movie-properties if an IMDB link is found
          (orgmdb-fill-movie-properties nil)))))
  )
