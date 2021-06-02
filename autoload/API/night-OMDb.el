;;; autoload/API/night-OMDb.el -*- lexical-binding: t; -*-

;; (require 'orgmdb)
(after! orgmdb
  (setq orgmdb-omdb-apikey (getenv "OMDB_API"))
  (dolist (i
           ;; the list will be added in reverse, obviously
           ;; metascore is the same as metacritic
           '(plot actors awards tomatometer metacritic imdb-rating rated country type year genre runtime director imdb-id))
    (add-to-list 'orgmdb-fill-property-list i))
  ;; (setq orgmdb-fill-property-list nil)

  )
