;;; autoload/API/night-OMDb.el -*- lexical-binding: t; -*-

;; (require 'orgmdb)
(after! orgmdb
  (setq orgmdb-omdb-apikey (getenv "OMDB_API")))
