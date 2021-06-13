;;; autoload/org/night-books.el -*- lexical-binding: t; -*-

(after! org-books
  (setq org-books-file (concat (getenv "nightNotes") "/bookmarks/read/megalog.org")))
