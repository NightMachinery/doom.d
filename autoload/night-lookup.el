;;; autoload/night-lookup.el -*- lexical-binding: t; -*-

(setq-default
 +lookup-definition-functions
 '(;; +lookup-dictionary-definition-backend-fn
   +lookup-xref-definitions-backend-fn
   +lookup-dumb-jump-backend-fn
   +lookup-project-search-backend-fn
   +lookup-evil-goto-definition-backend-fn))
