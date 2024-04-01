;;; autoload/night-web.el -*- lexical-binding: t; -*-

(after! (counsel)
  (setq counsel-search-engine
        ;; "google"
        ;; @buggy? [jalali:1403/01/13/22:55]
        ;;;
        "ddg"
        ))
