;;; autoload/org/night-gpg.el -*- lexical-binding: t; -*-

(after! (org)
  (setq org-crypt-key "6156FCADABAFE9E8BC468F55C567089D40CA8367") ;; needs to be true-ish for the :CRYPTKET: property to work, but then passphrase mode won't work
  )
