;;; autoload/org/night-gpg.el -*- lexical-binding: t; -*-

(after! (org)
  (require 'org-crypt)
  (require 'epa-file)
;;;
  ;; I think doom already does all of the below, so I have commented them ...
  (comment
   (epa-file-enable)
   (org-crypt-use-before-save-magic)
   (setq org-tags-exclude-from-inheritance (quote ("crypt"))))
;;;
  (setq org-crypt-key "6156FCADABAFE9E8BC468F55C567089D40CA8367") ;; needs to be true-ish for the :CRYPTKET: property to work, but then passphrase mode won't work

  ;; @startupCrashCause setting =epg-gpg-program= incorrectly can make emacs start in a hanged state!
  (comment
   ;; This doesn't seem to work, `epg` still uses `gpg` from PATH.
   ;; Besides, it seems to need gpg 2.4.0 ... ?
   (let ((gpg-binary "/opt/homebrew/opt/gnupg@2.2/bin/gpg"))
     (when (f-exists-p gpg-binary)
       (custom-set-variables
        `(epg-gpg-program ,gpg-binary)))))
  )
