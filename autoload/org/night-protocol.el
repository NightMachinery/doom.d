;;; autoload/org/night-protocol.el -*- lexical-binding: t; -*-

(setq org-html-validation-link nil)  ;; removes validation link from exported html file
(require 'org-protocol)
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
  )
(setq org-capture-templates
      `(
        ("p" "Protocol" entry (file+headline ,(concat (getenv "nightNotes") "/org/inbox.org") "URLs")
                  ;; "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
                  "* %a\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?"
         )
        ("L" "Protocol Link" entry (file+headline ,(concat (getenv "nightNotes") "/org/inbox.org") "URLs")
         "* %?[[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
        ("o" "Link capture" entry
         (file+headline ,(concat (getenv "nightNotes") "/org/inbox.org") "URLs")
         "* %a"                         ; %U timestamp
         :immediate-finish t)
        ))
;; (setq org-protocol-default-template-key "o")
