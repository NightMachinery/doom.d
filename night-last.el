;;; night-last.el -*- lexical-binding: t; -*-

(after! yasnippet (yas-reload-all))
;;;
;; (add-hook 'after-init-hook #'night/load-tramp-theme)
;; (add-hook 'doom-init-ui-hook #'night/load-tramp-theme)
;; (add-hook 'doom-customize-theme-hook #'night/load-tramp-theme)

(add-hook 'doom-load-theme-hook #'night/load-tramp-theme) ; works ^_^
;; (night/load-tramp-theme) ; doesn't work

(defun night/terminfo-set ()
  (interactive)
  ;; https://emacs.stackexchange.com/questions/38597/why-does-emacs-getenv-term-return-incorrect-data
  (when (equalp (getenv "TERM")
                      "dumb"
                      )
    ;; (not (equalp (getenv "TERM")
    ;;                  (ignore-errors (z eval "echo $TERM"))
    ;;                  ))

      ;; (setenv "TERM" "xterm-emacs")
    (setenv "TERM" (getenv-internal "TERM" initial-environment))
    ))

(add-hook 'doom-load-theme-hook #'night/terminfo-set)
;;;
(defun night/company-keybindings-enable ()
  (interactive)
  ;; This TAB key was buggy on my old eOS machine and the new M1 (perhaps emacs@28 is to blame?). In the end, I bound a these to `global-map' for `night/company-yasnippet-or-completion', too.
  ;; Update: Using the below 'map!' snippet seems to have solved that problem.
  ;; Update: I think after changing ="\t"=  to =(kbd "<tab>")=, the below snippet would not be necessary after all!
  (map!
   :map company-active-map
   :ig
   "TAB" #'night/company-yasnippet-or-completion
   "<tab>" #'night/company-yasnippet-or-completion
   "\t" #'night/company-yasnippet-or-completion))
(night/company-keybindings-enable)
(after! (org evil company)              ;; @workaround sth is overriding our keybindings, so I am using trial-and-error to re-enable the keybindings
  (night/company-keybindings-enable))
;;;
(defvar night-loaded t "Use night/loaded-p to check this. Shows if our whole config has loaded completely.")
