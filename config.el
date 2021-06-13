;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;; this needs to be first, otherwise any error in our config will reset recentf
(require 'recentf)
(after! recentf
  ;; (customize-set-value 'recentf-auto-cleanup 3600) ; doesn't work

  (customize-set-variable 'recentf-auto-cleanup 3600)
  (setq recentf-auto-cleanup 3600)
  ;; (customize-set-variable 'recentf-auto-cleanup 'never)
  ;; (setq recentf-auto-cleanup 'never)

  (setq recentf-max-saved-items 50000)
  (recentf-load-list)
  )
;;;
(defmacro mycomment (&rest a)
  t)
(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)
;;;
(require 's)
(require 'server)

(setq server-socket-dir (concat (getenv "HOME") "/tmp/.emacs-servers"))
;; see also `server-name`

(defun night/server-name-set-auto ()
  (interactive)
  (setq server-name (let ((sn (getenv "emacs_night_server_name")))
                      (cond
                       ((and sn (not (equalp sn "")))
                        sn)
                       (t "server"))))
  server-name
  ;;;
  ;; (or
  ;;                         (and (boundp 'server-name) server-name (not (equalp server-name "")) server-name)
  ;;                         (getenv "EMACS_SERVER_NAME")
  ;;                         (getenv "EMACS_SERVER_NAME"))
  )

(night/server-name-set-auto)
(when (not (server-running-p))
  (cond
   ((s-ends-with-p "_alt1" server-name)
    ;; (night/unadvice #'ivy--switch-buffer-action)
    (advice-add #'ivy--switch-buffer-action :after #'night/irc-maybe-show-count-ni))
   ;; @note +irc--ivy-switch-to-buffer-action will also call ivy--switch-buffer-action
   )
  (server-start))

;;;
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Fereidoon"
      user-mail-address "rudiwillalwaysloveyou@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 29))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(cond
 ((s-ends-with-p "_alt1" server-name)
  (setq night-theme 'doom-nord-light))
 (t
  (setq night-theme 'solarized-light)
  ;; (setq night-theme 'doom-solarized-light) ; subtly different
  ))
(setq doom-theme night-theme)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (concat (getenv "cellar") "/notes/org"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; #spacemacs config imported
(setq evil-respect-visual-line-mode nil)
(load! "night-loader.el")
