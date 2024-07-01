;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;; this needs to be first, otherwise any error in our config will reset recentf
(require 'recentf)
(after! recentf
;;;
  ;; (customize-set-variable 'recentf-auto-cleanup 3600)
  ;; Cleanup each time Emacs has been idle that number of seconds.

  (customize-set-variable 'recentf-auto-cleanup 'never)
  ;; We do the cleanup ourselves.
;;;

  (setq recentf-max-saved-items 50000)
  (recentf-load-list)

  ;; @seeAlso [[./autoload/night-recentf.el]]
  )
;;;
(defmacro mycomment (&rest a)
  t)
(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)
;;;
(defun night/nop (&rest dummy))
;;;
(message "TERM: %s" (getenv "TERM"))
;;;
(require 'cl-extra)
(require 's)
(require 'f)
(require 'server)

(setq server-socket-dir (concat (getenv "HOME") "/tmp/.emacs-servers"))
;; This directory is created by [help:server-ensure-safe-dir] automatically.
;; see also `server-name`

(defun night/server-name-set-auto ()
  (interactive)
  (setq server-name (let ((sn (getenv "emacs_night_server_name")))
                      (cond
                       ((and sn (not (cl-equalp sn "")))
                        sn)
                       (t "server"))))
  server-name
;;;
  ;; (or
  ;;                         (and (boundp 'server-name) server-name (not (cl-equalp server-name "")) server-name)
  ;;                         (getenv "EMACS_SERVER_NAME")
  ;;                         (getenv "EMACS_SERVER_NAME"))
  )

(night/server-name-set-auto)
(defun night/server-alt1-p ()
  (interactive)
  (and (boundp 'server-name)
       (s-ends-with-p "_alt1" server-name)))

(defun night/darwin-p ()
  (interactive)
  (cl-equalp (symbol-name system-type) "darwin"))

(defun night/local-p ()
  (interactive)
  (or
   (night/darwin-p)))

(defun night/remote-p ()
  (interactive)
  (not (night/local-p)))


(defun night/pino-p ()
  (interactive)
  (cl-equalp (system-name) "Pinocchio"))

(defun night/t31-p ()
  (interactive)
  (cl-equalp (system-name) "gpu13"))

(defun night/m17-p ()
  (interactive)
  (cl-equalp (system-name) "sharif"))

(defun night/c0-p ()
  (interactive)
  (cl-equalp (system-name) "Taher"))

(defun night/system-name ()
  (cond
   ;; ((night/local-p) "Local")
   ((night/pino-p) "Pino")
   ((night/t31-p) "T31")
   (t (system-name))))

(when (not (server-running-p))
  (cond
   ((night/server-alt1-p)
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
;; (setq doom-font (font-spec :family "monospace" :size 29))
(setq doom-font (font-spec :family "Fira Mono" :size 29))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(progn
  (require 'doom-themes-ext-org)        ;; @workaround for upstream bugs

  (require 'solarized-theme)
  (require 'humanoid-themes)
  (require 'kaolin-themes)
  ;; (require 'spacemacs-theme)
  (require 'apropospriate-theme)
  (require 'darktooth-theme)
  (require 'rebecca-theme)
  (require 'solo-jazz-theme)
  (require 'github-theme)
  (require 'night-owl-theme)
  (require 'colorless-themes)
  (require 'leuven-theme)
  (require 'base16-theme)
  (require 'anti-zenburn-theme)
  (require 'moe-theme)
  (require 'poet-theme)
  (require 'zaiste-theme)
  (require 'modus-themes)
  (require 'ef-themes)
  (require 'material-theme)
  (require 'tao-theme)
  (require 'organic-green-theme)
  (require 'color-theme-sanityinc-tomorrow))
(cond
 ((night/pino-p)
  (setq night-theme
        'modus-operandi-deuteranopia
        ))
 ((night/t31-p)
  (setq night-theme
        'modus-operandi-tinted
        ))
 ((night/m17-p)
  (setq night-theme
        'sanityinc-tomorrow-day
        ))
 ((night/c0-p)
  (setq night-theme
        'ef-cyprus
        ))
 (;; nil
  (night/server-alt1-p)
  ;; (setq night-theme 'doom-one-light)
  ;; [[id:c0713162-d1bd-46fc-9ef4-f5495d7ff16f][doom/bugs, issues:@upstreamBug hlissner/doom-emacs#5629 {BUG} Some themes fail to build]]


  (setq night-theme 'solarized-selenized-white)
  ;; (setq night-theme 'tsdh-light)
  ;; (setq night-theme 'solarized-light)
  )
 ((display-graphic-p)
  (setq night-theme
        'modus-operandi-tritanopia
        ;; 'kaolin-light
        ;; 'solarized-selenized-white
        ;; 'doom-one-light
        ;; 'humanoid-light
        ;; 'doom-nord-light (has some bad color choices for orgmode)
        ;; 'doom-ayu-light
        ;; 'doom-one-light
        ))
 (t
  (setq night-theme
        'modus-operandi-tritanopia
        ;; 'kaolin-light
        )

  ;; (setq night-theme 'solarized-light)
  ;; (setq night-theme 'solarized-selenized-light) ;; @good
  ;; (setq night-theme 'doom-solarized-light) ; subtly different
  ))
(setq doom-theme night-theme)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (concat (getenv "cellar") "/notes/org"))

(setq night/roam-p nil)

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
