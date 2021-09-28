;;; autoload/night-irc.el -*- lexical-binding: t; -*-
;; - Use `=irc` to connect
;; - =+irc/ivy-jump-to-channel=
;;;
;; if you omit =:host=, ~SERVER~ will be used instead.
(after! (circe doom-modeline-core)
  (require 'circe-notifications)

  (setq doom-modeline-irc nil)

  (setq circe-server-send-unknown-command-p t)

  (setq lui-max-buffer-size 40000)     ;; 40000 chars (about 600 lines worth in most channels)
  (setq +irc-defer-notifications 1800)   ;; @futureCron do we need to increase this?
  (remove-hook 'circe-server-connected-hook #'+irc-init-circe-notifications-h) ;; completely disables the notifications, as we are getting nootifs via znc-push and thelounge

  (defun night/irc-set-watch-strings ()
    (when (not (boundp 'circe-notifications-watch-strings))
      ;; (setq circe-notifications-watch-strings '())
      (user-error! "night/irc-set-watch-strings: circe-notifications not loaded")
      )

    (dolist (i (list
                (getenv "ZNC_USER")
                ;; (getenv "IRC_USER") ;; triggered on all of my messages
                ))
      (add-to-list 'circe-notifications-watch-strings i)))

  (night/irc-set-watch-strings)

  (set-irc-server!

   "irc.zii.lilf.ir"
   ;; "irc.libera.chat"
   ;; "chat.freenode.net"

   `(:tls t
     :port 6635
     ;; :port 6697

     :user ,(getenv "ZNC_USER")
     :pass ,(getenv "ZNC_PASS")
     ;; :user ,(getenv "IRC_USER")
     ;; :pass ,(getenv "IRC_PASS")



     :nick ,(getenv "ZNC_USER")
     :sasl-username ,(getenv "ZNC_USER")
     :sasl-password ,(getenv "ZNC_IRC_PASS")

     ;; :nick ,(getenv "IRC_NICK")
     ;; :sasl-username ,(getenv "IRC_USER")
     ;; :sasl-password ,(getenv "IRC_PASS")
     :channels (
                "##anime"
                "##english"
                "#bash"
                "#clojure"
                "#commonlisp" "#lispcafe" "#quicklisp" "#clschool" "#lispgames" "#slime" "#lispweb"
                "#julia"
                "#css"
                "#devops"
                "#docker"
                "#emacs"
                "#emacsconf"
                "#evil-mode"
                "#emacs-circe" "#znc"
                "#fanfiction"
                "#git"
                "#go-nuts"
                "#html"
                "#javascript"
                "#lesswrong"
                "#lisp"
                "#lw-prog"
                "#nim"
                "#org-mode"
                "#org-roam"
                "#organice"             ;; @Matrix/#organice:matrix.org
                "#python" "#django"
                "#reddit-sysadmin"
                "#slime"
                "#thelounge" "#thelounge-offtopic"
                "#ubuntu" "#linux" "#debian" "#archlinux" "#cafelinux"
                "#vim"
                "#zsh" "#bash"
                "#security"
                "#lobsters" "##crustaceans"
                "#regex"
                "#perl" "#raku"
                "#ruby" "#jruby"
                "#erlang" "#elixir"
                "#php"
                "#lua"
                "#awk"
                "#sed"
                "#pandoc"
;;;
                "#git"
                "#docker"
                "#ffmpeg"
                "##programming"
                "#datahoarder"
                "##math"
                "##chat"
                "#libera"
                "##philosophy"
                "#fsf"
                ;; "##news"
                "#bitcoin" "##crypto"
                "##politics"
                )))

;;;
  (defun night/irc-maybe-show-count ()
    (interactive)
    (night/irc-maybe-show-count-ni))
  (defun night/irc-maybe-show-count-ni (&rest args)
    (when (member major-mode (list  'circe-chat-mode 'circe-channel-mode))
      (night/circe-count-users)))
;;;
  (defun night/swiper-irc (&rest args)
    (interactive)
    (letf! ((defun swiper-all-buffer-p (b)
              (s-prefix-p "#" (buffer-name b))))
      (let (
            (ivy--regex-function 'ivy--regex-plus) ;; @update this is useless, update =ivy-re-builders-alist=
            )
        (apply #'swiper-all args))))

  (defun night/swiper-irc-me ()
    (interactive)
    (night/swiper-irc
     ;; (concat (getenv "ZNC_USER") "|" (getenv "IRC_USER")) ;; see why this doesn't work with =ivy--regex-plus= ?
     (let ((me (concat "\\(" (getenv "ZNC_USER") "\\|" (getenv "IRC_USER") "\\)")))
       (concat me " !\\(^[[:space:]]*\\*+\\|^[[:space:]]*" me "\\)"))))
;;;
  (defun night/circe-count-users ()
    (interactive)
    (when (eq major-mode 'circe-channel-mode)
      (message "%i entities are online on %s." (length (circe-channel-nicks)) (buffer-name))))
;;;
  (add-hook 'circe-mode-hook #'night/disable-flycheck) ;; used resources

  ;; I am also using `company-global-modes' to disable it.
  (add-hook 'circe-mode-hook #'night/disable-company) ;; used a LOT of resources
  ;; (add-hook 'circe-mode-hook #'night/disable-company-frontends)

  ;; (add-hook 'circe-mode-hook #'night/circe-count-users)

  (enable-circe-color-nicks)
;;;
  (map! :map circe-mode-map
        :localleader
        "s" #'night/swiper-irc-me)
  (map! :map circe-mode-map
        :n
        "<RET>" #'lui-send-input)
;;;
  ;; (=irc)
  )
;;; @keybindings
(night/set-leader-keys "z i" #'+irc/ivy-jump-to-channel)
;;;
