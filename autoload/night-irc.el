;;; autoload/night-irc.el -*- lexical-binding: t; -*-

;; if you omit =:host=, ~SERVER~ will be used instead.
(after! circe
  (set-irc-server!
   "irc.libera.chat"
   ;; "chat.freenode.net"

   `(:tls t
     :port 6697
     :nick ,(getenv "IRC_NICK")
     :sasl-username ,(getenv "IRC_USER")
     :sasl-password ,(getenv "IRC_PASS")
     :channels (
                "#emacs"
                ;; "#debian" "#archlinux" "#cafelinux"
                ;; "#git"
                ;; "#docker"
                ;; "#ffmpeg"
                ;; "#commonlisp" "#lispgames" "#slime"
                ;; "#go-nuts"
                ;; "#bash"
                ;; "##programming"
                ;; "#datahoarder"
                ;; "##math"
                ;; "##chat"
                ;; "#libera"
                ;; "##philosophy"
                ;; "#fsf"
                ;; "##news"
                ;; "#anime"
                ;; "#bitcoin" "##crypto"
                ;; "##politics"
                ))))
