;;; ~/doom.d/autoload/night-proxy.el -*- lexical-binding: t; -*-

;; (setq url-proxy-services nil)
;;; I think you can just set the env vars instead. If not, uncomment this block:
(comment
 (progn (setq url-proxy-services
              '(("no_proxy" . "^\\(localhost\\|10.*\\)")
                ("http" . "127.0.0.1:1087")
                ("https" . "127.0.0.1:1087")))))


;;;
;; proxychains did not work for me on macOS
;;;
;; https://github.com/stardiviner/proxy-mode
;;;
;; https://github.com/junjiemars/.emacs.d/blob/cc/config/sockets.el
;; (setq url-gateway-method 'socks)
;; (setq socks-server '("Default server" "127.0.0.1" 1080 5))
;; (setq socks-noproxy '("127.0.0.1"))
