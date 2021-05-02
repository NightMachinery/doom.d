;;; autoload/night-telegram.el -*- lexical-binding: t; -*-

;;;
;; use `telega-kill` to restart to reload the proxy settings
(setq telega-proxies
      (list
       '(:server "127.0.0.1" :port 1080 :enable t
         :type (:@type "proxyTypeSocks5"
                :username "" :password ""))
       ))
;;;
;; telega-use-images should be non-nil for graphical Emacs

(setq telega-chat-bidi-display-reordering t)
