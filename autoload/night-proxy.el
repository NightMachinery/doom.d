;;; ~/doom.d/autoload/night-proxy.el -*- lexical-binding: t; -*-

;; (setq url-proxy-services nil)
;;; I think you can just set the env vars instead. If not, uncomment this block:
(comment
 (getenv "http_proxy")

 (progn (setq url-proxy-services
              '(("no_proxy" . "^\\(localhost\\|10.*\\|213.233.184.221\\)")
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
;;;
(defun night/myip-amazon ()
  "Fetch the current IP address from Amazon's checkip service."
  (interactive)
  (let* ((url "https://checkip.amazonaws.com")
         (url-request-method "GET")
         (response-buffer (url-retrieve-synchronously url)))
    (if response-buffer
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (if (re-search-forward "\n\n" nil t)
              (let ((ip (buffer-substring-no-properties (point) (point-at-eol))))
                (kill-buffer response-buffer)

                (if (called-interactively-p 'interactive)
                    (message "Your IP is: %s" ip))

                ip)
            (kill-buffer response-buffer)
            (error "Failed to retrieve IP address")))
      (error "Failed to connect to checkip service"))))
;;;
