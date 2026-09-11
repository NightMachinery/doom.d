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
(defconst night/proxy-env-vars
  '("http_proxy" "https_proxy" "ftp_proxy" "all_proxy" "no_proxy"
    "HTTP_PROXY" "HTTPS_PROXY" "FTP_PROXY" "ALL_PROXY" "NO_PROXY")
  "Environment variables that proxy-aware programs consult, in report order.

Both cases are listed deliberately.  curl reads the lowercase forms, some
tools read only the uppercase ones, and nothing keeps the two in step, so
a report showing one case could miss the variable actually doing the
damage.")

(defun night/h-proxy--value-string (value)
  "Render VALUE of a proxy variable for `night/proxy-status'.

Distinguishes unset from set-to-empty.  To curl an empty `http_proxy'
means \"send this request directly\" -- a configuration choice, not an
absence -- so collapsing the two would hide a real state."
  (cond
   ((null value) "<unset>")
   ((string-empty-p value) "<empty>")
   (t value)))

(defun night/h-proxy--group (entries)
  "Group ENTRIES, an alist of (VAR . VALUE), by shared VALUE.

Returns a list of (VALUE . VARS), each value in order of first
appearance.  The usual case is every proxy variable pointing at one
proxy, which collapses to a single line instead of six."
  (let ((groups nil))
    (dolist (entry entries)
      (let* ((value (cdr entry))
             (cell (assoc value groups)))
        (if cell
            (setcdr cell (cons (car entry) (cdr cell)))
          (push (cons value (list (car entry))) groups))))
    (mapcar (lambda (cell) (cons (car cell) (nreverse (cdr cell))))
            (nreverse groups))))

(defun night/proxy-status (&optional all)
  "Report the proxy environment variables of this Emacs process.

This reports the environment of *this process*, which a daemon froze at
the moment it was started and which does not track your shell
afterwards.  Turning a proxy off in a shell cannot reach an Emacs that
already exists, so the two drift apart silently: a daemon started from a
shell with a proxy active keeps sending every `plz' request through it
for the rest of its life.  That drift is what this command exists to
show.  `night/redis-reconnect' is the equivalent remedy for the other
piece of startup-frozen state.

A variable being set is NOT the same as the proxy being alive; this
command deliberately does not probe, so that it cannot hang.  To check
whether anything is actually listening:

  lsof -nP -iTCP:<port> -sTCP:LISTEN

A dead proxy looks distinctive from the caller's side: curl exits 7 in
0.0 seconds, because connecting to a closed local port is refused rather
than timing out.

Note that this uses plain `getenv' rather than `night/getenv-nonempty',
which the @warn in config.el asks for.  That warning is about
*overrides*; this is a report, and it must be able to tell an unset
variable from one set to the empty string.

Interactively, `message' the variables that are set, grouped by value.
With a prefix argument ALL, write every variable, set or not, to a
`*night/proxy-status*' buffer.  From Lisp, print nothing and return an
alist of (NAME . VALUE), VALUE being nil when unset."
  (interactive "P")
  (let* ((entries (mapcar (lambda (var) (cons var (getenv var)))
                          night/proxy-env-vars))
         (set-entries (cl-remove-if-not #'cdr entries)))
    (when (called-interactively-p 'interactive)
      (cond
       (all
        (with-output-to-temp-buffer "*night/proxy-status*"
          (princ (format "Proxy environment of this Emacs process (pid %d)\n"
                         (emacs-pid)))
          (princ "Frozen at process start; it does not follow your shell.\n\n")
          (dolist (entry entries)
            (princ (format "  %-12s %s\n"
                           (car entry)
                           (night/h-proxy--value-string (cdr entry)))))
          (princ "\nSet is not the same as alive.  Check a proxy with:\n")
          (princ "  lsof -nP -iTCP:<port> -sTCP:LISTEN\n")))
       (set-entries
        (message "%s"
                 (mapconcat
                  (lambda (group)
                    (format "%s=%s"
                            (string-join (cdr group) ",")
                            (night/h-proxy--value-string (car group))))
                  (night/h-proxy--group set-entries)
                  "\n")))
       (t
        (message "night/proxy-status: no proxy variables set in this Emacs"))))
    entries))
;;;
