;;; autoload/night-external.el -*- lexical-binding: t; -*-
;;;
(require 'memoize)
;;;
(defun night/path-checkable-p (path)
  "Whether PATH is a local path Emacs may check for existence itself.

Excludes URLs, and paths starting with a zsh named directory (=~mu/...=,
registered via =hash -d=). `expand-file-name' silently mis-resolves the latter
as relative instead of erroring, so only zsh can resolve them; see the comment
in `night/org-link-zopen-follow'."
  (not (or (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*://" path)
           (string-match-p "\\`~[^/]" path))))
;;;
(defvar night/path-convert--counter 0
  "Serial number making `night/path-convert' keys unique within this session.")

(defun night/path-convert (fn path)
  "Run the zsh function FN over PATH and return its output.

;; @workaround for the lack of support of non-utf-8 in brish
PATH travels through redis rather than the brish command line, so filenames
that are not valid utf-8 survive the round trip.

The key is per-call. It used to be a single shared \"emacs_input\", which both
Emacs daemons and both conversion directions wrote to, so the last writer won:
when the write silently failed under NOAUTH, this returned some other
process's path entirely, and an audiofile: link played the wrong file. Do not
collapse this back into one key -- the checked write below catches a dead
connection, but nothing can recover a value another process has overwritten."
  (let ((key (format "emacs_input::%d::%d"
                     (emacs-pid)
                     (cl-incf night/path-convert--counter))))
    (unwind-protect
        (progn
          (night/redis-set key path)
          ;; In case we die before the cleanup below runs.
          (night/redis-expire key 60)
          (let ((result (z eval (concat fn " \"$(redism get " key ")\""))))
            (if (and (stringp result) (not (string-empty-p result)))
                result
              (error "night/path-convert: %s returned nothing for: %s" fn path))))
      (ignore-errors (eredis-del key)))))

(defun night/path-unabbrev (path)
  (night/path-convert "path-unabbrev" path))
(comment
 (night/path-unabbrev "~mu/hi.mp3"))

(defun night/path-abbrev (path)
  (night/path-convert "path-abbrev" path))
(comment
 (night/path-abbrev "/Users/evar/my-music/hi.mp3"))

(defun night/path-abbrev-memoized (&rest args)
  (apply #'night/path-abbrev args))

;; `memoize' refuses to wrap an already-wrapped function, which made a plain
;; re-load of this file die here, half applied. The `defun' above has already
;; put the unmemoized definition back in the function cell, so clearing the
;; bookkeeping is enough -- and re-wrapping drops the cache, which is what you
;; want after changing how the value is computed.
(when (get 'night/path-abbrev-memoized :memoize-original-function)
  (put 'night/path-abbrev-memoized :memoize-original-function nil)
  (put 'night/path-abbrev-memoized 'function-documentation nil))
;; Safe to cache this long only because `night/path-convert' now signals on
;; failure instead of returning someone else's path, and `memoize' does not
;; cache a call that signalled.
(memoize #'night/path-abbrev-memoized "9999 hours")
;;;
(defun night/ensure-dir (path)
  (when (zb ensure-dir (identity path))
    path))
;;;
(defconst night/hs-alert-options
  '((:dur . "dur")
    (:flash . "flash")
    (:pos . "pos")
    (:id . "id")
    (:markup . "markup")
    (:color . "color"))
  "Maps `night/hs-alert' keywords to the knobs of zsh's =hs-alert-v2=.

The zsh side reads these as =alert_dur=, =alert_flash=, ... ; see the doc
comment above =hs-alert-v2= in =$NIGHTDIR/zshlang/auto-load/others/hammerspoon.zsh=.")

(cl-defun night/hs-alert (msg &key dur flash pos id markup color)
  "Show MSG as a Hammerspoon alert, asynchronously.

MSG may be long or multi-line; the zsh side passes it through a file, so it
does not need escaping.

The keywords cover the whole surface of =hs-alert-v2=. Any left out keeps the
zsh-side default:

  :DUR     seconds on screen (default 5)
  :FLASH   fullscreen flash before settling, in seconds; 0 skips it
  :POS     top (default), center, or bottom
  :ID      reusing an id updates that alert in place instead of stacking
  :MARKUP  plain (default) or md
  :COLOR   band colour by name: default, warn, amber, crit, agent, free

Asynchronous on purpose: a wedged Hammerspoon must never block Emacs."
;;; @tests
  (comment
   (night/hs-alert "hello")
   (night/hs-alert "**bold** and [red]{red bold}" :markup "md" :color "warn" :dur 10))
;;;
  (night/brishz-ll
   :name "night/hs-alert"
   :callback t
   :command (night/hs-alert-command
             msg :dur dur :flash flash :pos pos
             :id id :markup markup :color color)))

(cl-defun night/hs-alert-command (msg &key dur flash pos id markup color)
  "Build the brish command line `night/hs-alert' dispatches. See it for MSG and the keywords."
  (let ((opts '()))
    (dolist (cell night/hs-alert-options)
      (let ((val (cl-case (car cell)
                   (:dur dur) (:flash flash) (:pos pos)
                   (:id id) (:markup markup) (:color color))))
        (when val
          (setq opts (append opts (list (cdr cell) (format "%s" val)))))))
    ;; The knobs travel through zsh's `@opts', which is how the rest of the
    ;; codebase sets them. `hs-alert' has its `@opts' prefix pinned to `alert'
    ;; in hammerspoon.zsh; without that pin the dash in the name would silently
    ;; misroute every option. With no knobs at all we skip `@opts' entirely.
    (if opts
        (append (list "@opts") opts (list "@" "hs-alert" msg))
      (list "hs-alert" msg))))
;;;
