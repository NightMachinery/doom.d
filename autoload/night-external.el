;;; autoload/night-external.el -*- lexical-binding: t; -*-
;;;
(require 'memoize)
;;;
(defun night/path-unabbrev (path)
;;;
  ;; @workaround for the lack of support of non-utf-8 in brish
  (eredis-set "emacs_input" path)
  (z eval (concat "path-unabbrev \"$(redism get emacs_input)\""))
;;;
  ;; (z path-unabbrev (identity path))
;;;
  )
(comment
 (night/path-unabbrev "~mu/hi.mp3"))

(defun night/path-abbrev (path)
;;;
  ;; @workaround for the lack of support of non-utf-8 in brish
  (eredis-set "emacs_input" path)
  (z eval (concat "path-abbrev \"$(redism get emacs_input)\"")))
(comment
 (night/path-abbrev "/Users/evar/my-music/hi.mp3"))

(defun night/path-abbrev-memoized (&rest args)
  (apply #'night/path-abbrev args))

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
