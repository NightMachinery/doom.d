;;; autoload/night-vlf.el -*- lexical-binding: t; -*-

(require 'vlf-setup)

;; `ask', vlf's default, prompts when a file is over
;; `large-file-warning-threshold' (50MB, set in night-config.el). That prompt is
;; unanswerable when the open was driven non-interactively -- anything reaching
;; emacs through `server-eval-at', which is how the shell's `emc-open' works --
;; so it blocks for ever rather than failing. A 108MB org file sat on it for
;; three hours looking exactly like a hung job.
;;
;; `dont-ask' uses vlf for those files instead, which is the only practical way
;; to read one anyway: plain `find-file' on 108MB of org is not something emacs
;; finishes either. `emc-nowait2' in ~/scripts binds this as well, so a machine
;; whose doom config predates this change is still safe.
(setq vlf-application 'dont-ask)
;;;
(defun night/disable-font-lock ()
  (font-lock-mode -1)
  (setq-local font-lock-keywords nil))

(defun night/so-long ()
  "Disable font-lock and flycheck. See also `night/so-long-strong`."
  (interactive)
  (night/disable-font-lock)
  (night/disable-flycheck)
  ;; (linum-mode -1)
  )

(defun night/so-long-strong ()
  "A wrapper for `so-long`"
  (interactive)
  (so-long)
  (hl-line-mode)
)
;;;
