;;; autoload/night-copilot.el -*- lexical-binding: t; -*-
;;;
;;;
(require 'copilot)
(after! (org copilot)
;;;
  (defun night/copilot-overlay-disable ()
    (interactive)
    (setq copilot-idle-delay nil)
    ;; Complete immediately if set to 0.
    ;; Disable idle completion if set to nil.
    (message "Copilot overlays disabled.")
    )

  (defun night/copilot-overlay-enable ()
    (interactive)
    (setq copilot-idle-delay 0)
    (message "Copilot overlays enabled!"))

  (defun night/copilot-overlay-toggle ()
    (interactive)
    (cond
     ((null copilot-idle-delay)
      (night/copilot-overlay-enable))
     (t
      (night/copilot-overlay-disable))))
;;;
  (defun night/h-copilot-clear-overlay ()
    "Like `copilot-clear-overlay', but returns `t' if the overlay was visible."
    (when (copilot--overlay-visible)
      (copilot-clear-overlay) t))
  (add-hook 'doom-escape-hook #'night/h-copilot-clear-overlay)
;;;
  (defun night/h-copilot-active-p ()
    "Non-nil if `copilot-mode' is actually running in this buffer.

Not the same thing as the variable being set.  A file-local
`copilot-mode: t' -- and that sits in `safe-local-variable-values' --
sets the variable without running the minor mode body, so no hooks are
installed and nothing is synced to the agent, while `bound-and-true-p'
happily says yes.  What settles it is whether `copilot--mode-setup'
actually put its hook on this buffer."
    (and (bound-and-true-p copilot-mode)
         (memq #'copilot--post-command post-command-hook)
         t))

  ;; The one private copilot symbol read here.  Say so at load if an upgrade
  ;; renames it, rather than letting the predicate quietly answer nil forever.
  (unless (fboundp 'copilot--post-command)
    (display-warning
     'night/copilot
     "copilot--post-command is gone; night/h-copilot-active-p needs updating"))

  (defun night/copilot-ensure (&rest _)
    (interactive)
    (unless (night/h-copilot-active-p)
      (copilot-mode 1)
      (night/copilot-overlay-disable)))

  (advice-add 'copilot-complete :before #'night/copilot-ensure)
;;;
  (defun night/h-copilot--enabling-p (arg)
    "Non-nil if calling `copilot-mode' with ARG would turn it on.

Mirrors the `cond' `define-minor-mode' generates, measured rather than
assumed: `toggle' flips, a number below 1 disables, anything else --
including nil and an omitted argument -- enables."
    (cond
     ((eq arg 'toggle) (not (bound-and-true-p copilot-mode)))
     ((and (numberp arg) (< arg 1)) nil)
     (t t)))

  (defun night/h-copilot-gate (orig-fn &rest args)
    "Ask `night/llm-path-policy' before `copilot-mode' turns on.

Copilot sends the buffer when the mode is *enabled*, not when a
completion is asked for: the last form of `copilot--mode-setup' is a
didOpen carrying `copilot--get-source', and every later edit streams a
didChange delta.  A completion request itself carries only a position and
a path.  So this is the only place worth asking -- by the time you press
`C-.' the file has already gone.

Note that `copilot-disable-predicates' is *not* that place, despite
looking like the supported hook: it is consulted only in
`copilot--post-command-debounce', which decides whether to request a
completion, and never touches the sync.

`buffer' is the only scope offered, because it is the only one Copilot
can honour; the chooser therefore reduces to a confirmation that names
the size of what would be sent."
    (cond
     ((or (night/h-copilot-active-p)
          (not (night/h-copilot--enabling-p (car args))))
      (apply orig-fn args))
     ((night/h-llm--gate-scope :label "copilot" :scopes '(buffer))
      (apply orig-fn args))
     (t nil)))

  (advice-add 'copilot-mode :around #'night/h-copilot-gate)
;;;
  (map!
   :nig
   "C-." #'copilot-complete
   ;; "s-'" #'copilot-complete
   )
  (map!
   :leader
   "co" #'copilot-mode
   "c RET" #'night/copilot-overlay-toggle
   "cc" #'copilot-panel-complete
   "cv" #'copilot-next-completion
   "cx" #'copilot-previous-completion
   ;; "cx" #'copilot-accept-completion
   )
  (map! :map copilot-completion-map

        "C-<right>" #'copilot-accept-completion
        ;; "s-<right>" #'copilot-accept-completion
        ;; ("<tab>" . 'copilot-accept-completion)
        ;; ("TAB" . 'copilot-accept-completion)

        "M-C-<right>" #'copilot-accept-completion-by-word
        ;; "M-s-<right>" #'copilot-accept-completion-by-word
        ;; ("C-TAB" . 'copilot-accept-completion-by-word)
        ;; ("C-<tab>" . 'copilot-accept-completion-by-word)

        "M-C-<up>" #'copilot-previous-completion
        "M-C-<down>" #'copilot-next-completion
        "C-<left>" #'copilot-next-completion
        ;; "M-s-<up>" #'copilot-previous-completion
        ;; "M-s-<down>" #'copilot-next-completion
        ))
