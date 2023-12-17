;;; autoload/night-copilot.el -*- lexical-binding: t; -*-
;;;
;;;
(require 'copilot)
(after! (org copilot)
  (defun night/h-copilot-clear-overlay ()
    "Like `copilot-clear-overlay', but returns `t' if the overlay was visible."
    (when (copilot--overlay-visible)
      (copilot-clear-overlay) t))
  (add-hook 'doom-escape-hook #'night/h-copilot-clear-overlay)

  (map!
   :nig
   "C-." #'copilot-complete
   ;; "s-'" #'copilot-complete
   )
  (map!
   :leader
   "co" #'copilot-mode
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
