;;; autoload/night-copilot.el -*- lexical-binding: t; -*-
;;;
;;;
(after! copilot
  (defun night/h-copilot-clear-overlay ()
    "Like `copilot-clear-overlay', but returns `t' if the overlay was visible."
    (when (copilot--overlay-visible)
      (copilot-clear-overlay) t))
  (add-hook 'doom-escape-hook #'night/h-copilot-clear-overlay)

  (map!
   "s-'" #'copilot-complete
   :leader
   "cc" #'copilot-panel-complete
   "cv" #'copilot-next-completion
   "cx" #'copilot-accept-completion)
  (map! :map copilot-completion-map

        "s-<right>" #'copilot-accept-completion
        ;; ("<tab>" . 'copilot-accept-completion)
        ;; ("TAB" . 'copilot-accept-completion)

        "M-s-<right>" #'copilot-accept-completion-by-word
        ;; ("C-TAB" . 'copilot-accept-completion-by-word)
        ;; ("C-<tab>" . 'copilot-accept-completion-by-word)

        "M-s-<up>" #'copilot-previous-completion
        "M-s-<down>" #'copilot-next-completion
        ))
