;;; ~/doom.d/night-workarounds.el -*- lexical-binding: t; -*-

(remove-hook 'doom-load-theme-hook #'solaire-global-mode)
(after! solaire-mode (progn
                       (turn-off-solaire-mode)
                       (solaire-global-mode -1)))
(comment (progn
  (spacemacs/toggle-smartparens-globally-off)
  (remove-hook 'prog-mode-hook #'smartparens-mode)
  )) ;; This is the shit that keeps messing up the delimiters.

;;; Minibuffer bug
;; (setq evil-search-module 'isearch)    ; no longer seems to work

(defun kill-minibuffer ()
  (interactive)
  (when (windowp (active-minibuffer-window))
    (evil-ex-search-exit)))

(add-hook 'mouse-leave-buffer-hook #'kill-minibuffer)
;;;
(defun night/workarounds ()
  (interactive)
  (cancel-function-timers 'auto-revert-buffers))
