;;; ~/doom.d/autoload/night-unimpaired.el -*- lexical-binding: t; -*-
;;; https://github.com/hlissner/doom-emacs/blob/develop/modules/editor/evil/config.el#L425-L472

;;;
(defun evil-unimpaired/paste-above ()
  (interactive)
  (setq this-command 'evil-paste-after)
  (evil-insert-newline-above)
  (evil-paste-after 1))

(defun evil-unimpaired/paste-below ()
  (interactive)
  (setq this-command 'evil-paste-after)
  (evil-insert-newline-below)
  (evil-paste-after 1))

;;;
;; =g t=, =g T= for navigating tabs
;; (define-key evil-normal-state-map (kbd "[ o") 'evil-unimpaired/next-frame)
;; (define-key evil-normal-state-map (kbd "] o") 'evil-unimpaired/previous-frame)
(define-key evil-normal-state-map (kbd "[ p") 'evil-unimpaired/paste-above)
(define-key evil-normal-state-map (kbd "] p") 'evil-unimpaired/paste-below)
(define-key evil-normal-state-map (kbd "[ j") 'evil-jump-backward)
(define-key evil-normal-state-map (kbd "] j") 'evil-jump-forward)
(define-key evil-normal-state-map (kbd "[ e") 'move-text-up)
(define-key evil-normal-state-map (kbd "] e") 'move-text-down)
(define-key evil-normal-state-map (kbd "[ q") 'previous-error)
(define-key evil-normal-state-map (kbd "] q") 'next-error)

;;;
