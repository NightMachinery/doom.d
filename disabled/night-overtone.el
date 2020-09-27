;;; ~/doom.d/night-overtone.el -*- lexical-binding: t; -*-

(defun my/setup-overtone-hotkeys ()
  (interactive "")
  (define-key evil-normal-state-map (kbd "] '") '(lambda () (interactive "")
                                                   (cider-nrepl-sync-request:eval  "(overtone.live/stop)")))
  (define-key evil-normal-state-map (kbd "] r") '(lambda () (interactive "")
                                                   (cider-nrepl-sync-request:eval  "(overtone.live/recording-stop)")))
  (define-key evil-normal-state-map (kbd "[ r") '(lambda (file-name) (interactive "sSave to:")
                                                   (cider-nrepl-sync-request:eval  (concat "(overtone.live/recording-start \"/Base/- Art/Audio/i-laugh/" file-name ".wav\")")))))
