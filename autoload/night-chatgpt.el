;;; autoload/night-chatgpt.el -*- lexical-binding: t; -*-

(after! (org)
  (use-package chatgpt-shell
    :ensure t)
  (setq chatgpt-shell-openai-key (z var-get "openai_api_key"))
  (setq dall-e-shell-openai-key chatgpt-shell-openai-key)
  (require 'ob-chatgpt-shell)
  (ob-chatgpt-shell-setup)
;;;
  (defun night/h-before-chatgpt-shell-clear-buffer (&rest dummy)
    (chatgpt-shell-interrupt nil))

  (advice-add #'chatgpt-shell-clear-buffer :before #'night/h-before-chatgpt-shell-clear-buffer)
;;;
  (map! :map chatgpt-shell-mode-map
        ;; :leader
        ;; "br" #'chatgpt-shell-clear-buffer
        ;; This overrides all maps, not just =chatgpt-shell-mode-map=.
;;;
        :gni
        "s-k" #'chatgpt-shell-clear-buffer
        "s-i" #'chatgpt-shell-interrupt
        "s-m" #'chatgpt-shell-swap-model-version
        :localleader
        "i" #'chatgpt-shell-interrupt
        "k" #'chatgpt-shell-clear-buffer
        "m" #'chatgpt-shell-swap-model-version
        "p" #'chatgpt-shell-swap-system-prompt
        "s" #'chatgpt-shell-save-session-transcript
        ;; =chatgpt-shell-save-session-transcript= is almost the same as =write-file=.
        "y" #'night/copy-buffer-content
        "l" #'chatgpt-shell-restore-session-from-transcript
        )
  (map! :leader
        "zx" #'chatgpt-shell)
;;;
  )
