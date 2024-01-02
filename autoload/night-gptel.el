;;; night-gptel.el ---                               -*- lexical-binding: t; -*-
;;;
(after! (night-openai)
  ;; * @variables
  ;; ** [help:gptel-prompt-prefix-alist]
  ;; ** [help:gptel-proxy]
  ;;
  ;; * @functions
  ;; ** [help:gptel-set-topic]
;;;
  (use-package! gptel
    :config
    (setq! gptel-api-key (night/openai-key-get))
    (setq gptel-default-mode #'org-mode)

    ;; (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
    ;; (add-hook 'gptel-post-response-hook 'gptel-end-of-response)

    (map! :map gptel-mode-map
;;;
          ;; :gni
          ;; "M-RET" #'gptel-send
          ;; :gni
          ;; "M-<return>" #'gptel-send
;;;
          ;; shift-return is being overridden in org-mode insert mode by [help:+org/shift-return]
          :gni
          "S-RET" #'gptel-send
          :gni
          "S-<return>" #'gptel-send)))
;;;
