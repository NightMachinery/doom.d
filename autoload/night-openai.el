;;; night-openai.el ---                              -*- lexical-binding: t; -*-
;;;

(defvar night/openai-key (z var-get "openai_api_key"))
(defun night/openai-key-get ()
  night/openai-key)

;;;
(provide 'night-openai)
;;; night-openai.el ends here
