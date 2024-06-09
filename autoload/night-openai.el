;;; night-openai.el ---                              -*- lexical-binding: t; -*-
;;;
(defvar night/openai-key (z var-get "openai_api_key"))
(defun night/openai-key-get ()
  night/openai-key)

(defvar night/openrouter-key (z var-get "openrouter_api_key"))
(defun night/openrouter-key-get ()
  night/openrouter-key)

(defvar night/groq-key (z var-get "groq_api_key"))
(defun night/groq-key-get ()
  night/groq-key)

(defvar night/codestral-key (z var-get "codestral_api_key"))
(defun night/codestral-key-get ()
  night/codestral-key)
;;;
(provide 'night-openai)
;;; night-openai.el ends here
