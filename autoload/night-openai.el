;;; night-openai.el ---                              -*- lexical-binding: t; -*-
;;;
(require 'subr-x)
(declare-function night/brishz "night-brish")

(defun night/h-api-key-get (name)
  "Return optional shell credential NAME, or nil when it is unset."
  (condition-case nil
      (let ((key (night/brishz "var-get" name)))
        (and (stringp key) (not (string-empty-p key)) key))
    (end-of-file nil)))

(defvar night/openai-key (night/h-api-key-get "openai_api_key"))
(defun night/openai-key-get ()
  night/openai-key)

(defvar night/openrouter-key (night/h-api-key-get "openrouter_api_key"))
(defun night/openrouter-key-get ()
  night/openrouter-key)

(defvar night/groq-key (night/h-api-key-get "groq_api_key"))
(defun night/groq-key-get ()
  night/groq-key)

;; gemini-key
(defvar night/gemini-key (night/h-api-key-get "gemini_api_key"))
(defun night/gemini-key-get ()
  night/gemini-key)

(defvar night/codestral-key (night/h-api-key-get "codestral_api_key"))
(defun night/codestral-key-get ()
  night/codestral-key)

(defvar night/deepseek-key (night/h-api-key-get "deepseek_api_key"))
(defun night/deepseek-key-get ()
  night/deepseek-key)
;;;
(provide 'night-openai)
;;; night-openai.el ends here
