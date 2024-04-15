;;; autoload/night-ellama.el -*- lexical-binding: t; -*-

(require 'llm)
;; could be llm-openai for example
(require 'llm-ollama)
(require 'llm-openai)

;; (make-llm-openai :key my-openai-key)

;; YOU DON'T NEED NONE OF THIS CODE FOR SIMPLE INSTALL
;; IT IS AN EXAMPLE OF CUSTOMIZATION.
(require 'ellama)

;; setup key bindings
(setopt ellama-keymap-prefix "C-c e")
;; language you want ellama to translate to
(setopt ellama-language "English")
(setopt ellama-provider
	(make-llm-ollama
         :host "10.2.32.31"
	 ;; this model should be pulled to use it
	 ;; value should be the same as you print in terminal during pull
         ;;;
	 ;; :chat-model "codegemma:7b-code-q8_0"
	 ;; :embedding-model "codegemma:7b-code-q8_0"
         ;;;
	 ;; :chat-model "codegemma:7b-code"
	 ;; :embedding-model "codegemma:7b-code"
         ;;;
	 :chat-model "deepseek-coder:6.7b-base-q8_0"
	 :embedding-model "deepseek-coder:6.7b-base-q8_0"
         ))
(comment

 ;; Predefined llm providers for interactive switching.
 ;; You shouldn't add ollama providers here - it can be selected interactively
 ;; without it. It is just example.
 (setopt ellama-providers
	 '(("zephyr" . (make-llm-ollama
		        :chat-model "zephyr:7b-beta-q6_K"
		        :embedding-model "zephyr:7b-beta-q6_K"))
	   ("mistral" . (make-llm-ollama
			 :chat-model "mistral:7b-instruct-v0.2-q6_K"
			 :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
	   ("mixtral" . (make-llm-ollama
			 :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
			 :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))

 ;; Naming new sessions with llm
 (setopt ellama-naming-provider
	 (make-llm-ollama
	  :chat-model "mistral:7b-instruct-v0.2-q6_K"
	  :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
 (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
 ;; Translation llm provider
 (setopt ellama-translation-provider (make-llm-ollama
				      :chat-model "sskostyaev/openchat:8k"
				      :embedding-model "nomic-embed-text")))
