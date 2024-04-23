;;; autoload/night-ellama.el -*- lexical-binding: t; -*-

(require 'llm)
;; could be llm-openai for example
(require 'llm-ollama)
(require 'llm-openai)
(require 'ellama)

(setq llm-warn-on-nonfree nil)
(setq llm-log t)

(setopt ellama-language "English")
;; language you want ellama to translate to
(after! (night-openai)
;;;
  ;; * Ellama Providers
  (progn
    (setopt ellama-provider
            (make-llm-openai-compatible
             :key (night/groq-key-get)
             :url "https://api.groq.com/openai/v1"
;;;
             ;; :chat-model "microsoft/wizardlm-2-8x22b"
             ;; :embedding-model "microsoft/wizardlm-2-8x22b"
;;;
             ;; :chat-model "mistralai/mixtral-8x22b"
             ;; :embedding-model "mistralai/mixtral-8x22b"
;;;
             :chat-model "llama3-70b-8192"
             :embedding-model "llama3-70b-8192"
;;;
             )))
  (comment
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
            )))
  (comment
   (setopt ellama-provider
	   (make-llm-openai-compatible
            :key (night/openrouter-key-get)
            :url "https://openrouter.ai/api/v1"
;;;
	    ;; :chat-model "microsoft/wizardlm-2-8x22b"
	    ;; :embedding-model "microsoft/wizardlm-2-8x22b"
;;;
	    ;; :chat-model "mistralai/mixtral-8x22b"
	    ;; :embedding-model "mistralai/mixtral-8x22b"
;;;
	    :chat-model "meta-llama/llama-3-70b-instruct"
	    :embedding-model "meta-llama/llama-3-70b-instruct"
;;;
            )))

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
;;;
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
;;;;
  ;; * Ellama Completers
  (defvar night/ellama-code-complete-prompt-template
    "Continue the following code, only write new code in format ```language\n...\n```:\n```\n%s\n```"
    "Prompt template for `night/ellama-code-complete'.")

  (defvar night/ellama--code-prefix
    "^\\s-*```\\(?:\\(?:\\s-\\|\n\\|\r\\)*\\([^\n\r]*\\)\\)?\\s-*[\n\r]+"
    ;; (rx (minimal-match
    ;;      (zero-or-more anything) (literal "```") (zero-or-more anything) (+ (or "\n" "\r"))))
    )

  (defvar night/ellama--code-suffix
    (rx (minimal-match
         (literal "```") (zero-or-more anything))))

  (defvar night/ellama--code-context-before 1000
    "Number of characters before the point to include as context.")

  (defvar night/ellama--code-context-after 1000
    "Number of characters after the point to include as context.")

  (defvar night/ellama--marker-text
    ;; "..............."
    "ELLAMA-END\n"
    ;; ""
    "Text to insert as a marker in the buffer.")

  (defvar night/ellama--filter-duplicate-code t
    "Whether to filter out duplicate code at the COMPLETE_HERE marker.")

  (defvar night/ellama--complete-here-marker "COMPLETE_HERE"
    "Marker text used to indicate where code completion should occur.")

  (defvar night/ellama-code-fill-in-the-middle-prompt-template
    "Fill in the code at '%s' in the following snippet, only write new code in format ```language\n...\n```:\n```\n%s\n```"
    "Prompt template for `night/ellama-code-fill-in-the-middle'.")

  (defun night/h-ellama-marker-comment-get ()
    (concat comment-start comment-padding night/ellama--marker-text))

  (defun night/ellama-marker-rm ()
    "Find and remove the ellama marker comment from point to the end of the buffer.
   Highlight the region from point to where the ellama marker was."
    (interactive)
    (let ((start (point))
          (marker-text (night/h-ellama-marker-comment-get))
          end)
      ;; Search for the ellama marker comment
      (if (search-forward marker-text nil t)
          (progn
            ;; Set the end point just before the marker text
            (setq end (match-beginning 0))
            ;; Delete the marker text
            (delete-region (match-beginning 0) (match-end 0))
            ;; Highlight the region from the original point to the end of the deleted comment
            (night/flash-region start end :delay t :backend 'overlay-timer :face 'highlight))
        (message "Ellama marker not found"))))
;;;
  (defun night/ellama-complete ()
    "Complete text in current buffer."
    ;; @upstreamBug [[id:d816b4e5-ce8f-4d0c-abd8-924c6ff79877][{Q/FR} "Legacy" Completion · Issue #45 · ahyatt/llm]]
    (interactive)
    (let* ((beg (if (region-active-p)
		    (region-beginning)
		  (max
                   (- (point) night/ellama--code-context-before)
                   (point-min))))
	   (end (if (region-active-p)
		    (region-end)
		  (point)))
	   (text (buffer-substring-no-properties beg end)))
      (ellama-stream text)))
;;;
  (defun night/ellama-code-complete ()
    "Complete selected code or code in current buffer."
    (interactive)
    (let* ((done-mode "rm-marker")
           (point-pos (point))
           (beg (if (region-active-p)
                    (region-beginning)
                  (max (- point-pos night/ellama--code-context-before) (point-min))))
           (end (if (region-active-p)
                    (region-end)
                  point-pos))
           (content-before-marker (buffer-substring-no-properties beg point-pos))
           (content-after-marker
            ;; (buffer-substring-no-properties point-pos end)
            ""
            )
           (full-text (buffer-substring-no-properties beg end)))
      (when (>= (length night/ellama--marker-text) 1)
        (save-excursion
          (insert (night/h-ellama-marker-comment-get))))
      (ellama-stream
       (format
        night/ellama-code-complete-prompt-template
        full-text)
       :filter (apply-partially
                #'night/ellama--code-filter
                content-before-marker
                content-after-marker)
       :point point-pos
       :on-done (lambda (response)
                  (when (string= done-mode "rm-marker")
                    (save-excursion
                      (goto-char point-pos)
                      (night/ellama-marker-rm)))))))
;;;
  (defun night/ellama-code-fill-in-the-middle ()
    "Complete code around the point in the current buffer."
    (interactive)
    (let* ((done-mode "rm-marker")
           (point-pos (point))
           (beg (max (- point-pos night/ellama--code-context-before) (point-min)))
           (end (min (+ point-pos night/ellama--code-context-after) (point-max)))
           (content-before-marker (buffer-substring-no-properties (line-beginning-position) (point)))
           (content-after-marker (buffer-substring-no-properties (point) (line-end-position)))
           (text-before (buffer-substring-no-properties beg point-pos))
           (text-after (buffer-substring-no-properties point-pos end))
           (full-text (concat text-before night/ellama--complete-here-marker text-after)))
      (when (>= (length night/ellama--marker-text) 1)
        (save-excursion
          (insert (night/h-ellama-marker-comment-get))))
      (ellama-stream
       (format
        night/ellama-code-fill-in-the-middle-prompt-template
        night/ellama--complete-here-marker
        full-text)
       :filter (apply-partially
                #'night/ellama--code-filter
                content-before-marker
                content-after-marker)
       :point point-pos
       :on-done (lambda (response)
                  (when (string= done-mode "rm-marker")
                    (save-excursion
                      (goto-char point-pos)
                      (night/ellama-marker-rm)))))))

  (defun night/ellama--code-filter (content-before-marker content-after-marker text)
    "Filter code prefix/suffix and optionally duplicate code from TEXT."
    (let ((cleaned-text
           (s-trim-right
            (string-trim-right
             (string-trim-left text night/ellama--code-prefix)
             night/ellama--code-suffix))))
      (if night/ellama--filter-duplicate-code
          (night/ellama--remove-duplicate-code
           content-before-marker
           content-after-marker
           cleaned-text)
        cleaned-text)))

  (defun night/ellama--remove-duplicate-code (content-before-marker content-after-marker text)
    "Remove content-before-marker from the start of text and content-after-marker from the end of text if present, ignoring leading and trailing whitespace."
    (let* ((verbose-p nil)
           (whitespace-regex "\\(?:\\s-\\|\n\\|\r\\)*")
           (before-regex (concat whitespace-regex (regexp-quote content-before-marker)))
           (after-regex (concat (regexp-quote content-after-marker) whitespace-regex "\\'"))
           (trimmed-before-regex (concat whitespace-regex (regexp-quote (s-trim content-before-marker))))
           (trimmed-after-regex (concat (regexp-quote (s-trim content-after-marker)) whitespace-regex "\\'")))
      (when verbose-p
        (message "content-before-marker: %s\n\ncontent-after-marker: %s\n\ntext:\n%s" before-regex after-regex text))
      (setq text (if (string-match before-regex text)
                     (replace-match "" t t text)
                   (if (string-match trimmed-before-regex text)
                       (replace-match "" t t text)
                     text)))
      (setq text (if (string-match after-regex text)
                     (replace-match "" t t text)
                   (if (string-match trimmed-after-regex text)
                       (replace-match "" t t text)
                     text)))
      (when verbose-p
        (message "cleaned text:\n%s" text))
      text))

  (comment
   (string-match
    ;; ")\\s-*\\'"
    "\\(\\s-\\|\n\\)*\\'"
    "k)\n"))

  (defun night/ellama--remove-duplicate-code-v1 (content-before-marker content-after-marker text)
    "Remove content-before-marker from the start of text if present."
    (let*
        ((text
          (if (string-prefix-p content-before-marker text)
              (substring text (length content-before-marker))
            text))
         (text (if (string-suffix-p content-after-marker text)
                   (substring text 0 (- (length text) (length content-after-marker)))
                 text))))
    text)
;;;
  ;; * setup key bindings
  ;; (setopt ellama-keymap-prefix "C-c e")
  (map! :leader
        :desc "Ellama keymap" "." ellama-command-map)
  (map! :leader
        ". ." #'night/ellama-code-fill-in-the-middle
        )
;;;
  )
