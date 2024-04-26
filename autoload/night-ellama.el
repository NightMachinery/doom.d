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
  (defun night/ellama-provider-show ()
    "Shows the current `ellama-provider'."
    (interactive)
    (message (format "Ellama provider: %s"
                     (cl-struct-slot-value
                      (type-of ellama-provider)
                      'chat-model ellama-provider))))
  
  (setopt
   ellama-providers
   `(
     ("GQ-Llama3" .
      ,(make-llm-openai-compatible
        :key (night/groq-key-get)
        :url "https://api.groq.com/openai/v1"
        :chat-model "llama3-70b-8192"
        :embedding-model "llama3-70b-8192"))
     ("OR-Llama3" .
      ,(make-llm-openai-compatible
        :key (night/openrouter-key-get)
        :url "https://openrouter.ai/api/v1"
        :chat-model "meta-llama/llama-3-70b-instruct:nitro"
        :embedding-model "meta-llama/llama-3-70b-instruct:nitro"
        ))
     ("OR-Gemini-1.5" .
      ,(make-llm-openai-compatible
        :key (night/openrouter-key-get)
        :url "https://openrouter.ai/api/v1"
        :chat-model "google/gemini-pro-1.5"
        :embedding-model "google/gemini-pro-1.5"
        ))
     ("OR-Mixtral-8x22B" .
      ,(make-llm-openai-compatible
        :key (night/openrouter-key-get)
        :url "https://openrouter.ai/api/v1"
        :chat-model "mistralai/mixtral-8x22b-instruct"
        :embedding-model "mistralai/mixtral-8x22b-instruct"
        ))
     ("OR-WizardLM2" .
      ,(make-llm-openai-compatible
        :key (night/openrouter-key-get)
        :url "https://openrouter.ai/api/v1"
        :chat-model "microsoft/wizardlm-2-8x22b:nitro"
        :embedding-model "microsoft/wizardlm-2-8x22b:nitro"
        ))
     ("OR-Claude3-Opus" .
      ,(make-llm-openai-compatible
        :key (night/openrouter-key-get)
        :url "https://openrouter.ai/api/v1"
        :chat-model "anthropic/claude-3-opus:beta"
        :embedding-model "anthropic/claude-3-opus:beta"
        ))
     ("GPT4-Turbo" .
      ,(make-llm-openai
        :key (night/openai-key-get)
        :chat-model "gpt-4-turbo"
        :embedding-model "gpt-4-turbo"))
     ))
  (setopt ellama-provider (cdar ellama-providers))
  ;; Setting the default provider to the first one in the ellama-providers list

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
    "\\`\\(?:.*\n\\)*?\\s-*```\\(?:\\(?:\\s-\\|\n\\|\r\\)*\\(\\S-*\\)\\)?\\s-*[\n\r]+"
    )

  (defvar night/ellama--code-suffix
    (rx (minimal-match
         (literal "```") (zero-or-more anything))))

  (defvar night/ellama--code-context-before 1000
    "Number of characters before the point to include as context.")

  (defvar night/ellama--code-dup-lines-before 20)
  (defvar night/ellama--code-dup-lines-after 20)

  (defvar night/ellama--code-context-after 1000
    "Number of characters after the point to include as context.")

  (defvar night/ellama--marker-text
    ;; "..............."
    "ELLAMA-END\n"
    ;; ""
    "Text to insert as a marker in the buffer.")

  (defvar night/ellama--filter-duplicate-code t
    "Whether to filter out duplicate code at the COMPLETE_HERE marker.")

  (defvar night/ellama--complete-here-marker
    "<COMPLETE_HERE>"
    ;; "<FILL_HERE>"
    ;; "COMPLETE_HERE"
    "Marker text used to indicate where code completion should occur.")

  (defvar night/ellama-code-fill-in-the-middle-prompt-template
    ;; "Fill in the code at '%s' in the following snippet, only write new code in format ```language\n...\n```:\n```\n%s\n```"
    ;; "Fill in at '%s' in the following snippet, only write new code in format ```language\n...\n```. Start your output from the line marked for completion:\n```\n%s\n```"
    ;; "Fill in at '%s' in the following snippet, only write in format ```language\n...\n```. Sometimes you need to complete comments, sometimes code, and sometimes other miscellaneous stuff. Always use a language specifier for the code block, even if it is `plaintext`. Start your output from the line marked for completion:\n```\n%s\n```"
    ;; "Fill in at '%s' in the following snippet, only write in format ```language\n...\n```. Always use a language specifier for the code block.\n\n```\n%s\n```"
    ;; "Fill in at '%s' in the following snippet, only write in format ```language\n...\n```. Always use a language specifier for the code block. Always start your output from THE BEGINNING OF THE LINE marked for completion.\n\n```\n%s\n```"
    ;; "Fill in at '%s' in the following snippet, only write in format ```language\n...\n```:\n\n```\n%s\n```"
    "Fill in at '%s' in the following snippet. Only write in format ```language\n...\n```. Start your output from the line marked for completion:\n\n```\n%s\n```"
    ;; You are a helpful assistant. Assistant will output only and only code as a response.
    "Prompt template for `night/ellama-code-fill-in-the-middle'.")

  (defun night/h-ellama-marker-comment-get ()
    (concat comment-start comment-padding night/ellama--marker-text))

  (defun night/ellama-marker-rm ()
    "Find and remove the ellama marker comment from point to the end of the buffer.
   Highlight the region from point to where the ellama marker was."
    (interactive)
    (when (night/bool-smart night/ellama--marker-text)
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

              ;; Heuristic for inserting new lines after the completion:
              (when (and (eolp) (not (looking-at-p "\n\\(?:\n\\|#\\)")))
                (insert "\n"))

              ;; Highlight the region from the original point to the end of the deleted comment
              (night/flash-region start end :delay t :backend 'overlay-timer :face 'highlight))
          (message "Ellama marker not found"))))

    (when (eq major-mode 'org-mode)
      (night/with-messages-suppressed
        (org-indent-indent-buffer))))

  (defun night/h-get-lines (point-pos num-lines direction)
    "Capture lines before or after POINT-POS up to NUM-LINES.
DIRECTION should be either 'before' or 'after'.
POINT-POS defaults to current point, NUM-LINES defaults to 2."
    ;; The returned list is ordered from more specific to less specific, i.e., from longer to shorter.
    (cl-block night/h-get-lines
      (let* ((point-pos (or point-pos (point)))
             (num-lines (or num-lines 2))
             (lines '()))
        (save-excursion
          (goto-char point-pos)
          (dotimes (i num-lines)
            (push (buffer-substring-no-properties
                   (if (eq direction 'before) (line-beginning-position) point-pos)
                   (if (eq direction 'after) (line-end-position) point-pos))
                  lines)
            (if (or (and (eq direction 'before) (bobp))
                    (and (eq direction 'after) (eobp)))
                (cl-return-from night/h-get-lines lines))
            (forward-line (if (eq direction 'before) -1 1))))
        lines)))

  (defun night/h-get-lines-before-point (&optional point-pos num-lines)
    "Capture lines before POINT-POS up to NUM-LINES."
    ;; (interactive)
    (night/h-get-lines point-pos num-lines 'before))

  (defun night/h-get-lines-after-point (&optional point-pos num-lines)
    "Capture lines after POINT-POS up to NUM-LINES."
    ;; (interactive)
    (night/h-get-lines point-pos num-lines 'after))
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
    (let* (
           (verbose-p current-prefix-arg)
           (done-mode "rm-marker")
           (point-pos (point))
           (beg (if (region-active-p)
                    (region-beginning)
                  (max (- point-pos night/ellama--code-context-before) (point-min))))
           (end (if (region-active-p)
                    (region-end)
                  point-pos))
           (content-before-marker
            (night/h-get-lines-before-point point-pos night/ellama--code-dup-lines-before))
           (content-after-marker
            nil
            )
           (full-text (buffer-substring-no-properties beg end)))
      (when (night/bool-smart night/ellama--marker-text)
        (save-excursion
          (insert (night/h-ellama-marker-comment-get))))
      (ellama-stream
       (format
        night/ellama-code-complete-prompt-template
        full-text)
       :filter (apply-partially
                #'night/ellama--code-filter
                verbose-p
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
    (let* ((verbose-p current-prefix-arg)
           (done-mode "rm-marker")
           (point-pos (point))
           (beg (max (- point-pos night/ellama--code-context-before) (point-min)))
           (end (min (+ point-pos night/ellama--code-context-after) (point-max)))
           (content-before-marker
            (night/h-get-lines-before-point point-pos night/ellama--code-dup-lines-before))
           (content-after-marker (night/h-get-lines-after-point point-pos night/ellama--code-dup-lines-after))
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
                verbose-p
                content-before-marker
                content-after-marker)
       :point point-pos
       :on-done (lambda (response)
                  (when (string= done-mode "rm-marker")
                    (save-excursion
                      (goto-char point-pos)
                      (night/ellama-marker-rm)))))))

  (defun night/ellama--code-filter (verbose-p content-before-marker content-after-marker text)
    "Filter code prefix/suffix and optionally duplicate code from TEXT."
    (let* ((text-before-trim text)
           (text-after-prefix-trim (string-trim-left text night/ellama--code-prefix))
           (cleaned-text
            (s-trim-right
             (string-trim-right
              text-after-prefix-trim
              night/ellama--code-suffix))))
      (when verbose-p
        (message "Text before trimming: %S" text-before-trim)
        (message "Text after prefix trimming: %S" text-after-prefix-trim)
        (message "Text after suffix trimming: %S" cleaned-text))
      (if night/ellama--filter-duplicate-code
          (night/ellama--remove-duplicate-code
           content-before-marker
           content-after-marker
           cleaned-text
           verbose-p)
        cleaned-text)))

  (defun night/ellama--remove-duplicate-code (content-before-marker content-after-marker text &optional verbose-p)
    "Remove content-before-marker from the start of text and content-after-marker from the end of text if present, ignoring leading and trailing whitespace."
    (let* (
           (whitespace-regex "\\(?:\\s-\\|\n\\|\r\\)*"))
      (when verbose-p
        (message "content-before-marker: %s\n\ncontent-after-marker: %s\n\ntext:\n%s" content-before-marker content-after-marker text))
      (setq text (night/ellama--apply-regex-list content-before-marker whitespace-regex text t))
      (setq text (night/ellama--apply-regex-list content-after-marker whitespace-regex text nil))
      (when verbose-p
        (message "cleaned text:\n%s" text))
      text))

  
  (defun night/ellama--apply-regex-list (content-list whitespace-regex text is-before)
    "Apply the regex for each item in content-list to the text, removing matches from the beginning or end based on is-before."
    (when content-list
      (cl-block loop
        (dolist (content (if (listp content-list) content-list (list content-list)))
          (let* ((regex (night/ellama--build-regex content whitespace-regex is-before))
                 (new-text (night/ellama--apply-regex regex text is-before)))
            (unless (equal new-text text)
              (setq text new-text)
              (cl-return-from loop))))))
    text)

  (defun night/ellama--build-regex (content whitespace-regex is-before)
    "Build a regex based on content which can be nil, a list, or a string, adding whitespace handling."
    (when content
      (let*
          ((content-list (if (listp content) content (list content)))
           (anchor (if is-before "\\`" "\\'"))
           (quoted-contents
            (mapconcat
             (lambda (item)
               (concat
                (regexp-quote item)
                "\\|"
                (regexp-quote (s-trim-left item))
                "\\|"
                (regexp-quote (s-trim item))))
             content-list
             "\\|"))
           (pattern (concat "\\(?:" quoted-contents "\\)")))
        (if is-before
            (concat anchor whitespace-regex pattern ;; whitespace-regex
                    )
          (concat
           ;; whitespace-regex
           pattern whitespace-regex anchor)))))

  (defun night/ellama--apply-regex (regex text is-before)
    "Apply the regex to the text, removing matches from the beginning or end based on is-before."
    (if regex
        (if (string-match regex text)
            (replace-match "" t t text)
          text)
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
        ". ," #'night/ellama-code-complete
        ". /" #'ellama-provider-select
        ". ?" #'night/ellama-provider-show
        )
;;;
  )
