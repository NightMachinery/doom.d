;;; autoload/night-mistral-fim.el -*- lexical-binding: t; -*-

(after! (night-openai night/ellama)
  (require 'plz)
  (require 'json)

  (cl-defun night/mistral-fim-get
      (prefix
       &key
       (suffix nil)
       (max_tokens 64)
       ;; If `max_tokens' is nil, sometimes the model doesn't return anything at all, or perhaps it takes so long that it times out?
       (stop
        ;; "\n\n"
        "\n"
        )
       (temperature 0)
       (model nil)
       (callback))
    "Make an asynchronous request to the Codestral API using plz and call CALLBACK with the content of the first choice."
    (let* ((model (or model
                      "codestral-latest"))
           (api-key (night/codestral-key-get))
           (url "https://codestral.mistral.ai/v1/fim/completions")
           (headers `(("Content-Type" . "application/json")
                      ("Accept" . "application/json")
                      ("Authorization" . ,(concat "Bearer " api-key))))
           (data `(("model" . ,model)
                   ("prompt" . ,prefix)
                   ("temperature" . ,temperature)
                   ,@(when suffix `(("suffix" . ,suffix)))
                   ,@(when max_tokens `(("max_tokens" . ,max_tokens)))
                   ,@(when stop `(("stop" . ,stop)))
                   )))
      (plz 'post url
        :headers headers
        :body (json-encode data)
        :as 'string
        :then (lambda (response)
                ;; (message "fim response received:\n%s" response)
                (let ((json-response (condition-case err
                                         (json-read-from-string response)
                                       (json-readtable-error
                                        (funcall callback (error "Failed to parse JSON: %s" err))))))
                  (if json-response
                      (let ((choices (alist-get 'choices json-response)))
                        (if choices
                            (let ((first-choice (condition-case err
                                                    (elt choices 0)
                                                  (error
                                                   (funcall callback (error "Failed to get first choice: %s" err))))))
                              (if first-choice
                                  (let ((message (alist-get 'message first-choice)))
                                    (if message
                                        (let ((content (alist-get 'content message)))
                                          (if content
                                              (funcall callback content)
                                            (funcall callback (error "Content not found in message"))))
                                      (funcall callback (error "Message not found in first choice"))))
                                (funcall callback (error "First choice is nil"))))
                          (funcall callback (error "Choices not found in JSON response"))))
                    (funcall callback (error "JSON response is nil"))))))))

  (comment
   (night/mistral-fim-get
    "def randomize_case(text):\n\t"
    :suffix "\n\nprint(randomize_case(\"hello\"))"
    :callback (lambda (result)
                (message "FIM Result:\n%s" result))
    ))
;;;
  (cl-defun night/mistral-fim-insert-at-point (&key (point nil) (model nil))
    "Insert the FIM suggestion at the given POINT (default to current point) and highlight it."
    (interactive)
    (lexical-let*
        ((point (or point (point)))
         (marker (copy-marker point))
         (context-bounds (night/h-llm-code-context-bounds point))
         (prefix-start (car context-bounds))
         (suffix-end (cdr context-bounds))
         (prefix (buffer-substring-no-properties prefix-start point))
         (suffix (buffer-substring-no-properties point suffix-end)))

      ;; (message "Prefix: %s$\nSuffix: %s$" prefix suffix)
      (night/mistral-fim-get prefix
                             :suffix suffix
                             ;; :model model
                             :callback
                             (lambda (result)
                               ;; (message "FIM Result:\n%s" result)
                               (night/h-mistral-fim-insert-result marker result)
                               ))))

  (defun night/h-llm-code-context-bounds (point)
    "Return a cons cell (prefix-start . suffix-end) for the code context around POINT."
    (let* ((before-point (max (point-min) (- point night/ellama--code-context-before-fast)))
           (after-point (min (point-max) (+ point night/ellama--code-context-after-fast)))
           (start-of-line-before (save-excursion
                                   (goto-char before-point)
                                   (forward-line 0)
                                   (point)))
           (end-of-line-after (save-excursion
                                (goto-char after-point)
                                (end-of-line)
                                (point)))
           (prefix-start (if (> (- start-of-line-before before-point) night/ellama--code-context-line-tol)
                             before-point
                           start-of-line-before))
           (suffix-end (if (> (- after-point end-of-line-after) night/ellama--code-context-line-tol)
                           after-point
                         end-of-line-after)))
      (cons prefix-start suffix-end)))

  (defun night/h-mistral-fim-insert-result (marker result)
    "Insert RESULT at MARKER and highlight it."
    (cond
     ((night/whitespace-p result)
      (message "Result is whitespace, not inserting."))
     (t
      (let*
          ((buffer (marker-buffer marker)))

        ;; Codestral is buggy and often returns an extra space.
        ;; Removing the space might also introduce bad outputs sometimes, but it should at least be less common.
        (when (equalp (substring result 0 1) " ")
          (setq result (substring result 1)))

        (when buffer
          (with-current-buffer buffer
            (save-excursion
              (goto-char marker)
              (insert result)
              (let ((start marker)
                    (end (point)))
                (night/flash-region start end
                                    :delay t
                                    :backend 'overlay-timer
                                    :face 'highlight)))
            (evil-normal-state)
            ;; `evil-normal-state' must be run in the correct buffer, as well.
            ))))))
;;;
  )
