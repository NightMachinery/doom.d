;;; autoload/night-mistral-fim.el -*- lexical-binding: t; -*-

(after! (night-openai night/ellama)
  (require 'plz)
  (require 'json)

;;;
  (defcustom night/mistral-fim-model "codestral-latest"
    "Model used by `night/mistral-fim-insert-at-point'."
    :type 'string
    :group 'night)

  (defcustom night/mistral-fim-verbose t
    "When non-nil, report FIM progress and outcomes in the echo area.
Failures are always reported, regardless of this option."
    :type 'boolean
    :group 'night)

  (defcustom night/mistral-fim-timeout 20
    "Seconds before an in-flight FIM request is aborted.
`plz' imposes no total timeout by default, only `plz-connect-timeout'."
    :type 'number
    :group 'night)

  (defvar night/mistral-fim--counter 0
    "Monotonic id source for FIM requests, used to detect stale replies.")

  (defvar-local night/mistral-fim--pending nil
    "In-flight FIM request in this buffer, as a plist of :id, :process, :overlay.")

  (defun night/h-mistral-fim--report (fmt &rest args)
    "Report FMT/ARGS in the echo area, unless `night/mistral-fim-verbose' is nil."
    (when night/mistral-fim-verbose
      (message "%s" (concat "FIM: " (apply #'format fmt args)))))

  (defun night/h-mistral-fim--report-error (fmt &rest args)
    "Report FMT/ARGS in the echo area as a failure.  Always shown."
    (message "%s"
             (propertize (concat "FIM: " (apply #'format fmt args))
                         'face 'error)))

  (defun night/h-mistral-fim--truncate (object &optional len)
    "Return OBJECT as a string of at most LEN (default 200) characters."
    (let ((s (format "%s" object))
          (len (or len 200)))
      (if (> (length s) len)
          (concat (substring s 0 len) "…")
        s)))

;;;
  (defun night/h-mistral-fim--ghost-make (marker)
    "Show a pending-request indicator at MARKER and return its overlay."
    (let ((buffer (marker-buffer marker)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((overlay (make-overlay marker marker buffer)))
            (overlay-put overlay 'after-string (propertize "⋯" 'face 'shadow))
            ;; Registering here means `night/clear-overlays' (on
            ;; `doom-escape-hook') also disposes of it.
            (push overlay night/active-overlays)
            overlay)))))

  (defun night/h-mistral-fim--ghost-clear (overlay)
    "Remove the pending-request indicator OVERLAY."
    (when (overlayp overlay)
      (delete-overlay overlay)
      (setq night/active-overlays (remove overlay night/active-overlays))))

  (defun night/h-mistral-fim--claim (buffer id)
    "Return non-nil if request ID is still the pending one in BUFFER.
Claiming a request clears the pending slot, so it can only succeed once."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq id (plist-get night/mistral-fim--pending :id))
          (setq night/mistral-fim--pending nil)
          t))))

  (cl-defun night/h-mistral-fim--cancel (&key (quiet nil))
    "Abort the in-flight FIM request in the current buffer, if any.
Returns non-nil if a request was actually aborted."
    (when-let* ((pending night/mistral-fim--pending))
      ;; Dropping the pending slot invalidates the request's id, so its
      ;; callbacks (including the `plz' error `plz' raises for the killed
      ;; process) find themselves stale and stay silent.
      (setq night/mistral-fim--pending nil)
      (night/h-mistral-fim--ghost-clear (plist-get pending :overlay))
      (let ((process (plist-get pending :process)))
        (when (process-live-p process)
          (delete-process process)))
      (unless quiet
        (night/h-mistral-fim--report "aborted"))
      t))

  (defun night/h-mistral-fim-escape ()
    "Cancel a pending FIM request on `C-g'.
Always returns nil so that `doom/escape' still does its usual work."
    (night/h-mistral-fim--cancel)
    nil)

  (add-hook! 'doom-escape-hook 'night/h-mistral-fim-escape)

;;;
  (defun night/h-mistral-fim--api-message (body)
    "Extract Mistral's error message from response BODY, or return BODY truncated."
    (if (or (null body) (string-empty-p (string-trim body)))
        "(empty response body)"
      (or (ignore-errors
            (let ((json (json-read-from-string body)))
              ;; Mistral uses `detail' for auth and validation failures and
              ;; `message' elsewhere; `error.message' is the OpenAI shape.
              (when-let* ((message (or (alist-get 'detail json)
                                       (alist-get 'message json)
                                       (alist-get 'message (alist-get 'error json)))))
                (night/h-mistral-fim--truncate message))))
          (night/h-mistral-fim--truncate (string-trim body)))))

  (defun night/h-mistral-fim--error-string (err)
    "Render the `plz-error' ERR as a human-readable string."
    (let ((curl-error (plz-error-curl-error err))
          (response (plz-error-response err)))
      (cond
       (curl-error
        (format "curl error %s: %s" (car curl-error) (cdr curl-error)))
       (response
        (format "HTTP %s — %s"
                (plz-response-status response)
                (night/h-mistral-fim--api-message (plz-response-body response))))
       (t
        (format "%s" (or (plz-error-message err) err))))))

;;;
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
       (callback)
       (on-error #'night/h-mistral-fim--report-error)
       (finally nil))
    "Make an asynchronous request to the Codestral API using plz and call CALLBACK with the content of the first choice.

CALLBACK receives the completion string.  ON-ERROR is called like
`message' with a format string and arguments describing the failure; it
defaults to reporting the failure in the echo area.  FINALLY, when given,
runs with no arguments after whichever of the two ran.

Returns the curl process, so the caller can abort the request."
    (let* ((model (or model night/mistral-fim-model))
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
        :noquery t
        :timeout night/mistral-fim-timeout
        :then (lambda (response)
                ;; (message "fim response received:\n%s" response)
                (condition-case err
                    (let* ((json-response (json-read-from-string response))
                           (choices (alist-get 'choices json-response))
                           (first-choice (when (> (length choices) 0)
                                           (elt choices 0)))
                           (content (alist-get 'content
                                               (alist-get 'message first-choice))))
                      (if (stringp content)
                          (funcall callback content)
                        (funcall on-error "no completion in response: %s"
                                 (night/h-mistral-fim--truncate response))))
                  (error
                   (funcall on-error "unreadable response (%s): %s"
                            (error-message-string err)
                            (night/h-mistral-fim--truncate response)))))
        ;; Without `:else', `plz' hands the `plz-error' struct to `:then'
        ;; instead of a body, and the failure surfaces as a wrong-type error
        ;; inside the process sentinel.
        :else (lambda (err)
                (funcall on-error "%s" (night/h-mistral-fim--error-string err)))
        :finally finally)))

  (comment
   (night/mistral-fim-get
    "def randomize_case(text):\n\t"
    :suffix "\n\nprint(randomize_case(\"hello\"))"
    :callback (lambda (result)
                (message "FIM Result:\n%s" result))
    ))
;;;
  (cl-defun night/mistral-fim-insert-at-point (&key (point nil) (model nil))
    "Insert the FIM suggestion at the given POINT (default to current point) and highlight it.

While the request is in flight a `⋯' indicator sits at POINT and the echo
area says so; the outcome (inserted, empty, or the API's error) is
reported when it completes.  Starting a new request in the same buffer
aborts the previous one, as does `C-g'."
    (interactive)
    (night/h-mistral-fim--cancel :quiet t)
    (let* ((point (or point (point)))
           (buffer (current-buffer))
           (marker (copy-marker point))
           (model (or model night/mistral-fim-model))
           (context-bounds (night/h-llm-code-context-bounds point))
           (prefix-start (car context-bounds))
           (suffix-end (cdr context-bounds))
           (prefix (buffer-substring-no-properties prefix-start point))
           (suffix (buffer-substring-no-properties point suffix-end))
           (started (float-time))
           (id (cl-incf night/mistral-fim--counter))
           (overlay (night/h-mistral-fim--ghost-make marker))
           (guard
            ;; Call FN with the elapsed seconds, but only for the request
            ;; this buffer is still waiting on.
            (lambda (fn)
              (cond
               ((not (buffer-live-p buffer))
                (night/h-mistral-fim--report "buffer gone, discarded completion"))
               ((night/h-mistral-fim--claim buffer id)
                (funcall fn (- (float-time) started))))))
           (process
            ;; (message "Prefix: %s$\nSuffix: %s$" prefix suffix)
            (night/mistral-fim-get
             prefix
             :suffix suffix
             :model model
             :callback
             (lambda (result)
               ;; (message "FIM Result:\n%s" result)
               (funcall guard
                        (lambda (elapsed)
                          (night/h-mistral-fim-insert-result marker result elapsed))))
             :on-error
             (lambda (fmt &rest args)
               (funcall guard
                        (lambda (elapsed)
                          (night/h-mistral-fim--report-error
                           "%s (%.1fs)" (apply #'format fmt args) elapsed))))
             :finally
             (lambda ()
               (night/h-mistral-fim--ghost-clear overlay)))))
      (setq night/mistral-fim--pending
            (list :id id :process process :overlay overlay))
      (night/h-mistral-fim--report "requesting %s…" model)
      process))

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

  (defun night/h-mistral-fim-insert-result (marker result &optional elapsed)
    "Insert RESULT at MARKER, highlight it, and report the outcome.
ELAPSED, when given, is the request's duration in seconds."
    (let ((buffer (marker-buffer marker))
          (took (if elapsed (format " in %.1fs" elapsed) "")))
      (cond
       ((night/whitespace-p result)
        (night/h-mistral-fim--report "empty completion%s" took))
       ((not (buffer-live-p buffer))
        (night/h-mistral-fim--report "buffer gone, discarded completion"))
       (t
        (with-current-buffer buffer
          ;; Codestral is buggy and often returns an extra space.
          ;; Removing the space might also introduce bad outputs sometimes, but it should at least be less common.
          (when (string-prefix-p " " result)
            (setq result (substring result 1)))

          (if buffer-read-only
              (night/h-mistral-fim--report-error "buffer is read-only, not inserting")
            ;; We run from a process sentinel, so no boundary is added for us
            ;; and the completion would otherwise merge into the user's last
            ;; edit for undo purposes.
            (undo-boundary)
            (save-excursion
              (goto-char marker)
              (insert result)
              (let ((start marker)
                    (end (point)))
                (night/flash-region start end
                                    :delay t
                                    :backend 'overlay-timer
                                    :face 'highlight)))
            (undo-boundary)
            (evil-normal-state)
            ;; `evil-normal-state' must be run in the correct buffer, as well.
            (night/h-mistral-fim--report "inserted %d chars%s"
                                         (length result) took)))))))
;;;
  )
