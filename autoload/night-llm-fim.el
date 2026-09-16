;;; autoload/night-llm-fim.el -*- lexical-binding: t; -*-

(after! (night-openai night/ellama night-llm-context)
  (require 'plz)
  (require 'json)

;;;
  (defcustom night/llm-fim-providers
    '((codestral
       :endpoint "https://codestral.mistral.ai/v1/fim/completions"
       :key-fn night/codestral-key-get
       :model "codestral-latest"
       :extract chat)
      (deepseek
       :endpoint "https://api.deepseek.com/beta/completions"
       :key-fn night/deepseek-key-get
       ;; DeepSeek-V4-Pro-0813, the flagship. FIM lives on the /beta base URL
       ;; and works in non-thinking mode only.
       :model "deepseek-v4-pro"
       :extract text)
      (deepseek-flash
       :endpoint "https://api.deepseek.com/beta/completions"
       :key-fn night/deepseek-key-get
       ;; DeepSeek-V4-Flash-0731. Same FIM support, much lower latency, which
       ;; is what actually matters for a single line behind a hotkey.
       :model "deepseek-v4-flash"
       :extract text))
    "Alist of FIM providers, mapping a name to a plist.

All of these APIs take the same request body, so an entry only has to say
where to send it and how to read the reply:

  :endpoint    URL of the FIM completion endpoint.
  :key-fn      Function returning the API key, or nil for an unauthenticated
               endpoint such as a local server.
  :model       Model name sent as `model'.
  :extract     Shape of the response: `chat' for choices[0].message.content
               (Mistral) or `text' for choices[0].text (OpenAI-style).

Optional :max-tokens, :stop and :temperature override the corresponding
`night/llm-fim-*' defaults for that provider."
    :type '(alist :key-type symbol :value-type plist)
    :group 'night)

  (defcustom night/llm-fim-provider 'codestral
    "Provider in `night/llm-fim-providers' used by `night/llm-fim-insert-at-point'."
    :type 'symbol
    :group 'night)

  (defcustom night/llm-fim-max-tokens 64
    "Token cap for a FIM completion.
With a nil cap the model sometimes returns nothing at all, or takes long
enough that the request times out."
    :type '(choice integer (const nil))
    :group 'night)

  (defcustom night/llm-fim-stop "\n"
    "Stop sequence for a FIM completion.
The default caps the completion at a single line during generation, rather
than truncating a longer one after paying for it."
    :type '(choice string (repeat string) (const nil))
    :group 'night)

  (defcustom night/llm-fim-temperature 0
    "Sampling temperature for a FIM completion."
    :type 'number
    :group 'night)

  (defcustom night/llm-fim-verbose t
    "When non-nil, report FIM progress and outcomes in the echo area.
Failures are always reported, regardless of this option."
    :type 'boolean
    :group 'night)

  (defcustom night/llm-fim-strip-leading-space nil
    "When non-nil, drop one leading space from a FIM completion.

This was once done unconditionally, on the belief that Codestral had a bug
that prepended a stray space.  Measured across 29 contexts per provider,
that is not what happens:

- All three providers do it at the same rate, so it was never a Codestral
  bug but the ordinary whitespace ambiguity of infilling.
- Where the prefix ends in an operator (`x =', `=>', `|', `a +') the space
  is simply correct, and dropping it yields `count =0'.
- Where point sits on an empty line the model supplies the whole indent;
  dropping one space turned eight into seven and broke the Python it was
  completing.
- Where the space really was spurious it was usually *two* of them, the
  model repeating an indent the prefix already had, so dropping one leaves
  the line misaligned regardless.

One case in 87 came out better for it.  Kept as an option rather than
deleted, because a later model may well go back to prepending one."
    :type 'boolean
    :group 'night)

  (defcustom night/llm-fim-timeout 20
    "Seconds before an in-flight FIM request is aborted.
`plz' imposes no total timeout by default, only `plz-connect-timeout'."
    :type 'number
    :group 'night)

;;;
  (defvar night/llm-fim--counter 0
    "Monotonic id source for FIM requests, used to detect stale replies.")

  (defvar-local night/llm-fim--pending nil
    "In-flight FIM request in this buffer, as a plist of :id, :process, :overlay.")

  (defun night/h-llm-fim--report (fmt &rest args)
    "Report FMT/ARGS in the echo area, unless `night/llm-fim-verbose' is nil."
    (when night/llm-fim-verbose
      (message "%s" (concat "FIM: " (apply #'format fmt args)))))

  (defun night/h-llm-fim--report-error (fmt &rest args)
    "Report FMT/ARGS in the echo area as a failure.  Always shown."
    (message "%s"
             (propertize (concat "FIM: " (apply #'format fmt args))
                         'face 'error)))

  (defun night/h-llm-fim--truncate (object &optional len)
    "Return OBJECT as a string of at most LEN (default 200) characters."
    (let ((s (format "%s" object))
          (len (or len 200)))
      (if (> (length s) len)
          (concat (substring s 0 len) "…")
        s)))

;;;
  (defun night/h-llm-fim--provider (&optional name)
    "Return the plist for provider NAME, defaulting to `night/llm-fim-provider'."
    (let ((name (or name night/llm-fim-provider)))
      (or (alist-get name night/llm-fim-providers)
          (error "night/llm-fim: unknown provider `%s'" name))))

  (defun night/h-llm-fim--read-provider (&optional prompt)
    "Read a provider name from `night/llm-fim-providers'."
    (intern
     (completing-read (or prompt "FIM provider: ")
                      (mapcar (lambda (entry) (symbol-name (car entry)))
                              night/llm-fim-providers)
                      nil t nil nil (symbol-name night/llm-fim-provider))))

  (defun night/llm-fim-provider-show ()
    "Echo the current FIM provider and the model it uses."
    (interactive)
    (message "FIM provider: %s (%s)"
             night/llm-fim-provider
             (plist-get (night/h-llm-fim--provider) :model)))

  (defun night/llm-fim-provider-select (provider)
    "Make PROVIDER the default for subsequent FIM completions."
    (interactive (list (night/h-llm-fim--read-provider)))
    ;; Fail before changing anything if the name is not in the table.
    (night/h-llm-fim--provider provider)
    (setq night/llm-fim-provider provider)
    (night/llm-fim-provider-show))

  (defun night/h-llm-fim--extract (extract json)
    "Pull the completion text out of JSON, per the provider's EXTRACT shape."
    (let* ((choices (alist-get 'choices json))
           (choice (when (> (length choices) 0)
                     (elt choices 0))))
      (pcase extract
        ('chat (alist-get 'content (alist-get 'message choice)))
        ('text (alist-get 'text choice))
        (_ (error "unknown :extract shape `%s'" extract)))))

;;;
  (defun night/h-llm-fim--ghost-make (marker)
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

  (defun night/h-llm-fim--ghost-clear (overlay)
    "Remove the pending-request indicator OVERLAY."
    (night/h-llm--overlay-clear overlay))

  (defun night/h-llm-fim--claim (buffer id)
    "Return non-nil if request ID is still the pending one in BUFFER.
Claiming a request clears the pending slot, so it can only succeed once."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq id (plist-get night/llm-fim--pending :id))
          (setq night/llm-fim--pending nil)
          t))))

  (cl-defun night/h-llm-fim--cancel (&key (quiet nil))
    "Abort the in-flight FIM request in the current buffer, if any.
Returns non-nil if a request was actually aborted."
    (when-let* ((pending night/llm-fim--pending))
      ;; Dropping the pending slot invalidates the request's id, so its
      ;; callbacks (including the error `plz' raises for the killed process)
      ;; find themselves stale and stay silent.
      (setq night/llm-fim--pending nil)
      (night/h-llm-fim--ghost-clear (plist-get pending :overlay))
      (let ((process (plist-get pending :process)))
        (when (process-live-p process)
          (delete-process process)))
      (unless quiet
        (night/h-llm-fim--report "aborted"))
      t))

  (defun night/h-llm-fim-escape ()
    "Cancel a pending FIM request on `C-g'.
Always returns nil so that `doom/escape' still does its usual work."
    (night/h-llm-fim--cancel)
    nil)

  (add-hook! 'doom-escape-hook 'night/h-llm-fim-escape)

;;;
  (defun night/h-llm-fim--api-message (body)
    "Extract the API's error message from response BODY, or return it truncated."
    (if (or (null body) (string-empty-p (string-trim body)))
        "(empty response body)"
      (or (ignore-errors
            (let ((json (json-read-from-string body)))
              ;; Mistral uses `detail' for auth and validation failures and
              ;; `message' elsewhere; `error.message' is the OpenAI shape,
              ;; which is what DeepSeek returns.
              (when-let* ((message (or (alist-get 'detail json)
                                       (alist-get 'message json)
                                       (alist-get 'message (alist-get 'error json)))))
                (night/h-llm-fim--truncate message))))
          (night/h-llm-fim--truncate (string-trim body)))))

  (defun night/h-llm-fim--error-string (err)
    "Render the `plz-error' ERR as a human-readable string."
    (let ((curl-error (plz-error-curl-error err))
          (response (plz-error-response err)))
      (cond
       (curl-error
        (format "curl error %s: %s" (car curl-error) (cdr curl-error)))
       (response
        (format "HTTP %s — %s"
                (plz-response-status response)
                (night/h-llm-fim--api-message (plz-response-body response))))
       (t
        (format "%s" (or (plz-error-message err) err))))))

;;;
  (cl-defun night/llm-fim-get
      (prefix
       &key
       (suffix nil)
       (provider nil)
       (max_tokens nil)
       (stop nil)
       (temperature nil)
       (model nil)
       (callback)
       (on-error #'night/h-llm-fim--report-error)
       (finally nil))
    "Asynchronously request a FIM completion for PREFIX and SUFFIX.

PROVIDER names an entry in `night/llm-fim-providers', defaulting to
`night/llm-fim-provider'.  MODEL, MAX_TOKENS, STOP and TEMPERATURE override
the provider's entry, which in turn overrides the `night/llm-fim-*' defaults.

CALLBACK receives the completion string.  ON-ERROR is called like
`message' with a format string and arguments describing the failure; it
defaults to reporting the failure in the echo area.  FINALLY, when given,
runs with no arguments after whichever of the two ran.

Returns the curl process so the caller can abort the request, or nil when
no request was made."
    (let* ((name (or provider night/llm-fim-provider))
           (entry (night/h-llm-fim--provider name))
           (url (plist-get entry :endpoint))
           (extract (plist-get entry :extract))
           (model (or model (plist-get entry :model)))
           (max_tokens (or max_tokens (plist-get entry :max-tokens)
                           night/llm-fim-max-tokens))
           (stop (or stop (plist-get entry :stop) night/llm-fim-stop))
           (temperature (or temperature (plist-get entry :temperature)
                            night/llm-fim-temperature))
           (key-fn (plist-get entry :key-fn))
           (api-key (when key-fn (funcall key-fn)))
           (data `(("model" . ,model)
                   ("prompt" . ,prefix)
                   ("temperature" . ,temperature)
                   ,@(when suffix `(("suffix" . ,suffix)))
                   ,@(when max_tokens `(("max_tokens" . ,max_tokens)))
                   ,@(when stop `(("stop" . ,stop)))
                   )))
      (if (and key-fn (not (and (stringp api-key)
                                (not (string-empty-p api-key)))))
          ;; Better than shipping "Bearer nil" and reading back a 401.
          (progn
            (funcall on-error "no API key for %s" name)
            (when finally (funcall finally))
            nil)
        (plz 'post url
          :headers `(("Content-Type" . "application/json")
                     ("Accept" . "application/json")
                     ,@(when api-key
                         `(("Authorization" . ,(concat "Bearer " api-key)))))
          :body (json-encode data)
          :as 'string
          :noquery t
          :timeout night/llm-fim-timeout
          :then (lambda (response)
                  (condition-case err
                      (let ((content (night/h-llm-fim--extract
                                      extract (json-read-from-string response))))
                        (if (stringp content)
                            (funcall callback content)
                          (funcall on-error "no completion in response: %s"
                                   (night/h-llm-fim--truncate response))))
                    (error
                     (funcall on-error "unreadable response (%s): %s"
                              (error-message-string err)
                              (night/h-llm-fim--truncate response)))))
          ;; Without `:else', `plz' hands the `plz-error' struct to `:then'
          ;; instead of a body, and the failure surfaces as a wrong-type error
          ;; inside the process sentinel.
          :else (lambda (err)
                  (funcall on-error "%s" (night/h-llm-fim--error-string err)))
          :finally finally))))

  (comment
   (night/llm-fim-get
    "def randomize_case(text):\n\t"
    :suffix "\n\nprint(randomize_case(\"hello\"))"
    :callback (lambda (result)
                (message "FIM Result:\n%s" result))
    ))
;;;
  (cl-defun night/llm-fim-insert-at-point (&key (point nil) (provider nil)
                                            (model nil) (scope nil))
    "Insert a FIM suggestion at the given POINT (default to current point) and highlight it.

With a prefix argument, read the PROVIDER to use for this one call, without
changing `night/llm-fim-provider'.

SCOPE limits what may be read, for this call only, overriding the
buffer's own -- see `night/llm-scope'.  Naming it explicitly also stands
in for the `confirm' prompt of `night/llm-path-policy': the caller has
already said what it sends.  With SCOPE nil the buffer's effective scope
applies and a `confirm' rule asks.

While the request is in flight a `⋯' indicator sits at POINT and the echo
area says so; the outcome (inserted, empty, or the API's error) is
reported when it completes.  Starting a new request in the same buffer
aborts the previous one, as does `C-g'."
    (interactive (when current-prefix-arg
                   (list :provider (night/h-llm-fim--read-provider))))
    ;; Ahead of `night/h-llm-fim--cancel', so that a refused invocation cannot tear
    ;; down a request that is legitimately in flight.
    (let ((point (or point (point))))
      (setq scope (night/h-llm--gate-scope
                   :scope scope :label "FIM"
                   :report #'night/h-llm-fim--report
                   :report-error #'night/h-llm-fim--report-error))
      (unless scope
        (cl-return-from night/llm-fim-insert-at-point nil))
      (let ((context-bounds
             (night/h-llm--narrow (night/llm-context-bounds :pos point)
                                  :scope scope :pos point :label "FIM"
                                  :report-error #'night/h-llm-fim--report-error)))
        (unless context-bounds
          (cl-return-from night/llm-fim-insert-at-point nil))
        (night/h-llm-fim--cancel :quiet t)
        (let* ((buffer (current-buffer))
               (marker (copy-marker point))
               (name (or provider night/llm-fim-provider))
               (model (or model (plist-get (night/h-llm-fim--provider name) :model)))
               (prefix-start (car context-bounds))
               (suffix-end (cdr context-bounds))
               (prefix (buffer-substring-no-properties prefix-start point))
               (suffix (buffer-substring-no-properties point suffix-end))
               (started (float-time))
               (id (cl-incf night/llm-fim--counter))
               (overlay nil)
               (guard
                ;; Call FN with the elapsed seconds, but only for the request
                ;; this buffer is still waiting on.
                (lambda (fn)
                  (cond
                   ((not (buffer-live-p buffer))
                    (night/h-llm-fim--report "buffer gone, discarded completion"))
                   ((night/h-llm-fim--claim buffer id)
                    (funcall fn (- (float-time) started)))))))
          (when (and (= prefix-start point) (= suffix-end point))
            (night/h-llm-fim--report-error "`%s' leaves no context here" scope)
            (cl-return-from night/llm-fim-insert-at-point nil))
          (night/h-llm--flash (cons prefix-start suffix-end) scope)
          (setq overlay (night/h-llm-fim--ghost-make marker))
          ;; Claim the slot before the request, because a missing API key reports
          ;; synchronously and the guard has to recognise it as current.
          (setq night/llm-fim--pending (list :id id :process nil :overlay overlay))
          (night/h-llm-fim--report "requesting %s (%s)…" model scope)
          (let ((process
                 (night/llm-fim-get
                  prefix
                  :suffix suffix
                  :provider name
                  :model model
                  :callback
                  (lambda (result)
                    (funcall guard
                             (lambda (elapsed)
                               (night/h-llm-fim-insert-result marker result elapsed))))
                  :on-error
                  (lambda (fmt &rest args)
                    (funcall guard
                             (lambda (elapsed)
                               (night/h-llm-fim--report-error
                                "%s (%.1fs)" (apply #'format fmt args) elapsed))))
                  :finally
                  (lambda ()
                    (night/h-llm-fim--ghost-clear overlay)))))
            (when (eq id (plist-get night/llm-fim--pending :id))
              (setq night/llm-fim--pending
                    (plist-put night/llm-fim--pending :process process)))
            process)))))

;;;
  (defun night/llm-fim-insert-nearby ()
    "Complete at point, reading only the text around point."
    (interactive)
    (night/llm-fim-insert-at-point :scope 'nearby))

  (defun night/llm-fim-insert-in-block ()
    "Complete at point, reading only the enclosing block.
In `org-mode' that is the Org block around point; anywhere else, the
enclosing defun."
    (interactive)
    (night/llm-fim-insert-at-point :scope 'block))

  (defun night/llm-fim-insert-in-subtree ()
    "Complete at point, reading only the current heading and its children."
    (interactive)
    (night/llm-fim-insert-at-point :scope 'subtree))

  (defun night/llm-fim-insert-choose ()
    "Choose a scope, each candidate highlighted, then complete within it.
Chooses for this call only; use `night/llm-scope-select' to change what
the buffer does by default."
    (interactive)
    (let ((scope (night/h-llm--scope-choose :prompt "FIM: complete reading")))
      (cond
       (scope (night/llm-fim-insert-at-point :scope scope))
       (t (night/h-llm-fim--report "cancelled")))))


  (defun night/h-llm-fim-insert-result (marker result &optional elapsed)
    "Insert RESULT at MARKER, highlight it, and report the outcome.
ELAPSED, when given, is the request's duration in seconds."
    (let ((buffer (marker-buffer marker))
          (took (if elapsed (format " in %.1fs" elapsed) "")))
      (cond
       ((night/whitespace-p result)
        (night/h-llm-fim--report "empty completion%s" took))
       ((not (buffer-live-p buffer))
        (night/h-llm-fim--report "buffer gone, discarded completion"))
       (t
        (with-current-buffer buffer
          ;; Off by default; see `night/llm-fim-strip-leading-space'.
          (when (and night/llm-fim-strip-leading-space
                     (string-prefix-p " " result))
            (setq result (substring result 1)))

          (if buffer-read-only
              (night/h-llm-fim--report-error "buffer is read-only, not inserting")
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
            (night/h-llm-fim--report "inserted %d chars%s"
                                 (length result) took)))))))
;;;
  )
