;;; autoload/night-mistral-fim.el -*- lexical-binding: t; -*-

(after! (night-openai night/ellama)
  (require 'plz)
  (require 'json)

;;;
  (defcustom night/fim-providers
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
`night/fim-*' defaults for that provider."
    :type '(alist :key-type symbol :value-type plist)
    :group 'night)

  (defcustom night/fim-provider 'codestral
    "Provider in `night/fim-providers' used by `night/fim-insert-at-point'."
    :type 'symbol
    :group 'night)

  (defcustom night/fim-max-tokens 64
    "Token cap for a FIM completion.
With a nil cap the model sometimes returns nothing at all, or takes long
enough that the request times out."
    :type '(choice integer (const nil))
    :group 'night)

  (defcustom night/fim-stop "\n"
    "Stop sequence for a FIM completion.
The default caps the completion at a single line during generation, rather
than truncating a longer one after paying for it."
    :type '(choice string (repeat string) (const nil))
    :group 'night)

  (defcustom night/fim-temperature 0
    "Sampling temperature for a FIM completion."
    :type 'number
    :group 'night)

  (defcustom night/fim-verbose t
    "When non-nil, report FIM progress and outcomes in the echo area.
Failures are always reported, regardless of this option."
    :type 'boolean
    :group 'night)

  (defcustom night/fim-strip-leading-space nil
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

  (defcustom night/fim-timeout 20
    "Seconds before an in-flight FIM request is aborted.
`plz' imposes no total timeout by default, only `plz-connect-timeout'."
    :type 'number
    :group 'night)

;;;
  (defcustom night/fim-path-policy
    '((encrypted                  . refuse)
      ("/\\.keys/"                . refuse)
      ("/\\.privateShell\\Z"      . refuse)
      ("/\\.authinfo(\\.gpg)?\\Z" . refuse)
      ("/\\.netrc\\Z"             . refuse)
      ("/\\.ssh/"                 . refuse)
      ;; macOS resolves /tmp and /var into /private/, so without this the
      ;; rule below would ask about every scratch file.
      ("\\A/private/(tmp|var)/"    . allow)
      ("/private/"                . confirm))
    "What FIM may do in a buffer, most specific rule first.

FIM sends the text around point to a third-party API, so some buffers
have no business being completed at all.  Each rule is a cons of a
matcher and a level:

  matcher  a PCRE, tested against both the buffer's file name and its
           truename -- see `night/file-path-candidates' -- or a symbol
           naming a predicate in `night/h-fim-policy-predicates'.
  level    `refuse' declines and names the rule that said so;
           `confirm' asks once per buffer; `allow' sends.

The FIRST matching rule decides, which is what makes carve-outs
expressible: an allow rule for \"/private/pub/\" placed above the
confirm rule for \"/private/\" exempts that subtree.  That is equally
the hazard -- a broad allow near the top silently disarms everything
under it -- so keep the specific rules on top.  A buffer matching no
rule is allowed, and a buffer visiting no file matches no PCRE rule.

A matcher naming a predicate that does not exist refuses rather than
being skipped: a typo here must not quietly widen the policy."
    :type '(alist :key-type (choice (string :tag "PCRE")
                                    (symbol :tag "Predicate"))
                  :value-type (choice (const refuse)
                                      (const confirm)
                                      (const allow)))
    :group 'night)

  (defvar night/h-fim-policy-predicates
    '((encrypted . night/buffer-encrypted-p))
    "Symbols usable as matchers in `night/fim-path-policy'.
Each maps to a function of one argument, the buffer to judge.")

  (defvar-local night/fim--path-confirmed nil
    "Non-nil once a `confirm' rule has been approved for this buffer.
Buffer-local and never persisted, so revisiting the file asks again.")

  (defun night/h-fim--policy-rule-match-p (rule paths buffer)
    "Non-nil if RULE of `night/fim-path-policy' applies to BUFFER.

PATHS is what `night/file-path-candidates' returned for it.  A symbol
matcher with no entry in `night/h-fim-policy-predicates' counts as a
match, so that `night/h-fim--gate' is the one that gets to refuse on it."
    (let ((matcher (car rule)))
      (if (stringp matcher)
          (let ((regexp (night/pcre-to-regexp matcher)))
            ;; A pattern that will not compile counts as a match, so that
            ;; `night/h-fim--gate' refuses on it instead of walking past.
            (or (null regexp)
                (cl-some (lambda (path) (string-match-p regexp path)) paths)))
        (let ((fn (alist-get matcher night/h-fim-policy-predicates)))
          (if fn (funcall fn buffer) t)))))

  (defun night/h-fim--gate (&optional buffer)
    "Decide whether FIM may run in BUFFER, per `night/fim-path-policy'.

Return nil to go ahead, or a cons of a severity -- `error' or `info' --
and a reason to report.  This may prompt: a `confirm' rule asks once and
then remembers the answer for as long as the buffer lives."
    (let* ((buffer (or buffer (current-buffer)))
           (case-fold-search nil)
           (paths (night/file-path-candidates (buffer-file-name buffer)))
           (rule (cl-find-if
                  (lambda (rule)
                    (night/h-fim--policy-rule-match-p rule paths buffer))
                  night/fim-path-policy)))
      (when rule
        (let ((matcher (car rule))
              (level (cdr rule)))
          (cond
           ((and (stringp matcher) (null (night/pcre-to-regexp matcher)))
            (cons 'error (format "cannot read `%s' as a PCRE; refusing" matcher)))
           ((and (not (stringp matcher))
                 (not (alist-get matcher night/h-fim-policy-predicates)))
            (cons 'error (format "`%s' names no predicate; refusing" matcher)))
           ((eq level 'allow) nil)
           ((eq level 'refuse)
            (cons 'error (format "refused by `%s'" matcher)))
           ((eq level 'confirm)
            (with-current-buffer buffer
              (cond
               (night/fim--path-confirmed nil)
               ((y-or-n-p
                 (format "FIM: %s matches `%s'; send its text to a remote model? "
                         (buffer-name buffer) matcher))
                (setq night/fim--path-confirmed t)
                nil)
               (t (cons 'info "declined")))))
           (t
            ;; Fail closed on a level nobody defined.
            (cons 'error (format "unknown level `%s' in `%s'" level matcher))))))))
;;;
  (defvar night/fim--counter 0
    "Monotonic id source for FIM requests, used to detect stale replies.")

  (defvar-local night/fim--pending nil
    "In-flight FIM request in this buffer, as a plist of :id, :process, :overlay.")

  (defun night/h-fim--report (fmt &rest args)
    "Report FMT/ARGS in the echo area, unless `night/fim-verbose' is nil."
    (when night/fim-verbose
      (message "%s" (concat "FIM: " (apply #'format fmt args)))))

  (defun night/h-fim--report-error (fmt &rest args)
    "Report FMT/ARGS in the echo area as a failure.  Always shown."
    (message "%s"
             (propertize (concat "FIM: " (apply #'format fmt args))
                         'face 'error)))

  (defun night/h-fim--truncate (object &optional len)
    "Return OBJECT as a string of at most LEN (default 200) characters."
    (let ((s (format "%s" object))
          (len (or len 200)))
      (if (> (length s) len)
          (concat (substring s 0 len) "…")
        s)))

;;;
  (defun night/h-fim--provider (&optional name)
    "Return the plist for provider NAME, defaulting to `night/fim-provider'."
    (let ((name (or name night/fim-provider)))
      (or (alist-get name night/fim-providers)
          (error "night/fim: unknown provider `%s'" name))))

  (defun night/h-fim--read-provider (&optional prompt)
    "Read a provider name from `night/fim-providers'."
    (intern
     (completing-read (or prompt "FIM provider: ")
                      (mapcar (lambda (entry) (symbol-name (car entry)))
                              night/fim-providers)
                      nil t nil nil (symbol-name night/fim-provider))))

  (defun night/fim-provider-show ()
    "Echo the current FIM provider and the model it uses."
    (interactive)
    (message "FIM provider: %s (%s)"
             night/fim-provider
             (plist-get (night/h-fim--provider) :model)))

  (defun night/fim-provider-select (provider)
    "Make PROVIDER the default for subsequent FIM completions."
    (interactive (list (night/h-fim--read-provider)))
    ;; Fail before changing anything if the name is not in the table.
    (night/h-fim--provider provider)
    (setq night/fim-provider provider)
    (night/fim-provider-show))

  (defun night/h-fim--extract (extract json)
    "Pull the completion text out of JSON, per the provider's EXTRACT shape."
    (let* ((choices (alist-get 'choices json))
           (choice (when (> (length choices) 0)
                     (elt choices 0))))
      (pcase extract
        ('chat (alist-get 'content (alist-get 'message choice)))
        ('text (alist-get 'text choice))
        (_ (error "unknown :extract shape `%s'" extract)))))

;;;
  (defun night/h-fim--ghost-make (marker)
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

  (defun night/h-fim--ghost-clear (overlay)
    "Remove the pending-request indicator OVERLAY."
    (when (overlayp overlay)
      (delete-overlay overlay)
      (setq night/active-overlays (remove overlay night/active-overlays))))

  (defun night/h-fim--claim (buffer id)
    "Return non-nil if request ID is still the pending one in BUFFER.
Claiming a request clears the pending slot, so it can only succeed once."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq id (plist-get night/fim--pending :id))
          (setq night/fim--pending nil)
          t))))

  (cl-defun night/h-fim--cancel (&key (quiet nil))
    "Abort the in-flight FIM request in the current buffer, if any.
Returns non-nil if a request was actually aborted."
    (when-let* ((pending night/fim--pending))
      ;; Dropping the pending slot invalidates the request's id, so its
      ;; callbacks (including the error `plz' raises for the killed process)
      ;; find themselves stale and stay silent.
      (setq night/fim--pending nil)
      (night/h-fim--ghost-clear (plist-get pending :overlay))
      (let ((process (plist-get pending :process)))
        (when (process-live-p process)
          (delete-process process)))
      (unless quiet
        (night/h-fim--report "aborted"))
      t))

  (defun night/h-fim-escape ()
    "Cancel a pending FIM request on `C-g'.
Always returns nil so that `doom/escape' still does its usual work."
    (night/h-fim--cancel)
    nil)

  (add-hook! 'doom-escape-hook 'night/h-fim-escape)

;;;
  (defun night/h-fim--api-message (body)
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
                (night/h-fim--truncate message))))
          (night/h-fim--truncate (string-trim body)))))

  (defun night/h-fim--error-string (err)
    "Render the `plz-error' ERR as a human-readable string."
    (let ((curl-error (plz-error-curl-error err))
          (response (plz-error-response err)))
      (cond
       (curl-error
        (format "curl error %s: %s" (car curl-error) (cdr curl-error)))
       (response
        (format "HTTP %s — %s"
                (plz-response-status response)
                (night/h-fim--api-message (plz-response-body response))))
       (t
        (format "%s" (or (plz-error-message err) err))))))

;;;
  (cl-defun night/fim-get
      (prefix
       &key
       (suffix nil)
       (provider nil)
       (max_tokens nil)
       (stop nil)
       (temperature nil)
       (model nil)
       (callback)
       (on-error #'night/h-fim--report-error)
       (finally nil))
    "Asynchronously request a FIM completion for PREFIX and SUFFIX.

PROVIDER names an entry in `night/fim-providers', defaulting to
`night/fim-provider'.  MODEL, MAX_TOKENS, STOP and TEMPERATURE override
the provider's entry, which in turn overrides the `night/fim-*' defaults.

CALLBACK receives the completion string.  ON-ERROR is called like
`message' with a format string and arguments describing the failure; it
defaults to reporting the failure in the echo area.  FINALLY, when given,
runs with no arguments after whichever of the two ran.

Returns the curl process so the caller can abort the request, or nil when
no request was made."
    (let* ((name (or provider night/fim-provider))
           (entry (night/h-fim--provider name))
           (url (plist-get entry :endpoint))
           (extract (plist-get entry :extract))
           (model (or model (plist-get entry :model)))
           (max_tokens (or max_tokens (plist-get entry :max-tokens)
                           night/fim-max-tokens))
           (stop (or stop (plist-get entry :stop) night/fim-stop))
           (temperature (or temperature (plist-get entry :temperature)
                            night/fim-temperature))
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
          :timeout night/fim-timeout
          :then (lambda (response)
                  (condition-case err
                      (let ((content (night/h-fim--extract
                                      extract (json-read-from-string response))))
                        (if (stringp content)
                            (funcall callback content)
                          (funcall on-error "no completion in response: %s"
                                   (night/h-fim--truncate response))))
                    (error
                     (funcall on-error "unreadable response (%s): %s"
                              (error-message-string err)
                              (night/h-fim--truncate response)))))
          ;; Without `:else', `plz' hands the `plz-error' struct to `:then'
          ;; instead of a body, and the failure surfaces as a wrong-type error
          ;; inside the process sentinel.
          :else (lambda (err)
                  (funcall on-error "%s" (night/h-fim--error-string err)))
          :finally finally))))

  (comment
   (night/fim-get
    "def randomize_case(text):\n\t"
    :suffix "\n\nprint(randomize_case(\"hello\"))"
    :callback (lambda (result)
                (message "FIM Result:\n%s" result))
    ))
;;;
  (cl-defun night/fim-insert-at-point (&key (point nil) (provider nil) (model nil))
    "Insert a FIM suggestion at the given POINT (default to current point) and highlight it.

With a prefix argument, read the PROVIDER to use for this one call, without
changing `night/fim-provider'.

While the request is in flight a `⋯' indicator sits at POINT and the echo
area says so; the outcome (inserted, empty, or the API's error) is
reported when it completes.  Starting a new request in the same buffer
aborts the previous one, as does `C-g'."
    (interactive (when current-prefix-arg
                   (list :provider (night/h-fim--read-provider))))
    ;; Ahead of `night/h-fim--cancel', so that a refused invocation cannot tear
    ;; down a request that is legitimately in flight.
    (when-let ((blocked (night/h-fim--gate)))
      (if (eq (car blocked) 'info)
          (night/h-fim--report "%s" (cdr blocked))
        (night/h-fim--report-error "%s" (cdr blocked)))
      (cl-return-from night/fim-insert-at-point nil))
    (night/h-fim--cancel :quiet t)
    (let* ((point (or point (point)))
           (buffer (current-buffer))
           (marker (copy-marker point))
           (name (or provider night/fim-provider))
           (model (or model (plist-get (night/h-fim--provider name) :model)))
           (context-bounds (night/h-llm-code-context-bounds point))
           (prefix-start (car context-bounds))
           (suffix-end (cdr context-bounds))
           (prefix (buffer-substring-no-properties prefix-start point))
           (suffix (buffer-substring-no-properties point suffix-end))
           (started (float-time))
           (id (cl-incf night/fim--counter))
           (overlay (night/h-fim--ghost-make marker))
           (guard
            ;; Call FN with the elapsed seconds, but only for the request
            ;; this buffer is still waiting on.
            (lambda (fn)
              (cond
               ((not (buffer-live-p buffer))
                (night/h-fim--report "buffer gone, discarded completion"))
               ((night/h-fim--claim buffer id)
                (funcall fn (- (float-time) started)))))))
      ;; Claim the slot before the request, because a missing API key reports
      ;; synchronously and the guard has to recognise it as current.
      (setq night/fim--pending (list :id id :process nil :overlay overlay))
      (night/h-fim--report "requesting %s…" model)
      (let ((process
             (night/fim-get
              prefix
              :suffix suffix
              :provider name
              :model model
              :callback
              (lambda (result)
                (funcall guard
                         (lambda (elapsed)
                           (night/h-fim-insert-result marker result elapsed))))
              :on-error
              (lambda (fmt &rest args)
                (funcall guard
                         (lambda (elapsed)
                           (night/h-fim--report-error
                            "%s (%.1fs)" (apply #'format fmt args) elapsed))))
              :finally
              (lambda ()
                (night/h-fim--ghost-clear overlay)))))
        (when (eq id (plist-get night/fim--pending :id))
          (setq night/fim--pending
                (plist-put night/fim--pending :process process)))
        process)))

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

  (defun night/h-fim-insert-result (marker result &optional elapsed)
    "Insert RESULT at MARKER, highlight it, and report the outcome.
ELAPSED, when given, is the request's duration in seconds."
    (let ((buffer (marker-buffer marker))
          (took (if elapsed (format " in %.1fs" elapsed) "")))
      (cond
       ((night/whitespace-p result)
        (night/h-fim--report "empty completion%s" took))
       ((not (buffer-live-p buffer))
        (night/h-fim--report "buffer gone, discarded completion"))
       (t
        (with-current-buffer buffer
          ;; Off by default; see `night/fim-strip-leading-space'.
          (when (and night/fim-strip-leading-space
                     (string-prefix-p " " result))
            (setq result (substring result 1)))

          (if buffer-read-only
              (night/h-fim--report-error "buffer is read-only, not inserting")
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
            (night/h-fim--report "inserted %d chars%s"
                                 (length result) took)))))))
;;;
  (define-obsolete-function-alias 'night/mistral-fim-get
    #'night/fim-get "2026-08-31")
  (define-obsolete-function-alias 'night/mistral-fim-insert-at-point
    #'night/fim-insert-at-point "2026-08-31")
  (define-obsolete-variable-alias 'night/mistral-fim-verbose
    'night/fim-verbose "2026-08-31")
  (define-obsolete-variable-alias 'night/mistral-fim-timeout
    'night/fim-timeout "2026-08-31")
;;;
  )
