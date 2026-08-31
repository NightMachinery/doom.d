# Codestral fill-in-the-middle

`night/mistral-fim-insert-at-point` (bound to `M-.` in normal, insert and
visual state, and to `leader . ,`) sends the text around point to Codestral's
fill-in-the-middle endpoint and inserts the completion at point.

Everything lives in `autoload/night-mistral-fim.el`. `night/mistral-fim-get`
is the transport layer; `night/mistral-fim-insert-at-point` is the command;
`night/h-mistral-fim-insert-result` does the insertion and the highlight.

## Feedback while the request is in flight

A request is asynchronous and usually takes a fraction of a second, but on a
slow link it can take long enough that "nothing happened" is indistinguishable
from "it failed". Two things now say otherwise:

- The echo area says `FIM: requesting codestral-latest…`.
- A `⋯` in the `shadow` face sits at the insertion point. This is the part the
  echo area cannot do: it marks *where* the completion will land, and it
  survives any other package writing a message over yours.

The indicator is registered in `night/active-overlays`, so `night/clear-overlays`
on `doom-escape-hook` disposes of it along with everything else.

## Feedback when it finishes

Every terminal path reports, with the elapsed time:

- `FIM: inserted 13 chars in 0.3s`
- `FIM: empty completion in 0.3s` — the model returned only whitespace
- `FIM: buffer is read-only, not inserting`
- `FIM: buffer gone, discarded completion` — the buffer was killed mid-request
- `FIM: aborted` — cancelled by `C-g`
- `FIM: HTTP 401 — Invalid API Key (0.1s)` — and any other failure

Failures are shown in the `error` face and ignore `night/mistral-fim-verbose`;
successes and progress honour it.

## Why the errors were invisible before

Two independent bugs, both of which sent the failure into a process sentinel
where nothing readable comes out of it:

- `plz` was called without `:else`. Its documented behaviour in that case is to
  call `:then` with a `plz-error` struct *instead of* the response body, so a
  401 ran `json-read-from-string` on a struct and signalled
  `wrong-type-argument` from the sentinel.
- The eight in-band error branches were written as
  `(funcall callback (error "…"))`. `error` signals while its arguments are
  being evaluated, so the callback was never reached and the code after it was
  unreachable.

The transport now follows plz's own split: `:callback` for the completion,
`:on-error` called like `message` with a human-readable description, and an
optional `:finally`. Response parsing is one `condition-case` around a
straight-line extraction rather than a ladder of `if`s.

`night/h-mistral-fim--error-string` renders a `plz-error`: curl failures as
`curl error N: …`, HTTP failures as the status plus the API's own message.
Mistral spells that message `detail` for auth and validation failures and
`message` elsewhere, so `night/h-mistral-fim--api-message` tries both (plus the
OpenAI-shaped `error.message`) before falling back to the raw body.

## Cancellation and concurrency

Each request gets an id from `night/mistral-fim--counter`, and the buffer-local
`night/mistral-fim--pending` holds the current one's id, process and indicator.
A reply only acts if it can claim that slot (`night/h-mistral-fim--claim`), and
claiming clears it, so a reply can act at most once.

`night/h-mistral-fim--cancel` drops the slot first and only then kills the curl
process. That ordering matters: killing the process makes `plz` report a curl
failure, and the handler must already look stale by the time it runs, or every
cancellation would announce itself as an error.

Consequences:

- Pressing `M-.` again supersedes the previous request instead of racing it, so
  an impatient double press cannot produce two insertions.
- `C-g` aborts, via `night/h-mistral-fim-escape` on `doom-escape-hook`. It
  returns nil so that `doom/escape` still performs its normal quit —
  `doom-escape-hook` runs under `run-hook-with-args-until-success`.
- `:noquery t` keeps a pending request from blocking Emacs exit.
- `night/mistral-fim-timeout` (20s) caps the request. `plz` sets no total
  timeout by default; only `plz-connect-timeout` applies, and it covers the
  connect phase alone.

## Options

- `night/mistral-fim-model`, default `codestral-latest`. The `:model` argument
  of `night/mistral-fim-insert-at-point` used to be accepted and then dropped
  on the floor; it is now passed through.
- `night/mistral-fim-verbose`, default `t`.
- `night/mistral-fim-timeout`, default 20 seconds.
