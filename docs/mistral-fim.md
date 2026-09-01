# Fill-in-the-middle completion

`night/fim-insert-at-point` (bound to `M-.` in normal, insert and visual state,
and to `leader . ,`) sends the text around point to a fill-in-the-middle
endpoint and inserts the completion at point. One keystroke, one line, no
preview step.

Everything lives in `autoload/night-mistral-fim.el` (the filename predates
multi-provider support). `night/fim-get` is the transport layer;
`night/fim-insert-at-point` is the command; `night/h-fim-insert-result` does the
insertion and the highlight.

There is a zsh twin on `alt+.`, `fim-get` in
`~/scripts/zshlang/auto-load/others/fim.zsh`, documented at
`~/scripts/docs/fim.md`. It carries the same provider table and sends the same
body, so a change to either wants the same change to the other. It is also
callable from here as `z fim-get <prefix> <suffix>`, should this ever be worth
collapsing into one implementation.

## Providers

`night/fim-providers` is an alist from a name to a plist. All the FIM APIs
worth using take the *same* request body — `model`, `prompt`, `suffix`,
`max_tokens`, `stop`, `temperature` — so an entry only says where to send it
and how to read the reply:

- `:endpoint` — URL of the FIM completion endpoint
- `:key-fn` — function returning the API key, or nil for an unauthenticated
  endpoint such as a local server
- `:model` — sent as `model`
- `:extract` — `chat` for `choices[0].message.content` (Mistral) or `text` for
  `choices[0].text` (OpenAI-style)
- optional `:max-tokens`, `:stop`, `:temperature` override the corresponding
  `night/fim-*` default for that provider

Configured:

- `codestral` — `codestral.mistral.ai/v1/fim/completions`, `codestral-latest`.
  Fastest of the three, around 0.3s for a line.
- `deepseek` — `api.deepseek.com/beta/completions`, `deepseek-v4-pro`
  (V4-Pro-0813). Around 2s, and noticeably better completions.
- `deepseek-flash` — same endpoint, `deepseek-v4-flash` (V4-Flash-0731).
  Around 1s. FIM on DeepSeek lives on the `/beta` base URL, is still flagged
  beta, and works in non-thinking mode only.

`night/fim-provider` picks the default; `night/fim-provider-select`
(`leader . f`) changes it and `night/fim-provider-show` (`leader . F`) echoes
it. `f`/`F` because `p`/`P` are taken by `ellama-command-map`.

`C-u M-.` reads a provider for that one call without changing the default.

There is deliberately **no** automatic fallback to a second provider on error.
A hotkey that silently switches models hides an expired key for weeks, and
switching is one keystroke.

## Who actually has a FIM API

Worth recording, because it is mostly a list of dead ends.

Native FIM, meaning a real `suffix` field rather than prompt engineering:
Mistral/Codestral, DeepSeek, Ollama (`suffix` on `/api/generate`), llama.cpp
(`/infill` with `input_prefix`/`input_suffix`), and any OpenAI-compatible host
serving a FIM-trained model — Qwen-Coder, StarCoder2, CodeGemma — through
legacy `/v1/completions` with `suffix`. The last group needs no new code here,
only a table entry with `:extract text`.

No FIM API at all:

- **Google.** The Gemini API is `generateContent` only. Vertex's Codey
  `code-gecko@002` did have literal `prefix`/`suffix` fields, but that is
  PaLM-era and the Vertex generative SDK path is deprecated (removal
  2026-06-24). CodeGemma is genuinely FIM-trained, with `<|fim_prefix|>` /
  `<|fim_suffix|>` / `<|fim_middle|>` tokens, but Google does not serve it with
  a FIM endpoint — it reaches us through Ollama or llama.cpp. Vertex's actual
  answer to "we need FIM" is reselling Mistral's Codestral 2.
- **OpenRouter** — chat-only, no `suffix` in the schema, so that key is no help.
- **Groq** — chat-only. **Anthropic** — none. **OpenAI** — effectively gone;
  `suffix` survives only on legacy `/v1/completions` with
  `gpt-3.5-turbo-instruct`.

## Why not a package

[minuet-ai.el](https://github.com/milanglacier/minuet-ai.el) (GNU ELPA) is the
one package that really does multi-provider FIM, and it has no direct-insert
command: its UX is ghost text plus `minuet-accept-suggestion-line`. Adopting it
means either taking on a preview step we do not want, or wrapping its private
`minuet--<provider>-complete` functions — still a custom command, now on
undocumented internals — and losing the echo-area error reporting below, since
minuet logs failures to its own `*minuet*` buffer.
[wingman](https://github.com/mjrusso/wingman) is the same shape: ghost text,
llama.cpp `/infill` plus gptel-emulated FIM, no Codestral or DeepSeek.

Since every native FIM API takes an identical body, the provider table above is
the entire thing a package would have given us. minuet is still worth
installing separately, on a different key, if ghost text is ever wanted.

## Feedback while the request is in flight

A request is asynchronous and usually takes a fraction of a second, but on a
slow link it can take long enough that "nothing happened" is indistinguishable
from "it failed". Two things say otherwise:

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
- `FIM: no API key for deepseek` — reported before any request is sent, rather
  than shipping `Bearer nil` and reading back a 401
- `FIM: HTTP 401 — Authentication Fails, Your api key: ****-key is invalid` —
  and any other failure

Failures are shown in the `error` face and ignore `night/fim-verbose`;
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

The transport follows plz's own split: `:callback` for the completion,
`:on-error` called like `message` with a human-readable description, and an
optional `:finally`. Response parsing is one `condition-case` around
`night/h-fim--extract` rather than a ladder of `if`s.

`night/h-fim--error-string` renders a `plz-error`: curl failures as
`curl error N: …`, HTTP failures as the status plus the API's own message.
Providers disagree on where that message lives — Mistral uses `detail` for auth
and validation failures and `message` elsewhere, DeepSeek uses the OpenAI-shaped
`error.message` — so `night/h-fim--api-message` tries all three before falling
back to the raw body.

## Cancellation and concurrency

Each request gets an id from `night/fim--counter`, and the buffer-local
`night/fim--pending` holds the current one's id, process and indicator. A reply
only acts if it can claim that slot (`night/h-fim--claim`), and claiming clears
it, so a reply can act at most once.

The slot is claimed *before* the request is sent, not after: a missing API key
reports synchronously, and the guard has to recognise that report as current or
it would be swallowed as stale.

`night/h-fim--cancel` drops the slot first and only then kills the curl
process. That ordering matters too: killing the process makes `plz` report a
curl failure, and the handler must already look stale by the time it runs, or
every cancellation would announce itself as an error.

Consequences:

- Pressing `M-.` again supersedes the previous request instead of racing it, so
  an impatient double press cannot produce two insertions.
- `C-g` aborts, via `night/h-fim-escape` on `doom-escape-hook`. It returns nil
  so that `doom/escape` still performs its normal quit — `doom-escape-hook` runs
  under `run-hook-with-args-until-success`.
- `:noquery t` keeps a pending request from blocking Emacs exit.
- `night/fim-timeout` (20s) caps the request. `plz` sets no total timeout by
  default; only `plz-connect-timeout` applies, and it covers the connect phase
  alone.

## Options

- `night/fim-provider`, default `codestral`.
- `night/fim-max-tokens` (64) and `night/fim-stop` (`"\n"`). Together these cap
  the completion at one line *during generation*, rather than truncating a
  longer one after paying for it. Note that `nil` in a provider entry means
  "inherit", not "no stop sequence".
- `night/fim-temperature`, default 0.
- `night/fim-verbose`, default `t`.
- `night/fim-timeout`, default 20 seconds.

The old `night/mistral-fim-*` names remain as obsolete aliases.
`night/mistral-fim-model` is gone, replaced by the `:model` of the selected
provider.
