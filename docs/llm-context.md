# What a command may read, and whether it may send at all

Five things send buffer text to a third-party model:

- `night/fim-insert-at-point` (`M-.`), and its scoped siblings
- `night/ellama-code-fill-in-the-middle` (`leader . .`)
- `night/ellama-code-complete`
- `night/ellama-complete`
- `copilot-mode` (`leader c o`, and `C-.` via `night/copilot-ensure`)

They agree on two things, and this file is where both live —
`autoload/night-llm-context.el`. A **scope** says how much of the buffer a
command may read. A **path policy** says whether it may read the buffer at
all. FIM's own machinery — transport, providers, insertion — stays in
`docs/mistral-fim.md`.

## Why a third file

`night-mistral-fim.el` is wrapped in `(after! (night-openai night/ellama) …)`
and `provide`s no feature, so its body evaluates strictly after
`night-ellama.el`'s in the same `provide` cascade and nothing can depend on it.
The shared code therefore cannot live in either file without inverting a
dependency; both `after!` this one instead.

The names were all `night/fim-*` when this served only FIM. Every public one
keeps an obsolete alias, and the variable aliases are declared *before* their
referents on purpose — the other way round the new `defvar` wins and a value
customized under the old name is silently dropped.

## The window

`night/llm-context-bounds` computes it, and all four commands call it:

    (night/llm-context-bounds :pos POS :before N :after M :line-tol T)

`:before` and `:after` default to `night/llm-context-before-fast` and
`-after-fast` (1000), which is what the FIM commands use. The ellama commands
pass `night/llm-context-before` / `-after` (10000). A budget of 0 pins that
side to point exactly, which the two prefix-only commands need: rounding out
there would reach past point and hand the model the answer.

The window is rounded out to whole lines when that costs at most
`night/llm-context-line-tol` (200) characters, so a model is never handed half
a token, and one very long line cannot drag in far more than was asked for.

### The tolerance was dead for a long time

The test used to read

    (> (- start-of-line-before before-point) line-tol)

and `start-of-line-before` is never greater than `before-point`, so the
difference was never positive and the branch could not fire. The window always
rounded out, however long the line — the exact opposite of what the tolerance
exists for. The suffix side had the same inversion. `night/h-llm--snap`
subtracts the other way round and measures what rounding actually costs.

## Scope: how much it is allowed to read

A completion sends the text around point. A *scope* narrows which text that
may be, independently of the privacy policy below:

- `nearby` — the ±1000 char window described under **Options**. The default,
  and what FIM always did before.
- `block` — the enclosing block. In `org-mode` the Org block around point
  (`src`, `example`, `quote` and the rest); anywhere else, the enclosing
  defun.
- `subtree` — the current heading and its children. `org-mode` only.
- `buffer` — the whole file. Only Copilot, which cannot do anything narrower.

For the first three, what is actually sent is the scope **intersected with**
the `nearby` window: a scope only ever narrows, and cannot buy a bigger budget
than `night/llm-context-before-fast` allows. `buffer` carries `:windowed nil`
and is the exception — Copilot syncs the whole file, and intersecting that with
a ±1000 window would show a reassuring lie. Its bounds are *widened* ones,
because `copilot--get-source` widens.

A caller says which scopes it can honour with `:scopes`, defaulting to the
three window-based ones. That list also decides how the caller is judged: where
the buffer's own scope is not one it can honour, the gate uses the widest scope
it *can* do. Without that, Copilot with the default `nearby` in force would be
waved through by a confirmation you had granted to FIM for a thousand
characters.

The ranking is the reason `buffer` is widest rather than just another entry.
Consent is stored as a rank, so agreeing to send the whole file covers a later
FIM window, and agreeing to a FIM window does **not** cover Copilot. Measured
in both directions.

`night/llm-scope` sets it everywhere, `night/llm--scope-local` per buffer, and
an explicit `:scope` for one call; each overrides the one before it. Only
`night/llm-scope-select` (`leader . o`, this buffer) and
`night/llm-scope-select-global` (`leader . O`) ever write those two. Nothing
else does — not the chooser, not the per-scope commands, not the privacy
prompt — so what a keystroke sends never changes behind your back.
`night/llm-scope-show` (`leader . C-o`) reports all three.

One-shot commands, which read that much regardless of the buffer's scope:
`night/fim-insert-in-block` (`leader . b`), `night/fim-insert-in-subtree`
(`leader . h`, and `alt+cmd+.`), `night/fim-insert-nearby` (`leader . n`), and
`night/fim-insert-choose` (`leader . C-,`), which asks.

A scope that does not resolve — `subtree` outside `org-mode`, `block` with
point in neither a block nor a defun — **refuses**. It does not quietly fall
back to something wider. Widening in silence is the single failure this whole
mechanism exists to prevent, and it is the failure you would never notice.

### Choosing one, and seeing it

The chooser highlights every candidate *at once*, in three nested faces, and
then takes a single keypress.

That works because the candidates nest: `block ∩ window` sits inside
`subtree ∩ window` sits inside `window`. One rendering therefore answers all
three questions, where previewing one at a time would show strictly less for
strictly more keystrokes. `night/llm-scope-block-face` is the strongest of the
three and has the highest overlay priority, so the innermost region wins where
they overlap.

The sizes are in the prompt as well as on screen, because a subtree is
routinely taller than the window: the highlight alone would quietly
under-report what is about to leave the machine.

Overlays are removed in an `unwind-protect`, so aborting the prompt cannot
leave the buffer painted, and they are registered in `night/active-overlays`
so `C-g` is a second net under that.

The regions are deliberately *not* split into prefix and suffix halves. Six
faces is unreadable, and the cursor already marks where the split falls.

### Flashing what went

`night/llm-flash-context` (default `t`) flashes the region that was actually
sent, in the scope's own face, so the colour matches whatever the chooser
showed when you picked it. Every command that sends does it, in every buffer,
not only the ones the policy asks about.

## What it refuses to send

These commands send buffer text to a third-party API, which is fine for code
and wrong for a decrypted GPG file. `night/llm-path-policy` decides, per
buffer, whether any of them may run at all. Its default:

    ((encrypted                  . refuse)
     ("/\\.keys/"                . refuse)
     ("/\\.privateShell\\Z"      . refuse)
     ("/\\.authinfo(\\.gpg)?\\Z" . refuse)
     ("/\\.netrc\\Z"             . refuse)
     ("/\\.ssh/"                 . refuse)
     ("\\A/private/(tmp|var)/"   . allow)
     ("/notes/private/research/" . allow)
     ("/private/"                . confirm))

Each rule pairs a matcher with a level. A matcher is a PCRE, or a symbol
naming a predicate in `night/h-llm-policy-predicates` — `encrypted` is the
only one so far, and resolves to `night/buffer-encrypted-p`. A level is
`refuse` (decline, naming the rule that said so), `confirm` (ask first) or
`allow` (send).

The first matching rule decides, which is what makes exceptions expressible:
an `allow` rule for `/private/pub/` placed above the `confirm` rule for
`/private/` exempts that subtree. It is equally the hazard — a broad `allow`
near the top disarms everything under it — so the specific rules go on top. A
buffer matching no rule is sent exactly as before, and a buffer visiting no
file matches no PCRE rule.

The `allow` rule for `/private/tmp/` and `/private/var/` is that mechanism
earning its keep rather than a special case: macOS resolves `/tmp` and `/var`
into `/private/`, so a buffer visiting `/tmp/scratch.py` has that in its
truename and would otherwise ask on every scratch file. A negative lookahead
would have been the obvious fix elsewhere; Emacs regexps have none, and
ordering does the job instead.

A `confirm` raises the scope chooser rather than a yes/no question, so the
answer to "may I send this?" can be "only this block". Cancelling declines, as
a `no` did.

What it remembers is the scope you approved at, not a bare yes —
`night/llm--path-confirmed`. A later completion in that buffer goes through
unasked while its scope is no wider (`block` < `subtree` < `nearby`); a wider
one asks again. Without that, approving `block` once and then pressing `M-.`
would hit the remembered approval and fall through to the buffer's own scope,
usually `nearby` — consent for a block silently spent on the whole window.
Recording the scope is not the same as *setting* it: this never feeds
`night/h-llm--scope-effective`, so the rule that only the selectors change a
scope still holds.

It stays buffer-local and is never persisted, so killing and revisiting the
file asks again.

An explicit scoped command — `alt+cmd+.` and friends — is its own consent and
is not prompted at all: it already named what it sends. A `refuse` rule still
refuses it.

There is nothing that overrides a `refuse`. That matches the Hammerspoon twin,
whose Secure Input check offers no way through either.

### Both names, always

Every PCRE is tested against the buffer's `buffer-file-name` *and* against its
`file-truename`. A symlink whose own name looks harmless can point into a tree
that is not, and the reverse turns up just as often, so matching only one of
the two would leave the policy easy to step around by accident.

### Failing closed

Three things can be wrong with a policy, and all three refuse rather than
proceed: a matcher naming a predicate that does not exist, a level nobody
defined, and a PCRE that will not convert. A typo must not quietly widen the
policy — the whole trouble with a guard that stops matching is that nothing
tells you.

### `\z` is a trap

`night/pcre-to-regexp` converts PCRE with `pcre2el`, which implements `\A` and
`\Z` but *not* `\z`: it renders that as a literal `z`, without complaining.
`"\\.age\\z"` therefore comes back matching `.agez`, which no file is. Write
`\Z`. The conversion now rejects `\z` outright, so this cannot recur silently.

Lookaround is unavailable for real, Emacs regexps having none, so a rule using
it refuses under the previous heading rather than being skipped.

`pcre2el` is declared in `packages.el`. Until the next `doom sync` it sits on
disk unactivated, so `night/h-pcre-available-p` points `load-path` at
straight's build directory itself; afterwards the plain `require` succeeds and
that second arm never runs.

### The shared predicate

`night/buffer-encrypted-p` is not FIM's own. It lives in `night-buffer.el`,
over `night/file-encrypted-p` in `night-file.el`, and it is also what
`night/close-fileless-buffers` and the evil marker persistence use to
recognise an encrypted file. All three used to spell it as a separate
`(s-ends-with-p ".gpg" ...)`; they now agree with each other, and agree about
`.age`, `.asc` and `.pgp` as well.

The buffer-level predicate adds the one thing a filename cannot show: a
buffer-local `epa-file-encrypt-to`, which epa leaves set in a buffer it
decrypted. The locality test is load-bearing, because `epa-file-encrypt-to` is
also a global preference — read globally it would declare every buffer in the
session encrypted.

## Copilot

Copilot is gated at **mode activation**, not at completion, and that is the
whole design. `copilot--mode-setup` ends with `(copilot--on-doc-focus
(selected-window))`, which sends a `textDocument/didOpen` carrying
`copilot--get-source` — the buffer text — the instant the mode turns on. Every
later edit streams a `didChange` delta, so the agent holds a live mirror. A
completion request itself (`copilot--generate-doc`) carries only a position
and a path. By the time you press `C-.`, the file has already gone.

Two traps worth recording. `copilot--get-source` does `save-restriction` +
`widen`, so **narrowing does not protect you**; `copilot-max-char` is 100000,
so anything smaller goes whole. And `copilot-disable-predicates` looks exactly
like the supported hook for this — it is a `defcustom`, stable across upgrades,
two lines to use — but it is consulted only in
`copilot--post-command-debounce`, which decides whether to *request a
completion*, and never touches the sync. It would be a guard that reads
correctly and stops nothing.

`night/h-copilot-gate` is `:around` advice on `copilot-mode`. It passes through
any call that disables, and any call in a buffer where the mode is already
live — past that point there is nothing left to protect. Otherwise it asks,
offering only the `buffer` scope, so the chooser reduces to a confirmation that
highlights the whole file and names its size.

`night/h-copilot--enabling-p` mirrors the `cond` that `define-minor-mode`
generates: `toggle` flips, a number below 1 disables, anything else — nil and
an omitted argument included — enables.

### A minor mode set by a file-local never ran

`(copilot-mode . t)` is in `safe-local-variable-values`. A file-local variable
that happens to name a minor mode **sets the variable without running the mode
body**, so no hooks are installed and nothing is synced. It is not a leak.

It did break `night/copilot-ensure`, which tested `bound-and-true-p` and so
skipped real activation, leaving `C-.` doing nothing useful in such a file.
`night/h-copilot-active-p` asks instead whether `copilot--mode-setup` actually
put its hook on the buffer. That is the one private Copilot symbol read here,
so a load-time `fboundp` check warns if an upgrade renames it; the failure mode
is the predicate going false, which costs a redundant activation, never a
silent send.

## Everything else that talks to a model

Not covered, and worth knowing about:

- The upstream `ellama-command-map` on `leader .` — around seventeen
  `ellama-stream` call sites, mostly sending the region or the whole buffer.
- gptel (`autoload/night-gptel.el`), where `gptel-org-branching-context` sends
  the Org ancestry of point and `gptel-log-level` writes prompts to a log
  buffer.
- chatgpt-shell and its `ob-chatgpt-shell` babel blocks.

`whisper` is local-only — `whisper.cpp` under `~/code/misc/` — and its keymap
is inside a `(comment …)`, so it is not a sender.
