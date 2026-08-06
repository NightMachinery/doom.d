# Org LaTeX preview performance on large files

## Problem

Stock org (<= 9.7) previews LaTeX **synchronously, one fragment at a
time**: each fragment spawns its own `latex` run (re-reading the entire
preamble every time, ~0.3-1 s) plus `dvisvgm`. A file with hundreds of
fragments (e.g. LLM-generated notes converted via
[md2org](~/scripts/docs/md2org-latex/readme.md) — 539 fragments) freezes Emacs
for **minutes**.

**The actual trigger in this config** (found 2026-08-02 during
verification): `night/org-latex-preview-buffer`
(autoload/org/night-ui.el) is called by `night/org-interactive-startup`
on every graphical org-file open and used to call the internal
`org--latex-preview-region` over the whole buffer directly — a
synchronous whole-buffer compile on open, regardless of `#+STARTUP:
latexpreview`. It now routes through the lazy machinery below. (The
`org-latex-preview` advice alone did not help this path, since the
internal function was called directly.)

Note the flip side of org's design: preview images are cached by a content
hash in `org-preview-latex-image-directory` (default `ltximg/` next to the
file), so *re*-opening an unchanged file is fast. The hang is first-ever
previews, and again after edits or theme/scale changes (the hash covers the
fragment text, preamble, options, and resolved fg/bg colors; our config
pins `:foreground "black"` which keeps hashes theme-stable).

## Strategies considered (2026-08-02)

### 1. Lazy windowed preview — IMPLEMENTED

`night/org-latex-preview-lazy` in
`autoload/org/night-latex-preview-lazy.el`:

- One `org-element` parse collects all fragment positions (no LaTeX).
- **Nothing compiles synchronously and no timer is scheduled directly.**
  The drain arms via a one-shot `post-command-hook`: timers (idle *or*
  wall-clock) fire during any `sit-for` — including ones inside the very
  command/eval that opened the file, since Lisp execution does not reset
  the user-idle clock — so scheduling immediately lets the drain hijack
  and stretch the opening command itself (observed: a ~1 s open stretched
  to minutes). Redisplay-time hooks (`window-buffer-change-functions`)
  are unsafe for the same reason.
- Once armed, each tick works until a wall-clock budget is spent
  (`night/org-latex-preview-lazy-tick-seconds`, default 0.5), with
  wall-clock rests (`night/org-latex-preview-lazy-rest-delay`) in
  between so the event loop breathes; the drain parks on an idle timer
  whenever the user is active or in the minibuffer. The time budget
  (rather than a fragment count) is what makes warm caches fast: a
  cache-hit render costs ~1ms (no LaTeX runs), so a previously-previewed
  buffer drains in bulk — measured 539 warm fragments in 0.13s — while a
  cold compile (~0.3-1s) naturally caps a tick at about one fragment.
- Each tick re-prioritizes the queue toward the current viewport, so
  previews follow where you are looking.
- `night/org-latex-preview-lazy-stop` cancels; already-previewed fragments
  (existing overlays) are skipped without consuming chunk slots.

Verified 2026-08-02 in an isolated Doom instance on the 539-fragment
file with a cold cache: open 1.4 s (was minutes); eval latency during
the drain 0.1-1 s typical; drain ~2 fragments/s, completing in ~5 min
with all 539 preview overlays present.

**Lazy is the default for every multi-fragment preview path** (extended
2026-08-06 after "RET on a heading freezes Emacs" report):

- `night/org-latex-preview-lazy-region` is the region-bounded entry
  point; it merges into an in-progress queue instead of restarting it,
  and extends its bounds to element boundaries (a narrowed parse of a
  partial element would misread it; the extension stops at boundaries so
  it never swallows a following headline's whole subtree).
- The `org-latex-preview` advice reroutes the `'(16)` whole-buffer path
  — which includes org's `#+STARTUP: latexpreview` handling during
  `org-mode` initialization (org.el ~line 5102), i.e. the case where
  Emacs would freeze *before the user can run any command* — plus the
  active-region path and the no-prefix "point not on a fragment" path.
  That last one renders the whole *section* synchronously in stock org,
  and org-fragtog's fragment-exit handler (no-arg `org-latex-preview`)
  can land there when its stale parse misses the fragment. Toggling the
  single fragment at point, and the clearing prefixes, stay synchronous.
- `night/org-dwim-at-point` (RET on a headline) used to call the
  internal `org--latex-preview-region` over the **whole subtree**
  directly, bypassing the advice entirely — this was the "click a
  heading and all its fragments render synchronously" freeze. It now
  routes through `night/org-latex-preview-lazy-region`.

**Edits and pastes are watched** (same date): the queue used to be a
one-shot snapshot — fragments pasted after the drain finished had no
path into the system. Now the first lazy run installs a buffer-local
`after-change-functions` watcher (O(1): it only widens a dirty-region
marker pair and re-arms via the post-command hook). The next tick
rescans just the dirty region (element-aligned) and merges new,
un-previewed fragments into the queue. The fragment containing point is
left to org-fragtog's exit handling, so half-typed LaTeX is never
compiled. Interactive `night/org-latex-preview-lazy-stop` removes the
watcher; normal drain completion keeps it.

Total CPU work is unchanged — this converts "frozen for minutes" into
"progressive and responsive". Pauses of up to ~1-2 s per chunk can still be
felt while it works through the queue.

**Pinning previews** (`night/org-latex-preview-pin-toggle`, same file):
stock org previews are sticky — they only disappear on text edits or
explicit toggles; the hide-under-cursor behavior comes from
`org-fragtog-mode` (enabled per buffer by
`night/org-interactive-startup`). The toggle disables fragtog in the
buffer and lazily restores the fragments it left raw; toggling again
re-enables fragtog.

**Per-buffer cache clearing**
(`night/org-latex-preview-cache-clear-buffer`, same file): deletes the
cached images of the current buffer's fragments only — for previewed
fragments the exact path is read off the overlay's display plist; for
the rest the content hash is recomputed exactly as `org-format-latex`
does (verified byte-identical against a real compile). Caveats, since
the cache dir is shared and content-addressed: identical fragment text
in another file shares the same image file (clearing here cold-caches
it there too), and the hash includes theme-resolved colors, so images
rendered under a different theme are not found. Preview overlays are
cleared too, so the next preview command recompiles from scratch —
useful for testing cold-cache behavior.

**Global pinning** (`night/org-latex-preview-pin-global-toggle`): same
idea for all org buffers, current and future — sweeps `(buffer-list)`
and sets `night/org-latex-preview-pin-global-p`, which
`night/org-interactive-startup` consults for newly opened files. Simple
stomp semantics: per-buffer overrides survive only until the next
global toggle; unpinning re-enables fragtog in graphical sessions.

### 2. Cache warming (not implemented)

Point `org-preview-latex-image-directory` at one absolute shared dir and
have the md2org pipeline (or a `night/org-latex-warm-cache` command) run a
**background batch Emacs** that pre-populates the cache; opening the file
then hits cache for every fragment. Requires exact hash fidelity between
the batch process and the GUI (same options plist, same resolved colors —
our pinned `:foreground "black"` helps; `clear-image-cache` must be stubbed
in batch). Only helps files warmed in advance.

### 3. The real fix: async preview overhaul (tecosaur/karthink)

Complete rewrite of the preview system: fully async, **one LaTeX process
for all fragments**, **precompiled preamble** (compiled once ever via
`mylatexformat`), streaming dvisvgm, built-in persistent cache,
auto/live-preview modes. Order-of-magnitude faster; Emacs never blocks.

- Lives on the `dev` branch of <https://git.tecosaur.net/tec/org-mode>;
  setup guide: <https://abode.karthinks.com/org-latex-preview/> (includes a
  Doom `packages.el` recipe).
- Expected to merge into mainline org ~9.8.
- Not adopted yet because: replaces org itself (fork build must load before
  all org-dependent packages), reported macOS SVG/Ghostscript issues,
  conflicts with org-fragtog etc., and open blocker issues as of 2026-04.
- Karthik's standalone `org-preview` package is an archived
  proof-of-concept — the fork is the only maintained form.

### 4. Cheap knobs (partially relevant)

`process-adaptive-read-buffering nil`, pipes instead of ptys, larger
`read-process-output-max` — community-measured but modest gains (tens of
percent). Avoid `#+startup: latexpreview` on huge files (our config already
sets `org-startup-with-latex-preview nil`).

## Decision

Option 1 implemented now; revisit option 3 when the overhaul merges into
mainline org (or if first-preview latency becomes painful enough to accept
fork risk). Option 2 remains available as an add-on for the md2org
pipeline.

## Migration watch

`night/org-latex-preview-new-system-p` (same file) detects the new system
by the presence of the dedicated `org-latex-preview.el` library, which
stock org <= 9.7 does not have. At startup, if it is detected, a
`night-org` warning reminds us that:

- `night/org-latex-preview-lazy` targets old internals
  (`org--latex-preview-region`, `org-overlay-type` overlays) and is
  obsolete under the new system (which previews asynchronously natively);
- other `night/` code touching `org-format-latex` /
  `org-create-formula-image` should be audited.

The lazy command also refuses to run under the new system.

## References

- Scaling analysis: <https://karthinks.com/software/scaling-latex-previews-in-emacs/>
- Overhaul thread: <https://list.orgmode.org/orgmode/87lek2up0w.fsf@tec.tecosaur.net/>
- Todo/issue tracker: <https://github.com/tecosaur/org-latex-preview-todos>
- Related bug doc: `~/scripts/docs/md2org-latex/readme.md` (moved to the scripts repo, where the md2org tooling lives)
