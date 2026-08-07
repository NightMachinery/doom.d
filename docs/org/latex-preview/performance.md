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

**Lazy is the default for every multi-fragment preview path via ONE
choke-point advice** (2026-08-06, reworked same day after "RET on a
heading freezes Emacs" report): `org--latex-preview-region` is the
internal function every synchronous preview funnels through — all
`org-latex-preview` branches (whole-buffer `C-u C-u` including
`#+STARTUP: latexpreview` during `org-mode` initialization, org.el
~line 5102, i.e. the case where Emacs would freeze *before the user
can run any command*; active region; the no-prefix section path that
org-fragtog's exit handler can land in; the single-fragment toggle),
the `night/org-dwim-at-point` headline branch (the original "RET on a
heading renders the whole subtree synchronously" freeze), and
`night/org-latex-preview-buffer`. A single `:around` advice
(`night/h-olpl-around-org--latex-preview-region`) therefore covers
every current and future caller with no per-site patching (an earlier
iteration advised `org-latex-preview` and patched call sites
individually — all reverted). Dispatch, cheapest test first:

- The drain's own compiles pass through untouched (a let-bound
  reentrancy flag, `night/h-olpl-inhibit-reroute`).
- A raw regexp candidate count with early exit (org's own math-start
  regexp; C-speed, no element parsing) lets regions with at most
  `night/org-latex-preview-lazy-sync-threshold` fragments (default 1)
  run synchronously — the org-fragtog exit-re-render hot path costs
  ~one bounded regexp search. The threshold is 1 because the cost
  driver is cold compiles (~0.3-1s each vs ~1ms cache hits) and
  coldness is unknowable at dispatch time: 1 bounds the worst
  synchronous freeze to a single LaTeX run while keeping
  single-fragment toggles instant.
- Otherwise a precise scan runs: `night/h-olpl-fragments` now uses the
  `org-format-latex` technique — regexp candidates confirmed by the
  cache-backed `org-element-context` — so its cost is proportional to
  the number of math candidates, not buffer size (the old
  `org-element-parse-buffer` ignored org's element cache entirely).
  Real count <= threshold still syncs.
- Cache-hit dispatch (unified rule): if the region has at most
  `night/org-latex-preview-lazy-sync-cached-max` fragments (default
  5000; the count is checked *before* any hashing so the cap bounds
  the check itself) and at most `...-sync-threshold` of them are
  UNCACHED (sha1 + `file-exists-p`, ~10-30µs each, early-exiting once
  the miss count exceeds the threshold), the region renders
  synchronously — warm renders cost ~0.25ms per fragment, so "cached ⇒
  instant", tolerating the usual cold-compile budget. A mostly-cached
  region with one edited formula therefore still appears immediately;
  this subsumes both the plain count-≤-threshold case and the
  all-cached case.
- Everything else merges into the lazy queue (deduplicated, bounds
  extended to element boundaries) and drains as described above.

**Pasted fragments** (same date): the queue is a one-shot snapshot and
org-fragtog only previews the fragment point *exits*, so bulk pastes
used to slip through entirely (typed LaTeX needs no handling — fragtog
covers it). A single `:around` advice on `insert-for-yank` — the paste
choke point that `yank`/`yank-pop`, evil's `p`/`P`, `org-yank`, mouse
yanks, and this config's `night/org-paste-*` helpers all funnel
through — records the pasted span in a dirty-region marker pair and
arms the drain; the next tick rescans just that span (element-aligned)
and queues any new fragments. The fragment containing point is left to
fragtog so half-typed LaTeX is never compiled.

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

**Modes** (`night/org-latex-preview-lazy-mode`, set with
`night/org-latex-preview-lazy-mode-set` — plain call is buffer-local,
prefix arg sets the global default and stomps buffer-locals):

- `original`: both advices pass straight through, restoring stock
  *synchronous* previews, and the lazy commands refuse — the emergency
  kill switch for working around bugs. Switching to it halts any
  in-flight drain and warming immediately.
- `timer-ticks`: foreground drain only (the pre-warming behavior).
- `timer+bg`: the drain compiles at most one cold fragment per tick
  *while* background pipelines warm the rest in parallel (an in-flight
  set prevents duplicate work).
- `bg` (default): fully event-driven — no queue, no timers, no idle
  waits. Dispatch renders already-cached fragments synchronously on
  the spot (a warm file renders *during the open itself*, measured
  59ms for a 14-fragment buffer) and pipelines the cold ones; each
  pipeline's sentinel renders its chunk the moment it lands, with
  per-chunk progress messages and a final summary. The foreground
  never runs LaTeX: even a single cold fragment goes through a solo
  pipeline (same wall latency as blocking — identical processes plus
  ~30-50ms of fork overhead — but zero freeze). Set
  `night/org-latex-preview-lazy-bg-sync-threshold` (default 0) above
  zero to let that many uncached fragments compile synchronously (the
  old blocking-but-atomic feel for single-fragment toggles).
  Sentinel-rendered fragments are re-validated against the current
  buffer first, so fragments edited mid-compile render nothing (their
  next preview recompiles), duplicate-content fragments share one
  compile, and the ASYNC path leaves the fragment under point to
  org-fragtog. The synchronous cached-render path deliberately does
  NOT skip the fragment under point: org-fragtog's exit handler
  re-previews with point save-excursion'd back inside the fragment,
  so skipping there broke every fragtog exit re-render.
  `revert-buffer' re-runs the graphical startup (fragtog + previews)
  via `after-revert-hook' — reverting reinitializes modes and drops
  overlays but never re-runs `find-file-hook'.
  `night/org-latex-preview-lazy-warm-min` is a `timer+bg`-only knob
  (default 2).

`night/org-latex-preview-lazy-toggle` /
`night/org-latex-preview-lazy-global-toggle` flip between `original`
and the last active mode, with the same stomp semantics as global
pinning.

### 2. Parallel background cache warming — IMPLEMENTED (2026-08-07)

Cold fragments are compiled OUTSIDE Emacs, in parallel, directly into
the shared cache at the exact paths `night/h-olpl-cache-file` computes
(the hash mirror is verified byte-identical against org's own), and
the drain then renders them as ~1ms cache hits. Design points:

- **No batch-Emacs workers.** The GUI assembles the `.tex` documents
  itself — it must anyway, since the preamble comes from
  `org-latex-make-preamble` over the *buffer's* export environment
  (`#+LATEX_HEADER:` keywords), which a buffer-less batch process
  cannot read — and runs `latex` + `dvisvgm` directly as
  sentinel-chained subprocesses (each stage's process sentinel
  launches the next; Emacs never blocks and failures are attributed
  per stage). The `.tex`/color/scale assembly mirrors org 9.7's
  `org-create-formula-image` (including its swapped-args
  `string-suffix-p` quirk) — verified: warmed SVGs are identical to
  stock-compiled ones modulo dvisvgm's internal glyph-id numbering.
- **Batched to amortize the preamble.** The dominant per-fragment
  cost is latex re-parsing the preamble (~250-400ms of each
  ~300-500ms run). Chunks of
  `night/org-latex-preview-lazy-warm-batch-size` (20) fragments go
  into ONE multi-page document — one preamble parse per chunk — and
  ONE `dvisvgm --page=1- --output=out-%p.svg` run converts all pages,
  which are then renamed (atomically, via temp + rename) onto their
  hash paths. A failed chunk (page-count mismatch) retries its
  fragments as single-fragment chunks, isolating the broken one,
  which is reported and dropped. A precompiled-preamble format
  (mylatexformat) could shave the residual per-chunk parse but was
  deliberately skipped: fragile format cache, redundant with
  batching.
- **Parallelism = `(num-processors)`** pipelines
  (`night/org-latex-preview-lazy-warm-workers`, nil = auto; 8 on this
  M2), no cap and no core reservation — every command runs under
  `nice -n 10`, so the scheduler yields cores to interactive work on
  demand.
- Lifecycle: pipelines are killed by
  `night/org-latex-preview-lazy-stop`, the mode toggles, buffer kill,
  and mode changes away from the bg modes; temp dirs and the
  in-flight set are always cleaned.

Measured (2026-08-07, M2): the 539-fragment J-Space file, cache fully
cleared, mode `bg`: dispatch ~0.5s (539 fragments dedupe to 433
unique compiles in 22 chunks), **all 539 overlays present after 10s**
with zero foreground blocking — vs ~5min for the serial foreground
drain and a comparable frozen stretch for stock. A fully warm
14-fragment buffer rendered synchronously at dispatch in 59ms.

Hard-won implementation notes (each cost a debugging round):

- Never render cache hits through `org-format-latex`: it calls
  `clear-image-cache` on EVERY invocation, forcing all visible images
  to re-rasterize per render (hundreds of calls starved the main
  loop). `night/h-olpl-render-cached` places the overlay directly via
  `org--make-preview-overlay`. And never stub the primitive with
  cl-letf — redefining a C subr makes native-comp build a trampoline,
  which can ICE.
- Pipeline processes use `:connection-type 'pipe` and `:buffer nil`:
  latex's chatty nonstopmode output on default PTYs (~16KB buffers)
  BLOCKS the children when Emacs doesn't drain fast enough.
  Diagnostics live in the tmpdir's .log.
- dvisvgm ZERO-PADS `%p` output names once a document has >= 10 pages
  (out-01.svg) — predict nothing, glob and sort numerically. (Before
  this fix every full chunk "failed" the page-count check and
  silently fell back to per-fragment compiles.)
- No global in-flight registry: a stale entry (from an aborted
  sentinel) silently blocked fragments from ever warming. The
  buffer-local task index is the only bookkeeping; the worst case is
  two buffers compiling the same hash concurrently — harmless
  (identical bytes, atomic rename).

In `bg` mode overlays are placed by the pipelines' sentinels directly
(event-driven; the timer drain is not involved at all); in `timer+bg`
warming only produces cache files and the drain sweeps them in.

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

Options 1 and 2 implemented (lazy drain 2026-08-02; parallel warming
2026-08-07); revisit option 3 when the overhaul merges into mainline
org — it obsoletes both (async native previews, precompiled preambles,
theme-independent caching).

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
