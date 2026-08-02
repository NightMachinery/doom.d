# Org LaTeX preview performance on large files

## Problem

Stock org (<= 9.7) previews LaTeX **synchronously, one fragment at a
time**: each fragment spawns its own `latex` run (re-reading the entire
preamble every time, ~0.3-1 s) plus `dvisvgm`. A file with hundreds of
fragments (e.g. LLM-generated notes converted via
[md2org](../../bugs/md2org-latex/readme.md) — 539 fragments) freezes Emacs
for **minutes**.

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
- Fragments visible in the window compile first (one chunk immediately if
  the buffer is displayed); the rest drain from **idle timers** in chunks
  of `night/org-latex-preview-lazy-chunk-size` (default 2), so between
  chunks Emacs is fully responsive.
- Each tick re-prioritizes the queue toward the current viewport, so
  previews follow where you are looking.
- `night/org-latex-preview-lazy-stop` cancels; already-previewed fragments
  (existing overlays) are skipped.

**Lazy is the default for whole-buffer previews**: `org-latex-preview` is
advised so that the `'(16)` whole-buffer path — which includes org's
`#+STARTUP: latexpreview` handling during `org-mode` initialization
(org.el ~line 5102), i.e. the case where Emacs would freeze *before the
user can run any command* — goes through the lazy machinery instead.
Opening a `#+STARTUP: latexpreview` file with many fragments is instant;
previews fill in on idle. Section-level previews (`C-c C-x C-l` with no
prefix) stay synchronous since sections are small.

Total CPU work is unchanged — this converts "frozen for minutes" into
"progressive and responsive". Pauses of up to ~1-2 s per chunk can still be
felt while it works through the queue.

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
- Related local bug doc: [md2org-latex](../../bugs/md2org-latex/readme.md)
