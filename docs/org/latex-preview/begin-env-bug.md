# `\[...\]` display math dies on a line-initial `\begin{...}`

## Symptom

LLM-style display math written *directly into an org file* — by a coding
agent (Claude Code, aider), by a plain `p` paste, or by hand — previews
only its innermost environment. The delimiters and everything outside the
environment are left as literal prose:

```
\[
\text{layer } \ell \;\text{is}\;
\begin{cases}
\text{full attention}, & \ell \equiv 3 \pmod 4,\\
\text{linear attention}, & \text{otherwise},
\end{cases}
\qquad \ell = 0,1,\dots,L-1 .
\]
```

renders as three lines of monospace source (`\[`, the `\text{layer }...`
line) wrapped around one image of the `cases` block, plus two more source
lines.

## Root cause (Org 9.7.34)

`org-element--latex-begin-environment` is

```
^[ \t]*\\begin{\([A-Za-z0-9*]+\)}
```

Note what is *not* there: no `$` anchor. Any line whose first non-blank
text is `\begin{`, with or without trailing content on that line, opens a
`latex-environment` **element** — and an element terminates the enclosing
paragraph.

`org-element-latex-fragment-parser` then reaches `\[` and runs
`search-forward "\\]"` bounded by its own paragraph. The closing `\]` now
lives two elements away, so no fragment is produced at all and `\[`
degrades to plain text.

`org-element-parse-buffer` on the snippet above:

```
((paragraph 1 35) (latex-fragment 4 18) (latex-fragment 24 33)
 (latex-environment 35 78) (paragraph 78 96) (latex-fragment 78 85))
```

This is **not** a preview bug. `org-format-latex` walks
`org-element-context`, not `org-latex-regexps`, so the regexp order in
that variable is irrelevant. Export breaks identically: `\[` and `\]` are
exported as literal text around a standalone `cases` environment.

Two source forms do work, both verified by construction:

- `\begin{equation*}` ... `\end{equation*}` parses as a single
  `latex-environment`. Environment parsing only scans for the matching
  `\end{...}`, so it is immune to *every* paragraph-breaking construct in
  the body, not just this one.
- `\[ ... \]` in which no line *starts* with `\begin{` — e.g.
  `\text{is}\; \begin{cases}` on one line. This works but stays fragile:
  it holds only until the next blank line, list-looking line, or
  reflowed `\begin{`.

`\begin{cases} x, & y` at column 0 does **not** work; the missing `$`
anchor means trailing content on the line does not help.

## Fix: `night/org-latex-fix-begin-env-bug`

Defined in `autoload/org/night-latex.el`. Interactive; acts on the region
when there is one, otherwise the whole buffer, and reports how many
blocks it rewrote.

It rewrites affected blocks to `\begin{equation*}...\end{equation*}` —
the same normal form that
`$NIGHTDIR/python/pandoc_filters/org_math_env.lua` emits for the
`md2org` paste path, so text arriving by either route converges on one
style.

How it decides:

- Openers and closers are found with regexps that require an *unescaped*
  backslash (`\(?:^\|[^\\]\)\(\\\[\)`), so the LaTeX row-break-with-spacing
  idiom `\\[2ex]` inside a `cases`/`align` body is not mistaken for an
  opener.
- Single-line blocks are skipped; they cannot hit the bug.
- The decision itself is delegated to `org-element-context` at the opener
  rather than to a `\begin{`-specific regexp. A block is rewritten only
  when org does *not* already parse it as a `latex-fragment` reaching the
  closer. That makes the command correct for the sibling failure modes
  documented in `~/scripts/docs/md2org-latex/readme.md` (a lone `+`, `-`
  or `=` line splitting the paragraph) without special-casing each one.
- Contexts in `night/h-org-latex-display-math-skip-types` are never
  touched: `src-block`, `example-block`, `export-block`, comments,
  `fixed-width`, `keyword`, `code`, `verbatim`, `link`,
  `latex-environment`, and tables. The block types matter because
  documentation *about* this bug quotes the broken form inside
  `#+begin_src` / `#+begin_example`; the table types matter because a
  multi-line environment inside a cell would smear across rows.
- Blocks are collected in a read-only pass and rewritten last-first, so
  earlier positions stay valid.
- `\begin{equation*}` / `\end{equation*}` are placed alone on their own
  line, keeping the opener's indentation (org accepts a leading
  `[ \t]*`, which is what keeps math inside a list item aligned). Text
  that shared a line with the old delimiter is pushed to the adjacent
  line, absorbing the whitespace that separated them.

Because a rewritten block has no `\[` left to match, the command is
idempotent — a second run reports 0.

### Limitation

A `\[ ... \]` block containing a **blank line** is also flagged and
rewritten. Org will then parse it as one environment, but LaTeX still
rejects a blank line in math mode, so the rewrite makes such a block no
worse rather than correct.

## Why the pandoc filter does not cover this

`md2org` already handles the conversion path correctly, via
`org_math_env.lua` (see `~/scripts/docs/md2org-latex/readme.md`). The
routes that bypass its standalone-paragraph pass were checked and are all
safe: lead-in text with no blank line and list items get reflowed onto a
single line by its pass 3, blockquotes get a proper `equation*`, and table
cells are single-line by its pass 1.

The gap is text that never reaches pandoc at all — agent-written files,
`night/org-paste-yank` (plain `p`), hand-typed math, and `--from=gfm`
converters, which have no `tex_math_single_backslash` and therefore no
filter in the loop.

## Verification

`org-element-parse-buffer` before the fix yields `paragraph`,
`latex-environment`, `paragraph`; afterwards a single `latex-environment`
covering the whole block. End-to-end in the GUI server, the preview
overlay covers only `\begin{cases}...\end{cases}` before and the entire
`\begin{equation*}...\end{equation*}` after.
