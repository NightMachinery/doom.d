# pandoc md→org breaks LLM-style LaTeX math

## Symptom

Markdown produced by LLM chat UIs delimits math with `\[ ... \]` (display)
and `\( ... \)` (inline), often with the display math spread over many
lines, including *operators alone on a line* (`+`, `-`, `=`). Converting
such markdown to org with pandoc yields org files whose math is mangled
and/or cannot be LaTeX-previewed (`org-latex-preview` errors with
*"...dvi wasn't produced. Please adjust 'dvisvgm'..."* even though latex
and dvisvgm work fine).

Observed with pandoc 3.7.0.2. Repro input: [`repro.md`](./repro.md).

## Failure modes

### 1. `--from=gfm` (default reader in e.g. `h-claude-code-session-to-org-pandoc`)

GFM has no single-backslash TeX math extension at all (`tex_math_single_backslash`
is rejected for gfm). The equation body is parsed as ordinary markdown:

- `\[` / `\]` / `\(` / `\)` are backslash-escapes → literal `[`, `]`, `(`, `)`;
  no math remains.
- A text line followed by a lone `=` line is a **setext heading** → equation
  chunks become org `*` headlines (with `:CUSTOM_ID:` drawers) mid-formula.
- `\\` row separators in matrices collapse to `\`.

```
* [ c_{[2\times 1]}
:PROPERTIES:
:CUSTOM_ID: -c_2times-1
:END:
...
\begin{bmatrix} 0&1\ 1&0 \end{bmatrix}. ]
```

### 2. `--from=markdown` (default extensions)

`\[...\]` is again not math (only `tex_math_dollars` is on by default). No
bogus headings, but intraword underscores pair up as *emphasis*, destroying
subscripts:

```
[ c_{[2\times 1]} = V^\dagger/{[2\times d]}h/{[d\times 1]} + ... ]
```

### 3. `--from=markdown+tex_math_single_backslash` — correct pandoc parse, still broken in org

With this extension pandoc recognizes the math and emits it verbatim as org
display math — **preserving the interior newlines**, including the lone `+`
line. Org-mode's element parser then breaks on it:

- A line consisting of just `+` or `-` is a valid (empty) **plain-list
  item**, which terminates the containing paragraph. The `\[` can no longer
  find its `\]` within its own paragraph, so *no display-math fragment
  exists*.
- Leftover pieces like `\begin{bmatrix}` still match as one-macro fragments;
  previewing compiles them *outside math mode* → `! Missing $ inserted` →
  no DVI → the whole `org-latex-preview` aborts with the misleading dvisvgm
  message.
- (Same applies to hand-pasted markdown math in org files — this is exactly
  the `tmp.org` incident of 2026-08-01.)

`org-element` evidence (types found in the converted file):
multi-line math → `(plain-list latex-fragment latex-fragment latex-fragment
latex-fragment)`; reflowed math → `(latex-fragment latex-fragment)`.

## Working conversion

Reflow each math element onto a single line with a tiny Lua filter,
[`reflow-math.lua`](./reflow-math.lua):

```lua
function Math(el)
  el.text = el.text:gsub("%s*\n%s*", " ")
  return el
end
```

```zsh
pandoc --from=markdown+tex_math_single_backslash \
    --lua-filter=reflow-math.lua \
    --to=org --wrap=none repro.md
```

Output (parses as clean org latex fragments, previews fine):

```
A minimal LLM-style equation:

\[ c_{[2\times 1]} = V^\dagger_{[2\times d]}h_{[d\times 1]} + S_{[2\times 2]}c_{[2\times 1]}, \qquad S_{[2\times 2]} = \begin{bmatrix} 0&1\\ 1&0 \end{bmatrix}. \]

Inline math like \(\alpha\in\mathbb R\) is also affected.
```

Caveats:

- `markdown` reader + emphasis extensions still differ from what chat UIs
  render (they use CommonMark-ish dialects); for math-heavy notes this
  combination has been sufficient.
- If the source must be read as gfm (tables etc.), there is no
  `tex_math_single_backslash`; pre-convert the delimiters to `$$`/`$`
  first, or use `--from=markdown+...` anyway.

## Alternative: explicit environments (preserves newlines)

Instead of reflowing, [`math-env.lua`](./math-env.lua) converts each
*standalone* display-math paragraph into an explicit

```
\begin{equation*}
...original lines, newlines preserved...
\end{equation*}
```

block (emitted as a raw org block). Org parses `\begin{...}`/`\end{...}`
line-wise as a `latex-environment` *element*, so interior lone `+`/`-`/`=`
lines are harmless — no reliance on the fragile `\[...\]` fragment
heuristics. Inline math, and display math mixed inside a text paragraph
(which cannot become a block), are still reflowed onto one line.

Verified properties:

- org-element parses the result as `latex-environment` (no stray
  `plain-list`), including indented occurrences inside list items.
- **Previewable**: `org-format-latex`/`org-latex-preview` compiled the
  multi-line environment to SVG via dvisvgm without errors.
- **HTML export**: the environment is passed through verbatim for MathJax
  (default `org-html-with-latex` handling), same as `\[...\]`.
- Rendering semantics match `\[...\]` (`equation*` = unnumbered display
  math; needs amsmath, which org's default preview/export preamble loads).

Choose by taste: `reflow-math.lua` keeps `\[...\]` but flattens equations
onto (possibly very long) single lines; `math-env.lua` keeps the source's
line structure at the cost of rewriting the delimiters.

## Reproduction commands

```zsh
cd docs/bugs/md2org-latex
pandoc --from=gfm --to=org --wrap=none repro.md                                  # failure mode 1
pandoc --from=markdown --to=org --wrap=none repro.md                             # failure mode 2
pandoc --from=markdown+tex_math_single_backslash --to=org --wrap=none repro.md   # failure mode 3
pandoc --from=markdown+tex_math_single_backslash --lua-filter=reflow-math.lua \
    --to=org --wrap=none repro.md                                                # works
```

To check the org side, parse the output with `org-element-parse-buffer` and
look for stray `plain-list`/`headline` elements where the math should be.
