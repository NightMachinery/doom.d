# Yasnippet notes

## Stale active field state

`packages.el` installs `yasnippet` from the `NightMachinery/yasnippet` fork on
the `fix-stale-active-field` branch. Keep stale-field fixes in that fork instead
of local Doom advice.

The fork handles Yasnippet edge cases where internal snippet state is stale:

- `yas--active-field-overlay` can point to a field that no live snippet covers.
  `yas-next-field` should clear that state instead of raising
  `wrong-type-argument yas--snippet nil`.
- Fieldless snippets can leave a live control overlay with `yas-keymap`, causing
  `TAB` to call `yas-next-field-or-maybe-expand` even though normal snippet
  expansion should run. The fork tries trigger expansion first in that case, and
  exits stale fieldless snippets when no expansion is available.

## Shared helpers via `.yas-setup.el`

`yas-reload-all` loads a `.yas-setup.el` from each mode directory, so helper
functions used by snippets can live next to them and reload with them. See
`night-snippets/fundamental-mode/.yas-setup.el`.

Snippets in `fundamental-mode/` are available in every buffer: yasnippet
appends `fundamental-mode` to every mode's table list, so a snippet defined
there can also be looked up by name from any other mode.

## Rendering a snippet to a string

`night/yas-snippet-string` expands a snippet into a plain string, so one snippet
can reuse another's output without duplicating its body. Two things to know:

- `yas-expand-snippet` refuses to run in a buffer without a set-up
  `yas-minor-mode`, even for a body with no fields. The helper enables it in its
  temp buffer.
- Rendering flattens fields to their default text. A body containing
  `${1:python}` renders as `python`, so this is only meaningful for snippets
  that have no fields.

## One-shot shrink after expansion

`night/yas-insert-shrinkable` inserts the first of several variants and arms the
single next keypress: if it is a backspace, the insertion is replaced by the
next variant. Any other key behaves completely normally and disarms it.
Repeating backspace walks further down the list. Nothing is global and nothing
outlives the moment of expansion, so text written earlier is never affected.

A variant is a string, or a function that inserts at point. Later variants are
only evaluated when actually reached, which matters when producing one costs a
shell call.

Why not a yasnippet field. `yas-keymap` binds DEL to `yas-maybe-clear-field`,
which fires only at the start of an unmodified field, and the same condition
drives clear-on-type. A `${1:...}` date would therefore be destroyed by the
first character typed after expanding, which is the common case.

Implementation notes, each of which was checked against the Emacs 29.2 sources:

- `set-transient-map` with a nil keep-pred is documented as using the map "only
  once, to look up the very next key". It installs into
  `overriding-terminal-local-map`, consulted ahead of evil's maps and ahead of
  the `keymap` char property `yas-keymap` rides on, so it wins over both.
- `DEL` and `[backspace]` are both bound and are not duplicates: a GUI looks up
  the raw backspace event first and only translates it to `DEL` when unbound.
  Note `(kbd "DEL")` and `[?\C-?]` are the same key.
- There is deliberately no `on-exit` cleanup. `subr.el` calls `on-exit` from
  `pre-command-hook`, which runs before the bound command, so clearing the
  marker there would break the deletion it is about to perform.
- Only text behind point can be shrunk, since the region runs from the start
  marker to point. A snippet that parks the cursor before trailing text cannot
  use this unchanged.

## What is shrinkable today

Model tags (`gpt`, `gpt5`, `fb`, `op`, `sonnet`, `gem`, `fl`) insert the tag, a space
and the `timee` date. One backspace drops the date and the space with it,
leaving the bare tag.

`gpt` inserts `@GPT6`; `gpt5` preserves the previous `@GPT5.6T` tag.

Three time snippets shrink to their existing short twins, so a backspace after
the long form gives exactly what the short key would have given:

    time   [2026-09-02 Wed 10:01]     ->  timee    [2026-09-02 Wed]
    timea  <2026-09-02 Wed 10:01>     ->  timeaa   <2026-09-02 Wed>
    timej  [jalali:1405/06/11/10:01]  ->  timejj   [jalali:1405/06/11]

`timee` and `timeaa` remain ordinary snippets and stay the single source of
their formats. The jalali pair is different: `z` shells out at roughly 1.7s a
call, so `night/yas-jalali-strings` builds both forms from one `datej` call, and
both `timej` and `timejj` go through it.

Testing this by hand is awkward and worth a warning. Driving `execute-kbd-macro`
inside a `with-temp-buffer` does not test what it looks like: the command loop
runs against the selected window's buffer, so the keys land somewhere else
entirely. Run keypress tests in a batch Emacs with `set-window-buffer` pointed
at the test buffer.
