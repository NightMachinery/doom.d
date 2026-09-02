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

## Model tags reuse `timee`

The model-tag snippets (`gpt`, `fb`, `op`, `sonnet`, `gem`, `fl`) are
`type: command` snippets whose body is a single call:

    (night/yas-expand-model-tag "GPT5.6T")

`night/yas-expand-model-tag` appends the tag, a space, and the raw body of the
`timee` snippet, then expands the whole thing. The backquoted
`format-time-string` inside `timee` is therefore evaluated at expansion time.

This keeps one source of truth for each concern. The date format lives only in
the `timee` snippet, the tag-plus-space layout lives only in the helper, and
each model file names only its own tag.
