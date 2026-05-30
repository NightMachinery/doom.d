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
