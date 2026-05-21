# Yasnippet notes

## Stale active field state

`autoload/night-yasnippet.el` advises `yas-next-field` with
`night/h-yas-next-field-around`.

This handles a Yasnippet edge case where `yas--active-field-overlay` still
points to a `yas--field`, but `yas-active-snippets` no longer returns a live
snippet covering that field. In that state, `yas-next-field` passes `nil` as the
snippet to `yas--find-next-field`, which raises:

```text
wrong-type-argument yas--snippet nil
```

The advice treats this as stale editor state, clears the active field and
protection overlays, and returns without throwing.
