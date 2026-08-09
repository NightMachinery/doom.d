# `org-store-link` descriptions for `id:` links

Two bugs made `org-store-link` produce a link with no description (and, on
insertion, a description equal to the raw link string) whenever point was on a
line **before the first heading** — most visibly on the `#+title:` line, where
the generated search string reads like `+title: Some Title`.

## Bug 1 (upstream Org): the precise target's nil description clobbers the good one

`org-id-store-link` first derives a description from the ID location: the
`#+TITLE:` keyword when before the first heading (falling back to the file
name), otherwise the heading text.

Then, when both `org-link-context-for-files` and `org-id-link-use-context` are
non-nil (both default to `t`), it calls `org-link-precise-link-target` and, if
the target position is after the ID location, appends `::SEARCH` to the link
**and unconditionally replaces the description** with the target's own
description.

That description is `nil` by design for region-based and current-line-based
targets — which is exactly what `org-link-precise-link-target` returns for any
line before the first heading. So the good `#+TITLE:`-derived description was
thrown away. The search string itself comes from the current line with any
leading `#`/`*` characters stripped by `org-link--normalize-string`, which is
why the `#+title:` line yields `+title: ...`.

Fix: `night/h-org-id-store-link-keep-desc`, an `:around` advice on
`org-id-store-link` in `autoload/org/links/night-org-id-links.el`. It runs the
original, and if `org-store-link-plist`'s `:description` came back nil, it
recomputes the ID-location description with
`night/h-org-id-store-link-fallback-desc` (which mirrors upstream's own `cond`)
and writes it back with `org-link-add-props`.

Notes:

- Use `org-link-add-props`, not `org-link-store-props` — the latter replaces
  `org-store-link-plist` wholesale.
- The registered store function is `org-id-store-link-maybe`, which calls
  `org-id-store-link`; advising the inner function is fine because
  `org-link--try-link-store-functions` copies the plist only after the store
  function returns.
- The advice never overrides a description that the precise target actually
  supplied, so `#+name:`d elements keep their own description.
- The `::SEARCH` suffix is deliberately kept, so precise navigation still works.

## Bug 2 (this config): the description formatter could not resolve `id:UUID::SEARCH`

`night/org-description-formatter` in `autoload/org/links/night-links.el` passed
the whole `id:` payload to `org-id-find`, including any `::SEARCH` suffix.
`night/org-id-find` does not strip that suffix (only `org-id-open` does, via
`night/h-org-id-open-extras`), so the lookup failed, `file` and `tail` ended up
empty, and the final `cond` fell through to returning the link itself as the
description.

Fix: split the payload on `"::"` and resolve only the bare ID, mirroring what
`night/h-org-id-open-extras` already does. Any `id:UUID::SEARCH` link with no
description now falls back to the normal `night/org-title` description of the
target file.

Known edge case left alone: IDs that themselves contain `::`. Upstream only
handles those as a fallback inside `org-id-open`.

## Related

- `docs/org-id-to-links.md`

## Verifying

Non-mutating check — `(org-store-link nil nil)` returns the link string without
touching `org-stored-links` or the buffer:

```elisp
(substring-no-properties (org-store-link nil nil))
```

With point on the `#+title:` line of a file whose top-level ID exists, expect
`[[id:UUID::+title: Some Title][Some Title]]` rather than a bare
`[[id:UUID::+title: Some Title]]`.
