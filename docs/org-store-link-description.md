# `org-store-link` on lines before the first heading

Three bugs conspired to make `org-store-link` produce
`[[id:UUID::+title: Some Title]]` — a useless search string and no description
at all — whenever point was on a line **before the first heading**, most visibly
the `#+title:` line.

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

## Bug 3 (upstream Org): the context-line fallback does not check the line's kind

Before the first heading there is no heading to anchor to, so
`org-link-precise-link-target` falls back to `org-current-line-string`. It
applies no filter beyond dropping blank lines. But the preamble is normally all
metadata — `#+title:` and other keywords, `# comments`, and the file-level
property drawer that holds the very ID being linked to — so the resulting search
string re-targets exactly where the bare `id:UUID` already lands.

`org-link--normalize-string` also strips the leading `#`, so the stored search
string (`+title: Some Title`) is not even the literal line text; it resolves only
through `org-link-search`'s fuzzy text fallback.

Fix: `night/h-org-link-precise-target-skip-noise`, an `:around` advice on
`org-link-precise-link-target` in `autoload/org/links/night-org-id-links.el`. It
returns nil — no search string — when point is in an Org buffer, before the first
heading, with no active region, on an element with no `#+name:`, and
`night/h-org-link-noise-context-line-p` says the line is metadata: element type
`keyword`, `comment`, `property-drawer` or `node-property`, or a
`#+begin_`/`#+end_` block delimiter (`org-at-block-p`), which would otherwise
yield strings like `+begin_src sh`.

Deliberately narrow: a prose line in a long preamble still gets its `::context`
search string, since that is genuinely useful. Only non-content lines are
suppressed.

This also improves `file:` links, since `org-link--file-link-to-here` goes
through the same function.

With no precise target, `org-id-store-link` keeps its own `#+TITLE:`-derived
description natively, so the Bug 1 advice becomes a no-op on the title line. It
is still needed for prose preamble lines and for body lines under a heading.

## Related

- `docs/org-id-to-links.md`

## Verifying

Non-mutating check — `(org-store-link nil nil)` returns the link string without
touching `org-stored-links` or the buffer:

```elisp
(substring-no-properties (org-store-link nil nil))
```

With point on the `#+title:` line of a file whose top-level ID exists, expect a
bare `[[id:UUID][Some Title]]` — no search string, description intact.
