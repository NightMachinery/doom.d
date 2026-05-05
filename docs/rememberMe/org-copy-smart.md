# Org copy smart

When adding or changing copy helpers, distinguish source-buffer copies from
generated strings.

`night/advice-kill-new-unescape-org-enabled-p` defaults to `smart`.  In smart
mode, `kill-new` unescapes Org code escaping only when it has trusted source
bounds and those bounds are wholly inside a fenced Org block body.  Without
trusted bounds, smart mode intentionally copies raw.

Safe source-copy paths already capture bounds:

- `kill-ring-save`
- `copy-region-as-kill`
- `clipboard-kill-ring-save`, via `kill-ring-save`
- `evil-yank`
- `night/org-inline-copy`, for `+org/dwim-at-point` block-content copies

If a future helper computes a buffer-origin payload and calls `kill-new`
directly, it must either bind `night/org-copy-smart--bounds` around `kill-new`
or route through one of the advised region-copy commands.  Otherwise smart mode
will see no bounds and preserve raw Org escaping such as `,*`.

Generated or transformed strings should usually keep the no-bounds behavior.
Examples include Babel result formatting, checklist regex helpers, zsh wrapper
outputs, and other commands where the copied text is no longer a direct slice of
the Org buffer.
