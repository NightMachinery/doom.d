# Org heading navigation

`}` and `{` are bound to `night/org-next-visible-heading` and
`night/org-previous-visible-heading` (`autoload/org/night-org-keybindings.el`).
They are thin wrappers over stock `org-next-visible-heading` that add one
behavior: when there is no further visible heading, **point does not move**,
instead of running to the end (or the beginning) of the buffer.

`night/org-next-less-nested-heading` and `night/org-previous-less-nested-heading`
share the same wrapper, so they skip folded headings and stay put too. They
previously used `outline-next-heading`, which ignores folding entirely.

## The old monkeypatch, and why it was wrong

`night/org-next-visible-heading` used to be an `:override` advice holding a copy
of upstream `org-next-visible-heading` with the fold-skipping block commented
out:

```elisp
;; (when (org-fold-folded-p)
;;   (goto-char (org-fold-next-visibility-change))
;;   (skip-chars-forward " \t\n")
;;   (end-of-line))
```

Without that block, forward motion accepted whatever `re-search-forward` found,
so `}` landed on headings hidden inside a fold. The backward loop kept its skip,
which is why `{` behaved and `}` did not.

The patch was made to work around a different complaint: on a file whose entire
tail is one folded subtree, stock `org-next-visible-heading` walks past the last
visible heading and lands at `point-max`. That is technically correct — there is
no next visible heading — but it reads as "jumps to the end of the file". So the
patch traded a cosmetic annoyance for a real correctness bug, and it also broke
`org-paste-subtree` in folded buffers (see below).

Stock org 9.7.34 skips folds correctly under this config, including
`org-fold-core-style` set to `overlays` in
`autoload/org/links/night-org-links-ui.el`. The override is gone.

## Why the stay-put behavior lives in the commands, not in an advice

Org itself depends on `org-next-visible-heading` moving to `point-max` when it
finds nothing:

- `org-paste-subtree` — "Paste before the next visible heading or at end of
  buffer". With stay-put semantics, pasting a subtree while on the last heading
  would insert it in the wrong place.
- The `next-level` guess in the same function.
- `org-cycle.el`, for the inline-image region when cycling to `children`.
- `night/org-heading-region-get` in `autoload/org/night-hider.el`, which uses
  the landing point as the region end.

Those call sites keep calling stock `org-next-visible-heading` /
`org-previous-visible-heading` directly. Only the interactive entry points —
`}`, `{`, and the `org-shiftleft` / `org-shiftright` hooks — go through the
stay-put wrappers.

The wrapper follows the shape upstream already uses in
`org-forward-heading-same-level`: remember where you started, run the motion,
and only keep the new position if it is a real, visible heading.

The visibility test is `(org-invisible-p (point) t)` at the beginning of the
line. The `t` means folding-only, so it is not fooled by fontification-based
invisibility such as this config's custom link display.

## Reload hazard

The old override is installed in any Emacs server started before this change.
Reloading `night-org-keybindings.el` while that stale advice is live would make
the new `night/org-next-visible-heading` call `org-next-visible-heading`, which
is advised back to it — infinite recursion.

The file therefore carries a permanent, idempotent guard evaluated before the
new definitions:

```elisp
(advice-remove 'org-next-visible-heading 'night/org-next-visible-heading)
```

## A note on `*` inside example blocks

A line starting with `*` at column 0 inside `#+begin_example` is treated as a
heading by org's regexp-based outline machinery — both for folding and for
motion. So `}` stops on it. That is stock org behavior, not something these
wrappers introduce; escape such lines with `,*` if you do not want it.
