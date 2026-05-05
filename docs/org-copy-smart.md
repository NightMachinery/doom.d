# Org copy smart

Org copy unescaping is controlled by
`night/advice-kill-new-unescape-org-enabled-p`.  Its default value is `smart`,
which copies exactly as written unless trusted copy bounds show that the
selection is wholly inside the body of a fenced Org block.  Inside a block body,
the `kill-new` advice uses Org's code unescaping before placing text on the kill
ring.

For example, selecting only this line inside an example block:

```org
,* Hi!
```

copies:

```org
* Hi!
```

Selecting the whole block keeps the block source unchanged, including the
leading comma:

```org
#+begin_example
,* Hi!
#+end_example
```

This is useful when extracting text from Org code/example blocks for use
outside Org, while keeping whole-block copies safe for pasting back into Org.

The block-body check uses regex fence scanning instead of Org parser APIs, so it
does not parse large Org buffers just to copy a small region.  If smart mode
cannot see trusted copy bounds, it falls back to raw copy.

`night/org-copy-smart` explicitly enables `smart` behavior around
`kill-ring-save`.  Bind `night/advice-kill-new-unescape-org-enabled-p` to `t`
for the older broad multiline Org unescape behavior, or to nil for raw copying.
