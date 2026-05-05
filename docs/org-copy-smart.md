# Org copy smart

`night/org-copy-smart` copies the active region exactly as written unless the
selection is wholly inside the body of a fenced Org block.  Inside a block body,
it uses Org's code unescaping before placing text on the kill ring.

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
does not parse large Org buffers just to copy a small region.
