# Smart text paste

`night/smart-text-paste`, bound under the paste-transform leader group as
`, s`, sentence-cases the clipboard as it inserts it.  It takes one of two
paths depending on where point is.

In an Org buffer outside an `md`/`markdown` source block it delegates to
`night/paste-md2org-sentencecased`, which converts the clipboard from Markdown
to Org with `md2org` (pandoc) and only then sentence-cases the result and
adjusts heading levels.  Everywhere else — including inside an `md` source
block, which `night/h-org-md-src-block-p` detects — it sentence-cases the
clipboard text directly and inserts it.

`night/sentence-case` itself is documented in `sentence-case.md`.

## The prefix argument

`C-u , s` first joins the clipboard's lines with the zsh `newline2space`
filter.  Use it for text that arrives with a wrap baked in: a paragraph copied
back out of a terminal prompt box, or a block of prose copied out of a PDF.
Such text carries hard newlines at the terminal or column width, trailing
whitespace at each wrap point, and often a two-space indent on continuation
lines.

The two paths implement it differently, because each keeps the clipboard source
it already used.  The non-Org path calls `night/newline2space`, a text-in
text-out wrapper around the filter.  The Org path swaps its command list for
`reval-paste reval-to md2org newline2space`, which is a single asynchronous
call expanding to `pbpaste | newline2space | md2org`; `reval-to` takes its
destination first, so that argument order reads backwards.

## Why it is opt-in rather than always on

The Org path does not need it for unwrapping alone.  Pandoc already joins soft
line breaks, keeps paragraph breaks and lists intact, and does not turn the
trailing whitespace into `\\` hard breaks:

```
$ printf 'anyway). you can can   \n  launch one subagent\n' | md2org
anyway). you can can launch one subagent
```

The non-Org path does have a real problem without it, because
`night/sentence-case` copies whitespace gaps through verbatim and treats any
gap containing a newline as a sentence boundary.  Every soft wrap therefore
starts a new "sentence":

```elisp
(night/sentence-case "anyway). you can can \n  launch one subagent")
;; => "Anyway). You can can \n  Launch one subagent"
```

But `newline2space` is a PDF-copy unwrapper, and deliberately aggressive, so
turning it on for every paste would cost more than it fixes:

- It flattens everything to one line.  Lists, paragraph breaks and code fences
  all go.  `- item one\n- item two\n\nsecond para` becomes
  `- item one - item two  second para`.
- It leaves a doubled space at every join, because its Perl runs one line at a
  time and each join contributes both the substituted space and the previous
  line's trailing space.
- It de-hyphenates a word split across lines, which is right for a PDF and
  wrong for a terminal wrap: `one claude-\nwork subagent` becomes
  `one claudework subagent`.

Behind `C-u` those are a judgement made per paste, with knowledge of what is
actually on the clipboard, rather than a surprise.

## Known limitations

- Plain `, s` in a non-Org buffer still capitalizes the first word after a soft
  wrap, as shown above.  Fixing that in `night/h-sentence-case-transform` would
  mean distinguishing a soft wrap from a paragraph break, which would change
  behaviour the sentence-caser currently treats as intentional — a list such as
  `- hello\n> quoted start` capitalizes both lines by design.
- The doubled space at each join comes from `newline2space` itself
  (`~/scripts/zshlang/auto-load/others/text, string/text.zsh`), so `, n`
  (`night/pns`) has it too.  Slurping the whole input with `perl -0777` would
  fix it in one pass, but that changes every caller of the filter.

## Related commands

- `, n` (`night/pns`, `night/p-newline2space`) runs the same filter on the
  clipboard with no sentence-casing.  In a minibuffer it also downcases and
  regex-quotes the result; see `minibuffer-paste-regex-dialects.md`.
- `, m` (`night/paste-md2org`) is the Org path without sentence-casing.
