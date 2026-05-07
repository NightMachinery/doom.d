# Sentence case

`night/sentence-case` converts prose to sentence case.

- Non-interactive use returns transformed text:
  `(night/sentence-case "hello. world?")` returns `"Hello. World?"`.
- Interactive use reads from the clipboard/kill-ring and inserts the transformed
  text at point.
- `night/paste-md2org-sentencecased` converts the clipboard from Markdown to
  Org with `md2org`, sentence-cases the converted text, and inserts it with the
  same Org level adjustment used by `night/paste-md2org`.
- `night/smart-text-paste`, bound under the paste-transform leader group as
  `, s`, sentence-cases every paste.  In Org buffers outside `md`/`markdown`
  source blocks it first converts Markdown clipboard text to Org; elsewhere it
  inserts the sentence-cased clipboard text directly.

The command capitalizes the first alphabetic character at the start of text,
after `.`, `?`, `!`, and after newlines.  Non-starting words keep their original
case, so names and existing acronyms are left alone in mixed-case text.  Before
capitalizing sentences, it always applies whole-word replacements from
`night/sentence-case-always-replacements`, such as standalone `i` to `I`.  It
can also apply optional whole-word replacements from
`night/sentence-case-replacements`, such as `sth` to `something`, informal
shorthand like `tho` to `though`, and common missing-apostrophe contractions.

Optional replacement behavior is enabled by default via
`night/sentence-case-enable-replacements`.  Non-interactive callers can pass
`:replacements-p` to override that default for a call:
`(night/sentence-case text :replacements-p nil)` disables replacements, and
`(night/sentence-case text :replacements-p t)` enables them.

If the input contains letters but no lowercase letters, it is treated as shouted
or all-caps text: the whole text is downcased first, then sentence-cased.

Examples:

```elisp
(night/sentence-case "hello iPhone. use API")
;; => "Hello iPhone. Use API"

(night/sentence-case "hello i think i can")
;; => "Hello I think I can"

(night/sentence-case "whats up? dont use sth")
;; => "What's up? Don't use something"

(night/sentence-case "pls dont do that tho")
;; => "Please don't do that though"

(night/sentence-case "theyre sure itll work")
;; => "They're sure it'll work"

(night/sentence-case "whats up" :replacements-p nil)
;; => "Whats up"

(night/sentence-case "i know whats up" :replacements-p nil)
;; => "I know whats up"

(night/sentence-case "HELLO WORLD. HOW ARE YOU?")
;; => "Hello world. How are you?"

(night/sentence-case "- hello\n> quoted start")
;; => "- Hello\n> Quoted start"
```

Known v1 tradeoff: all-uppercase input is downcased wholesale, so acronym-like
words such as `API` become `api` in that mode.
