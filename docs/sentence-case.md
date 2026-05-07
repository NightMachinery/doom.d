# Sentence case

`night/sentence-case` converts prose to sentence case.

- Non-interactive use returns transformed text:
  `(night/sentence-case "hello. world?")` returns `"Hello. World?"`.
- Interactive use reads from the clipboard/kill-ring and inserts the transformed
  text at point.  It is bound under the paste-transform leader group as `, s`.

The command capitalizes the first alphabetic character at the start of text,
after `.`, `?`, `!`, and after newlines.  Non-starting words keep their original
case, so names and existing acronyms are left alone in mixed-case text.  Before
capitalizing sentences, it can also apply whole-word replacements from
`night/sentence-case-replacements`, such as standalone `i` to `I`, `sth` to
`something`, and common missing-apostrophe contractions.

Replacement behavior is enabled by default via
`night/sentence-case-enable-replacements`.  Non-interactive callers can pass a
third argument to override that default for a call:
`(night/sentence-case text nil nil)` disables replacements, and
`(night/sentence-case text nil t)` enables them.

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

(night/sentence-case "whats up" nil nil)
;; => "Whats up"

(night/sentence-case "HELLO WORLD. HOW ARE YOU?")
;; => "Hello world. How are you?"

(night/sentence-case "- hello\n> quoted start")
;; => "- Hello\n> Quoted start"
```

Known v1 tradeoff: all-uppercase input is downcased wholesale, so acronym-like
words such as `API` become `api` in that mode.
