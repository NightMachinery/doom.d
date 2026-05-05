# Sentence case

`night/sentence-case` converts prose to sentence case.

- Non-interactive use returns transformed text:
  `(night/sentence-case "hello. world?")` returns `"Hello. World?"`.
- Interactive use reads from the clipboard/kill-ring and inserts the transformed
  text at point.  It is bound under the paste-transform leader group as `, s`.

The command capitalizes the first alphabetic character at the start of text,
after `.`, `?`, `!`, and after newlines.  Non-starting words keep their original
case, so names and existing acronyms are left alone in mixed-case text.

If the input contains letters but no lowercase letters, it is treated as shouted
or all-caps text: the whole text is downcased first, then sentence-cased.

Examples:

```elisp
(night/sentence-case "hello iPhone. use API")
;; => "Hello iPhone. Use API"

(night/sentence-case "HELLO WORLD. HOW ARE YOU?")
;; => "Hello world. How are you?"

(night/sentence-case "- hello\n> quoted start")
;; => "- Hello\n> Quoted start"
```

Known v1 tradeoff: all-uppercase input is downcased wholesale, so acronym-like
words such as `API` become `api` in that mode.
