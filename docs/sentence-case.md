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

## How it decides

The text is split into whitespace-delimited tokens, and each token is handled as
a unit.  Non-starting words keep their original case, so names and existing
acronyms are left alone in mixed-case text.  Before capitalizing sentences, it
always applies whole-word replacements from
`night/sentence-case-always-replacements`, such as standalone `i` to `I`.  It
can also apply optional whole-word replacements from
`night/sentence-case-replacements`, such as `sth` to `something`, informal
shorthand like `tho` to `though`, and common missing-apostrophe contractions.

A sentence ends only at the *end* of a token.  A dot in the middle of a token
never ends a sentence, which is what keeps `i.e.`, `~/.claude/bin`, `a.com`,
`file.el` and `1.2.3` intact.  A trailing dot is also discounted for an
abbreviation listed in `night/sentence-case-abbreviations`, for a single-letter
initial such as `J.`, and for a dotted form such as `U.S.A.`  Trailing quotes
and brackets are ignored when looking at the end of a token, so `hi."` and
`(done.)` still end their sentences.

Abbreviations count as prose, not code, so one at the start of a sentence is
still capitalized on its first letter — `i.e., append ...` becomes
`I.e., append ...` — but they are never rewritten by the replacements, which is
what stops the standalone `i` rule from turning a mid-sentence `i.e.` into
`I.e.`

Some tokens are protected, meaning they are neither capitalized nor touched by
the replacements:

- Anything that looks like code: a token containing `/`, `\`, `@`, `$`, `~`,
  `_`, a backtick, or a dot between two alphanumerics.  That covers URLs, paths,
  snake_case, backticked code, dotted names, and version numbers.
- Anything carrying an uppercase letter past its first character, such as
  `iPhone`, `eBay`, or `ID`.

Protected tokens also *consume* a pending capitalization rather than passing it
along, so in `~/.claude/bin is first.` the word `is` is not capitalized.

List and quote markers are transparent instead: a token that is only `-`, `*`,
`+`, `>`, `#`, `=`, `|`, or a number followed by `.` or `)` is skipped without
consuming the pending capitalization, so the word after it still starts the
sentence.  When a token does get capitalized, leading opening punctuation is
skipped first, so `**bold` and `"quoted` are capitalized on their first letter.

Because tokens are classified before the replacements run, the result no longer
depends on the major mode of the buffer you paste into.  It used to: `\_<` and
`\_>` resolve against the calling buffer's syntax table, and `.` is a symbol
constituent in some modes but not others, so the same clipboard text could come
out differently in `org-mode` than in `sh-mode`.

Optional replacement behavior is enabled by default via
`night/sentence-case-enable-replacements`.  Non-interactive callers can pass
`:replacements-p` to override that default for a call:
`(night/sentence-case text :replacements-p nil)` disables replacements, and
`(night/sentence-case text :replacements-p t)` enables them.

If the prose contains letters but no lowercase letters, it is treated as shouted
or all-caps text: it is downcased first, then sentence-cased.  Code tokens are
excluded from that judgement and from the downcasing, so `SEE THE README.md
FILE` becomes `See the README.md file` with the filename intact.

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

(night/sentence-case "see i.e. now")
;; => "See i.e. now"

(night/sentence-case "see e.g. the docs. also cf. this")
;; => "See e.g. the docs. Also cf. this"

(night/sentence-case "visit https://a.com/x now. ok")
;; => "Visit https://a.com/x now. Ok"

(night/sentence-case "~/.claude/bin is first. done")
;; => "~/.claude/bin is first. Done"

(night/sentence-case "i know the ID and user_id here")
;; => "I know the ID and user_id here"
```

## Known tradeoffs

- A sentence that genuinely ends in a listed abbreviation leaves the next word
  uncapitalized: `cats, dogs, etc. then i left` becomes
  `Cats, dogs, etc. then I left`.  The alternative is capitalizing the word
  after every `e.g.`, which is wrong far more often.
- An abbreviation not in `night/sentence-case-abbreviations` still
  over-capitalizes the word after it.  Add it to the list.
- A code-like token at the start of a sentence is never capitalized, so
  `file.el is ok` stays lowercase.  Leaving such text alone is the safer
  failure: over-capitalizing a path corrupts it, under-capitalizing prose is
  cosmetic.
- Protection is per token.  Prose inside a fenced code block is still processed.
- All-caps prose is downcased wholesale, so an acronym such as `API` becomes
  `api` in that mode unless it is part of a code token.

## Tests

ERT tests live in the `(comment ...)` block at the end of
`autoload/night-sentence-case.el`.  Evaluate the block's contents and then
`(ert-run-tests-batch "night/sentence-case")`.  Each case is checked under
`org-mode`, `text-mode` and `sh-mode`, and reports `mode-dependent` if the three
ever disagree.
