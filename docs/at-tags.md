# At-tag highlighting

`autoload/night-ui.el` configures at-tags for hl-todo, shell buffers, and Org
special tags. Percent signs are tag characters: `@25%` and `@12.5%` are
highlighted in full. Org's shared special-tag rule also accepts `%` in hashtags.
Dots join tag segments, so the period in `@25%.` remains outside the tag.

Both the regular expressions and hl-todo's private syntax table must accept
`%`. Updating only the expressions leaves hl-todo's word boundary checks
unable to end a match after the percent sign.
