# Bookmarks

`SPC b m` (or `M-x bookmark-set`) creates an ordinary file bookmark without
saving point or surrounding text. Opening it visits the file without moving
to a saved location: an existing buffer keeps its point, and a newly opened
buffer uses its usual initial position.

Use `C-u SPC b m`, `C-u M-x bookmark-set`, or
`M-x night/bookmark-set-with-position` to save the current position and the
context Emacs uses to relocate it after edits. There is no extra confirmation.

The interactive prefix now selects position recording instead of retaining
duplicate bookmark names. Setting an existing name replaces its record, so
setting it without a prefix also removes its previously saved location.
Existing bookmarks keep their saved locations until replaced.

Bookmarks with special handlers, such as PDF or directory bookmarks, retain
their handler data. Direct Lisp calls to `bookmark-set` still interpret the
second argument as `no-overwrite`; they create ordinary file bookmarks without
position data. Record generation outside `bookmark-set` is unchanged, including
the records used for persistent Evil markers.

Bookmark storage order and immediate saving remain configured in
`autoload/night-bookmarks.el`.

Run the isolated regression checks with:

```sh
emacs -Q --batch -l tests/night-bookmarks-test.el -f ert-run-tests-batch-and-exit
```
