# Org Babel comments

Org's shared `org-comment-or-uncomment-region` already comments `src` block bodies with the source language's major mode.  `night-org-babel-comments.el` extends that same shared path to language-tagged example blocks such as `#+begin_example js`.

This means commands that ultimately use Org's comment handler, including normal Emacs commenting commands and Evil Nerd Commenter fallback paths, comment block bodies with the embedded language syntax.  For example, JavaScript lines in `#+begin_example js` toggle with `//` instead of Org `#` comments.

Only regions wholly inside the example block body are handled this way.  Block headers, footers, untagged example blocks, and normal Org text keep standard Org comment behavior.
