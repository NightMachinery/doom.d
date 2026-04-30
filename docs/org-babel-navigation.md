# Org Babel navigation

`night/org-babel-next-src-block` and `night/org-babel-previous-src-block` are Evil motions.

Why: plain interactive commands can be bound in Evil visual states, but Evil's visual/visual-line selection machinery may not treat them as motions. Defining them with `evil-define-motion` lets bindings such as `C-<down>` work after pressing `V` (Evil visual-line state), extending/moving the visual selection to the next Org block marker.

The motions navigate between `#+begin_` and `#+end_` block markers, not only source-block heads.

Compatibility note: these commands keep an optional `COUNT` argument, so existing Elisp call sites such as `(night/org-babel-previous-src-block)` continue to work without passing an Evil count.
