# Org Babel navigation

`night/org-babel-next-src-block` and `night/org-babel-previous-src-block` are Evil motions.

Why: plain interactive commands can be bound in Evil visual states, but Evil's visual/visual-line selection machinery may not treat them as motions. Defining them with `evil-define-motion` lets bindings such as `C-<down>` work after pressing `V` (Evil visual-line state), extending/moving the visual selection to the next Org block marker.

The motions navigate between `#+begin_` and `#+end_` block markers, not only source-block heads.

Compatibility note: these commands keep an optional `COUNT` argument, so existing Elisp call sites such as `(night/org-babel-previous-src-block)` continue to work without passing an Evil count.

## evil-define-motion vs defun
- @GPT5.5T

In Emacs Lisp, `defun` defines an ordinary function. `evil-define-motion` defines an **Evil motion command**: a function with extra Evil metadata and behavior so it works correctly as a Vim-style motion/operator target.

### `defun`

Use `defun` for normal Emacs commands/functions.

```elisp
(defun my-forward-word ()
  (interactive)
  (forward-word 1))
```

This creates a command, but Evil does **not** automatically know it is a motion. In operator-pending mode, visual selections, repeat behavior, type handling, etc., it may not behave like a proper Evil motion.

### `evil-define-motion`

Use `evil-define-motion` when the command is meant to move point and be usable by Evil operators like `d`, `c`, `y`, visual mode, etc.

```elisp
(evil-define-motion my-forward-word (count)
  "Move forward COUNT words."
  :type exclusive
  (forward-word (or count 1)))
```

Then you can bind it as a motion:

```elisp
(define-key evil-normal-state-map "w" #'my-forward-word)
(define-key evil-motion-state-map "w" #'my-forward-word)
```

Now `dw`, `cw`, `yw`, visual selection extension, and count handling can work in Evil’s motion system.

### Key differences

| Feature                                                |                         `defun` | `evil-define-motion` |
| ------------------------------------------------------ | ------------------------------: | -------------------: |
| Defines a function                                     |                             Yes |                  Yes |
| Creates an interactive command                         | Only if you add `(interactive)` |    Yes, motion-aware |
| Works as Evil operator target                          |               Not automatically |                  Yes |
| Supports motion type metadata                          |                              No |                  Yes |
| Handles Evil count conventions                         |                        Manually |             Built in |
| Supports `:type` like `exclusive`, `inclusive`, `line` |                              No |                  Yes |

### Motion type matters

The `:type` tells Evil what text range the motion covers when used with operators.

```elisp
:type exclusive
```

means the final character is excluded, common for forward motions.

```elisp
:type inclusive
```

means include the final character.

```elisp
:type line
```

means operate on full lines, like `dd`.

### Rule of thumb

Use `defun` when you are writing a normal Emacs function.

Use `evil-define-motion` when the function is conceptually a Vim/Evil motion and should compose with operators like:

```text
d{motion}
c{motion}
y{motion}
v{motion}
```

So: **`evil-define-motion` is basically `defun` plus Evil motion semantics.**

