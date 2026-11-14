# Org archiving and `@retired` headings

`autoload/org/night-archive.el` and `autoload/org/night-org-keybindings.el` configure Org subtree archiving and a `@retired` heading at-tag toggle.

## Archiving advice and save behavior

`org-archive-subtree-default` delegates to `org-archive-default-command` (which defaults to `org-archive-subtree`). `org-archive-subtree` is wrapped by `:around` advice `night/h-org-archive-subtree-around` (replacing the older `fix-hierarchy` `defadvice`), which:

1. Recreates parent headings and local tags in the archive buffer when called without a prefix argument or active region.
2. Suppresses `org-archive-subtree-save-file-p` during the underlying `org-archive-subtree` call so the archive file is not written before hierarchy reconstruction finishes.
3. Saves or prompts once at the outermost archiving call according to `night/org-archive-save-mode`:
   - `auto` (default): saves the source buffer (resolving indirect buffers via `buffer-base-buffer`) and the archive buffer silently, without prompting or touching unrelated dirty buffers.
   - `ask`: calls `save-some-buffers` with a predicate matching only the source and archive buffers.
   - `ask-all`: calls `(save-some-buffers)` across all modified buffers.
   - `nil`: leaves both buffers unsaved.

## Keybindings and `@retired` toggle

In `autoload/org/night-org-keybindings.el`, Doom's `org-mode-map` localleader `s` bindings are swapped:

- `\sa` runs `org-archive-subtree-default` (was `org-toggle-archive-tag`).
- `\sA` runs `night/org-toggle-retired-tag` (was `org-archive-subtree-default`; note that `\A` remains `org-archive-subtree-default`).

`night/org-toggle-retired-tag` calls `night/org-toggle-heading-at-tag` with `"@retired"`. It matches standalone `@retired` anywhere inside the heading title (case-sensitive, excluding Org `:tags:` at the end of the line and compound tokens such as `@retired-v2`), removes all occurrences if present, and otherwise inserts `@retired ` at the beginning of the heading title (after stars, any TODO keyword, and any priority cookie). When `org-loop-over-headlines-in-active-region` is non-nil and a region is active, it maps over headings in the region.

`autoload/night-ui.el` defines `retired-face` and `night/retired-tag-regex` and includes them in `night/highlight-background` (also refreshed right after `night/org-toggle-heading-at-tag` runs).
