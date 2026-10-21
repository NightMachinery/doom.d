# Projectile file lists

Projectile includes a project-root `TASKS.org` in its file lists even when Git
ignores it. This applies across projects, including file pickers that use
`projectile-project-files`.

The configuration in `autoload/night-projectile.el` uses:

```elisp
(setq projectile-indexing-method 'hybrid)
(add-to-list 'projectile-globally-unignored-files "TASKS.org")
```

Hybrid indexing uses external tools, then applies Projectile's include and
exclude rules. It adds processing overhead compared with `alien`, which skips
these rules. Other ignored files remain excluded unless explicitly unignored.
`TASKS.org` remains ignored by Git.

This controls file enumeration. Search commands that invoke ripgrep or another
tool independently may still skip Git-ignored files. For example, to search
only the root task file, run `rg 'pattern' TASKS.org` from the project root.

Caching is disabled in this configuration, so the next file listing reflects
the setting immediately. With caching enabled, invalidate the project's cache
using `M-x projectile-invalidate-cache` after changing include rules.
