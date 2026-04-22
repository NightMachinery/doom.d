# Cross-project Org ID links

- `id-to:` links use the form `id-to:Project::ID` and may keep any normal Org ID suffix like `::SEARCH` or `::/regex/`.
- The `Project` part resolves through:
  - `(night/path-unabbrev (concat "~[" project "]/"))`
- After resolving the project root, link opening reuses the existing Org ID lookup flow from that root.
- `M-x night/paste-id-to-link` converts the latest stored `id:` link in `org-stored-links` into an `id-to:` link.
- The helper derives `Project` from the stored link's source file project-root basename.
- The helper intentionally uses the cached file path from the most recent `org-id-store-link`, instead of resolving the ID by grep.
- The helper errors clearly when:
  - there is no stored link
  - the stored link is not an `id:` link
  - the latest stored link no longer has cached file context; in that case, store the link again
  - the stored target has no detectable project root
  - the `~[Project]/` alias cannot be resolved when opening
