# `audiofile:` links and the audio mpv server

Following an `audiofile:` link whose target has gone missing used to kill the
persistent audio mpv server. Once the server was gone, every later `hear-*`
command failed at `assert test -e "$soc"` until `hear-start-server` was run
again — so a single stale link in a notes file silently broke music playback
for the rest of the session.

## The path a link takes

`[[audiofile:...]]` is registered in `autoload/org/links/night-org-links-audio.el`
and follows through the generic zsh-file handler:

- `night/org-link-zshfile-follow` (`autoload/org/links/night-org-links-zshfile.el`)
  resolves the path with `night/path-unabbrev` and calls `org-link-open-as-file`.
- Emacs opens the file, and the audio-extension arm of
  `night/file-extension-actions2` (`autoload/night-extension-hooks.el`) kills
  that buffer and calls `night/hear`.
- `night/hear` (`autoload/night-audio.el`) dispatches
  `awaysh-oneinstance <marker> hear-loadfile-begin <path>` through brish.
- `hear-loadfile` (`$NIGHTDIR/zshlang/auto-load/others/mpv.zsh`) sends
  `loadfile <path> replace` over `$mpv_audio_ipc`.

So playback is never a direct mpv call from Emacs; it is a `loadfile` RPC to a
server that is expected to already be running.

## Why a missing file killed the server

`hear-start-server`
(`$NIGHTDIR/zshlang/auto-load/others/music/music.zsh`) launches the server on a
seed track with `--loop-playlist=inf`, and inherits `--keep-open=no` from
`hear-noipc`. It had no `--idle`.

`loadfile ... replace` *discards* the current playlist before loading the
replacement. When the replacement failed to load, mpv's playlist was empty, and
without `--idle` mpv exits on an empty playlist — taking the IPC socket with it.

## The invariant

**`hear-start-server` must pass `--idle=yes`.** It is what makes the server
survive any load failure, not just a missing file: an unmounted volume, a
permission error, or an unsupported codec all empty the playlist the same way.
This matters because `~mu/` points at external drives (`shortcuts.zsh` maps
several `/Volumes/...` music directories onto it), so "the file is not there"
is a routine condition, not an edge case.

`--idle` is orthogonal to `--keep-open`: `--keep-open` governs what happens at
the end of a file, `--idle` governs what happens when the playlist runs out.

Do **not** move `--idle=yes` into `hear-ipc` itself — that is also used as a
one-shot instance player (`hear-playlist` with mode `instance`), where
lingering forever would be wrong.

## Failing loudly

`hear-loadfile` is the single chokepoint: `hear-loadfile-begin`,
`hear-load-playlist` (via `mpv_load_command=loadlist`) and the video sibling
`mpv-loadfile` (via `fnswap hear-do mpv-do`) all funnel through it. It now:

- rejects a local path that does not exist, with a non-zero return and an
  `hs-alert`, before any IPC traffic happens;
- prunes entries whose files have gone missing out of `loadlist` playlists,
  writing a pruned copy to a fresh temp file rather than rewriting the input
  (callers may pass a playlist we do not own), and aborting only when nothing
  playable is left;
- leaves anything with a URL scheme alone, so streaming still works.

Alerts go through `silence reval-timeout 10 ... || ecgray ...` so a wedged
Hammerspoon cannot hold up playback for the 30s timeout baked into the
`hammerspoon` wrapper.

Emacs checks too, in `night/hear` and in `night/org-subtree-play-as-playlist`.
That is purely for latency: `night/hear` dispatches through
`awaysh-oneinstance`, so a zsh-side failure never comes back, and without a
local check nothing appears in the echo area. `night/audio-path-checkable-p`
decides what Emacs may check — it excludes URLs, and paths starting with a zsh
named directory such as `~mu/`, which `expand-file-name` silently mis-resolves
as relative instead of erroring.

## `night/hs-alert`

`night/hs-alert` (`autoload/night-external.el`) is the elisp wrapper over zsh's
`hs-alert`. It is asynchronous on purpose — a wedged Hammerspoon must never
block Emacs — and takes keywords covering the whole `hs-alert-v2` surface:
`:dur`, `:flash`, `:pos`, `:id`, `:markup`, `:color`. Anything omitted keeps
the zsh-side default.

The knobs travel through zsh's `@opts` rather than environment variables. That
needs one non-obvious piece of setup: `h_@opts` derives its variable prefix
from `magic_opts_prefixes[$cmd[1]]`, falling back to `${cmd[1]}_`, and
`ensure-var-name` *sanitises* a dash into an underscore rather than rejecting
it. So `@opts dur 10 @ hs-alert msg` would set `hs_alert_dur`, which nothing
reads, and the option would be silently ignored. `hammerspoon.zsh` pins the
prefix explicitly:

```zsh
@opts-setprefix hs-alert-v2 alert
@opts-setprefix hs-alert alert
@opts-setprefix alert alert
```

## Picking up changes

brish evaluates each command in a subshell of a long-lived session, so editing
these zsh files does not affect an already-running session — the old function
body stays loaded. Run `brishz-restart` to pick the changes up. A running mpv
server keeps its original command line too, so `--idle=yes` only takes effect
after the server is restarted (`hear-start-server`).
