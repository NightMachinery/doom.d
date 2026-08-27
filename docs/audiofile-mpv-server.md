# `audiofile:` links and the audio mpv server

Following an `audiofile:` link whose target has gone missing used to kill the
persistent audio mpv server. Once the server was gone, every later `hear-*`
command failed at `assert test -e "$soc"` until `hear-start-server` was run
again — so a single stale link in a notes file silently broke music playback
for the rest of the session.

## The path a link takes

`[[audiofile:...]]` is registered in `autoload/org/links/night-org-links-audio.el`:

- `night/org-link-audiofile-follow` resolves the path with
  `night/path-unabbrev` and calls `night/hear` on it. No buffer is involved.
- `night/hear` (`autoload/night-audio.el`) dispatches
  `awaysh-oneinstance <marker> hear-loadfile-begin <path>` through brish.
- `hear-loadfile` (`$NIGHTDIR/zshlang/auto-load/others/mpv.zsh`) sends
  `loadfile <path> replace` over `$mpv_audio_ipc`.

So playback is never a direct mpv call from Emacs; it is a `loadfile` RPC to a
server that is expected to already be running.

It used to go the long way round, through the generic zsh-file handler
`night/org-link-zshfile-follow`: `find-file` the track, wait for
`window-configuration-change-hook` to notice the extension, and let
`night/file-extension-actions2` kill the buffer again and call `night/hear`.
Two things made that fragile. `org-open-file` skips its existence check
whenever `org-file-apps` resolves to `emacs` — "Emacs has no problems with
non-ex files" — and ours resolves everything that way via `(t . emacs)`, so a
link to a missing file quietly became an empty buffer visiting it. And
`night/file-extension-actions2` killed that buffer with `kill-current-buffer`,
an interactive command Doom advises `:before-until`, which declines to kill
when the buffer is on show in another window. A media buffer that survived then
replayed itself on every window configuration change.

`night/file-extension-actions2` is now only the fallback for media opened some
other way — dired, `find-file` — and it uses plain `kill-buffer` and a
buffer-local flag so it acts at most once per buffer.
`night/org-link-zshfile-follow` consults `org-open-non-existing-files` before
handing a path to org, so `zf:` links no longer create files by being followed;
set that variable non-nil if you want the old behaviour back.

## Why a missing file killed the server

`hear-start-server`
(`$NIGHTDIR/zshlang/auto-load/others/music/music.zsh`) launches the server on a
seed track with `--loop-playlist=inf`, and inherits `--keep-open=no` from
`hear-noipc`. It had no `--idle`.

Measured against throwaway servers launched exactly like the real one, only the
last of these actually kills mpv:

- `loadfile <missing> replace`, including the whole `hear-loadfile-begin`
  sequence with its 100 `hear-seek-begin` retries — **survives** either way.
  `--loop-playlist=inf` keeps looping the failed entry, so the playlist never
  empties.
- `loadlist <missing playlist>` and `loadlist <empty playlist>` — **survive**
  either way. The `loadlist` fails outright and the previous playlist is left
  in place.
- `loadlist <playlist whose entries all fail>` — **dies** without `--idle`,
  survives with it. Here the playlist really is replaced, every entry fails,
  and the playlist is exhausted.

So the fatal shape is specifically an exhausted playlist, which in practice
means the `hear-load-playlist` path — `night/org-subtree-play-as-playlist`, or
`hear-playlist` — pointed at tracks that have all gone missing.

Note this does **not** reproduce the original single-link report. A dead
`audiofile:` link on its own could not be made to kill the server here; the
guards below stop it reaching mpv regardless, but the exact mechanism behind
that first sighting is unconfirmed.

## The invariant

**`hear-start-server` must pass `--idle=yes`.** It is what makes the server
survive an exhausted playlist, whatever emptied it — not just missing files but
an unmounted volume, a permission error, or an unsupported codec across a whole
playlist. This matters because `~mu/` points at external drives
(`shortcuts.zsh` maps several `/Volumes/...` music directories onto it), so
"the files are not there" is a routine condition, not an edge case.

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
local check nothing appears in the echo area. `night/path-checkable-p`
(`autoload/night-external.el`) decides what Emacs may check — it excludes URLs,
and paths starting with a zsh named directory such as `~mu/`, which
`expand-file-name` silently mis-resolves as relative instead of erroring.

One caveat on trusting that message. It reports the path Emacs actually
resolved, which is not necessarily the one in the link: a broken redis
connection once made `night/path-unabbrev` return another process's path
entirely, and the resulting "file does not exist" named a file nothing had
asked for. See `docs/redis-eredis-auth.md`.

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

BrishGarden keeps persistent zsh shells, so it does not see zshlang edits on its
own — the old function body stays loaded, and testing in a fresh `zsh -ic`
proves nothing about what the garden is running. Run `brishz-restart` after
changing any of this.

A running mpv server keeps the command line it was started with, so `--idle=yes`
only takes effect once the server itself is restarted. It is launched from the
`ivy` tmux session (`tmux.zsh`, `ivy-self`), so restarting it means killing the
mpv process and re-running `hear-start-server` in that pane — which interrupts
whatever is playing.

The zsh side lives in `$NIGHTDIR`, which is version-controlled with
`vcsh night.sh` rather than plain `git`; see `$NIGHTDIR/AGENTS.md`.
