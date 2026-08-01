# GUI Emacs server socket mismatch (`withemcgui emc-eval` fails)

## Symptom

`withemcgui emc-eval '...'` fails with:

```
No such server: /Users/evar/tmp/.emacs-servers/server_gui
```

even though a GUI Emacs is clearly running.

## Root cause

There are several independent `server-start` call sites, and a race between
two of them creates the mismatch:

1. **`config.el:119`** — the intended one. Runs during user-config load,
   *after* `server-socket-dir` (config.el:37) and `server-name`
   (config.el:56, from `$emacs_night_server_name`) are set. Guarded by
   `(when (not (server-running-p)) ...)`.
2. **Doom core, `~/.emacs.d/lisp/doom-editor.el:404-412`** — a lazy
   `use-package! server` block with
   `:after-call doom-first-input-hook doom-first-file-hook focus-out-hook`
   and `:defer 1`. `doom-editor.el` is required from Doom's core `init.el`,
   so these triggers are armed **before** config.el loads. If one of them
   fires early (e.g. `focus-out-hook` when you switch to another app while
   the slow Doom startup is still running), `server-start` runs with the
   **default** `server-name` (`"server"`) and **default** `server-socket-dir`
   (`$XDG_RUNTIME_DIR/emacs`), binding e.g. `/tmp/runtime-sth/emacs/server`.
   It also overrides `server-name` from `$EMACS_SERVER_NAME` if that is
   exported (it normally is not).
3. **`with-editor--setup`** (magit commits etc.) — if `server-process` is
   dead it may *rename* `server-name` to `server<PID>` and `server-start`.
4. **`+default/restart-server`** (interactive command) and daemon launches
   (`--bg-daemon=PATH`, socket bound in C before any config) — benign.

Normally the early default-socket bind from (2) self-heals: config.el:119's
`server-start` kills the previous server process and rebinds at the correct
`server_gui` path. The bug manifests only when **an older GUI Emacs is still
alive and listening on `server_gui`** at the moment config.el:112 runs its
`server-running-p` check: the check returns t (someone *is* serving that
name), so the new instance skips `server-start` and keeps the stray
default socket. When the old Emacs later exits, it removes the `server_gui`
socket file — leaving *no* listener on the expected path. That is why the
problem appears only after launching a new GUI Emacs while the previous one
was still running.

Inspecting the running Emacs shows the mismatch:

- `server-name` ⇒ `/Users/evar/tmp/.emacs-servers/server_gui`
- actual socket (via `lsof -p <emacs-pid> | grep unix`) ⇒
  `/tmp/runtime-sth/emacs/server`

`withemcgui emc-eval` connects using `$EMACS_GUI_SOCKET_NAME`, hence the
"No such server" error.

## Diagnosis recipe

1. Confirm the GUI Emacs is running: `ps aux | grep -i emacs | grep -v grep`
   (the GUI instance has no `--bg-daemon` flag; the terminal daemon does).
2. Find where it actually listens:
   `lsof -p <gui-emacs-pid> | grep unix` — look for a `.../emacs/server` path.
3. Talk to it through the real socket:

   ```zsh
   EMACS_GUI_SOCKET_NAME=/tmp/runtime-sth/emacs/server \
       withemcgui emc-eval '(list server-name (window-system))'
   ```

## Fix (without restarting Emacs)

Restart the server from inside — `server-name` is already correct, so a new
`server-start` creates the socket at the expected path and removes the stray
one. Schedule it with a timer so the reply survives the socket teardown:

```zsh
EMACS_GUI_SOCKET_NAME=<actual-socket-path> withemcgui emc-eval \
    '(progn (run-at-time 0.5 nil (lambda () (server-start))) t)'
```

Then verify `withemcgui emc-eval '(list :ok (window-system) server-name)'`
works and `/Users/evar/tmp/.emacs-servers/server_gui` exists.

## Implemented fix (2026-08-01)

`config.el` now starts the server via `night/server-start-carefully`
(defined next to the old call site) instead of the raw
`server-running-p`-guarded `server-start`:

1. **Owner check** — if `server-process` is live AND
   `(process-contact server-process :service)` equals
   `(expand-file-name server-name server-socket-dir)`, we already serve the
   correct socket; do nothing. The `process-contact` path comparison is what
   catches the Doom stray-early-bind case (`process-live-p` alone would
   wrongly skip).
2. Otherwise, if another process listens on our name, wait up to
   `night/server-occupied-wait-seconds` (default 10), printing a
   "waiting for ... to be released" message each second.
3. If the socket is freed (or was never taken), `server-start` — this also
   kills our own stray wrong-path server, if any.
4. If still occupied, `display-warning` and follow
   `night/server-occupied-policy`:
   - `no-server` (default): this Emacs runs without a server.
   - `alt-name`: start under the first free `<server-name>_<n>` and warn
     about the name used.

Covered by batch-mode tests during development: free socket → normal start;
occupied → wait + warning + no server; occupied + `alt-name` → `..._1`
socket; live stray wrong-path server → rebound at the expected path.

## Notes

- `emc-eval` wraps the form in
  `(with-current-buffer (window-buffer (selected-window)) ...)`, so results
  depend on whichever window the user has focused. When targeting a specific
  file, wrap the form in
  `(with-current-buffer (find-buffer-visiting "/abs/path") ...)`.
- Observed on 2026-08-01 with emacs-plus@29 29.2 on macOS.
