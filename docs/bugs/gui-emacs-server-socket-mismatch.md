# GUI Emacs server socket mismatch (`withemcgui emc-eval` fails)

## Symptom

`withemcgui emc-eval '...'` fails with:

```
No such server: /Users/evar/tmp/.emacs-servers/server_gui
```

even though a GUI Emacs is clearly running.

## Root cause

The GUI Emacs calls `server-start` **before** the config sets
`server-name`/`server-socket-dir`. At that point `server-name` is still the
default `"server"` and the socket directory is derived from
`XDG_RUNTIME_DIR` (e.g. `/tmp/runtime-sth`), so the socket is created at:

```
$XDG_RUNTIME_DIR/emacs/server        # e.g. /tmp/runtime-sth/emacs/server
```

Later the config sets `server-name` to the absolute path
`/Users/evar/tmp/.emacs-servers/server_gui` (from `$EMACS_GUI_SOCKET_NAME`),
but since `server-start` is not run again, the variable and the actual
listening socket disagree. Inspecting the running Emacs shows the mismatch:

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

## Proper fix (TODO)

Ensure the GUI startup path sets `server-name` (from
`$EMACS_GUI_SOCKET_NAME` / `emacs_night_server_name`) **before** the first
`server-start` runs, or call `server-start` again after setting it.

## Notes

- `emc-eval` wraps the form in
  `(with-current-buffer (window-buffer (selected-window)) ...)`, so results
  depend on whichever window the user has focused. When targeting a specific
  file, wrap the form in
  `(with-current-buffer (find-buffer-visiting "/abs/path") ...)`.
- Observed on 2026-08-01 with emacs-plus@29 29.2 on macOS.
