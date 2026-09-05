# Mobile line numbers: overlay investigation

Investigated on 2026-09-05 using the installed Emacs 29.2, in an isolated
`emacs -Q -nw` pseudo-terminal. No running desktop buffers were changed.

`display-line-numbers` is buffer-local. Disabling its mode in a shared buffer
also disables numbers in desktop windows displaying that buffer.

An overlay with `window` set to the mobile window and
`display-line-numbers-disable` set to `t` works for ordinary text. In two
side-by-side windows showing the same buffer, the target window's text began
at its left edge while the other retained its numbered gutter.
`line-number-display-width` reported desktop 2, mobile 0. This API's value
should not be confused with the full visual gutter including padding.

The test covered a whole-buffer overlay created with rear-advance enabled.
Appending text extended the overlay and retained the same measured widths.
The shared `display-line-numbers` value remained `t` throughout.

## End-of-buffer limitation

Emacs 29's `src/xdisp.c` checks ordinary positions using
`get-char-property` with the window, which honors window-specific overlays.
At end-of-buffer it instead calls `disable_line_numbers_overlay_at_eob` in
`src/buffer.c`. That helper scans overlays without checking their `window`
property. The isolated terminal output confirmed the desktop's final empty
line number was also suppressed by the mobile overlay.

After erasing the buffer, both windows reported width 2, including the
target window: this does not fully reclaim space for an empty buffer.

Therefore the simple overlay approach is not fully isolated on this Emacs
version. It was not installed as a live feature. A compromise could exclude
the final newline/end-of-buffer from suppression and skip empty buffers,
leaving a numbered final line. Full isolation needs a redisplay fix or a
different design, such as indirect buffers with separate local settings.

Before any production implementation, test relative numbers, narrowing,
window splits, buffer switches, and overlay cleanup. Explicitly mark mobile
frames; `xterm-emacs` also identifies desktop Kitty sessions.

## Sources

- [Overlay properties](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlay-Properties.html)
- [Emacs 29 redisplay](https://github.com/emacs-mirror/emacs/blob/emacs-29/src/xdisp.c)
- [Emacs 29 end-of-buffer overlay lookup](https://github.com/emacs-mirror/emacs/blob/emacs-29/src/buffer.c)
