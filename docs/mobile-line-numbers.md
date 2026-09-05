# Mobile line numbers

## Usage

From the remote shell in Termux, run `emc-mobile` instead of `emc-gateway`.
It preserves the gateway's server selection and truecolor setup, and marks
the new terminal frame with `night/mobile=t`. Desktop launches are unchanged.

For an existing terminal frame, run `M-x night/mobile-frame-toggle`.
`M-x night/mobile-line-numbers-mode` globally enables/disables the manager;
disabling it removes all owned overlays without changing buffer settings.

`autoload/night-mobile.el` keeps one window-specific overlay per eligible
mobile window. A single pre-redisplay hook reconciles all overlays before
any window renders, including after edits, narrowing, splits, buffer switches,
or window deletion. Unchanged overlays are reused, not recreated.

The final logical line remains numbered. Overlays also stop strictly before
end-of-buffer, so trailing blank lines can retain numbers. Empty and single-line
buffers retain numbers. This boundary rule avoids the Emacs 29 bug below.

Regression checks: `emacs --batch -Q -l tests/night-mobile-test.el` from the
Doom configuration directory. Tests use isolated buffers and windows.

## Original investigation

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
property. This creates a cross-window suppression risk at end-of-buffer.
The terminal's final empty line was also blank in a later no-overlay baseline,
so that visual observation alone did not establish an overlay regression.

After erasing the buffer, both windows reported width 2, including the
target window: this does not fully reclaim space for an empty buffer.

The implemented compromise excludes the final logical line and never lets
an overlay touch accessible end-of-buffer. Mobile frames are explicitly
marked; `xterm-emacs` also identifies desktop Kitty sessions.

## Sources

- [Overlay properties](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlay-Properties.html)
- [Emacs 29 redisplay](https://github.com/emacs-mirror/emacs/blob/emacs-29/src/xdisp.c)
- [Emacs 29 end-of-buffer overlay lookup](https://github.com/emacs-mirror/emacs/blob/emacs-29/src/buffer.c)
