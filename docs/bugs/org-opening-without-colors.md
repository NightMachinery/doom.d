# Org buffers sometimes open without colors

Investigation: 2026-10-01. Report: an Org file sometimes opens with all text
black, and reloading the buffer restores its colors.

## What is established

The exact regression and triggering conditions are **not identified** yet.
Recent history contains one direct change to Org font-lock setup:
`22bd43e` (2026-09-24), in `autoload/night-ui.el`. It replaces Doom's
special-tag rule with a rule that permits dots inside tags, excluding a final
sentence period. This is a candidate to isolate, not a confirmed cause.

The GUI server's existing messages contained:

```text
Error during redisplay: (jit-lock-function 1) signaled (args-out-of-range 0 0)
```

This establishes a fontification failure in that session. The message does
not identify its buffer, major mode, time, or stack, so its connection to the
reported Org symptom remains unproven. Reverting can recover from a failed
fontification pass by rebuilding mode state and removing text properties.

Both running servers use Emacs 29.2 and Org 9.7.34. Synthetic tests passed in
both: ten cases, each with the current Org tag rule and with Doom's previous
rule restored for the test. Cases cover headings, metadata, empty and nonempty
example blocks, source blocks, tags inside markup and links, cloze deletions,
file/help links, tables, and an empty buffer. All 40 checks passed.

A scratch file also opened with font-lock and jit-lock enabled and a colored
heading in both servers. Reverting preserved that result. These tests forced
fontification explicitly; they cannot rule out a timing problem during normal
redisplay. Newly scheduled startup timers were canceled during these file
tests, so the deferred ANSI callback was not exercised there. A separate
direct ANSI-processing check preserved a plain Org heading's face.

Repeated font-lock setup does not inherently accumulate the new rule: Org's
`org-set-font-lock-defaults` builds a fresh, dynamically bound keyword list
before running its setup hook. Current and previous-rule tests both produced
46 Org keywords. A claim that the append itself necessarily creates duplicates
is unsupported.

## Other history examined

- `ecad688` (2026-09-24) changes slash syntax inside hl-todo's own syntax
  table. It fixes tag boundaries; there is no demonstrated connection to
  losing all Org colors.
- `2160f52` (2026-08-07) removes an interactive startup hook after revert and
  guards file-extension actions against errors. It concerns previews and file
  opening, without directly changing font-lock.
- August theme changes affect frame background polarity. No face or theme
  failure was reproduced. A buffer-specific failure repaired by reload is a
  weaker fit for a frame-wide theme problem.
- `jit-lock-defer-time` has been set to zero since 2021. It is not a recent
  configuration change.

## Proposed fixes and how to choose one

First capture a failing buffer **before reverting**, because reverting removes
the evidence. In that buffer, evaluate with `M-:`:

```elisp
(list :mode major-mode
      :font-lock font-lock-mode
      :jit-lock jit-lock-mode
      :keyword-count (length font-lock-keywords)
      :fontified (get-text-property (point) 'fontified)
      :face (get-text-property (point) 'face)
      :large-file (bound-and-true-p doom-large-file-p)
      :so-long (bound-and-true-p so-long-minor-mode))
```

Put point on heading text for this check. An uncolored ordinary paragraph is
expected and gives less useful evidence.

### Capture the failing call, then fix it

Enable `M-x toggle-debug-on-error`, then explicitly request visible-region
fontification with `M-:`:

```elisp
(progn
  (font-lock-flush)
  (font-lock-ensure (window-start) (window-end nil t)))
```

Running this explicitly gives errors a chance to reach the debugger outside
redisplay. Record the backtrace if it fails, then turn debug-on-error off.
If the failure only happens on initial open, a temporary diagnostic wrapper
around fontification should capture a stack at that moment. Keep logs outside
the repository: backtraces can contain file contents and paths.

The preferred permanent fix is to repair the failing matcher or startup hook
named by that stack. This preserves tag formatting and avoids adding work to
every file open. The tradeoff is that the intermittent failure must be captured
first.

### Compare only the new Org tag rule

If the issue began around September 24, temporarily remove
`night/org-fontify-special-tags` from `org-font-lock-set-keywords-hook` and
restore `doom-themes-org-fontify-special-tags` to `t`. Test fresh file opens
over enough attempts to reproduce the usual failure rate.

If that stops the failures, fix or replace only the Org portion of `22bd43e`.
Its zsh and hl-todo tag changes can remain. The tradeoff is that the old Org
rule also highlights a final sentence period. Passing a few opens is not
conclusive for an intermittent bug; the synthetic comparison already passed
with both rules.

### Restore colors without reloading the file

The flush/ensure expression above is also a recovery attempt. If font-lock is
off, first evaluate `(font-lock-mode 1)`; if its state needs rebuilding, use
`M-x org-restart-font-lock` instead. These do not reread the file or discard
unsaved text. They can still reproduce the underlying error.

A deferred, buffer-specific flush after initial open is a possible workaround
if logging establishes that startup leaves stale fontification state. Capture
the originating buffer, check that it is live and in Org mode, and fontify only
the displayed region. Do not unconditionally fontify the whole buffer in a
file-opening hook: large Org files would pay that cost on every open, and the
underlying exception would remain.

## Separate startup defect

`night/org-interactive-startup` schedules a timer whose callback uses whatever
buffer is current when it runs. It does not capture the originating Org buffer
or check whether that buffer is still live. Consequently it can set
`evil-shift-width` and apply ANSI processing to another buffer. This code
predates the recent commits, and the plain-text ANSI check did not remove Org
colors, so it is not an established explanation for this report. A separate
fix should capture the buffer and use `with-current-buffer` after checking
`buffer-live-p` and `derived-mode-p`.

No runtime configuration fix was applied during this investigation.
