# MB2: things to do when upgrading/rebuilding Emacs

## Remove the pinned Homebrew `jpeg` 9f (local tap)

Status as of 2026-08-06: `evar/local/jpeg` (IJG jpeg 9f) is installed
and pinned solely so the *current* emacs-plus@29 29.2 binary can load
`/opt/homebrew/opt/jpeg/lib/libjpeg.9.dylib`. The next Emacs
rebuild/upgrade links against current libraries and makes it obsolete.

After the rebuild, check:

```bash
otool -L /opt/homebrew/opt/emacs-plus@29/Emacs.app/Contents/MacOS/Emacs \
  | grep -E 'jpeg|zlib|jansson'
```

(Adjust the path for whatever emacs-plus version is current.) Once
`libjpeg.9.dylib` is no longer referenced:

```bash
brew unpin jpeg
brew uninstall jpeg
brew untap evar/local
```

## Why this exists (2026-08-06 incident)

Symptom: the Emacs binary failed to launch with
`dyld: Library not loaded: /opt/homebrew/opt/jpeg/lib/libjpeg.9.dylib`.
`emc-eval` *appeared* dead, but the running servers were fine — the
failure was `emacsclient -a ''` falling back to spawning the broken
binary. `otool -L` showed three missing Homebrew deps: `jpeg`
(libjpeg.9), `zlib` (libz.1), `jansson` (libjansson.4).

Root cause: recent Homebrew runs **autoremove by default** after
uninstall/upgrade operations. The current emacs-plus@29 formula no
longer declares jpeg/zlib/jansson as dependencies, so the libraries the
*installed* (older) build actually links against looked like orphans
and were swept. This can recur for any Homebrew keg whose formula's
dependency list has drifted since the keg was built — after any big
`brew upgrade`/`autoremove`, a dyld "Library not loaded" error from an
old binary is probably this.

Fix applied: `brew install zlib jansson` (straightforward). Core's
`jpeg` formula is now v10 and ships `libjpeg.10.dylib` — a different
major soname, and symlinking across sonames is ABI-unsafe — so jpeg 9f
was installed from a local tap: the old formula was fetched from
homebrew-core's git history (commit `90e6012b73dd`,
`Formula/j/jpeg.rb`, with the `no_autobump!` line removed since that
DSL is core-only) into `evar/local`, installed from its still-hosted
bottle, and pinned.

Collateral from the same incident: the autoremove that ran during
`brew uninstall jpeg` (removing the useless v10) also swept `qtbase`
6.11.1. Per brew's dependency graph nothing installed needs it;
`brew install qtbase` if something outside brew's knowledge used it.
