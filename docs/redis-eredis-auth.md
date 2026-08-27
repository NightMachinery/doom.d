# Redis from Emacs: auth, and why failures used to be invisible

Redis has a `requirepass`. The password lives in `~/.redis-auth` (one line,
mode 600); zsh picks it up in `h-redis-auth-ensure`, which exports it as
`REDISCLI_AUTH` for `redism`. Emacs has to authenticate separately, and for a
while it did not.

## The three things eredis does that combine badly

**It cannot authenticate on connect.** `eredis-connect` takes host, port and a
`nowait` flag, and nothing else. AUTH exists as an ordinary command,
`eredis-auth`, so it has to be sent as a separate step after connecting.

**It returns error replies as if they were data.** `eredis-parse-error-response`
is defined as a call to `eredis-parse-status-response`. Both strip the first
byte and hand back the rest as a string, so `-NOAUTH Authentication required.`
arrives looking exactly like `+OK`. Nothing signals. Every caller sees a
plausible string and carries on.

**It never reconnects.** `eredis-sentinel` nils out `eredis--current-process`
and deletes it. After that, calls signal "redis not connected" until something
dials again, and nothing did except startup.

## What that cost us

An unauthenticated Emacs did not fail; it lied. `night/path-unabbrev` writes
its argument to a redis key and asks zsh to read it back (the workaround for
brish not handling non-utf-8 arguments). The write returned "NOAUTH
Authentication required." and did nothing, so zsh read whatever was already
under that key and resolved *that*. Following an `audiofile:` link therefore
reported, and would have played, a completely unrelated file — one another
process had put there. See the post-mortem below.

The password cache did the same thing in a more alarming direction:
`eredis-hget` returned the NOAUTH string, which is neither nil nor empty, so
`password-in-cache-p-redis` called it a hit and `password-read-from-cache-redis`
returned it as the passphrase. And `night/redis-lock` could never see the `1`
it wanted from `eredis-setnx`, so it exhausted its retries and reported a
contended lock, while `with-redis-lock` ran its body unlocked anyway.

Only one Emacs showed any of this. The terminal daemon had connected before
`requirepass` was applied, and redis leaves already-connected clients
authenticated, so it kept working — a grandfathered connection that would have
broken silently at its next restart. **Test redis changes in the GUI daemon**,
or in a freshly connected one; a daemon that predates the password proves
nothing.

## How it connects now

`night/redis-connect` (`autoload/night-redis.el`) connects, sends AUTH with the
password from `night/redis-auth-file`, and confirms with PING before setting
`night/redis-connected-p`. It never signals: a downed redis warns and returns
nil, because it runs at load time and must not be able to break startup. A
server with no `requirepass` answers AUTH with an error, which is treated as
benign — the PING is what decides.

`night/redis-reconnect` is the recovery command, for after a redis restart or a
password rotation, since nothing re-dials on its own.

The connect lives in `autoload/night-redis.el` rather than `night-basic.el`,
because `night-loader.el` loads `night-basic.el` first and the auth helpers
would not exist yet. `night-basic.el` keeps the `require` so `(featurep
'eredis)` is true in time for `night-password.el`'s `after!`. Nothing between
the two touches redis at load time.

## Checked wrappers

Prefer `night/redis-set`, `night/redis-setnx`, `night/redis-expire` and
`night/redis-get` over the raw eredis calls, so an error reply signals
`night/redis-error` instead of travelling on as a value.

For status and integer replies the check is exact. A command whose success
reply is a fixed string can be compared against it, and integer replies arrive
as elisp integers while errors arrive as strings, so the types cannot collide.

`night/redis-get` is the weak one and says so in its docstring. Once eredis has
stripped the leading `-` there is nothing left to distinguish an error from a
bulk string, so it matches known error-code prefixes — `NOAUTH`, `WRONGTYPE`,
`ERR` and friends. A stored value that genuinely begins that way would be
rejected. That is acceptable for the value domains here, paths and cached
passwords, and it is a tripwire for the next unforeseen failure rather than the
main defence: an authenticated connection does not produce NOAUTH mid-session.

Where a wrong answer is dangerous rather than merely wrong, do not signal —
treat it as absence. `night/password--redis-get` returns nil for an error
reply, a signal, or a non-string, so the caller falls through to the local
cache and then to prompting.

## Post-mortem: one key, several writers

`night/path-unabbrev` and `night/path-abbrev` shared a single redis key,
`emacs_input`. Two Emacs daemons and both conversion directions wrote to it, so
even with working auth a concurrent pair of calls could return each other's
answers. The auth failure just made it happen every single time, and made it
look like a bug in the audio code.

`night/path-convert` now uses `emacs_input::<pid>::<serial>`, checks the write,
refuses an empty result from zsh, and deletes the key afterwards with a TTL as
backstop. Do not collapse it back to one key to save a round trip: the checked
write catches a dead connection, but nothing can recover a value that another
process has already overwritten.

`night/path-abbrev-memoized` caches for 9999 hours. That is only safe because
failures now signal, and `memoize` does not cache a call that signalled. A
session that cached bad values before the fix can be cleared by re-loading
`night-external.el`, which re-wraps the function and drops the cache.
