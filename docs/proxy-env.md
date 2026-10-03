# Proxies, and the environment a daemon froze at startup

A process's environment is fixed when it is created. An Emacs daemon inherits
whatever the shell that launched it had exported, and keeps it for the rest of
its life. Nothing in a shell can reach back into a process that already exists,
so turning a proxy off — or on — in a terminal has no effect whatever on a
running daemon.

That is the whole failure mode. A daemon started from a shell with a proxy
active goes on sending every request through that proxy for days. When the proxy
later dies, every affected call fails, and nothing about the failure mentions a
proxy.

## Why the breakage looks selective

Emacs has two unrelated notions of "the proxy", and they do not consult each
other.

`plz` shells out to `curl`, and `curl` reads `http_proxy`, `https_proxy`,
`all_proxy` and their uppercase spellings straight out of the environment.
Anything built on `plz` therefore follows the frozen environment: the FIM
commands, `ement`, and the `llm` package behind ellama.

`url.el` consults `url-proxy-services` and can lazily import scheme proxy
variables from the environment into that cache. Clearing the environment alone
does not remove a proxy it already cached. Packages may also configure their
own transport, so a working gptel or `night/myip-amazon` call does not establish
that the environment used by curl is correct.

So the symptom is a subset of network features failing while others are
obviously fine, which reads like a bug in the failing feature rather than a
machine-wide condition. It is worth checking the environment early for exactly
that reason.

## The signature of a dead local proxy

A proxy that has died leaves a distinctive trace:

    curl error 7: Failed to connect to host. (0.0s)

Exit code 7 is "could not connect". Connecting to a closed port on the loopback
interface is refused immediately, which explains the near-zero duration.
This is a useful clue, not proof of a proxy failure: a remote refusal can also
be immediate. Confirm the configured route and whether anything is listening.

The message names no host and no port, which is what makes this expensive to
diagnose. That loss is in `plz`: it discards curl's stderr and substitutes a
fixed string looked up from its own `plz-curl-errors` table. curl itself says
which address it failed to reach, and that text never survives.

## Looking at it

`night/proxy-status` reports the proxy variables as the current Emacs process
sees them. With no prefix argument it messages the ones that are set, grouped by
value, so the common case of every variable pointing at one proxy is a single
line. With a prefix argument it writes all of them, set or not, to a buffer.
From Lisp it prints nothing and returns an alist.

It reports both the lowercase and uppercase spellings deliberately. curl reads
the lowercase forms, some tools read only the uppercase ones, nothing keeps them
in step, and a report showing one case could miss the variable actually causing
the trouble.

It also distinguishes a variable that is unset from one set to the empty string.
To curl an empty `http_proxy` means "send this request directly", which is a
configuration choice rather than an absence, and collapsing the two would hide
it. This is why it uses plain `getenv` rather than `night/getenv-nonempty`; the
`@warn` in `config.el` is about treating an empty override as a value, and this
is a report rather than an override.

## What it deliberately does not do

It never probes whether the proxy is alive. A variable being set is not the same
as something listening, but a liveness check means opening a connection, and a
diagnostic that can hang is worse than one that reports a little less. Check by
hand instead:

    lsof -nP -iTCP:<port> -sTCP:LISTEN

No listener means that local proxy cannot accept a connection. An
unauthenticated request to the provider endpoint can distinguish a transport
failure from an API failure: an HTTP 401 still proves the route connected.

## Getting out of it

If the proxy is intended, restore its service and retry. This keeps the route
that Emacs was configured to use. FIM does not retry directly or automatically
switch providers after a connection failure.

If direct connections or a different proxy are intended, explicitly set the
desired environment variables in each affected server and update
`url-proxy-services`, or restart the daemon from the correct shell environment.
There is no way to make an existing process re-read its parent's environment.

## The same shape elsewhere

This is one instance of a general hazard: state captured once at daemon startup,
never refreshed, and invisible afterwards. The redis connection is the other
known case — `night/redis-connect` runs once at load and nothing re-dials on its
own. See `docs/redis-eredis-auth.md`. When something works everywhere except in
a long-lived Emacs, suspect startup-frozen state before suspecting the feature.
