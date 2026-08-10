#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Set up Doom for this config, from nothing.
#:
#: Lives here rather than in any one bootstrap repo because this config is
#: installed by several of them; they should only have to provide a working
#: `emacs' on PATH and then call this.
#:
#: Usage:
#:   sh ~/doom.d/bootstrap/install.sh
#:
#: Environment:
#:   DOOMDIR           where this config lives         (default ~/doom.d)
#:   DOOMLOCALDIR      package tree + generated files  (default doom's own)
#:   NIGHT_DOOM_REF    doomemacs revision to pin       (default below)
#:   NIGHT_DOOM_NO_LOCKFILE=y   skip the version lockfile
#:
#: See README.org in this directory for why each step is the way it is.

set -eu

: "${DOOMDIR:=${HOME}/doom.d}"
: "${NIGHT_DOOM_REF:=fb9b359db}"
: "${DOOM_EMACS_DIR:=${HOME}/.emacs.d}"

log()  { printf '==> %s\n' "$*" >&2 ; }
warn() { printf 'warn %s\n' "$*" >&2 ; }
die()  { printf ' err %s\n' "$*" >&2 ; exit 1 ; }

command -v emacs >/dev/null || die "no emacs on PATH; install one first"
command -v git   >/dev/null || die "git is required"

#: Make a `doom sync --rebuild' safe to run.
#:
#: A rebuild deletes every build directory. Two things make that fail on an
#: NFS home, and both leave the tree half-built rather than erroring cleanly:
#:
#:   1. A running Emacs keeps a loaded dynamic module (emacs-zmq.so) mmap'd.
#:      Deleting an open file over NFS silly-renames it to .nfsXXXX instead of
#:      unlinking, so the parent directory will not rmdir.
#:   2. That .so is a *real file* in a directory straight otherwise fills with
#:      symlinks, and we are the ones who put it there. It is re-fetched after
#:      the rebuild anyway, so it must not be present during one.
#:
#: @warn Match daemons by exact process name. `pkill -f emacs' also matches
#: the shell running this script, which has "emacs" in its own command line --
#: that mistake killed the invoking shell twice while writing this.
night_prepare_rebuild() {
    local_dir="${DOOMLOCALDIR:-${DOOM_EMACS_DIR}/.local}"

    for p in $(pgrep -x emacs 2>/dev/null || true) ; do
        [ "${p}" = "$$" ] && continue
        if grep -qa "straight/build" "/proc/${p}/maps" 2>/dev/null ; then
            log "stopping emacs pid ${p}: it has a build directory mapped"
            kill -TERM "${p}" 2>/dev/null || true
        fi
    done

    #: Give them a moment to exit and release the mappings.
    n=0
    while [ "${n}" -lt 10 ] ; do
        pgrep -x emacs >/dev/null 2>&1 || break
        still=0
        for p in $(pgrep -x emacs 2>/dev/null || true) ; do
            grep -qa "straight/build" "/proc/${p}/maps" 2>/dev/null && still=1
        done
        [ "${still}" = 0 ] && break
        sleep 1
        n=$((n + 1))
    done

    find "${local_dir}/straight" -maxdepth 3 \
         \( -name 'emacs-zmq*.so' -o -name '.nfs*' \) -delete 2>/dev/null || true
}

#: Nothing here may block on stdin: this runs unattended.
export GIT_TERMINAL_PROMPT=0

log "emacs: $(emacs --version 2>&1 | head -1)"

##
#: --- doomemacs itself, at a pinned revision ---
#: Doom 3.x master moves fast and this config carries version-specific
#: assumptions, so the revision is chosen deliberately rather than by the
#: accident of when you happened to clone.
if [ -d "${DOOM_EMACS_DIR}/.git" ] ; then
    log "doom already cloned"
else
    log "cloning doomemacs"
    git clone https://github.com/doomemacs/doomemacs "${DOOM_EMACS_DIR}"
fi

current_ref="$(git -C "${DOOM_EMACS_DIR}" rev-parse --short HEAD 2>/dev/null || echo none)"
case "${NIGHT_DOOM_REF}" in
    "${current_ref}"*) log "doom pinned at ${current_ref}" ;;
    *)
        log "pinning doom to ${NIGHT_DOOM_REF} (was ${current_ref})"
        #: A --depth 1 clone cannot reach an older commit.
        [ -f "${DOOM_EMACS_DIR}/.git/shallow" ] && \
            git -C "${DOOM_EMACS_DIR}" fetch --quiet --unshallow origin || true
        git -C "${DOOM_EMACS_DIR}" fetch --quiet origin || true
        git -C "${DOOM_EMACS_DIR}" checkout --quiet "${NIGHT_DOOM_REF}" \
            || die "could not pin doom to ${NIGHT_DOOM_REF}"
        ;;
esac

##
#: --- the package tree must match the doom revision that built it ---
#: Changing the pin after a sync leaves straight/ holding the other revision's
#: package set; `doom sync' then tries to move already-built repos and stops on
#: an interactive "How to proceed? (1,2,3,4,5)" conflict prompt. With stdin
#: closed that aborts *before* the generated profile init is written, and the
#: next daemon start dies with
#:   Symbol's value as variable is void: doom--profile-default
#: A fresh tree cannot conflict, so rebuild whenever the pin moves.
if [ -n "${DOOMLOCALDIR:-}" ] ; then
    ref_stamp="${DOOMLOCALDIR}/.night-doom-ref"
    ref_now="$(git -C "${DOOM_EMACS_DIR}" rev-parse HEAD)"
    if [ -f "${ref_stamp}" ] && [ "$(cat "${ref_stamp}")" != "${ref_now}" ] ; then
        warn "doom revision changed since the last sync; rebuilding the package tree"
        mv "${DOOMLOCALDIR%/}" "${DOOMLOCALDIR%/}.stale-$$" || true
        mkdir -p "${DOOMLOCALDIR}"
    fi
fi

##
#: --- submodules this config actually loads ---
#: Only the ones night-loader.el references. `radian' and `emacswiki' are
#: deliberately skipped: neither is loaded, and emacswiki's .gitmodules URL
#: uses the git:// protocol, which GitHub disabled in 2022.
if [ -d "${DOOMDIR}/.git" ] ; then
    ( cd "${DOOMDIR}" && git submodule update --init --depth 1 \
        gitmodules/fzf.el \
        gitmodules/osx-clipboard-mode \
        gitmodules/pdf-continuous-scroll-mode.el ) || warn "submodule init incomplete"
fi

##
#: --- install ---
#: --no-config: this config is already present.
#: --no-env:    a generated envvars file bakes one machine's environment into
#:              the config; on a shared home that is actively wrong. Set the
#:              few variables the config needs (DOOMDIR!) in your shell instead.
#: --no-hooks:  it otherwise prompts on stdin and dies with end-of-file.
if [ ! -e "${DOOMLOCALDIR:-${DOOM_EMACS_DIR}/.local}/straight" ] ; then
    log "doom install"
    "${DOOM_EMACS_DIR}/bin/doom" install --no-env --no-config --no-hooks < /dev/null \
        || warn "doom install reported an error; continuing to sync"
fi

log "doom sync"
"${DOOM_EMACS_DIR}/bin/doom" sync < /dev/null || warn "doom sync reported an error"

##
#: --- pin package versions (reproducibility layer) ---
#: See README.org: packages.el `:pin' is for *constraints*; this lockfile is
#: for *reproducibility*, and is generated, never hand-edited.
#: @warn Opt-*in*, not opt-out. Thawing implies `doom sync --rebuild', and a
#: rebuild deletes and recreates every build directory. On an NFS home that
#: is genuinely destructive: deleting a file another process still holds open
#: leaves a silly-rename .nfsXXXX entry, `delete-directory' then fails with
#: "Directory not empty", and the package is left with neither its old build
#: nor a new one. That is exactly how this tree lost straight/repos/zmq --
#: after which `doom sync' reported "jupyter is up-to-date" from its build
#: cache while Doom failed to boot on (file-missing ... zmq).
#:
#: The benefit here is also smaller than it looks: measured on beta, 236 of
#: 236 comparable packages already matched the lockfile exactly, because Doom
#: pins its own package set. So the default is off, and you turn it on
#: deliberately, on a host you can afford to have offline.
lockfile="${DOOMDIR}/bootstrap/straight-versions.el"
if [ "${NIGHT_DOOM_LOCKFILE:-n}" != y ] ; then
    log "not thawing the version lockfile (set NIGHT_DOOM_LOCKFILE=y to opt in)"
elif [ -f "${lockfile}" ] && [ -n "${DOOMLOCALDIR:-}" ] ; then
    versions_dir="${DOOMLOCALDIR}/straight/versions"
    mkdir -p "${versions_dir}"
    cp "${lockfile}" "${versions_dir}/default.el"
    log "thawing package versions from the lockfile"
    #: Thaw must come *after* sync: it checks out revisions in
    #: straight/repos/, which do not exist until sync has cloned them.
    "${DOOM_EMACS_DIR}/bin/doom" sync < /dev/null >/dev/null 2>&1 || true
    emacs --batch --eval "(progn (require 'straight nil t) (when (fboundp 'straight-thaw-versions) (straight-thaw-versions)))" \
        >/dev/null 2>&1 || warn "straight-thaw-versions unavailable in batch; run it from inside Emacs"
    #: Byte-code must match the sources we just checked out, or you get stale
    #: .elc referencing files that no longer exist (file-missing ...el.gz).
    night_prepare_rebuild
    log "rebuilding against the pinned sources"
    "${DOOM_EMACS_DIR}/bin/doom" sync --rebuild < /dev/null || warn "rebuild reported an error"
fi

[ -n "${DOOMLOCALDIR:-}" ] && \
    printf '%s' "$(git -C "${DOOM_EMACS_DIR}" rev-parse HEAD)" > "${DOOMLOCALDIR}/.night-doom-ref"

##
#: --- emacs-zmq native module ---
#: jupyter.el pulls in emacs-zmq, which needs a dynamic module. Without it,
#: *every interactive startup* stops on
#:   ZMQ module not found. Build it? (y or n)
#: and a daemon, having no stdin, then fails the build outright.
#:
#: Upstream publishes prebuilt modules, but its auto-download cannot find them
#: for a conda-built Emacs: it matches release assets with `string-prefix-p'
#: against `system-configuration', which is x86_64-conda-linux-gnu, while the
#: published asset is emacs-zmq-x86_64-linux-gnu.tar.gz. Nothing matches, so it
#: silently falls through to compiling -- which then needs autotools and
#: libzmq that a sudo-less host does not have.
#:
#: The binary is a plain x86_64 glibc module and loads fine; only the triplet
#: in the filename differs. So fetch it directly.
zmq_build="$(find "${DOOMLOCALDIR:-${DOOM_EMACS_DIR}/.local}/straight" \
                  -maxdepth 2 -type d -name zmq 2>/dev/null | head -1)"
if [ -n "${zmq_build}" ] && [ ! -f "${zmq_build}/emacs-zmq.so" ] ; then
    log "fetching the prebuilt emacs-zmq module"
    zmq_ver="$(emacs --batch --eval \
        "(progn (add-to-list 'load-path \"${zmq_build}\") (require 'zmq nil t) (princ (or (bound-and-true-p zmq-emacs-version) \"v1.0.2\")))" \
        2>/dev/null || echo v1.0.2)"
    : "${zmq_ver:=v1.0.2}"
    zmq_tmp="$(mktemp -d)"
    zmq_url="https://github.com/nnicandro/emacs-zmq/releases/download/${zmq_ver}/emacs-zmq-x86_64-linux-gnu.tar.gz"
    if curl -fsSL -o "${zmq_tmp}/m.tar.gz" "${zmq_url}" &&
       tar -xzf "${zmq_tmp}/m.tar.gz" -C "${zmq_tmp}" ; then
        zmq_so="$(find "${zmq_tmp}" -name 'emacs-zmq*.so' | head -1)"
        if [ -n "${zmq_so}" ] ; then
            cp "${zmq_so}" "${zmq_build}/emacs-zmq.so"
            log "installed ${zmq_build}/emacs-zmq.so (${zmq_ver})"
        else
            warn "no .so inside the emacs-zmq tarball"
        fi
    else
        warn "could not fetch emacs-zmq ${zmq_ver}; startup will prompt to build it"
    fi
    rm -rf "${zmq_tmp}"
fi

##
#: --- verify ---
#: @warn A batch load of early-init.el is NOT sufficient: it succeeds even when
#: the generated profile init is missing, because only the interactive path
#: loads it. Start a real daemon -- that is the failure users actually hit.
#: @warn "the daemon answers" is NOT proof of a clean start: Doom catches
#: config errors, reports them as a warning, and carries on serving. So scan
#: the startup output for errors and for any interactive prompt too.
log "verifying with a real daemon start"
verify_log="$(mktemp)"
emacs --daemon=night-verify > "${verify_log}" 2>&1
verify_rc=$?

if emacsclient -s night-verify --eval '(+ 1 1)' >/dev/null 2>&1 ; then
    log "daemon answers"
else
    verify_rc=1
fi
emacsclient -s night-verify --eval '(kill-emacs)' >/dev/null 2>&1 || true

#: A verify daemon that outlives this script is not harmless: it keeps the
#: build tree mapped and will block the next rebuild (see
#: night_prepare_rebuild). kill-emacs does not always take -- a daemon that
#: failed to boot cleanly may not be serving the socket at all -- so make
#: sure. The pattern cannot match this script: our own command line is
#: install.sh, not "daemon=night-verify".
sleep 1
for p in $(pgrep -f 'daemon=night-verify' 2>/dev/null || true) ; do
    [ "${p}" = "$$" ] && continue
    warn "verify daemon ${p} outlived kill-emacs; terminating it"
    kill -TERM "${p}" 2>/dev/null || true
done

#: Ignore the unavoidable Gtk/X11 noise; anything else is a real problem.
if grep -qaE "error occurred while booting|\(y or n\)|went wrong|abnormally|Symbol.s (value|function)" "${verify_log}" ; then
    warn "startup produced errors or an interactive prompt:"
    grep -aE "error occurred while booting|\(y or n\)|went wrong|abnormally|Symbol.s (value|function)" \
        "${verify_log}" | head -5 >&2
    verify_rc=1
fi

rm -f "${verify_log}"
[ "${verify_rc}" -eq 0 ] || die "doom did not start cleanly; debug with: emacs --daemon --debug-init"
log "OK: doom starts cleanly"
