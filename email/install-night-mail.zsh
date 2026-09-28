#!/usr/bin/env zsh
# Install (or remove) night-mail: the rendered mail tool configs and the
# LaunchAgent that syncs mail every five minutes.
#
#   ./install-night-mail.zsh              # install / re-install (idempotent)
#   ./install-night-mail.zsh --uninstall  # remove the LaunchAgent only
#   ./install-night-mail.zsh --check      # report state, change nothing
#
# Prerequisites, all described in ../docs/email.md:
#   - brew install notmuch isync msmtp flock
#   - the private identity file (see identity.env.example)
#   - the account password in the login Keychain
#
# No sudo anywhere: this is a per-user LaunchAgent. Running it with sudo would
# resolve ~ to /var/root and install the job for the wrong user.

emulate -L zsh
set -o pipefail

LABEL='com.user.night-mail-sync'
SELF="${0:A}"          # inside a function, $0 is the function name
EMAIL_DIR="${SELF:h}"
DOOM_DIR="${EMAIL_DIR:h}"
RENDER="${EMAIL_DIR}/bin/night-mail-render"
SYNC="${EMAIL_DIR}/bin/night-mail-sync"
IDENTITY="${NIGHT_MAIL_IDENTITY:-${HOME}/.config/night-mail/identity.env}"
PLIST_DST="${HOME}/Library/LaunchAgents/${LABEL}.plist"
LOG_FILE="${HOME}/logs/night-mail-sync.log"
DOMAIN="gui/$(id -u)"
BREW_BIN='/opt/homebrew/bin'

# --- output helpers -----------------------------------------------------------
_info() { print -r -- "  $*" }
_step() { print -r -- $'\n''=> '"$*" }
_warn() { print -ru2 -- "!! $*" }
_die()  { print -ru2 -- "!! $*"$'\n''!! aborted; nothing further changed.'; exit 1 }

# Print KEY's value from the identity file, read in a subshell so that none of
# it leaks into this one.
_identity_get() {
    local key="$1"
    command bash -c '. "$1" && printf "%s\n" "${!2}"' _ "$IDENTITY" "$key"
}

# The notmuch version that packages.el pins the Emacs client to.
_pinned_notmuch_version() {
    command sed -n -E 's/^\(defconst night\/notmuch-pinned-version "([^"]+)".*/\1/p' \
        "${DOOM_DIR}/autoload/night-email.el" 2>/dev/null
}

_version_check() {
    local cli pinned
    cli="$("${BREW_BIN}/notmuch" --version 2>/dev/null)"
    cli="${cli#notmuch }"
    pinned="$(_pinned_notmuch_version)"
    if [[ -z "$pinned" ]] ; then
        _warn 'could not read night/notmuch-pinned-version from autoload/night-email.el.'
    elif [[ "$cli" == "$pinned" ]] ; then
        _info "notmuch CLI ${cli} matches the Emacs client's pin."
    else
        _warn "notmuch CLI is ${cli}, but Emacs is pinned to ${pinned}."
        _warn 're-pin notmuch in packages.el to the new tag and run doom sync; see docs/email.md.'
    fi
}

_preflight() {
    (( EUID == 0 )) && _die "run this as your normal user, not with sudo."

    local b
    for b in notmuch mbsync msmtp flock ; do
        [[ -x "${BREW_BIN}/${b}" ]] || _die "${BREW_BIN}/${b} is missing; brew install notmuch isync msmtp flock"
    done
    [[ -x "$RENDER" && -x "$SYNC" ]] || _die "the scripts in ${EMAIL_DIR}/bin are missing or not executable."

    [[ -r "$IDENTITY" ]] || _die "no identity file at ${IDENTITY}; copy ${EMAIL_DIR}/identity.env.example"
    local host user
    host="$(_identity_get NIGHT_MAIL_HOST)"
    user="$(_identity_get NIGHT_MAIL_USER)"
    [[ -n "$host" && -n "$user" ]] || _die "NIGHT_MAIL_HOST or NIGHT_MAIL_USER is empty in ${IDENTITY}"

    # Looks the item up without -w, so the password itself is never printed.
    /usr/bin/security find-internet-password -s "$host" -a "$user" >/dev/null 2>&1 ||
        _die "no Keychain password for ${user}@${host}; see 'Password' in docs/email.md."
}

_check() {
    _step "state of ${LABEL}"

    if [[ -e "$PLIST_DST" ]] ; then
        _info "plist installed: ${PLIST_DST}"
    else
        _info 'plist not installed.'
    fi

    if launchctl print "${DOMAIN}/${LABEL}" >/dev/null 2>&1 ; then
        _info 'job is bootstrapped. state and last exit status:'
        launchctl print "${DOMAIN}/${LABEL}" |
            command grep -E '^\s+(state|last exit code|program|run interval) ' |
            command sed 's/^/    /'
    else
        _info 'job is NOT bootstrapped.'
    fi

    if [[ -s "$LOG_FILE" ]] ; then
        _info "log tail (${LOG_FILE}):"
        command tail -n 5 "$LOG_FILE" | command sed 's/^/    /'
    else
        _info "log is empty or absent: ${LOG_FILE}"
    fi

    _step 'versions'
    _version_check
}

_install() {
    _preflight

    _step 'rendering configs'
    "$RENDER" --plist "$PLIST_DST" | command sed 's/^/  /' ||
        _die 'night-mail-render failed; see above.'
    command plutil -lint "$PLIST_DST" >/dev/null || _die 'the rendered plist does not parse.'

    local root account
    root="$(_identity_get NIGHT_MAIL_ROOT)"
    account="$(_identity_get NIGHT_MAIL_ACCOUNT)"
    command mkdir -p "${root:-${HOME}/Mail}/${account}" || _die 'could not create the Maildir root.'

    _step 'bootstrapping the job'
    # bootout first so a re-run picks up an edited plist; it fails harmlessly
    # when the job was not loaded, which is the normal first-install case.
    launchctl bootout "${DOMAIN}/${LABEL}" 2>/dev/null
    launchctl bootstrap "$DOMAIN" "$PLIST_DST" ||
        _die "launchctl bootstrap failed. try: launchctl print ${DOMAIN}/${LABEL}"
    _info 'bootstrapped; RunAtLoad means the first sync has already started.'

    _step 'versions'
    _version_check

    _step 'done'
    _info "inspect with: ${SELF:t} --check"
}

_uninstall() {
    _step 'removing the job'
    if launchctl bootout "${DOMAIN}/${LABEL}" 2>/dev/null ; then
        _info 'booted out.'
    else
        _info 'was not loaded.'
    fi

    if [[ -e "$PLIST_DST" ]] ; then
        command rm -f "$PLIST_DST" || _die "could not remove ${PLIST_DST}"
        _info "removed ${PLIST_DST}"
    else
        _info 'no plist to remove.'
    fi

    _step 'done'
    _info 'mail, tags and the rendered configs are untouched; see docs/email.md to remove them.'
}

case "${1:-}" in
    --uninstall) _uninstall ;;
    --check)     _check ;;
    ''|--install) _install ;;
    *)
        _die "unknown argument: ${1}. use --install, --uninstall or --check."
        ;;
esac
