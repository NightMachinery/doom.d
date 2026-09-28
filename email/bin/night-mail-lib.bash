# night-mail-lib.bash: sourced by night-mail-sync and the notmuch hooks.
# Bash 3.2 compatible (macOS /bin/bash). See docs/email.md.

# launchd and Emacs both hand us a thin PATH; notmuch and mbsync live here.
PATH="/opt/homebrew/bin:${PATH:-/usr/bin:/bin:/usr/sbin:/sbin}"
export PATH

night_mail_state_dir="$HOME/.local/state/night-mail"
night_mail_lock="$night_mail_state_dir/sync.lock"
# pre-new records the database revision here, so post-new can ask which
# messages this run changed (`lastmod:REV..').
night_mail_rev_file="$night_mail_state_dir/lastmod-before-sync"

nm_log() {
    printf '%s %s: %s\n' "$(command date '+%F %T')" "${0##*/}" "$*" >&2
}

nm_mail_root() {
    local root
    root="$(notmuch config get database.mail_root)" || return 1
    printf '%s\n' "${root%/}"
}

# Every top-level folder of the mail root with an INBOX is an account.
nm_accounts() {
    local root="$1" d
    for d in "$root"/*/INBOX ; do
        [ -d "$d" ] && command basename "$(command dirname "$d")"
    done
}

# nm_lock WAIT: take the sync lock on fd 9, waiting up to WAIT seconds
# (0 = do not wait). The lock is released when the process exits.
nm_lock() {
    local wait="$1"
    command mkdir -p "$night_mail_state_dir" || return 1
    exec 9>"$night_mail_lock" || return 1
    if [ "$wait" -gt 0 ] ; then
        flock -x -w "$wait" 9
    else
        flock -x -n 9
    fi
}
