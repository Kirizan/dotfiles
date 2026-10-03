#!/bin/bash
# SteamOS remote-access management for the kirby.network fleet.
#
# Enables (and reverses) remote management of a SteamOS machine — Steam Deck or
# Steam Machine — from mimir, the fleet control node.
#
# This is deliberately NOT a chezmoi run_ script. Remote access must never be a
# side effect of `chezmoi apply`:
#   - `run_` scripts execute on every apply, which is too eager.
#   - `run_onchange_` scripts execute only when their content changes, which is
#     too lazy — a SteamOS update reverts /etc without changing this file, so the
#     script would never re-fire when it is actually needed.
# An explicit, idempotent command is the right shape. Run it again after every
# SteamOS update to restore anything the image swap reverted.
#
# Usage:
#   ./steamos-remote-access.sh enable    # sshd + mimir's key + NOPASSWD sudo
#   ./steamos-remote-access.sh harden    # disable SSH password auth (after verifying)
#   ./steamos-remote-access.sh disable   # reverse `enable` entirely
#   ./steamos-remote-access.sh status    # report current state, change nothing
#
# Flags:
#   --force   skip the SteamOS guard (for testing on a non-SteamOS host)
#
# Invoked by `make enable-remote` / `harden-remote` / `disable-remote` /
# `remote-status` from the chezmoi source directory.

set -euo pipefail

# mimir's SSH public key. This is a PUBLIC key and is safe to commit — GitHub
# publishes every user's at github.com/<user>.keys. The matching private key never
# leaves mimir. Nothing secret belongs in this file: the deck password lives in
# OpenBao at kv/infra/steam_machine and is never read, written, or echoed here.
MIMIR_KEY_TYPE="ssh-ed25519"
MIMIR_KEY_BODY="AAAAC3NzaC1lZDI1NTE5AAAAIJNET4Rqz8x4qI59DB3norGW4DO+nrE670Y9D5mz81uw"
MIMIR_KEY_COMMENT="kirizan@mimir (fleet control node)"
MIMIR_KEY="${MIMIR_KEY_TYPE} ${MIMIR_KEY_BODY} ${MIMIR_KEY_COMMENT}"

SUDOERS_DROPIN="/etc/sudoers.d/${USER}-fleet-nopasswd"
SSHD_DROPIN="/etc/ssh/sshd_config.d/99-fleet-no-password.conf"
AUTH_KEYS="${HOME}/.ssh/authorized_keys"

FORCE=0
RELOCK_ON_EXIT=0

# ── output helpers ───────────────────────────────────────────────────────────
pass() { printf '  \033[0;32m✓\033[0m %s\n' "$*"; }
skip() { printf '  \033[0;36m·\033[0m %s\n' "$*"; }
warn() { printf '  \033[0;33m!\033[0m %s\n' "$*"; }
fail() { printf '  \033[0;31m✗\033[0m %s\n' "$*" >&2; }
head_() { printf '\n\033[0;36m%s\033[0m\n' "$*"; }
die() { fail "$*"; exit 1; }

# ── SteamOS detection and read-only root handling ────────────────────────────
is_steamos() {
    [[ -f /etc/steamos-release ]] && return 0
    grep -qE '^ID=steamos' /etc/os-release 2>/dev/null && return 0
    command -v steamos-readonly >/dev/null 2>&1 && return 0
    return 1
}

# SteamOS mounts / read-only. Unlock only when needed, and always restore the
# original state on exit — including on error or interrupt — so an aborted run
# never leaves the root filesystem writable.
relock() {
    if [[ $RELOCK_ON_EXIT -eq 1 ]]; then
        sudo steamos-readonly enable 2>/dev/null \
            && pass "root filesystem re-locked read-only" \
            || fail "COULD NOT RE-LOCK root filesystem — run: sudo steamos-readonly enable"
        RELOCK_ON_EXIT=0
    fi
}
trap relock EXIT INT TERM

unlock_root() {
    command -v steamos-readonly >/dev/null 2>&1 || { skip "no steamos-readonly (not SteamOS)"; return 0; }
    if [[ "$(sudo steamos-readonly status 2>/dev/null)" == "disabled" ]]; then
        skip "root filesystem already writable — leaving as found"
        return 0
    fi
    sudo steamos-readonly disable || die "could not unlock root filesystem"
    RELOCK_ON_EXIT=1
    pass "root filesystem unlocked (will re-lock on exit)"
}

sshd_unit() {
    # Arch (and therefore SteamOS) names it sshd; some distros use ssh.
    for u in sshd ssh; do
        systemctl list-unit-files "${u}.service" --no-legend 2>/dev/null | grep -q . && { echo "$u"; return 0; }
    done
    return 1
}

key_installed() {
    [[ -f "$AUTH_KEYS" ]] && grep -qF "$MIMIR_KEY_BODY" "$AUTH_KEYS"
}

# ── subcommands ──────────────────────────────────────────────────────────────
do_enable() {
    head_ "Enabling remote access from mimir"

    # 1. sshd
    local unit
    if ! unit="$(sshd_unit)"; then
        die "no sshd/ssh unit found — openssh is not installed"
    fi
    if systemctl is-enabled --quiet "$unit" 2>/dev/null; then
        skip "${unit}.service already enabled"
    else
        sudo systemctl enable "$unit" >/dev/null && pass "${unit}.service enabled at boot"
    fi
    if systemctl is-active --quiet "$unit" 2>/dev/null; then
        skip "${unit}.service already running"
    else
        sudo systemctl start "$unit" && pass "${unit}.service started"
    fi
    if ss -tlnH 2>/dev/null | grep -qE ':22\s'; then
        pass "listening on port 22"
    else
        fail "nothing listening on port 22 — check: systemctl status $unit"
    fi

    # 2. mimir's public key. Append only; never truncate, so a key added by hand
    #    or by another machine survives.
    mkdir -p "${HOME}/.ssh"
    chmod 700 "${HOME}/.ssh"
    if key_installed; then
        skip "mimir's key already in authorized_keys"
    else
        [[ -f "$AUTH_KEYS" && -s "$AUTH_KEYS" && -n "$(tail -c1 "$AUTH_KEYS")" ]] && echo >> "$AUTH_KEYS"
        echo "$MIMIR_KEY" >> "$AUTH_KEYS"
        pass "mimir's public key appended to authorized_keys"
    fi
    chmod 600 "$AUTH_KEYS"
    pass "permissions set (700 on ~/.ssh, 600 on authorized_keys)"

    # 3. NOPASSWD sudo, so management over SSH does not block on a prompt.
    #    The OpenBao password path (kv/infra/steam_machine) remains the fallback
    #    for when a SteamOS update reverts /etc/sudoers.d.
    if sudo test -f "$SUDOERS_DROPIN" 2>/dev/null; then
        skip "sudoers drop-in already present"
    else
        local tmp
        tmp="$(mktemp)"
        cat > "$tmp" <<EOF
# Passwordless sudo for fleet remote management from mimir.
# Installed by scripts/steamos-remote-access.sh — do not edit by hand.
# Reverse with: make disable-remote
${USER} ALL=(ALL) NOPASSWD: ALL
EOF
        # Validate BEFORE installing. A malformed sudoers file can lock sudo out
        # entirely, which on a console with no root password is unrecoverable
        # without a live USB.
        if ! visudo -c -f "$tmp" >/dev/null 2>&1; then
            rm -f "$tmp"
            die "generated sudoers file failed validation — not installing"
        fi
        unlock_root
        sudo install -m 0440 -o root -g root "$tmp" "$SUDOERS_DROPIN"
        rm -f "$tmp"
        pass "installed $SUDOERS_DROPIN (validated with visudo -c)"
    fi

    report_next_steps
}

do_harden() {
    head_ "Hardening: disabling SSH password authentication"

    # Refuse unless key auth is actually in place — otherwise this locks the box
    # to Desktop Mode with a keyboard attached.
    key_installed || die "mimir's key is NOT in $AUTH_KEYS — run 'make enable-remote' first"
    pass "mimir's key present in authorized_keys"

    warn "Confirm key auth works from mimir BEFORE continuing:"
    printf '      ssh -o IdentitiesOnly=yes -i ~/.ssh/id_ed25519 %s@%s\n' "$USER" "$(primary_ip)"
    read -r -p "  Has that succeeded from mimir? [y/N] " ans
    [[ "${ans,,}" == "y" ]] || die "aborted — verify key auth first"

    # A drop-in is only honoured if sshd_config includes the directory.
    if ! grep -qE '^\s*Include\s+/etc/ssh/sshd_config\.d/' /etc/ssh/sshd_config 2>/dev/null; then
        warn "/etc/ssh/sshd_config has no Include for sshd_config.d — the drop-in would be inert"
        die "add 'Include /etc/ssh/sshd_config.d/*.conf' to sshd_config first, then re-run"
    fi
    pass "sshd_config includes sshd_config.d"

    unlock_root
    local tmp; tmp="$(mktemp)"
    cat > "$tmp" <<'EOF'
# Fleet hardening: key-only SSH. The local account password still works for
# sudo at the console; it is simply no longer a remote login path.
# Installed by scripts/steamos-remote-access.sh — reverse with: make disable-remote
PasswordAuthentication no
KbdInteractiveAuthentication no
EOF
    sudo install -m 0644 -o root -g root "$tmp" "$SSHD_DROPIN"
    rm -f "$tmp"
    pass "installed $SSHD_DROPIN"

    # Validate the whole effective config before restarting. Never restart sshd
    # on a config that does not parse.
    if ! sudo sshd -t; then
        sudo rm -f "$SSHD_DROPIN"
        die "sshd config failed validation — drop-in removed, sshd untouched"
    fi
    pass "sshd -t passed"

    sudo systemctl reload "$(sshd_unit)" && pass "sshd reloaded — password auth disabled"
    printf '\n  The account password now unlocks local sudo only, not remote login.\n'
}

do_disable() {
    head_ "Disabling remote access"

    if sudo test -f "$SUDOERS_DROPIN" 2>/dev/null; then
        unlock_root
        sudo rm -f "$SUDOERS_DROPIN" && pass "removed $SUDOERS_DROPIN"
    else
        skip "no sudoers drop-in to remove"
    fi

    if sudo test -f "$SSHD_DROPIN" 2>/dev/null; then
        unlock_root
        sudo rm -f "$SSHD_DROPIN" && pass "removed $SSHD_DROPIN (password auth restored)"
    else
        skip "no sshd drop-in to remove"
    fi

    if key_installed; then
        # Remove only the line carrying mimir's key body; leave every other key.
        local tmp; tmp="$(mktemp)"
        grep -vF "$MIMIR_KEY_BODY" "$AUTH_KEYS" > "$tmp" || true
        mv "$tmp" "$AUTH_KEYS"
        chmod 600 "$AUTH_KEYS"
        pass "removed mimir's key from authorized_keys"
    else
        skip "mimir's key not present"
    fi

    local unit
    if unit="$(sshd_unit)" && systemctl is-enabled --quiet "$unit" 2>/dev/null; then
        sudo systemctl disable --now "$unit" >/dev/null && pass "${unit}.service stopped and disabled"
    else
        skip "sshd already disabled"
    fi
}

primary_ip() {
    ip -4 route get 1.1.1.1 2>/dev/null | awk '{for(i=1;i<=NF;i++) if($i=="src") {print $(i+1); exit}}'
}

do_status() {
    head_ "Remote access status"
    is_steamos && pass "SteamOS detected" || warn "not SteamOS"
    local unit
    unit="$(sshd_unit || echo '(none)')"
    printf '  sshd unit        : %s\n' "$unit"
    # `systemctl is-enabled/is-active` print their verdict AND exit non-zero when
    # not enabled/active, so a `|| echo` fallback would duplicate the line. Only
    # fall back when the command produced nothing at all.
    printf '  enabled at boot  : %s\n' "$(systemctl is-enabled "$unit" 2>/dev/null || true)"
    printf '  running          : %s\n' "$(systemctl is-active "$unit" 2>/dev/null || true)"
    printf '  listening on :22 : %s\n' "$(ss -tlnH 2>/dev/null | grep -qE ':22\s' && echo yes || echo no)"
    printf "  mimir's key      : %s\n" "$(key_installed && echo present || echo absent)"
    printf '  NOPASSWD sudo    : %s\n' "$(sudo test -f "$SUDOERS_DROPIN" 2>/dev/null && echo yes || echo no)"
    printf '  password auth    : %s\n' "$(sudo test -f "$SSHD_DROPIN" 2>/dev/null && echo disabled || echo enabled)"
    printf '  hostname / IP    : %s / %s\n' "$(hostname)" "$(primary_ip)"
    if command -v steamos-readonly >/dev/null 2>&1; then
        printf '  root filesystem  : %s\n' "$(sudo steamos-readonly status 2>/dev/null || echo unknown)"
    fi
}

report_next_steps() {
    local ip user
    ip="$(primary_ip)"
    user="$USER"
    cat <<EOF

Remote access is enabled. Verify from mimir:

    ssh -o IdentitiesOnly=yes -i ~/.ssh/id_ed25519 ${user}@${ip}

Then add this to ~/.ssh/config on mimir:

    Host $(hostname)
        HostName ${ip}
        User ${user}
        IdentityFile ~/.ssh/id_ed25519

Notes:
  * Re-run 'make enable-remote' after every SteamOS update — an atomic image
    swap discards /usr and can revert /etc, including the sudoers drop-in.
  * Once the SSH command above is confirmed working, run 'make harden-remote'
    to turn off SSH password authentication.
  * Ask for a DHCP reservation on the UDM-Pro so ${ip} does not move.
EOF
}

# ── entry point ──────────────────────────────────────────────────────────────
CMD=""
for arg in "$@"; do
    case "$arg" in
        --force) FORCE=1 ;;
        enable|harden|disable|status) CMD="$arg" ;;
        -h|--help) sed -n '2,28p' "$0" | sed 's/^# \?//'; exit 0 ;;
        *) die "unknown argument: $arg" ;;
    esac
done
[[ -n "$CMD" ]] || die "no subcommand given (enable|harden|disable|status); try --help"

if [[ "$CMD" != "status" && $FORCE -eq 0 ]] && ! is_steamos; then
    die "this does not look like SteamOS. Re-run with --force if that is wrong."
fi

case "$CMD" in
    enable)  do_enable ;;
    harden)  do_harden ;;
    disable) do_disable ;;
    status)  do_status ;;
esac
