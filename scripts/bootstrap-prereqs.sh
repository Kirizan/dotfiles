#!/usr/bin/env bash
# Install the minimum toolchain needed to use this repo's Makefile.
#
# Chicken-and-egg problem: `make install-prereqs` cannot install `make`. This
# script is the one thing you run by hand on a fresh *nix machine, before any
# make target works.
#
# Usage (run from the chezmoi source directory):
#   ./scripts/bootstrap-prereqs.sh            # install what is missing
#   ./scripts/bootstrap-prereqs.sh --check    # report only, change nothing
#   ./scripts/bootstrap-prereqs.sh --yes      # skip the SteamOS confirmation
#
# Idempotent: every already-satisfied requirement reports as a skip. Safe to
# re-run, and on SteamOS you will need to (see the warning it prints).
#
# Deliberately kept POSIX-ish and bash-3.2 compatible so it works on macOS's
# system bash as well as Linux. No arrays-of-arrays, no ${var,,}, no mapfile.

set -euo pipefail

# What the Makefile actually needs. Verified against the Makefile's recipes and
# `command -v` checks, not guessed:
#   make     - the Makefile itself
#   git      - chezmoi clones/updates the source repo and .chezmoiexternal.toml
#   curl     - the chezmoi installer, and the OpenBao SSH CA fetch
#   chezmoi  - what nearly every target drives
# Clipboard helpers (pbcopy/wl-copy/xclip) are optional and only used by one
# target, so they are reported but never installed.
REQUIRED_PKGS="make git curl"

CHECK_ONLY=0
ASSUME_YES=0
RELOCK_ON_EXIT=0
MISSING=""
FAILED=""

pass() { printf '  \033[0;32m✓\033[0m %s\n' "$*"; }
skip() { printf '  \033[0;36m·\033[0m %s\n' "$*"; }
warn() { printf '  \033[0;33m!\033[0m %s\n' "$*"; }
fail() { printf '  \033[0;31m✗\033[0m %s\n' "$*" >&2; }
head_() { printf '\n\033[0;36m%s\033[0m\n' "$*"; }
die() { fail "$*"; exit 1; }

have() { command -v "$1" >/dev/null 2>&1; }

# ── platform detection ───────────────────────────────────────────────────────
PLATFORM="unknown"
PKG_MGR=""
OS_ID=""
OS_NAME=""

detect_platform() {
    case "$(uname -s)" in
        Darwin)
            PLATFORM="macos"; OS_NAME="macOS"
            PKG_MGR="$(have brew && echo brew || echo xcode-clt)"
            return 0
            ;;
        Linux) : ;;
        FreeBSD) PLATFORM="freebsd"; OS_NAME="FreeBSD"; PKG_MGR="pkg"; return 0 ;;
        *) die "unsupported kernel: $(uname -s). Only *nix is in scope." ;;
    esac

    if [ -r /etc/os-release ]; then
        # shellcheck disable=SC1091
        OS_ID="$(. /etc/os-release && printf '%s' "${ID:-}")"
        OS_NAME="$(. /etc/os-release && printf '%s' "${PRETTY_NAME:-${NAME:-Linux}}")"
    fi

    # SteamOS before the generic Arch branch: it HAS pacman, but using it has
    # consequences no other Arch machine has. Mirrors the isArchBased override
    # in .chezmoi.toml.tmpl for the same reason.
    if [ -f /etc/steamos-release ] || [ "$OS_ID" = "steamos" ] \
       || have steamos-readonly || have steamos-update; then
        PLATFORM="steamos"; PKG_MGR="pacman"
        [ -n "$OS_NAME" ] || OS_NAME="SteamOS"
        return 0
    fi

    PLATFORM="linux"
    for mgr in pacman apt-get dnf zypper apk xbps-install emerge; do
        if have "$mgr"; then PKG_MGR="$mgr"; return 0; fi
    done
    return 0
}

# ── package installation per manager ─────────────────────────────────────────
install_pkgs() {
    pkgs="$1"
    case "$PKG_MGR" in
        brew)         brew install $pkgs ;;
        xcode-clt)    die "install Xcode Command Line Tools first: xcode-select --install" ;;
        pacman)       sudo pacman -S --needed --noconfirm $pkgs ;;
        apt-get)      sudo apt-get update -qq && sudo apt-get install -y $pkgs ;;
        dnf)          sudo dnf install -y $pkgs ;;
        zypper)       sudo zypper --non-interactive install $pkgs ;;
        apk)          sudo apk add $pkgs ;;
        xbps-install) sudo xbps-install -Sy $pkgs ;;
        emerge)       sudo emerge --noreplace $pkgs ;;
        pkg)          sudo pkg install -y $pkgs ;;
        *)            return 1 ;;
    esac
}

# ── SteamOS read-only root ───────────────────────────────────────────────────
relock() {
    if [ "$RELOCK_ON_EXIT" -eq 1 ]; then
        sudo steamos-readonly enable 2>/dev/null \
            && pass "root filesystem re-locked read-only" \
            || fail "COULD NOT RE-LOCK — run: sudo steamos-readonly enable"
        RELOCK_ON_EXIT=0
    fi
}
trap relock EXIT INT TERM

steamos_prepare() {
    if [ "$(sudo steamos-readonly status 2>/dev/null)" = "disabled" ]; then
        skip "root filesystem already writable — leaving as found"
    else
        sudo steamos-readonly disable || die "could not unlock root filesystem"
        RELOCK_ON_EXIT=1
        pass "root filesystem unlocked (will re-lock on exit)"
    fi

    # SteamOS ships an unpopulated keyring, so any pacman install fails its
    # signature check until this runs. Idempotent.
    if ! sudo pacman-key --list-keys >/dev/null 2>&1; then
        sudo pacman-key --init
        sudo pacman-key --populate archlinux holo 2>/dev/null \
            || sudo pacman-key --populate archlinux
        pass "pacman keyring initialised"
    else
        skip "pacman keyring already populated"
    fi

    # Only sync the db if it is actually absent. A gratuitous -Sy on SteamOS is
    # the partial-upgrade footgun: it can pull packages built against a newer
    # glibc than the shipped image.
    if [ -z "$(find /var/lib/pacman/sync -name '*.db' 2>/dev/null | head -1)" ]; then
        warn "pacman database is empty; syncing (-Sy) to proceed"
        sudo pacman -Sy --noconfirm
    else
        skip "pacman database present — not running -Sy"
    fi
}

steamos_warning() {
    cat <<'EOF'

  ┌──────────────────────────────────────────────────────────────────────┐
  │  SteamOS: packages installed here do NOT survive an OS update.       │
  └──────────────────────────────────────────────────────────────────────┘

  SteamOS updates are atomic A/B image swaps that discard everything under
  /usr. Anything pacman installs is gone after the next update, and you will
  need to re-run this script. That is expected, and it is idempotent.

  You may not need make at all. Every make target in this repo is a thin
  wrapper around a script that runs fine on its own:

      ./scripts/steamos-remote-access.sh enable     # = make enable-remote
      ./scripts/steamos-remote-access.sh status     # = make remote-status
      ./scripts/steamos-remote-access.sh harden     # = make harden-remote
      ./scripts/steamos-remote-access.sh disable    # = make disable-remote

  If you want a durable build environment rather than packages in /usr, use
  a container instead -- it lives in $HOME and survives updates:

      distrobox create --name dev --image archlinux:latest
      distrobox enter dev

EOF
}

# ── argument parsing ─────────────────────────────────────────────────────────
for arg in "$@"; do
    case "$arg" in
        --check|-n) CHECK_ONLY=1 ;;
        --yes|-y)   ASSUME_YES=1 ;;
        -h|--help)  sed -n '2,20p' "$0" | sed 's/^# \?//'; exit 0 ;;
        *)          die "unknown argument: $arg" ;;
    esac
done

# ── main ─────────────────────────────────────────────────────────────────────
detect_platform

head_ "Platform"
printf '  reported by OS  : %s\n' "${OS_NAME:-unknown}"
# Print the classification too, not just the OS's own name. They can disagree
# (SteamOS is detected by its own markers, and that is what selects the code
# path), and when something behaves unexpectedly this is the line that says why.
printf '  treated as      : %s\n' "$PLATFORM"
printf '  package manager : %s\n' "${PKG_MGR:-none found}"

head_ "Requirements"
for pkg in $REQUIRED_PKGS; do
    if have "$pkg"; then
        skip "$pkg present ($(command -v "$pkg"))"
    else
        fail "$pkg MISSING"
        MISSING="$MISSING $pkg"
    fi
done

# chezmoi is handled separately: its official installer drops a static binary
# into ~/.local/bin, which works identically on every platform and (crucially
# on SteamOS) lives in $HOME where an OS update cannot reach it.
CHEZMOI_MISSING=0
if have chezmoi; then
    skip "chezmoi present ($(command -v chezmoi))"
elif [ -x "$HOME/.local/bin/chezmoi" ]; then
    warn "chezmoi at ~/.local/bin/chezmoi but not on PATH"
else
    fail "chezmoi MISSING"
    CHEZMOI_MISSING=1
fi

# Optional, reported only.
CLIP=""
for c in pbcopy wl-copy xclip; do have "$c" && { CLIP="$c"; break; }; done
[ -n "$CLIP" ] && skip "clipboard helper present ($CLIP)" \
               || warn "no clipboard helper (pbcopy/wl-copy/xclip) — optional"

MISSING="$(printf '%s' "$MISSING" | sed 's/^ *//')"

if [ -z "$MISSING" ] && [ "$CHEZMOI_MISSING" -eq 0 ]; then
    head_ "Nothing to do"
    pass "all requirements satisfied — 'make help' will work"
    exit 0
fi

if [ "$CHECK_ONLY" -eq 1 ]; then
    head_ "Check only — nothing installed"
    [ -n "$MISSING" ] && printf '  would install: %s\n' "$MISSING"
    [ "$CHEZMOI_MISSING" -eq 1 ] && printf '  would install: chezmoi (via get.chezmoi.io)\n'
    exit 0
fi

if [ "$PLATFORM" = "steamos" ] && [ -n "$MISSING" ]; then
    steamos_warning
    if [ "$ASSUME_YES" -eq 0 ]; then
        printf '  Install %s via pacman anyway? [y/N] ' "$MISSING"
        read -r reply
        case "$reply" in
            y|Y|yes|YES) : ;;
            *) head_ "Aborted"; printf '  Nothing installed. Run the scripts directly, as shown above.\n'; exit 0 ;;
        esac
    fi
fi

head_ "Installing"

if [ -n "$MISSING" ]; then
    [ -n "$PKG_MGR" ] || die "no supported package manager found; install these by hand: $MISSING"
    [ "$PLATFORM" = "steamos" ] && steamos_prepare
    if install_pkgs "$MISSING"; then
        for pkg in $MISSING; do
            have "$pkg" && pass "$pkg installed" || { fail "$pkg still missing after install"; FAILED="$FAILED $pkg"; }
        done
    else
        die "package install failed via $PKG_MGR; install by hand: $MISSING"
    fi
fi

if [ "$CHEZMOI_MISSING" -eq 1 ]; then
    have curl || die "curl is required to install chezmoi"
    mkdir -p "$HOME/.local/bin"
    sh -c "$(curl -fsLS get.chezmoi.io)" -- -b "$HOME/.local/bin" >/dev/null
    if [ -x "$HOME/.local/bin/chezmoi" ]; then
        pass "chezmoi installed to ~/.local/bin (survives SteamOS updates)"
    else
        fail "chezmoi install failed"; FAILED="$FAILED chezmoi"
    fi
fi

head_ "Result"
case ":$PATH:" in
    *":$HOME/.local/bin:"*) : ;;
    *) warn "\$HOME/.local/bin is not on PATH — add it, or use the full path:"
       printf '      export PATH="$HOME/.local/bin:$PATH"\n' ;;
esac

if [ -n "$(printf '%s' "$FAILED" | sed 's/ //g')" ]; then
    fail "incomplete:$FAILED"
    exit 1
fi

pass "bootstrap complete"
printf '\n  Next: make help\n'
if [ "$PLATFORM" = "steamos" ]; then
    printf '  Re-run this script after every SteamOS update.\n'
fi
