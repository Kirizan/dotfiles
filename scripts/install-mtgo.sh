#!/bin/bash
# Install Magic The Gathering Online (MTGO) via Wine on Arch Linux
# Usage: install-mtgo.sh [--force]
#
# Based on: https://github.com/nathvnt/mtgo-linux-setup-script
# Adapted for CachyOS/Arch with:
#   - Dedicated Wine prefix (not ~/.wine)
#   - System winetricks (no download)
#   - Desktop entry creation
#   - Steam non-Steam game integration
#
# Prerequisites: wine, winetricks, cabextract, lib32-gnutls, samba, python-vdf

set -euo pipefail

# ── Configuration ─────────────────────────────────────────────────
WINEPREFIX="$HOME/.local/share/wineprefixes/mtgo"
MTGO_DIR="$WINEPREFIX/drive_c/mtgo"
MTGO_EXEC="$MTGO_DIR/mtgo.exe"
MTGO_URL="https://mtgo.patch.daybreakgames.com/patch/mtg/live/client/setup.exe?v=15"
LAUNCHER="$HOME/.local/bin/mtgo"
DESKTOP_FILE="$HOME/.local/share/applications/mtgo.desktop"
ICON_DIR="$HOME/.local/share/icons"
ICON_FILE="$ICON_DIR/mtgo.png"
ICON_URL="https://raw.githubusercontent.com/nathvnt/mtgo-linux-setup-script/main/mtgo/mtgo-icon.png"
STEAM_USERDATA="$HOME/.local/share/Steam/userdata"
FORCE=false

if [[ "${1:-}" == "--force" ]]; then
    FORCE=true
fi

# ── Colors ────────────────────────────────────────────────────────
BOLD='\033[1m'
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m'

TOTAL_STEPS=8
current_step=0

step() {
    current_step=$((current_step + 1))
    echo -e "\n${BOLD}${CYAN}[$current_step/$TOTAL_STEPS]${NC} ${BOLD}$1${NC}"
}

info()    { echo -e "  ${GREEN}✓${NC} $1"; }
warn()    { echo -e "  ${YELLOW}!${NC} $1"; }
error()   { echo -e "  ${RED}✗${NC} $1"; exit 1; }

# ── Preflight checks ─────────────────────────────────────────────
step "Checking prerequisites"

MISSING=()
for pkg in wine winetricks cabextract lib32-gnutls samba; do
    if ! pacman -Q "$pkg" &>/dev/null; then
        MISSING+=("$pkg")
    fi
done

if [[ ${#MISSING[@]} -gt 0 ]]; then
    error "Missing packages: ${MISSING[*]}\n  Install with: sudo pacman -S ${MISSING[*]}"
fi
info "All required packages installed"

if [[ -f "$MTGO_EXEC" && "$FORCE" != true ]]; then
    warn "MTGO already installed at $MTGO_EXEC"
    warn "Use --force to reinstall"
    exit 0
fi

# ── Step 2: Create Wine prefix ────────────────────────────────────
step "Creating dedicated Wine prefix"

if [[ -d "$WINEPREFIX" && "$FORCE" == true ]]; then
    warn "Removing existing prefix (--force)"
    rm -rf "$WINEPREFIX"
fi

mkdir -p "$WINEPREFIX"
WINEDEBUG=-all WINEPREFIX="$WINEPREFIX" wineboot --init
info "Wine prefix created at $WINEPREFIX"

# ── Step 3: Install winetricks dependencies ───────────────────────
step "Installing winetricks dependencies (this takes a while)"

export WINEPREFIX
export WINEDEBUG=-all

info "Installing fonts..."
winetricks -q arial times trebuchet verdana
winetricks -q pptfonts corefonts
winetricks -q calibri tahoma

info "Installing .NET 4.8 (this is the slow part)..."
winetricks -q dotnet48

info "Configuring Windows version and renderer..."
winetricks -q win7 sound=pulse
winetricks -q renderer=gdi

info "All winetricks dependencies installed"

# ── Step 4: Download MTGO ─────────────────────────────────────────
step "Downloading MTGO"

mkdir -p "$MTGO_DIR"
curl -fSL -o "$MTGO_EXEC" "$MTGO_URL"
info "MTGO downloaded to $MTGO_EXEC"

# ── Step 5: Apply sound workarounds ───────────────────────────────
step "Applying sound workarounds"

winetricks -q gdiplus=builtin winegstreamer=builtin wmp=builtin
winetricks -q sound=disabled
WINEPREFIX="$WINEPREFIX" wineboot -k
sleep 3
info "Sound workarounds applied (sound disabled to prevent crash)"

# Clean winetricks cache
rm -rf ~/.cache/winetricks
info "Winetricks cache cleaned"

# ── Step 6: Create launcher script ────────────────────────────────
step "Creating launcher script"

mkdir -p "$(dirname "$LAUNCHER")"
cat > "$LAUNCHER" << 'LAUNCHER_EOF'
#!/bin/bash
# MTGO Launcher — runs Magic The Gathering Online via Wine

WINEPREFIX="$HOME/.local/share/wineprefixes/mtgo"
MTGO_DIR="$WINEPREFIX/drive_c/mtgo"
export WINEPREFIX
export WINEDEBUG=-all

if ! [ -f "$MTGO_DIR/mtgo.exe" ]; then
    echo "MTGO not installed. Run install-mtgo.sh first."
    exit 1
fi

if pgrep -f 'MTGO.exe' >/dev/null 2>&1; then
    echo "MTGO is already running."
    exit 1
fi

cd "$MTGO_DIR"
exec wine mtgo.exe "$@"
LAUNCHER_EOF
chmod +x "$LAUNCHER"
info "Launcher created at $LAUNCHER"

# ── Step 7: Create desktop entry and icon ─────────────────────────
step "Creating desktop entry"

mkdir -p "$ICON_DIR"
if curl -fsSL -o "$ICON_FILE" "$ICON_URL"; then
    info "Icon downloaded to $ICON_FILE"
else
    warn "Could not download icon — desktop entry will work without it"
    ICON_FILE="applications-games"
fi

mkdir -p "$(dirname "$DESKTOP_FILE")"
cat > "$DESKTOP_FILE" << EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=MTGO
Comment=Magic The Gathering Online
Exec=$LAUNCHER
Icon=$ICON_FILE
Terminal=false
Categories=Game;Wine;
StartupWMClass=mtgo.exe
EOF
info "Desktop entry created at $DESKTOP_FILE"

# Update desktop database if available
if command -v update-desktop-database &>/dev/null; then
    update-desktop-database "$HOME/.local/share/applications" 2>/dev/null || true
fi

# ── Step 8: Add to Steam as non-Steam game ────────────────────────
step "Adding MTGO to Steam"

add_to_steam() {
    python3 << 'PYEOF'
import os
import struct
import glob
import vdf

steam_userdata = os.path.expanduser("~/.local/share/Steam/userdata")
launcher = os.path.expanduser("~/.local/bin/mtgo")
icon = os.path.expanduser("~/.local/share/icons/mtgo.png")

# Find all user shortcuts.vdf files
patterns = glob.glob(os.path.join(steam_userdata, "*/config/shortcuts.vdf"))
if not patterns:
    print("  No Steam userdata found — skipping Steam integration")
    print("  You can add MTGO manually: Steam → Add a Game → Add a Non-Steam Game")
    exit(0)

for vdf_path in patterns:
    # Load existing shortcuts or start fresh
    try:
        with open(vdf_path, "rb") as f:
            data = vdf.binary_load(f)
    except Exception:
        data = {"shortcuts": {}}

    shortcuts = data.get("shortcuts", {})

    # Check if MTGO already exists
    for key, entry in shortcuts.items():
        if entry.get("AppName", entry.get("appname", "")) == "MTGO":
            print(f"  MTGO already in Steam shortcuts at {vdf_path}")
            break
    else:
        # Find next available index
        if shortcuts:
            next_idx = str(max(int(k) for k in shortcuts.keys()) + 1)
        else:
            next_idx = "0"

        shortcuts[next_idx] = {
            "AppName": "MTGO",
            "Exe": f'"{launcher}"',
            "StartDir": f'"{os.path.dirname(launcher)}"',
            "icon": icon,
            "ShortcutPath": "",
            "LaunchOptions": "",
            "IsHidden": 0,
            "AllowDesktopConfig": 1,
            "AllowOverlay": 1,
            "OpenVR": 0,
            "Devkit": 0,
            "DevkitGameID": "",
            "DevkitOverrideAppID": 0,
            "LastPlayTime": 0,
            "tags": {},
        }

        data["shortcuts"] = shortcuts
        with open(vdf_path, "wb") as f:
            vdf.binary_dump(data, f)
        print(f"  MTGO added to Steam shortcuts at {vdf_path}")

PYEOF
}

if command -v steam &>/dev/null; then
    if add_to_steam; then
        info "Steam integration complete"
        warn "Restart Steam for the shortcut to appear"
    else
        warn "Steam integration failed — you can add manually via Steam → Add a Game"
    fi
else
    warn "Steam not installed — skipping Steam integration"
fi

# ── Done ──────────────────────────────────────────────────────────
echo -e "\n${BOLD}${GREEN}MTGO installation complete!${NC}"
echo -e "  Launch with: ${CYAN}mtgo${NC} (or from application menu / Steam)"
echo -e "  Wine prefix: ${CYAN}$WINEPREFIX${NC}"
echo -e "  Reinstall:   ${CYAN}install-mtgo.sh --force${NC}"
