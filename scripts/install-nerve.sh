#!/bin/bash
# Install Nerve — full-featured web UI for OpenClaw/NemoClaw
# Usage: install-nerve.sh [--force]
#
# Prerequisites: Node.js 22+, running NemoClaw sandbox with port forward on 18789
# Deploys: Nerve on http://127.0.0.1:3080/ as a systemd user service
#
# Data persistence:
#   ~/nerve/server/data/kanban/tasks.json  — Kanban task data
#   ~/.nerve/device-identity.json          — Device keypair
#   ~/nerve/.env                           — Configuration

set -euo pipefail

BOLD='\033[1m'
DIM='\033[2m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
YELLOW='\033[0;33m'
NC='\033[0m'

pass()  { echo -e "  ${GREEN}✓${NC} $1"; }
info()  { echo -e "  ${DIM}$1${NC}"; }
warn()  { echo -e "  ${YELLOW}⚠${NC} $1"; }

FORCE=false
[[ "${1:-}" == "--force" ]] && FORCE=true

NERVE_DIR="$HOME/nerve"
REPO="https://github.com/daggerhashimoto/openclaw-nerve.git"

echo ""
echo -e "${BOLD}${CYAN}  Nerve Web UI Installation${NC}"
echo ""

# Check Node.js
NODE_VER=$(node --version 2>/dev/null || echo "none")
if [[ "$NODE_VER" != v2[2-9].* ]] && [[ "$NODE_VER" != v[3-9]* ]]; then
    echo "Error: Node.js 22+ required (found: $NODE_VER)"
    exit 1
fi
pass "Node.js $NODE_VER"

# Clone or update
if [ -d "$NERVE_DIR/.git" ] && [[ "$FORCE" != true ]]; then
    info "Updating existing installation..."
    (cd "$NERVE_DIR" && git pull --ff-only) 2>&1 | tail -1
    pass "Nerve updated"
else
    rm -rf "$NERVE_DIR"
    info "Cloning Nerve..."
    git clone "$REPO" "$NERVE_DIR" 2>&1 | tail -1
    pass "Nerve cloned"
fi

# Install dependencies and build
info "Installing dependencies..."
(cd "$NERVE_DIR" && npm ci) 2>&1 | tail -3
pass "Dependencies installed"

info "Building..."
(cd "$NERVE_DIR" && npm run build) 2>&1 | tail -3
pass "Build complete"

# Get gateway token from sandbox
info "Retrieving gateway token..."
SSH_HOST="openshell-nemoclaw-spark-prod"
GATEWAY_TOKEN=$(ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" \
    'python3 -c "import json; print(json.load(open(\"/sandbox/.openclaw/openclaw.json\")).get(\"gateway\",{}).get(\"auth\",{}).get(\"token\",\"\"))"' 2>/dev/null || echo "")

if [ -z "$GATEWAY_TOKEN" ]; then
    warn "Could not retrieve gateway token — Nerve may not connect to gateway"
fi

# Create .env (preserve existing if not forcing)
if [ ! -f "$NERVE_DIR/.env" ] || [[ "$FORCE" == true ]]; then
    cat > "$NERVE_DIR/.env" << ENVEOF
# Nerve config for NemoClaw on DGX Spark
HOST=127.0.0.1
PORT=3080
OPENCLAW_GATEWAY=ws://127.0.0.1:18789
OPENCLAW_GATEWAY_TOKEN=$GATEWAY_TOKEN

# Sandbox paths — Nerve runs on host but openclaw CLI proxies to sandbox.
# These must match the sandbox filesystem, not the host.
MEMORY_PATH=/sandbox/.openclaw/workspace/MEMORY.md
MEMORY_DIR=/sandbox/.openclaw/workspace/memory
SESSIONS_DIR=/sandbox/.openclaw/agents/main/sessions
ENVEOF
    pass "Configuration created"
else
    pass "Configuration preserved (use --force to overwrite)"
fi

# Create systemd user service
info "Setting up systemd service..."
NODE_BIN=$(which node)
mkdir -p ~/.config/systemd/user
cat > ~/.config/systemd/user/nerve.service << SVCEOF
[Unit]
Description=Nerve - OpenClaw Web UI
After=network.target

[Service]
Type=simple
WorkingDirectory=%h/nerve
ExecStart=$NODE_BIN %h/nerve/server-dist/index.js
EnvironmentFile=%h/nerve/.env
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
SVCEOF

systemctl --user daemon-reload
systemctl --user enable nerve 2>/dev/null

# Stop any existing instance, then start
kill "$(lsof -ti:3080 2>/dev/null)" 2>/dev/null || true
sleep 1
systemctl --user restart nerve
sleep 4

if systemctl --user is-active --quiet nerve; then
    pass "Nerve service running"
else
    warn "Nerve service failed to start — check: journalctl --user -u nerve"
fi

echo ""
echo -e "${BOLD}${GREEN}  Nerve Ready${NC}"
echo ""
echo -e "  ${BOLD}Dashboard:${NC}  http://127.0.0.1:3080/"
echo -e "  ${BOLD}Service:${NC}    systemctl --user status nerve"
echo -e "  ${BOLD}Logs:${NC}       journalctl --user -u nerve -f"
echo ""
echo -e "  ${DIM}Data persists in ~/nerve/server/data/ and ~/.nerve/${NC}"
echo ""
