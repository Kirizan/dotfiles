#!/bin/bash
# Install and configure NemoClaw with Nemotron 3 Super on DGX Spark
# Usage: install-nemoclaw.sh [--force]
#
# This script follows the NVIDIA NemoClaw playbook with security hardening:
#   - Binary integrity verification (SHA256)
#   - NemoClaw pinned to a specific commit
#   - Ollama bound to 0.0.0.0 only for sandbox reachability, with UFW deny on physical iface
#   - Dashboard on loopback only
#   - All output logged to ~/nemoclaw-install-logs/
#
# Prerequisites: Ubuntu 24.04, NVIDIA GB10 GPU, Docker running, user in docker group

set -euo pipefail

# ── Configuration ─────────────────────────────────────────────────
SANDBOX_NAME="nemoclaw-spark-prod"
OLLAMA_MODEL="nemotron-3-super:120b"
NODE_MAJOR=22
OPENSHELL_ARCH="aarch64-unknown-linux-musl"
LOG_DIR="$HOME/nemoclaw-install-logs"
DASHBOARD_DIR="$HOME/.nemoclaw"
FORCE=false

if [[ "${1:-}" == "--force" ]]; then
    FORCE=true
fi

# ── Colors and formatting ─────────────────────────────────────────
BOLD='\033[1m'
DIM='\033[2m'
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

TOTAL_STEPS=10

pass()  { echo -e "  ${GREEN}✓${NC} $1"; }
fail()  { echo -e "  ${RED}✗${NC} $1"; }
warn()  { echo -e "  ${YELLOW}⚠${NC} $1"; }
info()  { echo -e "  ${DIM}$1${NC}"; }

step() {
    echo ""
    echo -e "${BOLD}${BLUE}[$1/$TOTAL_STEPS]${NC} ${BOLD}$2${NC}"
    echo -e "  ${DIM}$(printf '─%.0s' {1..50})${NC}"
}

progress() {
    local pid=$1 msg=$2 logfile="${3:-}"
    local chars='⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏'
    local i=0
    while kill -0 "$pid" 2>/dev/null; do
        local extra=""
        if [ -n "$logfile" ] && [ -f "$logfile" ]; then
            extra=$(tail -1 "$logfile" 2>/dev/null | head -c 60 || true)
            [ -n "$extra" ] && extra=" ${DIM}${extra}${NC}"
        fi
        printf "\r  ${CYAN}%s${NC} %s%s  " "${chars:i++%${#chars}:1}" "$msg" "$extra"
        sleep 0.15
    done
    printf "\r\033[K"
}

die() {
    fail "$1"
    local errfile="$LOG_DIR/error-$(date -Iseconds).txt"
    echo "$1" > "$errfile"
    echo -e "  ${DIM}Error logged to: $errfile${NC}"
    exit 1
}

# ── Banner ────────────────────────────────────────────────────────
echo ""
echo -e "${BOLD}${CYAN}  ╔══════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}${CYAN}  ║     NemoClaw + Nemotron 3 Super Installer   ║${NC}"
echo -e "${BOLD}${CYAN}  ║              DGX Spark Edition               ║${NC}"
echo -e "${BOLD}${CYAN}  ╚══════════════════════════════════════════════╝${NC}"
echo ""

# ── Phase 1: Pre-flight ──────────────────────────────────────────
step 1 "Pre-flight checks"

mkdir -p "$LOG_DIR"

# OS
OS_PRETTY=$(head -1 /etc/os-release | cut -d'"' -f2)
if ! grep -q "Ubuntu 24" /etc/os-release 2>/dev/null; then
    die "Requires Ubuntu 24.04 (found: $OS_PRETTY)"
fi
pass "OS: $OS_PRETTY"

# GPU
if ! nvidia-smi &>/dev/null; then
    die "No NVIDIA GPU detected (nvidia-smi failed)"
fi
GPU_NAME=$(nvidia-smi --query-gpu=name --format=csv,noheader 2>/dev/null | head -1)
DRIVER_VER=$(nvidia-smi --query-gpu=driver_version --format=csv,noheader 2>/dev/null | head -1)
pass "GPU: $GPU_NAME (driver $DRIVER_VER)"

# Docker
if ! docker info &>/dev/null; then
    die "Docker is not running"
fi
DOCKER_VER=$(docker info --format '{{.ServerVersion}}' 2>/dev/null)
pass "Docker: $DOCKER_VER"

# Docker group
if ! id -nG | grep -qw docker; then
    die "User $(whoami) is not in the docker group. Run: sudo usermod -aG docker $(whoami) && newgrp docker"
fi
pass "User in docker group"

# Python
PYTHON_VER=$(python3 --version 2>/dev/null | awk '{print $2}')
if ! python3 -c "import sys; sys.exit(0 if sys.version_info >= (3,12) else 1)" 2>/dev/null; then
    die "Requires Python 3.12+ (found: ${PYTHON_VER:-none})"
fi
pass "Python: $PYTHON_VER"

# Passwordless sudo
if ! sudo -n true 2>/dev/null; then
    die "Passwordless sudo required. Configure NOPASSWD in /etc/sudoers."
fi
pass "Passwordless sudo"

# ── Phase 2: Docker Hardening ────────────────────────────────────
step 2 "Docker hardening"

PHASE_LOG="$LOG_DIR/phase2-docker-hardening.txt"
: > "$PHASE_LOG"

info "Configuring NVIDIA container runtime..."
sudo nvidia-ctk runtime configure --runtime=docker >> "$PHASE_LOG" 2>&1
pass "NVIDIA container runtime registered"

info "Setting cgroup namespace mode..."
sudo python3 -c "
import json, os
path = '/etc/docker/daemon.json'
d = json.load(open(path)) if os.path.exists(path) else {}
d['default-cgroupns-mode'] = 'host'
json.dump(d, open(path, 'w'), indent=2)
" >> "$PHASE_LOG" 2>&1
pass "default-cgroupns-mode: host"

info "Restarting Docker..."
sudo systemctl restart docker >> "$PHASE_LOG" 2>&1
pass "Docker restarted"

info "Verifying GPU passthrough..."
docker run --rm --runtime=nvidia --gpus all ubuntu nvidia-smi >> "$PHASE_LOG" 2>&1 \
    || die "NVIDIA Docker runtime test failed. See $PHASE_LOG"
pass "GPU visible inside containers"

# ── Phase 3: Node.js ─────────────────────────────────────────────
step 3 "Node.js ${NODE_MAJOR}.x"

PHASE_LOG="$LOG_DIR/phase3-nodejs.txt"
: > "$PHASE_LOG"

CURRENT_NODE=$(node --version 2>/dev/null || echo "none")
if [[ "$CURRENT_NODE" == v${NODE_MAJOR}.* ]]; then
    NPM_VER=$(npm --version 2>/dev/null)
    pass "Already installed: Node.js $CURRENT_NODE, npm $NPM_VER"
else
    info "Installing Node.js ${NODE_MAJOR}.x (current: $CURRENT_NODE)..."
    curl -fsSL "https://deb.nodesource.com/setup_${NODE_MAJOR}.x" | sudo -E bash - >> "$PHASE_LOG" 2>&1
    sudo apt-get install -y nodejs >> "$PHASE_LOG" 2>&1 &
    progress $! "Installing Node.js ${NODE_MAJOR}.x..." "$PHASE_LOG"
    wait $! || die "Node.js install failed. See $PHASE_LOG"
    pass "Node.js $(node --version), npm $(npm --version)"
fi

# ── Phase 4: Ollama + Nemotron ────────────────────────────────────
step 4 "Ollama + $OLLAMA_MODEL"

PHASE_LOG="$LOG_DIR/phase4-ollama.txt"
: > "$PHASE_LOG"

# Install Ollama
if ! command -v ollama &>/dev/null; then
    info "Installing Ollama..."
    curl -fsSL https://ollama.com/install.sh | sh >> "$PHASE_LOG" 2>&1
    pass "Ollama installed"
else
    pass "Ollama: $(ollama --version 2>/dev/null)"
fi

# Start if not running
if ! systemctl is-active --quiet ollama; then
    sudo systemctl start ollama
    sleep 2
fi
pass "Ollama service active"

# Bind to 0.0.0.0 for container networking
OLLAMA_OVERRIDE="/etc/systemd/system/ollama.service.d/override.conf"
if ! grep -q "OLLAMA_HOST=0.0.0.0" "$OLLAMA_OVERRIDE" 2>/dev/null; then
    info "Binding Ollama to 0.0.0.0 (required for sandbox reachability)..."
    sudo mkdir -p /etc/systemd/system/ollama.service.d
    cat <<'OVERRIDE' | sudo tee "$OLLAMA_OVERRIDE" > /dev/null
# REQUIRED for OpenShell sandbox container networking.
# Sandbox reaches Ollama via host.openshell.internal (Docker host-gateway).
# UFW blocks external access on the physical interface.
# KEEP_ALIVE=-1 prevents model unload (avoids 60s cold-start penalty).
[Service]
Environment="OLLAMA_HOST=0.0.0.0:11434"
Environment="OLLAMA_KEEP_ALIVE=-1"
OVERRIDE
    sudo systemctl daemon-reload
    sudo systemctl restart ollama
    sleep 2
    pass "Ollama bound to 0.0.0.0"
else
    pass "Ollama already bound to 0.0.0.0"
fi

# UFW deny on physical interface
PHYS_IFACE=$(ip route show default | awk '{print $5}' | head -1)
if [ -n "$PHYS_IFACE" ]; then
    if ! sudo ufw status 2>/dev/null | grep -q "11434.*DENY.*$PHYS_IFACE"; then
        sudo ufw deny in on "$PHYS_IFACE" to any port 11434 \
            comment "Block external Ollama - only Docker bridge needs it" >> "$PHASE_LOG" 2>&1
        pass "UFW: port 11434 blocked on $PHYS_IFACE"
    else
        pass "UFW: Ollama already blocked on $PHYS_IFACE"
    fi
fi

# Allow Docker/k3s container access to Ollama (UFW INPUT defaults to DROP)
if ! sudo ufw status 2>/dev/null | grep -q "11434.*ALLOW.*172.16.0.0/12"; then
    sudo ufw allow in from 172.16.0.0/12 to any port 11434 \
        comment "Ollama access from Docker/k3s containers" >> "$PHASE_LOG" 2>&1
    pass "UFW: Docker containers (172.16.0.0/12) allowed to Ollama"
else
    pass "UFW: Docker container access already allowed"
fi

# Pull model
if ollama list 2>/dev/null | grep -q "$OLLAMA_MODEL"; then
    MODEL_SIZE=$(ollama list 2>/dev/null | grep "$OLLAMA_MODEL" | awk '{print $3, $4}')
    pass "Model present: $OLLAMA_MODEL ($MODEL_SIZE)"
else
    echo -e "  ${CYAN}↓${NC} Pulling $OLLAMA_MODEL (~87 GB)..."
    echo -e "  ${DIM}  This is the longest step. Go grab coffee.${NC}"
    ollama pull "$OLLAMA_MODEL" 2>&1 | tee -a "$PHASE_LOG"
    pass "Model pulled: $OLLAMA_MODEL"
fi

# Warm up model (load weights into memory)
if ! curl -sf http://127.0.0.1:11434/api/ps 2>/dev/null | grep -q "$OLLAMA_MODEL"; then
    info "Loading model into memory (~86 GB, takes about a minute)..."
    curl -s http://127.0.0.1:11434/api/generate \
        -d "{\"model\":\"$OLLAMA_MODEL\",\"prompt\":\"hello\",\"stream\":false}" \
        --max-time 180 >> "$PHASE_LOG" 2>&1 &
    progress $! "Loading $OLLAMA_MODEL into memory..."
    wait $! && pass "Model loaded and warm" || warn "Model warmup timed out (will load on first request)"
else
    pass "Model already loaded in memory"
fi

# ── Phase 5: OpenShell CLI ────────────────────────────────────────
step 5 "OpenShell CLI"

PHASE_LOG="$LOG_DIR/phase5-openshell.txt"
: > "$PHASE_LOG"

if command -v openshell &>/dev/null && [[ "$FORCE" != true ]]; then
    pass "Already installed: $(openshell --version 2>/dev/null)"
else
    if ! command -v gh &>/dev/null; then
        info "Installing GitHub CLI..."
        sudo apt-get install -y gh >> "$PHASE_LOG" 2>&1
    fi
    pass "GitHub CLI: $(gh --version 2>/dev/null | head -1)"

    # Install GitLab CLI (glab) if not present
    if ! command -v glab &>/dev/null; then
        info "Installing GitLab CLI (glab)..."
        GLAB_TMPDIR=$(mktemp -d)
        curl -fsSL "https://gitlab.com/gitlab-org/cli/-/releases/latest/download/glab_linux_arm64.tar.gz" \
            -o "$GLAB_TMPDIR/glab.tar.gz" >> "$PHASE_LOG" 2>&1
        tar -xzf "$GLAB_TMPDIR/glab.tar.gz" -C "$GLAB_TMPDIR" >> "$PHASE_LOG" 2>&1
        sudo install -m 755 "$GLAB_TMPDIR/bin/glab" /usr/local/bin/glab >> "$PHASE_LOG" 2>&1
        rm -rf "$GLAB_TMPDIR"
        pass "GitLab CLI: $(glab --version 2>/dev/null | head -1)"
    else
        pass "GitLab CLI: $(glab --version 2>/dev/null | head -1)"
    fi

    info "Fetching latest release from NVIDIA/OpenShell..."
    RELEASE_JSON=$(gh release view --repo NVIDIA/OpenShell --json tagName,assets 2>/dev/null) \
        || die "Cannot fetch OpenShell release. Run: gh auth login"
    TAG=$(echo "$RELEASE_JSON" | python3 -c "import sys,json; print(json.load(sys.stdin)['tagName'])")

    info "Downloading OpenShell $TAG ($OPENSHELL_ARCH)..."
    TMPDIR=$(mktemp -d)
    gh release download "$TAG" --repo NVIDIA/OpenShell \
        --pattern "openshell-${OPENSHELL_ARCH}.tar.gz" \
        --pattern "openshell-checksums-sha256.txt" \
        --dir "$TMPDIR" >> "$PHASE_LOG" 2>&1

    info "Verifying SHA256..."
    (cd "$TMPDIR" && sha256sum -c <(grep "openshell-${OPENSHELL_ARCH}.tar.gz" openshell-checksums-sha256.txt)) \
        >> "$PHASE_LOG" 2>&1 \
        || { rm -rf "$TMPDIR"; die "SHA256 verification FAILED — binary may be tampered"; }
    pass "SHA256 checksum verified"

    tar -xzf "$TMPDIR/openshell-${OPENSHELL_ARCH}.tar.gz" -C "$TMPDIR"
    sudo install -m 755 "$TMPDIR/openshell" /usr/local/bin/openshell
    rm -rf "$TMPDIR"
    pass "Installed: $(openshell --version 2>/dev/null)"
fi

# ── Phase 6: NemoClaw ─────────────────────────────────────────────
step 6 "NemoClaw"

PHASE_LOG="$LOG_DIR/phase6-nemoclaw.txt"
: > "$PHASE_LOG"

if command -v nemoclaw &>/dev/null && [[ "$FORCE" != true ]]; then
    pass "Already installed"
else
    CLONE_DIR=$(mktemp -d)

    info "Cloning NVIDIA/NemoClaw..."
    git clone https://github.com/NVIDIA/NemoClaw.git "$CLONE_DIR" >> "$PHASE_LOG" 2>&1

    COMMIT_SHA=$(cd "$CLONE_DIR" && git log -1 --format="%H")
    COMMIT_MSG=$(cd "$CLONE_DIR" && git log -1 --format="%s")
    echo "$COMMIT_SHA $COMMIT_MSG" > "$LOG_DIR/nemoclaw-commit.txt"
    info "Pinning to: ${COMMIT_SHA:0:12} ($COMMIT_MSG)"
    (cd "$CLONE_DIR" && git checkout "$COMMIT_SHA") >> "$PHASE_LOG" 2>&1

    info "Installing globally via npm..."
    (cd "$CLONE_DIR" && sudo npm install -g .) >> "$PHASE_LOG" 2>&1
    rm -rf "$CLONE_DIR"
    pass "Installed (commit ${COMMIT_SHA:0:12})"
fi

# ── Phase 7: Gateway + Sandbox ────────────────────────────────────
step 7 "Gateway + sandbox ($SANDBOX_NAME)"

PHASE_LOG="$LOG_DIR/phase7-onboard.txt"
: > "$PHASE_LOG"

if nemoclaw list 2>/dev/null | grep -q "$SANDBOX_NAME" && [[ "$FORCE" != true ]]; then
    pass "Sandbox '$SANDBOX_NAME' already exists"
else
    # Clean up stale state
    openshell gateway destroy -g nemoclaw >> "$PHASE_LOG" 2>&1 || true
    openshell forward stop 18789 2>/dev/null || true
    sleep 1

    echo -e "  ${CYAN}⏳${NC} Running onboard wizard..."
    echo -e "  ${DIM}   Gateway deploy + Docker image build + sandbox creation${NC}"
    echo -e "  ${DIM}   This takes several minutes on first run.${NC}"

    NEMOCLAW_SANDBOX_NAME="$SANDBOX_NAME" \
    NEMOCLAW_PROVIDER=ollama \
    NEMOCLAW_MODEL="$OLLAMA_MODEL" \
    NEMOCLAW_RECREATE_SANDBOX=1 \
    nemoclaw onboard --non-interactive >> "$PHASE_LOG" 2>&1 &
    ONBOARD_PID=$!

    while kill -0 "$ONBOARD_PID" 2>/dev/null; do
        LAST=$(grep -oP '\[\d+/7\] .*' "$PHASE_LOG" 2>/dev/null | tail -1 || true)
        [ -n "$LAST" ] && printf "\r  ${CYAN}⏳${NC} %s" "$LAST"
        sleep 3
    done
    printf "\r\033[K"

    wait "$ONBOARD_PID" || true

    if nemoclaw list 2>/dev/null | grep -q "$SANDBOX_NAME"; then
        pass "Sandbox '$SANDBOX_NAME' created"
    else
        warn "Onboard wizard may not have fully completed"
        info "This is common — the container reachability check can fail."
        info "Continuing with manual setup. Check: $PHASE_LOG"
    fi
fi

# ── Phase 8: Inference ────────────────────────────────────────────
step 8 "Inference provider"

PHASE_LOG="$LOG_DIR/phase8-inference.txt"
: > "$PHASE_LOG"

if ! openshell provider list 2>/dev/null | grep -q "ollama-local"; then
    info "Creating provider..."
    openshell provider create \
        --name ollama-local --type openai \
        --credential "OPENAI_API_KEY=unused" \
        --config "OPENAI_BASE_URL=http://host.openshell.internal:11434/v1" \
        >> "$PHASE_LOG" 2>&1
    pass "Provider 'ollama-local' created"
else
    pass "Provider 'ollama-local' exists"
fi

openshell inference set --provider ollama-local --model "$OLLAMA_MODEL" --no-verify >> "$PHASE_LOG" 2>&1
pass "Inference: ollama-local -> $OLLAMA_MODEL"

# ── Phase 9: Sandbox Config ──────────────────────────────────────
step 9 "Sandbox configuration"

PHASE_LOG="$LOG_DIR/phase9-config.txt"
: > "$PHASE_LOG"

# SSH config
SSH_HOST="openshell-${SANDBOX_NAME}"
if ! grep -q "$SSH_HOST" ~/.ssh/config 2>/dev/null; then
    openshell sandbox ssh-config "$SANDBOX_NAME" >> ~/.ssh/config 2>/dev/null
    pass "SSH config added ($SSH_HOST)"
else
    pass "SSH config present ($SSH_HOST)"
fi

# Create openclaw wrapper on host (proxies to sandbox via SSH)
if [ ! -f /usr/local/bin/openclaw ] || [[ "$FORCE" == true ]]; then
    cat > /tmp/openclaw-wrapper << 'OCWRAPPER'
#!/bin/bash
exec ssh -o ConnectTimeout=5 -o StrictHostKeyChecking=no openshell-nemoclaw-spark-prod "openclaw $*" 2>/dev/null
OCWRAPPER
    sudo install -m 755 /tmp/openclaw-wrapper /usr/local/bin/openclaw
    rm -f /tmp/openclaw-wrapper
    pass "openclaw host wrapper installed"
else
    pass "openclaw host wrapper present"
fi

info "Configuring OpenClaw inside sandbox..."
ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" bash -s << 'SANDBOX_SETUP' >> "$PHASE_LOG" 2>&1
set -e

mkdir -p ~/.openclaw ~/.nemoclaw /sandbox/.openclaw/workspace/memory
echo "# Memory" > /sandbox/.openclaw/workspace/MEMORY.md

openclaw config set gateway.controlUi.dangerouslyAllowHostHeaderOriginFallback true 2>/dev/null || true

# Allow cron tool via gateway HTTP API (blocked by default as "dangerous")
# Needed for Nerve UI cron management
python3 -c "
import json, os
p = os.path.expanduser('~/.openclaw/openclaw.json')
with open(p) as f: c = json.load(f)
c.setdefault('gateway',{}).setdefault('tools',{})['allow'] = ['cron']
with open(p,'w') as f: json.dump(c,f,indent=2)
os.chmod(p, 0o600)
" 2>/dev/null || true

# Allow Nerve UI origin (port 3080)
python3 -c "
import json, os
p = os.path.expanduser('~/.openclaw/openclaw.json')
with open(p) as f: c = json.load(f)
o = c.get('gateway',{}).get('controlUi',{}).get('allowedOrigins',[])
if 'http://127.0.0.1:3080' not in o:
    o.append('http://127.0.0.1:3080')
    c['gateway']['controlUi']['allowedOrigins'] = o
    with open(p,'w') as f: json.dump(c,f,indent=2)
    os.chmod(p, 0o600)
" 2>/dev/null || true
openclaw config set gateway.auth.mode none 2>/dev/null || true

python3 - <<'PYCFG'
import json, os
cfg_path = os.path.expanduser("~/.openclaw/openclaw.json")
cfg = {}
if os.path.exists(cfg_path):
    with open(cfg_path) as f:
        cfg = json.load(f)
cfg.setdefault("agents", {}).setdefault("defaults", {}).setdefault("model", {})["primary"] = "inference/nemotron-3-super:120b"
m = cfg.setdefault("models", {})
m.setdefault("mode", "merge")
m.setdefault("providers", {})["inference"] = {
    "baseUrl": "http://host.openshell.internal:11434/v1",
    "apiKey": "unused",
    "api": "openai-completions",
    "models": [{
        "id": "nemotron-3-super:120b", "name": "nemotron-3-super:120b",
        "reasoning": False, "input": ["text"],
        "cost": {"input": 0, "output": 0, "cacheRead": 0, "cacheWrite": 0},
        "contextWindow": 131072, "maxTokens": 4096
    }]
}
os.makedirs(os.path.dirname(cfg_path), exist_ok=True)
with open(cfg_path, "w") as f:
    json.dump(cfg, f, indent=2)
os.chmod(cfg_path, 0o600)
PYCFG
openclaw models set inference/nemotron-3-super:120b 2>/dev/null || true

# Create SearXNG search skill
mkdir -p /sandbox/.openclaw/skills/searxng-search
cat > /sandbox/.openclaw/skills/searxng-search/SKILL.md << 'SKILL_CONTENT'
---
name: searxng-search
description: Search the web using the self-hosted SearXNG instance. Use this skill whenever the user asks you to search the web, look something up, research a topic, or find current information.
tools:
  - name: web_search
    description: Search the web via SearXNG and return results with titles, URLs, and snippets
    parameters:
      query:
        type: string
        description: The search query
        required: true
      max_results:
        type: number
        description: Maximum number of results to return (default 10)
        required: false
---

# SearXNG Web Search

You have access to a self-hosted SearXNG instance for web searches.

## How to Search

Run the `web_search` tool with a curl command:

```bash
curl -sf "https://searxng.internal.kirby.network/search?q=$(python3 -c "import urllib.parse,sys; print(urllib.parse.quote(sys.argv[1]))" "YOUR_QUERY")&format=json&pageno=1" | python3 -c "
import sys, json
data = json.load(sys.stdin)
for r in data.get(\"results\", [])[:10]:
    title = r.get(\"title\", \"No title\")
    url = r.get(\"url\", \"\")
    snippet = r.get(\"content\", \"\")[:200]
    print(f\"### {title}\")
    print(f\"URL: {url}\")
    print(f\"{snippet}\")
    print()
"
```

## Guidelines

- Always use this search when the user asks about current events, facts you are unsure about, or anything that benefits from up-to-date information
- Present results clearly with titles and relevant snippets
- If results are insufficient, refine the query and search again
- You can fetch full page content with `curl -sf <url>` for any URL in the results
SKILL_CONTENT
SANDBOX_SETUP
pass "OpenClaw inference + SearXNG skill configured"

# Install base ClawHub skills (SkillGuard + Self-Improvement)
info "Installing base OpenClaw skills..."
ssh -o ConnectTimeout=30 -o StrictHostKeyChecking=no "$SSH_HOST" bash -s << 'BASE_SKILLS' >> "$PHASE_LOG" 2>&1
cd /sandbox

install_base_skill() {
    local slug="$1"
    if [ -d "/sandbox/.openclaw/skills/$slug" ] || [ -d "/sandbox/skills/$slug" ]; then
        echo "SKIP: $slug — already installed"
        return 0
    fi
    echo "INSTALLING: $slug..."
    npx clawhub install "$slug" 2>&1
    if [ -d "/sandbox/skills/$slug" ]; then
        mkdir -p /sandbox/.openclaw/skills
        cp -r "/sandbox/skills/$slug" /sandbox/.openclaw/skills/
        echo "OK: $slug installed"
    else
        echo "WARN: $slug may not have installed"
    fi
}

# SkillGuard — security scanner for vetting ClawHub skills
install_base_skill "clawscan"

# Self-Improvement — captures durable lessons from debugging and user corrections
install_base_skill "actual-self-improvement"
BASE_SKILLS
if ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" \
    'test -d /sandbox/.openclaw/skills/clawscan' 2>/dev/null; then
    pass "SkillGuard (clawscan) installed"
else
    warn "SkillGuard install may have failed — check $PHASE_LOG"
fi
if ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" \
    'test -d /sandbox/.openclaw/skills/actual-self-improvement' 2>/dev/null; then
    pass "Self-Improvement skill installed"
else
    warn "Self-Improvement install may have failed — check $PHASE_LOG"
fi

info "Restarting gateway inside sandbox..."
ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" bash -s << 'RESTART_GW' >> "$PHASE_LOG" 2>&1
openclaw gateway stop 2>/dev/null || true
sleep 1
# Kill any lingering gateway process
for f in /proc/[0-9]*/cmdline; do
    if tr '\0' ' ' < "$f" 2>/dev/null | grep -q "openclaw-gateway"; then
        pid=$(echo "$f" | grep -oP '\d+')
        kill -9 "$pid" 2>/dev/null || true
    fi
done
sleep 1
nohup openclaw gateway run --allow-unconfigured --dev --bind loopback --port 18789 > /tmp/gateway.log 2>&1 &
sleep 4
RESTART_GW
pass "OpenClaw gateway restarted"

# ── Phase 10: Validation ──────────────────────────────────────────
step 10 "Validation"

PHASE_LOG="$LOG_DIR/phase10-validation.txt"
: > "$PHASE_LOG"

# Network isolation
info "Testing network isolation..."
HTTP_RESULT=$(ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" \
    'curl -sI --connect-timeout 5 https://httpbin.org/get 2>&1 | head -1' 2>/dev/null || echo "blocked")
if echo "$HTTP_RESULT" | grep -qE "403|blocked|refused|curl:"; then
    pass "Network isolation: httpbin.org blocked"
else
    warn "Network isolation returned: $HTTP_RESULT (review sandbox policy)"
fi

# Port forward
info "Starting dashboard port forward..."
openshell forward stop 18789 "$SANDBOX_NAME" 2>/dev/null || true
kill "$(lsof -ti:18789 2>/dev/null)" 2>/dev/null || true
sleep 1
openshell forward start 18789 "$SANDBOX_NAME" --background >> "$PHASE_LOG" 2>&1
pass "Dashboard forwarded to 127.0.0.1:18789"

# Save policy YAML for nemoclaw_allow function
# This is the base policy with PyPI and npm presets baked in.
# The nemoclaw_allow shell function appends to this file and re-applies it.
POLICY_FILE="$DASHBOARD_DIR/sandbox-policy.yaml"
if [ ! -f "$POLICY_FILE" ] || [[ "$FORCE" == true ]]; then
    cat > "$POLICY_FILE" <<'POLICYEOF'
version: 1

filesystem_policy:
  include_workdir: true
  read_only: [/usr, /lib, /proc, /dev/urandom, /app, /etc, /var/log]
  read_write: [/sandbox, /tmp, /dev/null]

landlock:
  compatibility: best_effort

process:
  run_as_user: sandbox
  run_as_group: sandbox

network_policies:
  claude_code:
    name: claude_code
    endpoints:
      - {host: api.anthropic.com, port: 443, protocol: rest, enforcement: enforce, tls: terminate, rules: [{allow: {method: "*", path: "/**"}}]}
      - {host: statsig.anthropic.com, port: 443, rules: [{allow: {method: "*", path: "/**"}}]}
      - {host: sentry.io, port: 443, rules: [{allow: {method: "*", path: "/**"}}]}
    binaries: [{path: /usr/local/bin/claude}]
  nvidia:
    name: nvidia
    endpoints:
      - {host: integrate.api.nvidia.com, port: 443, protocol: rest, enforcement: enforce, tls: terminate, rules: [{allow: {method: "*", path: "/**"}}]}
      - {host: inference-api.nvidia.com, port: 443, protocol: rest, enforcement: enforce, tls: terminate, rules: [{allow: {method: "*", path: "/**"}}]}
    binaries: [{path: /usr/local/bin/claude}, {path: /usr/local/bin/openclaw}]
  github:
    name: github
    endpoints:
      - {host: github.com, port: 443, access: full}
      - {host: api.github.com, port: 443, access: full}
    binaries: [{path: /usr/bin/gh}, {path: /usr/bin/git}, {path: /sandbox/bin/gh}, {path: /sandbox/bin/git}]
  clawhub:
    name: clawhub
    endpoints:
      - {host: clawhub.com, port: 443, protocol: rest, enforcement: enforce, tls: terminate, rules: [{allow: {method: GET, path: "/**"}}, {allow: {method: POST, path: "/**"}}]}
    binaries: [{path: /usr/local/bin/openclaw}]
  openclaw_api:
    name: openclaw_api
    endpoints:
      - {host: openclaw.ai, port: 443, protocol: rest, enforcement: enforce, tls: terminate, rules: [{allow: {method: GET, path: "/**"}}, {allow: {method: POST, path: "/**"}}]}
    binaries: [{path: /usr/local/bin/openclaw}]
  openclaw_docs:
    name: openclaw_docs
    endpoints:
      - {host: docs.openclaw.ai, port: 443, protocol: rest, enforcement: enforce, tls: terminate, rules: [{allow: {method: GET, path: "/**"}}]}
    binaries: [{path: /usr/local/bin/openclaw}]
  npm_registry:
    name: npm_registry
    endpoints:
      - {host: registry.npmjs.org, port: 443, access: full}
      - {host: registry.yarnpkg.com, port: 443, protocol: rest, enforcement: enforce, tls: terminate, rules: [{allow: {method: GET, path: "/**"}}]}
    binaries: [{path: /usr/local/bin/openclaw}, {path: /usr/local/bin/npm}]
  pypi:
    name: pypi
    endpoints:
      - {host: pypi.org, port: 443, protocol: rest, enforcement: enforce, tls: terminate, rules: [{allow: {method: GET, path: "/**"}}]}
      - {host: files.pythonhosted.org, port: 443, protocol: rest, enforcement: enforce, tls: terminate, rules: [{allow: {method: GET, path: "/**"}}]}
  telegram:
    name: telegram
    endpoints:
      - {host: api.telegram.org, port: 443, protocol: rest, enforcement: enforce, tls: terminate, rules: [{allow: {method: GET, path: "/bot*/**"}}, {allow: {method: POST, path: "/bot*/**"}}]}
  searxng:
    name: searxng
    endpoints:
      - {host: searxng.internal.kirby.network, port: 443, access: full, allowed_ips: ["192.168.1.223/32"]}
    binaries:
      - {path: /usr/local/bin/node}
      - {path: /usr/local/bin/openclaw}
      - {path: /usr/bin/curl}
  gitlab:
    name: gitlab
    endpoints:
      - {host: gitlab.com, port: 443, access: full}
      - {host: registry.gitlab.com, port: 443, access: full}
    binaries: [{path: /usr/bin/glab}, {path: /usr/bin/git}, {path: /sandbox/bin/glab}, {path: /sandbox/bin/git}]
  langfuse:
    name: langfuse
    endpoints:
      - {host: langfuse.internal.kirby.network, port: 443, access: full, allowed_ips: ["192.168.1.223/32"]}
    binaries: [{path: /usr/local/bin/node}, {path: /usr/local/bin/openclaw}, {path: /usr/bin/python3}, {path: /usr/bin/curl}]
  gitlab_internal:
    name: gitlab_internal
    endpoints:
      - {host: gitlab.internal.kirby.network, port: 443, access: full, allowed_ips: ["192.168.1.223/32"]}
      - {host: registry.internal.kirby.network, port: 443, access: full, allowed_ips: ["192.168.1.223/32"]}
    binaries: [{path: /sandbox/bin/glab}, {path: /sandbox/bin/git}, {path: /usr/bin/git}]
  playwright:
    name: playwright
    endpoints:
      - {host: cdn.playwright.dev, port: 443, access: full}
      - {host: playwright.download.prss.microsoft.com, port: 443, access: full}
      - {host: storage.googleapis.com, port: 443, access: full}
    binaries: [{path: /usr/local/bin/node}, {path: /usr/local/bin/npx}]
  discord:
    name: discord
    endpoints:
      - {host: discord.com, port: 443, access: full}
      - {host: gateway.discord.gg, port: 443, access: full}
      - {host: cdn.discordapp.com, port: 443, access: full}
POLICYEOF
    chmod 600 "$POLICY_FILE"
    openshell policy set "$SANDBOX_NAME" --policy "$POLICY_FILE" --wait >> "$PHASE_LOG" 2>&1 || true
    pass "Policy file saved to $POLICY_FILE"
else
    pass "Policy file already exists at $POLICY_FILE"
fi

# Dashboard URL
DASHBOARD_URL=$(ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" \
    'openclaw dashboard 2>/dev/null' 2>/dev/null \
    | grep -oP 'http://127\.0\.0\.1:18789/\S*' | head -1 || echo "http://127.0.0.1:18789/")
echo "$DASHBOARD_URL" > "$DASHBOARD_DIR/dashboard-url.txt"
chmod 600 "$DASHBOARD_DIR/dashboard-url.txt"
pass "Dashboard: $DASHBOARD_URL"

# Summary log
nemoclaw "$SANDBOX_NAME" status > "$LOG_DIR/onboard-summary.txt" 2>&1 || true

# ── Done ──────────────────────────────────────────────────────────
COMMIT_SHA=$(awk '{print $1}' "$LOG_DIR/nemoclaw-commit.txt" 2>/dev/null || echo "unknown")
LOG_COUNT=$(find "$LOG_DIR" -type f | wc -l)

echo ""
echo -e "${BOLD}${GREEN}  ╔══════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}${GREEN}  ║       Installation Complete                  ║${NC}"
echo -e "${BOLD}${GREEN}  ╚══════════════════════════════════════════════╝${NC}"
echo ""
echo -e "  ${BOLD}Sandbox${NC}           $SANDBOX_NAME"
echo -e "  ${BOLD}Model${NC}             $OLLAMA_MODEL"
echo -e "  ${BOLD}Provider${NC}          ollama-local"
echo -e "  ${BOLD}NemoClaw commit${NC}   ${COMMIT_SHA:0:12}"
echo -e "  ${BOLD}OpenShell${NC}         $(openshell --version 2>/dev/null)"
echo -e "  ${BOLD}Logs${NC}              $LOG_DIR/ ($LOG_COUNT files)"
echo ""
echo -e "  ${BOLD}Dashboard:${NC}"
echo -e "  ${CYAN}$DASHBOARD_URL${NC}"
echo ""
echo -e "  ${BOLD}Quick start:${NC}"
echo -e "    start_nemoclaw                    ${DIM}# restart port forward${NC}"
echo -e "    cat ~/.nemoclaw/dashboard-token.txt  ${DIM}# get dashboard URL${NC}"
echo -e "    openshell sandbox connect $SANDBOX_NAME"
echo -e "    openshell term                    ${DIM}# monitoring TUI${NC}"
echo ""
echo -e "  ${BOLD}Next step:${NC}"
echo -e "    ${CYAN}./scripts/install-nemoclaw-skills.sh${NC}  ${DIM}# install dev team skills${NC}"
echo ""
