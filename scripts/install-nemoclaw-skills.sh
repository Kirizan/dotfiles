#!/bin/bash
# Configure NemoClaw OpenClaw sandbox as a full development team
# Usage: install-nemoclaw-skills.sh [--force]
#
# Prerequisites:
#   - NemoClaw sandbox running (install-nemoclaw.sh completed)
#   - Port forward active on 18789 (run start_nemoclaw if not)
#   - SSH access to sandbox: ssh openshell-nemoclaw-spark-prod
#
# External setup required BEFORE running this script:
#
#   1. OpenBao CLI (bao) — must be installed and authenticated:
#      - Install: sudo dpkg -i openbao_<ver>_linux_arm64.deb
#      - Auth:    bao login -method=userpass username=kirizan
#      - Secrets used:
#        kv/infra/github  (field: openclaw)  — GitHub PAT for gh CLI
#        kv/ai/gitlab     (field: token)     — GitLab PAT for glab CLI
#
#   2. Discord Bot (for notifications — Phase 13):
#      - Create bot at https://discord.com/developers/applications
#      - Enable MESSAGE_CONTENT intent
#      - Copy bot token
#      - Invite bot to your server with Send Messages permission
#      - Set env var: export DISCORD_BOT_TOKEN="your-token"
#      - Set env var: export DISCORD_CHANNEL_ID="your-channel-id"
#
#   3. Playwright (browser automation — Phase 11):
#      - Egress domains added automatically by this script
#      - Chromium downloaded inside sandbox automatically
#
# This script is idempotent — safe to re-run.

set -euo pipefail

# ── Configuration ─────────────────────────────────────────────────
SANDBOX_NAME="nemoclaw-spark-prod"
SSH_HOST="openshell-${SANDBOX_NAME}"
POLICY_FILE="$HOME/.nemoclaw/sandbox-policy.yaml"
LOG_DIR="$HOME/nemoclaw-install-logs"
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

TOTAL_PHASES=15

pass()  { echo -e "  ${GREEN}✓${NC} $1"; }
fail()  { echo -e "  ${RED}✗${NC} $1"; }
warn()  { echo -e "  ${YELLOW}⚠${NC} $1"; }
info()  { echo -e "  ${DIM}$1${NC}"; }

phase() {
    echo ""
    echo -e "${BOLD}${BLUE}[$1/$TOTAL_PHASES]${NC} ${BOLD}$2${NC}"
    echo -e "  ${DIM}$(printf '─%.0s' {1..50})${NC}"
}

die() {
    fail "$1"
    exit 1
}

# Run a command inside the sandbox via SSH
sandbox_exec() {
    ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" bash -s <<< "$1" 2>/dev/null
}

# Install a ClawHub skill inside the sandbox, scanning with clawscan first
# Usage: install_skill <clawhub_slug> <display_name>
install_skill() {
    local slug="$1"
    local name="${2:-$slug}"

    # Check if already installed
    if sandbox_exec "test -d /sandbox/.openclaw/skills/$slug || test -d /sandbox/skills/$slug" 2>/dev/null; then
        if [[ "$FORCE" != true ]]; then
            pass "$name — already installed"
            return 0
        fi
    fi

    # Scan with clawscan first (if available)
    if sandbox_exec "command -v clawscan" &>/dev/null; then
        info "Scanning $slug with SkillGuard..."
        local scan_result
        scan_result=$(sandbox_exec "clawscan scan $slug 2>&1" || true)
        if echo "$scan_result" | grep -qiE "critical|high.*risk|blocked|malicious"; then
            fail "$name — BLOCKED by SkillGuard: $scan_result"
            return 1
        fi
        pass "$name — passed security scan"
    fi

    info "Installing $slug..."
    local install_output
    install_output=$(sandbox_exec "cd /sandbox && npx clawhub install $slug 2>&1" || true)

    # If flagged as suspicious, show details and let the user decide
    if echo "$install_output" | grep -q "Use --force"; then
        echo ""
        warn "$name ($slug) — FLAGGED as suspicious by ClawHub"
        echo -e "  ${DIM}$(echo "$install_output" | grep -v Warning | grep -v "node --trace" | grep -v "^$" | head -5)${NC}"
        echo ""
        echo -e "  ${BOLD}[s]${NC} Skip for now"
        echo -e "  ${BOLD}[i]${NC} Inspect skill details, then decide"
        echo -e "  ${BOLD}[f]${NC} Force install anyway"
        echo ""
        echo -e "  ${DIM}To review/install later:${NC}"
        echo -e "  ${DIM}  ssh $SSH_HOST 'cd /sandbox && npx clawhub inspect $slug'${NC}"
        echo -e "  ${DIM}  ssh $SSH_HOST 'cd /sandbox && npx clawhub install $slug --force'${NC}"
        echo ""
        while true; do
            read -r -p "  Choice [s/i/f]: " choice
            case "$choice" in
                s|S)
                    warn "$name — skipped"
                    return 1
                    ;;
                i|I)
                    echo ""
                    sandbox_exec "cd /sandbox && npx clawhub inspect $slug 2>&1" | grep -v Warning | grep -v "node --trace"
                    echo ""
                    read -r -p "  Install after review? [y/N]: " confirm
                    if [[ "$confirm" =~ ^[yY]$ ]]; then
                        install_output=$(sandbox_exec "cd /sandbox && npx clawhub install $slug --force 2>&1" || true)
                        break
                    else
                        warn "$name — skipped after review"
                        return 1
                    fi
                    ;;
                f|F)
                    install_output=$(sandbox_exec "cd /sandbox && npx clawhub install $slug --force 2>&1" || true)
                    break
                    ;;
                *)
                    echo "  Please enter s, i, or f"
                    ;;
            esac
        done
    fi

    # ClawHub installs to /sandbox/skills/ but OpenClaw reads /sandbox/.openclaw/skills/
    if sandbox_exec "test -d /sandbox/skills/$slug" 2>/dev/null; then
        sandbox_exec "mkdir -p /sandbox/.openclaw/skills && cp -r /sandbox/skills/$slug /sandbox/.openclaw/skills/ 2>/dev/null" || true
    fi

    if sandbox_exec "test -d /sandbox/.openclaw/skills/$slug || test -d /sandbox/skills/$slug" 2>/dev/null; then
        pass "$name — installed"
    else
        warn "$name — install may have failed (check manually)"
        info "  Output: $(echo "$install_output" | tail -2)"
    fi
}

# Add a domain to the egress policy (idempotent)
# Usage: add_egress_domain <domain> <label> [extra_yaml_fields]
add_egress_domain() {
    local domain="$1"
    local label="$2"
    local access="${3:-rest}"

    if [ ! -f "$POLICY_FILE" ]; then
        warn "Policy file not found at $POLICY_FILE — skipping $domain"
        return 1
    fi

    if grep -q "host: ${domain}" "$POLICY_FILE" 2>/dev/null; then
        pass "Egress: $domain — already allowed"
        return 0
    fi

    python3 - "$POLICY_FILE" "$domain" "$label" "$access" <<'PYPOLICY'
import sys, yaml

policy_file, domain, label, access = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4]

with open(policy_file) as f:
    policy = yaml.safe_load(f)

if access == "full":
    endpoint = {"host": domain, "port": 443, "access": "full"}
else:
    endpoint = {
        "host": domain, "port": 443,
        "protocol": "rest", "enforcement": "enforce", "tls": "terminate",
        "rules": [
            {"allow": {"method": "GET", "path": "/**"}},
            {"allow": {"method": "POST", "path": "/**"}},
        ],
    }

policy.setdefault("network_policies", {})[label] = {
    "name": label,
    "endpoints": [endpoint],
}

with open(policy_file, "w") as f:
    yaml.dump(policy, f, default_flow_style=False, sort_keys=False)
PYPOLICY

    if [ $? -eq 0 ]; then
        pass "Egress: $domain — added (label: $label)"
    else
        fail "Egress: failed to add $domain"
        return 1
    fi
}

# ── Banner ────────────────────────────────────────────────────────
echo ""
echo -e "${BOLD}${CYAN}  ╔══════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}${CYAN}  ║   NemoClaw Dev Team Skills Configuration    ║${NC}"
echo -e "${BOLD}${CYAN}  ║              DGX Spark Edition               ║${NC}"
echo -e "${BOLD}${CYAN}  ╚══════════════════════════════════════════════╝${NC}"
echo ""

# ── Pre-flight ────────────────────────────────────────────────────
echo -e "${BOLD}Pre-flight checks${NC}"
echo -e "  ${DIM}$(printf '─%.0s' {1..50})${NC}"

mkdir -p "$LOG_DIR"

# Check sandbox is reachable
if ! ssh -o ConnectTimeout=5 -o StrictHostKeyChecking=no "$SSH_HOST" 'echo ok' &>/dev/null; then
    die "Cannot reach sandbox '$SANDBOX_NAME'. Is it running? Try: start_nemoclaw"
fi
pass "Sandbox reachable via SSH ($SSH_HOST)"

# Check policy file exists
if [ ! -f "$POLICY_FILE" ]; then
    die "Policy file not found at $POLICY_FILE. Run install-nemoclaw.sh first."
fi
pass "Policy file: $POLICY_FILE"

# Check OpenBao CLI (needed for auth configuration)
if command -v bao &>/dev/null; then
    if bao token lookup &>/dev/null; then
        pass "OpenBao CLI: authenticated"
    else
        warn "OpenBao CLI installed but not authenticated — gh/glab auth will be skipped"
        echo -e "  ${DIM}Run: bao login -method=userpass username=kirizan${NC}"
    fi
else
    warn "OpenBao CLI not installed — gh/glab auth will be skipped"
    echo -e "  ${DIM}Install from: https://github.com/openbao/openbao/releases${NC}"
fi

# Check Discord env vars (optional — warn only)
if [ -n "${DISCORD_BOT_TOKEN:-}" ] && [ -n "${DISCORD_CHANNEL_ID:-}" ]; then
    pass "Discord bot token and channel ID set"
else
    warn "Discord env vars not set — Discord integration will be skipped"
    echo -e "  ${DIM}Set DISCORD_BOT_TOKEN and DISCORD_CHANNEL_ID before running${NC}"
    echo -e "  ${DIM}See script header for Discord bot setup instructions${NC}"
fi

# Check base skills from install-nemoclaw.sh
if sandbox_exec "test -d /sandbox/.openclaw/skills/clawscan" 2>/dev/null; then
    pass "SkillGuard (clawscan) — present (installed by install-nemoclaw.sh)"
else
    warn "SkillGuard (clawscan) not found — run install-nemoclaw.sh first"
    echo -e "  ${DIM}Skills will not be scanned before installation${NC}"
fi
if sandbox_exec "test -d /sandbox/.openclaw/skills/actual-self-improvement" 2>/dev/null; then
    pass "Self-Improvement — present (installed by install-nemoclaw.sh)"
else
    warn "Self-Improvement skill not found — run install-nemoclaw.sh to install"
fi

echo ""
echo -e "${BOLD}Installing ~20 dev team skills across 15 phases.${NC}"
echo -e "${DIM}Skills are scanned by SkillGuard before installation.${NC}"
echo ""

# ── Phase 1: CLI Tool Dependencies ────────────────────────────────
phase 1 "CLI Tool Dependencies"

info "Installing required CLI tools into sandbox..."

# Detect architecture
HOST_ARCH=$(uname -m)
if [ "$HOST_ARCH" = "aarch64" ]; then
    GH_ARCH="linux_arm64"
    GLAB_ARCH="linux_arm64"
    KUBE_ARCH="arm64"
else
    GH_ARCH="linux_amd64"
    GLAB_ARCH="linux_amd64"
    KUBE_ARCH="amd64"
fi

# Create /sandbox/bin and ensure it's on PATH inside sandbox
sandbox_exec 'mkdir -p /sandbox/bin'
if ! sandbox_exec 'grep -q "/sandbox/bin" ~/.bashrc 2>/dev/null'; then
    sandbox_exec 'echo "export PATH=\"/sandbox/bin:\$PATH\"" >> ~/.bashrc'
    sandbox_exec 'echo "export PATH=\"/sandbox/bin:\$PATH\"" >> ~/.profile 2>/dev/null || true'
fi

# gh (GitHub CLI)
if sandbox_exec 'test -x /sandbox/bin/gh'; then
    pass "gh — already installed"
else
    info "Downloading gh CLI..."
    GH_VER=$(curl -sf https://api.github.com/repos/cli/cli/releases/latest | python3 -c "import sys,json; print(json.load(sys.stdin)['tag_name'].lstrip('v'))")
    GH_TMP=$(mktemp -d)
    curl -fsSL "https://github.com/cli/cli/releases/download/v${GH_VER}/gh_${GH_VER}_${GH_ARCH}.tar.gz" -o "$GH_TMP/gh.tar.gz"
    tar -xzf "$GH_TMP/gh.tar.gz" -C "$GH_TMP"
    cat "$GH_TMP/gh_${GH_VER}_${GH_ARCH}/bin/gh" | ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" 'cat > /sandbox/bin/gh && chmod +x /sandbox/bin/gh'
    rm -rf "$GH_TMP"
    pass "gh $GH_VER — installed"
fi

# glab (GitLab CLI)
if sandbox_exec 'test -x /sandbox/bin/glab'; then
    pass "glab — already installed"
else
    info "Downloading glab CLI..."
    GLAB_VER=$(curl -sf "https://gitlab.com/api/v4/projects/gitlab-org%2Fcli/releases" | python3 -c "import sys,json; print(json.load(sys.stdin)[0]['tag_name'].lstrip('v'))")
    GLAB_TMP=$(mktemp -d)
    curl -fsSL "https://gitlab.com/gitlab-org/cli/-/releases/v${GLAB_VER}/downloads/glab_${GLAB_VER}_${GLAB_ARCH}.tar.gz" -o "$GLAB_TMP/glab.tar.gz"
    tar -xzf "$GLAB_TMP/glab.tar.gz" -C "$GLAB_TMP"
    cat "$GLAB_TMP/bin/glab" | ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" 'cat > /sandbox/bin/glab && chmod +x /sandbox/bin/glab'
    rm -rf "$GLAB_TMP"
    pass "glab $GLAB_VER — installed"
fi

# shellcheck
if sandbox_exec 'test -x /sandbox/bin/shellcheck'; then
    pass "shellcheck — already installed"
else
    info "Downloading shellcheck..."
    SC_VER=$(curl -sf https://api.github.com/repos/koalaman/shellcheck/releases/latest | python3 -c "import sys,json; print(json.load(sys.stdin)['tag_name'])")
    SC_TMP=$(mktemp -d)
    curl -fsSL "https://github.com/koalaman/shellcheck/releases/download/${SC_VER}/shellcheck-${SC_VER}.linux.aarch64.tar.xz" -o "$SC_TMP/sc.tar.xz"
    tar -xJf "$SC_TMP/sc.tar.xz" -C "$SC_TMP"
    cat "$SC_TMP/shellcheck-${SC_VER}/shellcheck" | ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" 'cat > /sandbox/bin/shellcheck && chmod +x /sandbox/bin/shellcheck'
    rm -rf "$SC_TMP"
    pass "shellcheck $SC_VER — installed"
fi

# kubectl
if sandbox_exec 'test -x /sandbox/bin/kubectl'; then
    pass "kubectl — already installed"
else
    info "Downloading kubectl..."
    KUBE_VER=$(curl -fsSL https://dl.k8s.io/release/stable.txt)
    KUBE_TMP=$(mktemp -d)
    curl -fsSL "https://dl.k8s.io/release/${KUBE_VER}/bin/linux/${KUBE_ARCH}/kubectl" -o "$KUBE_TMP/kubectl"
    cat "$KUBE_TMP/kubectl" | ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" 'cat > /sandbox/bin/kubectl && chmod +x /sandbox/bin/kubectl'
    rm -rf "$KUBE_TMP"
    pass "kubectl $KUBE_VER — installed"
fi

# Configure auth from OpenBao
if command -v bao &>/dev/null && bao token lookup &>/dev/null; then
    info "Configuring CLI auth from OpenBao..."

    # gh auth (GitHub)
    GH_AUTH=$(sandbox_exec 'export PATH="/sandbox/bin:$PATH" && gh auth status 2>&1' || true)
    if echo "$GH_AUTH" | grep -q "Logged in"; then
        pass "gh auth — already configured"
    else
        GH_TOKEN=$(bao kv get -field=openclaw kv/infra/github 2>/dev/null || true)
        if [ -n "$GH_TOKEN" ]; then
            echo "$GH_TOKEN" | ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" \
                'export PATH="/sandbox/bin:$PATH" && gh auth login --with-token 2>&1' \
                && pass "gh auth — configured from OpenBao (kv/infra/github)" \
                || warn "gh auth — token from OpenBao failed (check scopes)"
        else
            warn "gh auth — no token at kv/infra/github in OpenBao"
        fi
        unset GH_TOKEN
    fi

    # glab auth (GitLab — self-hosted)
    GLAB_AUTH=$(sandbox_exec 'export PATH="/sandbox/bin:$PATH" && glab auth status --hostname gitlab.internal.kirby.network 2>&1' || true)
    if echo "$GLAB_AUTH" | grep -q "Logged in"; then
        pass "glab auth — already configured"
    else
        GL_TOKEN=$(bao kv get -field=token kv/ai/gitlab 2>/dev/null || true)
        if [ -n "$GL_TOKEN" ]; then
            echo "$GL_TOKEN" | ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" \
                'export PATH="/sandbox/bin:$PATH" && glab auth login --hostname gitlab.internal.kirby.network --stdin 2>&1' \
                && pass "glab auth — configured from OpenBao (kv/ai/gitlab)" \
                || warn "glab auth — token from OpenBao failed"
        else
            warn "glab auth — no token at kv/ai/gitlab in OpenBao"
        fi
        unset GL_TOKEN
    fi
else
    warn "OpenBao CLI not available or not authenticated — skipping auth configuration"
    info "  Install: sudo dpkg -i openbao_<ver>_linux_arm64.deb"
    info "  Auth:    bao login -method=userpass username=kirizan"
    info "  Then re-run this script to configure gh/glab auth automatically"
fi

# ── Phase 2: Memory Backend (QMD) ────────────────────────────────
phase 2 "Memory Backend (QMD)"

info "Installing QMD (hybrid markdown search engine)..."

# Install @tobilu/qmd (the real package, NOT the empty "qmd" placeholder)
if sandbox_exec 'test -x /sandbox/bin/qmd || test -x /sandbox/.npm-global/bin/qmd' 2>/dev/null; then
    pass "QMD binary — already installed"
else
    sandbox_exec '
    set -e
    mkdir -p /sandbox/.npm-global /sandbox/bin
    npm config set prefix /sandbox/.npm-global
    export PATH="/sandbox/.npm-global/bin:$PATH"
    # Install with --ignore-scripts to skip node-gyp (proxy blocks nodejs.org headers)
    npm install -g @tobilu/qmd --ignore-scripts 2>&1 | tail -3
    ln -sf /sandbox/.npm-global/bin/qmd /sandbox/bin/qmd
    ' 2>&1 | tail -5

    # Push prebuilt better-sqlite3 native addon (sandbox proxy blocks GitHub releases for prebuild-install)
    NODE_MOD_VER=$(ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" 'node -e "console.log(process.config.variables.node_module_version)"' 2>/dev/null)
    BSQLITE_VER=$(ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" 'node -e "console.log(require(\"/sandbox/.npm-global/lib/node_modules/@tobilu/qmd/node_modules/better-sqlite3/package.json\").version)"' 2>/dev/null)
    PREBUILD_URL="https://github.com/WiseLibs/better-sqlite3/releases/download/v${BSQLITE_VER}/better-sqlite3-v${BSQLITE_VER}-node-v${NODE_MOD_VER}-linux-arm64.tar.gz"

    info "Downloading better-sqlite3 prebuilt (node v${NODE_MOD_VER}, arm64)..."
    PREBUILD_TMP=$(mktemp -d)
    if curl -fsSL "$PREBUILD_URL" -o "$PREBUILD_TMP/prebuild.tar.gz" 2>/dev/null; then
        tar -xzf "$PREBUILD_TMP/prebuild.tar.gz" -C "$PREBUILD_TMP"
        cat "$PREBUILD_TMP/build/Release/better_sqlite3.node" | \
            ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" \
            'mkdir -p /sandbox/.npm-global/lib/node_modules/@tobilu/qmd/node_modules/better-sqlite3/build/Release && cat > /sandbox/.npm-global/lib/node_modules/@tobilu/qmd/node_modules/better-sqlite3/build/Release/better_sqlite3.node'
        pass "QMD + better-sqlite3 prebuilt — installed"
    else
        warn "Failed to download better-sqlite3 prebuilt from $PREBUILD_URL"
        info "  QMD will not work without the native sqlite addon"
    fi
    rm -rf "$PREBUILD_TMP"
fi

info "Configuring QMD as primary memory backend..."
sandbox_exec '
set -e

# Configure OpenClaw to use QMD
python3 - <<'"'"'PYCFG'"'"'
import json, os

cfg_path = os.path.expanduser("~/.openclaw/openclaw.json")
cfg = {}
if os.path.exists(cfg_path):
    with open(cfg_path) as f:
        cfg = json.load(f)

cfg["memory"] = {
    "backend": "qmd",
    "citations": "auto",
    "qmd": {
        "includeDefaultMemory": True,
        "update": {
            "interval": "5m",
            "debounceMs": 15000
        },
        "limits": {
            "maxResults": 6,
            "timeoutMs": 4000
        },
        "scope": {
            "default": "deny",
            "rules": [
                {"action": "allow", "match": {"chatType": "direct"}}
            ]
        }
    }
}

with open(cfg_path, "w") as f:
    json.dump(cfg, f, indent=2)
os.chmod(cfg_path, 0o600)
PYCFG
' 2>&1 | tail -5
pass "QMD memory backend configured"

# Pull nomic-embed-text for future Qdrant migration
info "Pulling nomic-embed-text embedding model (~274 MB)..."
if ollama list 2>/dev/null | grep -q "nomic-embed-text"; then
    pass "nomic-embed-text — already pulled"
else
    ollama pull nomic-embed-text 2>&1 | tail -3
    if ollama list 2>/dev/null | grep -q "nomic-embed-text"; then
        pass "nomic-embed-text — pulled"
    else
        warn "nomic-embed-text pull may have failed (non-critical — needed for future Qdrant migration)"
    fi
fi

# ── Phase 3: Core Development Skills ─────────────────────────────
phase 3 "Core Development Skills"

install_skill "agentic-coding"           "Coding Agent (structured workflow)"
install_skill "gh"                       "GitHub (gh CLI integration)"
install_skill "gitlab-manager"           "GitLab Manager (glab CLI integration)"
install_skill "test-runner"              "Test Runner (multi-framework)"
install_skill "test-sentinel"            "Test Sentinel (write + run tests)"

# ── Phase 4: Code Quality & Review ───────────────────────────────
phase 4 "Code Quality & Review"

install_skill "security-auditor"         "Security Auditor (code review)"
install_skill "security-scanner"         "Security Scanner (multi-language)"
install_skill "bash"                     "Bash/Shell Analysis"

# ── Phase 5: Documentation & API ─────────────────────────────────
phase 5 "Documentation & API"

install_skill "api-dev"                  "API Dev (scaffold + test + document)"
install_skill "api-tester"              "API Tester (HTTP requests + health checks)"

# ── Phase 6: CI/CD ───────────────────────────────────────────────
phase 6 "CI/CD"

install_skill "martok9803-ci-whisperer" "CI Whisperer (GitHub Actions analysis)"
install_skill "cicd-pipeline"           "CI/CD Pipeline (generate configs)"

# ── Phase 7: Database (Guidance Only) ────────────────────────────
phase 7 "Database Guidance"

install_skill "database-migrations"     "Database Migrations (best practices)"
install_skill "sql-toolkit"             "SQL Toolkit (queries + migrations)"

# ── Phase 8: Infrastructure ──────────────────────────────────────
phase 8 "Infrastructure"

install_skill "kubernetes"              "Kubernetes (ops + troubleshooting)"

# ── Phase 9: UX / Design ─────────────────────────────────────────
phase 9 "UX / Design"

install_skill "superdesign"             "SuperDesign (frontend components)"
install_skill "accessibility"           "Accessibility (WCAG compliance)"
install_skill "mermaid-diagram"         "Mermaid Diagram (diagrams)"
install_skill "design-system-patterns"  "Design System Patterns (tokens + components)"

# ── Phase 10: Project Management ──────────────────────────────────
phase 10 "Project Management"

install_skill "agent-orchestrator"      "Agent Orchestrator (multi-project pipeline)"

# ── Phase 11: Browser Automation (Playwright) ────────────────────
phase 11 "Browser Automation (Playwright)"

info "Adding browser download domains to egress policy..."
add_egress_domain "cdn.playwright.dev"                      "playwright_cdn"       "full"
add_egress_domain "playwright.download.prss.microsoft.com"  "playwright_ms_cdn"    "full"
add_egress_domain "storage.googleapis.com"                  "google_storage"       "full"

info "Applying updated egress policy..."
openshell policy set "$SANDBOX_NAME" --policy "$POLICY_FILE" --wait 2>/dev/null \
    && pass "Egress policy applied" \
    || warn "Failed to apply egress policy — apply manually"

info "Installing Chromium inside sandbox..."
CHROMIUM_RESULT=$(sandbox_exec "npx playwright install chromium 2>&1" || true)
if echo "$CHROMIUM_RESULT" | grep -qiE "success|already|chromium"; then
    pass "Chromium browser — installed"
else
    warn "Chromium install may have failed — check sandbox connectivity"
    info "  Retry inside sandbox: npx playwright install chromium"
    info "  Output: $(echo "$CHROMIUM_RESULT" | tail -2)"
fi

# Verify Playwright MCP is functional
if sandbox_exec "test -d /sandbox/.openclaw/skills/playwright-mcp || test -d /sandbox/skills/playwright" 2>/dev/null; then
    pass "Playwright MCP skill — present"
else
    info "Playwright MCP was installed during base NemoClaw setup"
fi

# ── Phase 12: Monitoring & Observability ──────────────────────────
phase 12 "Monitoring & Observability"

install_skill "monitoring"              "Monitoring (Prometheus + Grafana)"
install_skill "logging-observability"   "Logging Observability (OpenTelemetry)"

info "Adding Langfuse (LLM observability) to egress policy..."
add_egress_domain "langfuse.internal.kirby.network" "langfuse" "full"

info "Configuring Langfuse OTEL tracing for OpenClaw gateway..."

# Fetch Langfuse API keys from OpenBao
LANGFUSE_CONFIGURED=false
if command -v bao &>/dev/null && bao token lookup &>/dev/null; then
    LF_PK=$(bao kv get -field=public_key kv/ai/langfuse 2>/dev/null || true)
    LF_SK=$(bao kv get -field=secret_key kv/ai/langfuse 2>/dev/null || true)

    if [ -n "$LF_PK" ] && [ -n "$LF_SK" ]; then
        LF_AUTH_B64=$(echo -n "${LF_PK}:${LF_SK}" | base64 -w0)

        # Install OTEL SDK packages in sandbox
        if sandbox_exec "test -f /sandbox/otel-tracing/tracing.js" 2>/dev/null; then
            pass "OTEL tracing bootstrap — already installed"
        else
            info "Installing OTEL SDK packages..."
            sandbox_exec '
            mkdir -p /sandbox/otel-tracing && cd /sandbox/otel-tracing
            cat > package.json <<'"'"'PKG'"'"'
{
  "name": "openclaw-otel-tracing",
  "version": "1.0.0",
  "private": true,
  "dependencies": {
    "@opentelemetry/sdk-node": "^0.57.0",
    "@opentelemetry/auto-instrumentations-node": "^0.56.0",
    "@opentelemetry/exporter-trace-otlp-http": "^0.57.0"
  }
}
PKG
            npm install 2>&1 | tail -3
            ' 2>/dev/null

            # Create tracing bootstrap
            sandbox_exec 'cat > /sandbox/otel-tracing/tracing.js <<'"'"'TRACEJS'"'"'
const { NodeSDK } = require("@opentelemetry/sdk-node");
const { OTLPTraceExporter } = require("@opentelemetry/exporter-trace-otlp-http");
const { getNodeAutoInstrumentations } = require("@opentelemetry/auto-instrumentations-node");

const traceExporter = new OTLPTraceExporter();

const sdk = new NodeSDK({
  traceExporter,
  instrumentations: [
    getNodeAutoInstrumentations({
      "@opentelemetry/instrumentation-fs": { enabled: false },
      "@opentelemetry/instrumentation-dns": { enabled: false },
    }),
  ],
});

sdk.start();

process.on("SIGTERM", () => {
  sdk.shutdown().then(() => process.exit(0)).catch(() => process.exit(0));
});
TRACEJS
            ' 2>/dev/null
            pass "OTEL tracing bootstrap — installed"
        fi

        # Configure OTEL env vars in sandbox (env vars only — NOT NODE_OPTIONS)
        # NODE_OPTIONS must only be set on the gateway process to avoid breaking
        # openclaw CLI subcommands and tool execution
        if sandbox_exec 'grep -q "OTEL_EXPORTER_OTLP_ENDPOINT" ~/.bashrc' 2>/dev/null; then
            pass "OTEL env vars — already configured"
        else
            # shellcheck disable=SC2087
            ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" bash -c "
            for rcfile in ~/.bashrc ~/.profile; do
                cat >> \"\$rcfile\" <<OTELENV

# Langfuse OTEL Tracing (env vars only — NODE_OPTIONS set in gateway startup)
export OTEL_EXPORTER_OTLP_ENDPOINT=\"https://langfuse.internal.kirby.network/api/public/otel\"
export OTEL_EXPORTER_OTLP_PROTOCOL=\"http/json\"
export OTEL_EXPORTER_OTLP_HEADERS=\"Authorization=Basic ${LF_AUTH_B64}\"
export OTEL_SERVICE_NAME=\"openclaw-spark\"
OTELENV
            done
            " 2>/dev/null
            pass "OTEL env vars — configured (gateway-only NODE_OPTIONS)"
        fi
        LANGFUSE_CONFIGURED=true
        unset LF_PK LF_SK LF_AUTH_B64
    else
        warn "Langfuse API keys not found in OpenBao (kv/ai/langfuse) — skipping OTEL setup"
        info "  Store public_key and secret_key in kv/ai/langfuse, then re-run"
    fi
else
    warn "OpenBao not available — skipping Langfuse OTEL configuration"
    info "  OTEL tracing requires API keys from kv/ai/langfuse in OpenBao"
fi

# ── Phase 13: Communication (Discord) ────────────────────────────
phase 13 "Communication (Discord)"

if [ -n "${DISCORD_BOT_TOKEN:-}" ] && [ -n "${DISCORD_CHANNEL_ID:-}" ]; then
    info "Adding Discord domains to egress policy..."
    add_egress_domain "discord.com"          "discord_api"      "full"
    add_egress_domain "gateway.discord.gg"   "discord_gateway"  "full"
    add_egress_domain "cdn.discordapp.com"   "discord_cdn"      "full"

    info "Configuring Discord inside sandbox..."
    sandbox_exec "
    python3 - <<'PYCFG'
import json, os

cfg_path = os.path.expanduser('~/.openclaw/openclaw.json')
cfg = {}
if os.path.exists(cfg_path):
    with open(cfg_path) as f:
        cfg = json.load(f)

cfg.setdefault('integrations', {})['discord'] = {
    'enabled': True,
    'botToken': os.environ.get('DISCORD_BOT_TOKEN', '${DISCORD_BOT_TOKEN}'),
    'channelId': os.environ.get('DISCORD_CHANNEL_ID', '${DISCORD_CHANNEL_ID}'),
    'notifications': {
        'pr': True,
        'build': True,
        'deploy': True,
        'error': True
    }
}

with open(cfg_path, 'w') as f:
    json.dump(cfg, f, indent=2)
os.chmod(cfg_path, 0o600)
PYCFG
    " 2>/dev/null
    pass "Discord integration configured"
else
    warn "Discord skipped — DISCORD_BOT_TOKEN and DISCORD_CHANNEL_ID not set"
    echo -e "  ${DIM}To configure later:${NC}"
    echo -e "  ${DIM}  1. Create bot at https://discord.com/developers/applications${NC}"
    echo -e "  ${DIM}  2. Enable MESSAGE_CONTENT intent${NC}"
    echo -e "  ${DIM}  3. Invite bot to your server (Send Messages permission)${NC}"
    echo -e "  ${DIM}  4. Re-run with: DISCORD_BOT_TOKEN=... DISCORD_CHANNEL_ID=... $0${NC}"
fi

# ── Phase 14: Apply All Egress Policy Updates ─────────────────────
phase 14 "Apply Egress Policy (batched)"

# Add GitLab domains (needed for gitlab-api skill)
add_egress_domain "gitlab.com"           "gitlab"           "full"
add_egress_domain "registry.gitlab.com"  "gitlab_registry"  "full"

info "Applying final egress policy..."
if openshell policy set "$SANDBOX_NAME" --policy "$POLICY_FILE" --wait 2>/dev/null; then
    pass "All egress policies applied"
else
    warn "Policy apply failed — run manually: openshell policy set $SANDBOX_NAME --policy $POLICY_FILE --wait"
fi

# ── Phase 15: Restart Gateway & Validate ──────────────────────────
phase 15 "Restart Gateway & Validate"

info "Restarting OpenClaw gateway to pick up new skills..."
sandbox_exec '
openclaw gateway stop 2>/dev/null || true
sleep 2
# Kill lingering gateway processes
for f in /proc/[0-9]*/cmdline; do
    if tr "\0" " " < "$f" 2>/dev/null | grep -q "openclaw-gateway"; then
        pid=$(echo "$f" | grep -oP "\d+")
        kill -9 "$pid" 2>/dev/null || true
    fi
done
sleep 1
# Set NODE_OPTIONS only for the gateway process (not globally — breaks openclaw subcommands)
NODE_OPTIONS="--require /sandbox/otel-tracing/tracing.js" \
  nohup openclaw gateway run --allow-unconfigured --dev --bind loopback --port 18789 > /tmp/gateway.log 2>&1 &
sleep 4
' 2>/dev/null
pass "Gateway restarted"

# Count installed skills
info "Counting installed skills..."
SKILL_COUNT=$(sandbox_exec "ls -d /sandbox/.openclaw/skills/*/ /sandbox/skills/*/ 2>/dev/null | sort -u | wc -l" || echo "?")
pass "Total skills installed: $SKILL_COUNT"

# Quick smoke tests
info "Running smoke tests..."

# Test 1: Skill listing
if sandbox_exec "openclaw skills list 2>/dev/null | head -5" &>/dev/null; then
    pass "Smoke: openclaw skills list — OK"
else
    warn "Smoke: openclaw skills list — failed"
fi

# Test 2: Memory backend
MEMORY_CHECK=$(sandbox_exec "python3 -c \"
import json
cfg = json.load(open('/root/.openclaw/openclaw.json'))
print(cfg.get('memory',{}).get('backend','not set'))
\" 2>/dev/null" || echo "error")
if [ "$MEMORY_CHECK" = "qmd" ]; then
    pass "Smoke: QMD memory backend — configured"
else
    warn "Smoke: memory backend is '$MEMORY_CHECK' (expected 'qmd')"
fi

# Test 3: Gateway reachable
if curl -sf http://127.0.0.1:18789/ &>/dev/null; then
    pass "Smoke: Gateway reachable on :18789"
else
    warn "Smoke: Gateway not reachable — check port forward"
fi

# Test 4: Langfuse OTEL tracing
if [ "$LANGFUSE_CONFIGURED" = true ]; then
    LF_HEALTH=$(sandbox_exec 'curl -sk https://langfuse.internal.kirby.network/api/public/health 2>/dev/null' || echo "unreachable")
    if echo "$LF_HEALTH" | grep -q '"OK"'; then
        pass "Smoke: Langfuse reachable from sandbox"
    else
        warn "Smoke: Langfuse not reachable from sandbox — check egress policy"
    fi
    if sandbox_exec 'grep -q "LANGFUSE_OTEL_CONFIGURED" ~/.bashrc' 2>/dev/null; then
        pass "Smoke: OTEL env vars configured in sandbox"
    else
        warn "Smoke: OTEL env vars not found in sandbox .bashrc"
    fi
fi

# ── Summary ───────────────────────────────────────────────────────
echo ""
echo -e "${BOLD}${GREEN}  ╔══════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}${GREEN}  ║     Dev Team Skills — Setup Complete         ║${NC}"
echo -e "${BOLD}${GREEN}  ╚══════════════════════════════════════════════╝${NC}"
echo ""
echo -e "  ${BOLD}Skills installed:${NC}  ~$SKILL_COUNT"
echo -e "  ${BOLD}Memory backend:${NC}   QMD (local, zero-config)"
echo -e "  ${BOLD}Security:${NC}         SkillGuard scanning enabled"
echo -e "  ${BOLD}Browser:${NC}          Playwright + Chromium"
if [ "$LANGFUSE_CONFIGURED" = true ]; then
echo -e "  ${BOLD}Observability:${NC}    Langfuse OTEL tracing → langfuse.internal.kirby.network"
fi
echo ""
echo -e "  ${BOLD}Verification checklist:${NC}"
echo -e "    ${DIM}1. Ask OpenClaw to write a Python function       (coding-agent)${NC}"
echo -e "    ${DIM}2. Ask it to create a GitHub issue                (github)${NC}"
echo -e "    ${DIM}3. Ask it to run tests on a project               (test-runner)${NC}"
echo -e "    ${DIM}4. Ask it to review a code snippet                (code audit)${NC}"
echo -e "    ${DIM}5. Ask it to generate a CI config                 (ci-gen)${NC}"
echo -e "    ${DIM}6. Ask it to search the web and fetch a page      (searxng + playwright)${NC}"
echo -e "    ${DIM}7. Check memory persistence across sessions       (QMD)${NC}"
echo -e "    ${DIM}8. Ask it to generate a React component           (frontend-design)${NC}"
echo -e "    ${DIM}9. Ask it to audit a11y of a component            (accessibility)${NC}"
echo -e "   ${DIM}10. Create a task in DevClaw pipeline              (devclaw)${NC}"
if [ -n "${DISCORD_BOT_TOKEN:-}" ]; then
echo -e "   ${DIM}11. Verify Discord notification delivery           (discord)${NC}"
fi
echo ""
echo -e "  ${BOLD}Future deployments (see Obsidian notes):${NC}"
echo -e "    ${DIM}• Penpot MCP Server — deploy on Avalon, then configure with:${NC}"
echo -e "    ${DIM}  nemoclaw_allow penpot.internal.kirby.network penpot${NC}"
echo -e "    ${DIM}• Qdrant Vector DB — deploy on Avalon, migrate from QMD${NC}"
echo -e "    ${DIM}  nemoclaw_allow qdrant.internal.kirby.network qdrant${NC}"
echo ""
