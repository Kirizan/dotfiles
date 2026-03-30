#!/bin/bash
# Deploy multi-agent orchestration system to NemoClaw OpenClaw sandbox
# Usage: install-nemoclaw-orchestration.sh [--force]
#
# Source of truth: ~/Obsidian/Primary/3-Resources/NemoClaw/orchestration/
#
# This script deploys whatever is in the Obsidian orchestration directory.
# To add a new agent or skill, create it in Obsidian and re-run this script.
#
# Directory structure expected in Obsidian:
#   orchestration/
#     skills/{skill-name}/SKILL.md     — deployed to /sandbox/.openclaw/skills/
#     agents/{agent-id}/SOUL.md        — deployed to /sandbox/.openclaw/agents/{agent-id}/
#     agents/{agent-id}/AGENTS.md      — (optional) deployed alongside SOUL.md
#     anti-patterns/*.md               — deployed to workspace/anti-patterns/
#     templates/*.md                   — deployed to workspace/templates/
#
# Prerequisites:
#   - NemoClaw sandbox running (install-nemoclaw.sh completed)
#   - SSH access to sandbox: ssh openshell-nemoclaw-spark-prod
#   - Obsidian vault available at ~/Obsidian/Primary/
#
# This script is idempotent — safe to re-run.

set -euo pipefail

# ── Configuration ─────────────────────────────────────────────────
SANDBOX_NAME="nemoclaw-spark-prod"
SSH_HOST="openshell-${SANDBOX_NAME}"
OBSIDIAN_SRC="$HOME/Obsidian/Primary/3-Resources/NemoClaw/orchestration"
MODEL="inference/nemotron-3-super:120b"
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

TOTAL_PHASES=7

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

# Pipe a local file into the sandbox at a given path
# Usage: sandbox_write <local_file> <remote_path>
sandbox_write() {
    local local_file="$1"
    local remote_path="$2"
    cat "$local_file" | ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "$SSH_HOST" "mkdir -p '$(dirname "$remote_path")' && cat > '$remote_path'" 2>/dev/null
}

# ── Banner ────────────────────────────────────────────────────────
echo ""
echo -e "${BOLD}${CYAN}  ╔══════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}${CYAN}  ║   NemoClaw Multi-Agent Orchestration Setup  ║${NC}"
echo -e "${BOLD}${CYAN}  ║              DGX Spark Edition               ║${NC}"
echo -e "${BOLD}${CYAN}  ╚══════════════════════════════════════════════╝${NC}"
echo ""

# ── Phase 0: Prerequisites ────────────────────────────────────────
phase 0 "Prerequisites"

# Check Obsidian source
if [[ ! -d "$OBSIDIAN_SRC" ]]; then
    die "Obsidian orchestration directory not found: $OBSIDIAN_SRC"
fi
pass "Obsidian source: $OBSIDIAN_SRC"

# Check skills exist
SKILL_COUNT=$(find "$OBSIDIAN_SRC/skills" -name "SKILL.md" 2>/dev/null | wc -l)
if [[ "$SKILL_COUNT" -eq 0 ]]; then
    die "No skills found in $OBSIDIAN_SRC/skills/"
fi
pass "Found $SKILL_COUNT skills to deploy"

# Check agents exist
AGENT_COUNT=$(find "$OBSIDIAN_SRC/agents" -name "SOUL.md" 2>/dev/null | wc -l)
if [[ "$AGENT_COUNT" -eq 0 ]]; then
    die "No agents found in $OBSIDIAN_SRC/agents/"
fi
pass "Found $AGENT_COUNT agents to deploy"

# Check sandbox reachable
if ! sandbox_exec "echo ok" &>/dev/null; then
    die "Cannot SSH to sandbox: $SSH_HOST"
fi
pass "Sandbox reachable: $SSH_HOST"

# ── Phase 1: Deploy Skills ────────────────────────────────────────
phase 1 "Deploy Skills"

for skill_dir in "$OBSIDIAN_SRC"/skills/*/; do
    skill_name=$(basename "$skill_dir")

    # Check if already deployed and skip unless --force
    if [[ "$FORCE" != true ]]; then
        if sandbox_exec "test -f /sandbox/.openclaw/skills/$skill_name/SKILL.md" 2>/dev/null; then
            pass "$skill_name — already deployed"
            continue
        fi
    fi

    # Create skill directory and deploy all files in it
    sandbox_exec "mkdir -p /sandbox/.openclaw/skills/$skill_name"
    local_files_deployed=0
    for file in "$skill_dir"*; do
        [[ -f "$file" ]] || continue
        sandbox_write "$file" "/sandbox/.openclaw/skills/$skill_name/$(basename "$file")"
        local_files_deployed=$((local_files_deployed + 1))
    done
    pass "$skill_name — deployed ($local_files_deployed files)"
done

# ── Phase 2: Register & Deploy Agents ─────────────────────────────
phase 2 "Register & Deploy Agents"

# Get list of already-registered agents
EXISTING_AGENTS=$(sandbox_exec "openclaw agents list --json 2>/dev/null" | python3 -c "
import json, sys
try:
    agents = json.load(sys.stdin)
    for a in agents:
        print(a['id'])
except:
    pass
" 2>/dev/null || true)

for agent_dir in "$OBSIDIAN_SRC"/agents/*/; do
    agent_id=$(basename "$agent_dir")

    # Register agent if not already registered
    if echo "$EXISTING_AGENTS" | grep -qx "$agent_id"; then
        info "$agent_id — already registered"
    else
        info "Registering $agent_id..."
        sandbox_exec "openclaw agents add $agent_id \
            --workspace /sandbox/.openclaw/agents/$agent_id \
            --agent-dir /sandbox/.openclaw/agents/$agent_id/agent \
            --model $MODEL \
            --non-interactive" 2>/dev/null
        pass "$agent_id — registered"
    fi

    # Deploy all files (SOUL.md, AGENTS.md, etc.) regardless — always update
    sandbox_exec "mkdir -p /sandbox/.openclaw/agents/$agent_id"
    local_files_deployed=0
    for file in "$agent_dir"*; do
        [[ -f "$file" ]] || continue
        sandbox_write "$file" "/sandbox/.openclaw/agents/$agent_id/$(basename "$file")"
        local_files_deployed=$((local_files_deployed + 1))
    done
    pass "$agent_id — deployed ($local_files_deployed files)"
done

# ── Phase 3: Deploy Anti-Patterns ─────────────────────────────────
phase 3 "Deploy Anti-Patterns"

if [[ -d "$OBSIDIAN_SRC/anti-patterns" ]]; then
    sandbox_exec "mkdir -p /sandbox/.openclaw/workspace/anti-patterns"
    ap_count=0
    for file in "$OBSIDIAN_SRC"/anti-patterns/*.md; do
        [[ -f "$file" ]] || continue
        sandbox_write "$file" "/sandbox/.openclaw/workspace/anti-patterns/$(basename "$file")"
        ap_count=$((ap_count + 1))
    done
    pass "Deployed $ap_count anti-pattern files"
else
    warn "No anti-patterns directory found — skipping"
fi

# ── Phase 4: Deploy Templates ─────────────────────────────────────
phase 4 "Deploy Templates"

if [[ -d "$OBSIDIAN_SRC/templates" ]]; then
    sandbox_exec "mkdir -p /sandbox/.openclaw/workspace/templates"
    tmpl_count=0
    for file in "$OBSIDIAN_SRC"/templates/*.md; do
        [[ -f "$file" ]] || continue
        sandbox_write "$file" "/sandbox/.openclaw/workspace/templates/$(basename "$file")"
        tmpl_count=$((tmpl_count + 1))
    done
    pass "Deployed $tmpl_count template files"
else
    warn "No templates directory found — skipping"
fi

# ── Phase 5: Create Workspace Directories ─────────────────────────
phase 5 "Workspace Setup"

sandbox_exec "mkdir -p /sandbox/.openclaw/workspace/tasks"
pass "Tasks directory ready"

# ── Phase 6: Validate & Restart Gateway ───────────────────────────
phase 6 "Validate Configuration"

# Unlock config for validation
sandbox_exec "chmod 644 /sandbox/.openclaw/openclaw.json" 2>/dev/null || true

# Validate
VALIDATE_OUTPUT=$(sandbox_exec "openclaw config validate 2>&1" || true)
if echo "$VALIDATE_OUTPUT" | grep -q "Config valid"; then
    pass "Config validation passed"
else
    fail "Config validation failed:"
    echo "$VALIDATE_OUTPUT" | grep -v UNDICI | grep -v langfuse | grep -v punycode | grep -v trace-dep | grep -v trace-warn | grep -v "^\[plugins\]" | head -10
    sandbox_exec "chmod 444 /sandbox/.openclaw/openclaw.json" 2>/dev/null || true
    die "Fix config errors before proceeding"
fi

# Re-lock
sandbox_exec "chmod 444 /sandbox/.openclaw/openclaw.json" 2>/dev/null || true
pass "Config locked (chmod 444)"

# ── Phase 7: Restart Gateway ─────────────────────────────────────
phase 7 "Restart Gateway"

info "Stopping existing gateway..."
sandbox_exec '
openclaw gateway stop 2>/dev/null || true
sleep 2
for f in /proc/[0-9]*/cmdline; do
    if tr "\0" " " < "$f" 2>/dev/null | grep -q "openclaw-gateway"; then
        pid=$(echo "$f" | grep -oP "\d+")
        kill -9 "$pid" 2>/dev/null || true
    fi
done
sleep 1
' 2>/dev/null || true
pass "Gateway stopped"

info "Starting gateway with OTEL tracing..."
sandbox_exec '
source ~/.bashrc 2>/dev/null || true
PATH="/sandbox/.npm-global/bin:/sandbox/bin:$PATH" \
  NODE_OPTIONS="--require /sandbox/otel-tracing/tracing.js" \
  nohup openclaw gateway run --allow-unconfigured --dev --bind loopback --port 18789 > /tmp/gateway.log 2>&1 &
sleep 5
' 2>/dev/null

# Verify gateway is running
GW_CHECK=$(sandbox_exec '
for f in /proc/[0-9]*/cmdline; do
    if tr "\0" " " < "$f" 2>/dev/null | grep -q "openclaw-gateway"; then
        echo "running"
        break
    fi
done
' 2>/dev/null || true)

if [[ "$GW_CHECK" == *"running"* ]]; then
    pass "Gateway started"
else
    fail "Gateway may not have started — check /tmp/gateway.log in sandbox"
fi

# Re-lock config after gateway start
sandbox_exec "chmod 444 /sandbox/.openclaw/openclaw.json" 2>/dev/null || true
pass "Config re-locked"

# Count what was deployed
FINAL_AGENTS=$(sandbox_exec "openclaw agents list --json 2>/dev/null" | python3 -c "
import json, sys
try:
    print(len(json.load(sys.stdin)))
except:
    print('?')
" 2>/dev/null || echo "?")

FINAL_SKILLS=$(sandbox_exec "ls /sandbox/.openclaw/skills/ 2>/dev/null | wc -l" || echo "?")

# ── Summary ───────────────────────────────────────────────────────
echo ""
echo -e "${BOLD}${CYAN}  ╔══════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}${CYAN}  ║            Deployment Complete                ║${NC}"
echo -e "${BOLD}${CYAN}  ╚══════════════════════════════════════════════╝${NC}"
echo ""
echo -e "  ${BOLD}Agents registered:${NC}  $FINAL_AGENTS"
echo -e "  ${BOLD}Skills deployed:${NC}    $FINAL_SKILLS"
echo -e "  ${BOLD}Source:${NC}             $OBSIDIAN_SRC"
echo ""
echo -e "  ${DIM}To add agents/skills: create in Obsidian, re-run this script${NC}"
echo -e "  ${DIM}To force redeploy:    $0 --force${NC}"
echo ""
