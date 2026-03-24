#!/bin/bash
# Sync OpenClaw workspace persona files from Obsidian to sandbox
# Usage: sync-nemoclaw-workspace.sh [file...]
#   No args = sync all persona files
#   With args = sync only named files (e.g., SOUL.md TOOLS.md)

set -euo pipefail

OBSIDIAN_DIR="$HOME/Obsidian/Primary/3-Resources/NemoClaw/workspace"
SSH_HOST="openshell-nemoclaw-spark-prod"
WORKSPACE="/sandbox/.openclaw/workspace"
PERSONA_FILES=(SOUL.md IDENTITY.md USER.md TOOLS.md AGENTS.md HEARTBEAT.md)

if [[ ! -d "$OBSIDIAN_DIR" ]]; then
    echo "Error: Obsidian workspace dir not found: $OBSIDIAN_DIR"
    exit 1
fi

# If args provided, sync only those; otherwise sync all
if [[ $# -gt 0 ]]; then
    files=("$@")
else
    files=("${PERSONA_FILES[@]}")
fi

echo "Syncing workspace files to sandbox ($SSH_HOST)..."
for f in "${files[@]}"; do
    src="$OBSIDIAN_DIR/$f"
    if [[ -f "$src" ]]; then
        cat "$src" | ssh -o ConnectTimeout=10 "$SSH_HOST" "cat > $WORKSPACE/$f"
        echo "  ✓ $f"
    else
        echo "  ✗ $f not found in Obsidian"
    fi
done
echo "Done."
