#!/bin/bash
# Claude Code status line script
# Shows: Model | Directory | Context Usage %

# Read JSON input from stdin
input=$(cat)

# Extract information from JSON
MODEL=$(echo "$input" | jq -r '.model.display_name')
DIR=$(echo "$input" | jq -r '.workspace.current_dir' | sed 's|.*/||')
USED_PCT=$(echo "$input" | jq -r '.context_window.used_percentage // 0')

# Round to integer
USED_INT=$(printf "%.0f" "$USED_PCT")

echo "[$MODEL] $DIR | ctx: ${USED_INT}%"
