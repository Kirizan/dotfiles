#!/bin/bash
# Claude Code status line script
# Shows: Model | Directory | Token Usage

# Read JSON input from stdin
input=$(cat)

# Extract information from JSON
MODEL=$(echo "$input" | jq -r '.model.display_name')
DIR=$(echo "$input" | jq -r '.workspace.current_dir' | sed 's|.*/||')
CONTEXT_SIZE=$(echo "$input" | jq -r '.context_window.context_window_size')

# Get total input tokens from the session (not just current request)
TOTAL_INPUT=$(echo "$input" | jq -r '.context_window.total_input_tokens // 0')

# Calculate percentage
if [ "$TOTAL_INPUT" != "0" ] && [ "$TOTAL_INPUT" != "null" ]; then
    PERCENT_USED=$((TOTAL_INPUT * 100 / CONTEXT_SIZE))
    echo "[$MODEL] 📁 $DIR | ${TOTAL_INPUT}/${CONTEXT_SIZE} (${PERCENT_USED}%)"
else
    # No usage yet (start of session)
    echo "[$MODEL] 📁 $DIR | 0/${CONTEXT_SIZE} (0%)"
fi
