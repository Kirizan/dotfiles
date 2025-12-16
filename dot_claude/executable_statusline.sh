#!/bin/bash
# Claude Code status line script
# Shows: Model | Directory | Token Usage

# Read JSON input from stdin
input=$(cat)

# Extract information from JSON
MODEL=$(echo "$input" | jq -r '.model.display_name')
DIR=$(echo "$input" | jq -r '.workspace.current_dir' | sed 's|.*/||')
CONTEXT_SIZE=$(echo "$input" | jq -r '.context_window.context_window_size')
USAGE=$(echo "$input" | jq '.context_window.current_usage')

# Calculate token usage
if [ "$USAGE" != "null" ]; then
    INPUT_TOKENS=$(echo "$USAGE" | jq '.input_tokens')
    CACHE_CREATE=$(echo "$USAGE" | jq '.cache_creation_input_tokens')
    CACHE_READ=$(echo "$USAGE" | jq '.cache_read_input_tokens')

    # Total tokens used in context
    CURRENT_TOKENS=$((INPUT_TOKENS + CACHE_CREATE + CACHE_READ))
    PERCENT_USED=$((CURRENT_TOKENS * 100 / CONTEXT_SIZE))

    # Format: [Model] 📁 dir | 120182/200000 (60%)
    echo "[$MODEL] 📁 $DIR | ${CURRENT_TOKENS}/${CONTEXT_SIZE} (${PERCENT_USED}%)"
else
    # No usage yet (start of session)
    echo "[$MODEL] 📁 $DIR | 0/${CONTEXT_SIZE} (0%)"
fi
