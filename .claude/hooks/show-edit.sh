#!/bin/bash
# Show edited/read file in nvim after Edit/MultiEdit/Write/Read tools

# Read JSON input from stdin
INPUT=$(cat)

# Log to debug
echo "Hook triggered at $(date)" >> /tmp/claude-hook-debug.log
echo "Input: $INPUT" >> /tmp/claude-hook-debug.log

# Extract tool name and file path from JSON
TOOL_NAME=$(echo "$INPUT" | jq -r '.tool_name // empty')
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')

echo "Tool: $TOOL_NAME" >> /tmp/claude-hook-debug.log
echo "File: $FILE_PATH" >> /tmp/claude-hook-debug.log

# Only process Edit, MultiEdit, Write, and Read tools
if [[ "$TOOL_NAME" != "Edit" && "$TOOL_NAME" != "MultiEdit" && "$TOOL_NAME" != "Write" && "$TOOL_NAME" != "Read" ]]; then
    exit 0
fi

if [[ -z "$FILE_PATH" ]]; then
    exit 0
fi

# Check if nvim server is running first
SOCKET="/tmp/nvim-claude-$(pwd | md5sum | cut -d' ' -f1)"
if ! timeout 0.1 nvr --servername "$SOCKET" --remote-expr "1" &>/dev/null 2>&1; then
    echo "No nvim server running at $SOCKET, skipping" >> /tmp/claude-hook-debug.log
    exit 0
fi

# Use claude-nvr to open the file (with CLAUDE_HOOK=1 to prevent auto-start)
echo "Using claude-nvr to open: $FILE_PATH" >> /tmp/claude-hook-debug.log
CLAUDE_HOOK=1 claude-nvr --remote "$FILE_PATH" >> /tmp/claude-hook-debug.log 2>&1
echo "claude-nvr command completed" >> /tmp/claude-hook-debug.log

# If there's a line number from Edit tool, jump to it
if [[ "$TOOL_NAME" == "Edit" ]]; then
    # Find first line in new_string that differs from old_string
    OLD_STRING=$(echo "$INPUT" | jq -r '.tool_input.old_string // empty')
    NEW_STRING=$(echo "$INPUT" | jq -r '.tool_input.new_string // empty')
    # Get first line of new_string not in old_string
    SEARCH_LINE=$(comm -23 <(echo "$NEW_STRING" | sort -u) <(echo "$OLD_STRING" | sort -u) | head -1)
    if [[ -z "$SEARCH_LINE" ]]; then
        # Fallback to first line of new_string
        SEARCH_LINE=$(echo "$NEW_STRING" | head -1)
    fi
    if [[ -n "$SEARCH_LINE" ]]; then
        CLAUDE_HOOK=1 claude-nvr --remote-send ":/$SEARCH_LINE<CR>zz" &>/dev/null
    fi
elif [[ "$TOOL_NAME" == "Read" ]]; then
    # For Read tool, check if there's an offset (starting line)
    OFFSET=$(echo "$INPUT" | jq -r '.tool_input.offset // empty')
    echo "Read tool - OFFSET: '$OFFSET'" >> /tmp/claude-hook-debug.log
    if [[ -n "$OFFSET" && "$OFFSET" != "null" && "$OFFSET" != "" ]]; then
        # Jump to the specified line
        echo "Jumping to line $OFFSET" >> /tmp/claude-hook-debug.log
        CLAUDE_HOOK=1 claude-nvr --remote-send ":${OFFSET}<CR>zz" >> /tmp/claude-hook-debug.log 2>&1
    else
        echo "No offset to jump to" >> /tmp/claude-hook-debug.log
    fi
fi

exit 0
