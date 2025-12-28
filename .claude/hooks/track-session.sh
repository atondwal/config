#!/bin/bash
# Track current Claude Code session for cc-edit-session
INPUT=$(cat)
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
CWD=$(echo "$INPUT" | jq -r '.cwd // empty')

if [ -n "$SESSION_ID" ]; then
    echo "$SESSION_ID" > ~/.claude/.current-session-id
fi
if [ -n "$CWD" ]; then
    echo "$CWD" > ~/.claude/.current-session-cwd
fi
