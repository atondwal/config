---
description: Open current Claude session transcript in gvim
allowed-tools: "Bash(gvim:*, ls:*, pwd:*, find:*, stat:*, fzf:*, head:*, tail:*, jq:*)"
---

# Open Session in GVim

Open a Claude Code session transcript in gvim.

## Find the project directory:
Convert CWD to Claude's naming: `/home/atondwal` → `-home-atondwal`

## Find and select session:
List recent session files (excluding agent-* files) with timestamps and sizes. Use fzf or similar if multiple candidates exist, or ask the user to pick.

Useful context for picking:
- File size (larger = longer conversation)
- Modification time
- First user message: `head -5 <file> | jq -r 'select(.type=="human") | .message.content' 2>/dev/null`

## Open selected file:
```bash
gvim <session-file> &
```
