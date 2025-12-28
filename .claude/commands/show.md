---
description: Show code visually in a vim/neovim instance
allowed-tools: "Bash(nvr:*, nvim:*, vim:*, which:*, mlterm:*, claude-nvr:*)"
argument-hint: "[file:line] or 'start' to initialize"
---

# Show

Visually demonstrate code locations, changes, and navigation in a live vim/neovim instance.

## If argument is "start":
Start a new neovim server using claude-nvr:
```bash
claude-nvr start
```

## For showing code (file:line or just file):
Use claude-nvr which manages the socket automatically:
- Start if needed: `claude-nvr start`
- Open files: `claude-nvr --remote <file>`
- Jump to line: `claude-nvr --remote-send ":<line><CR>zz"`
- Highlight: `claude-nvr --remote-send "V"`
- Search: `claude-nvr --remote-send "/<pattern><CR>"`
- Paint lines: `claude-nvr highlight <start_line> [end_line]`
- Clear paint: `claude-nvr clear-highlights`

## Always:
- Provide clear narration about what you're showing in the editor
- Use visual selection to highlight important code sections
- Highlight the most relevant line (`V`) after jumping to locations
- If nvr is not available but vim has clientserver support, use `vim --servername CLAUDE` instead

$ARGUMENTS

