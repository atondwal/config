# Patch Claude Code's "btw" Side Questions Feature

Enable the hidden "btw" side questions feature that's implemented but disabled.

## Step 1: Find and backup cli.js

```bash
CLI_JS=$(which claude | xargs readlink -f | xargs dirname)/cli.js
echo "cli.js: $CLI_JS"
ls -la "$CLI_JS"
```

```bash
BACKUP="/tmp/claude-cli-backup-$(date +%Y%m%d-%H%M%S).js"
cp "$CLI_JS" "$BACKUP"
echo "Backup: $BACKUP"
```

## Step 2: Find the stubbed functions

### Pattern 1: Detection function

Returns `{isBtw:!1,question:""}` (always false). Find its name:

```bash
grep -oP 'function \K[a-zA-Z0-9]+(?=\(A\)\{return\{isBtw:!1,question:""\}\})' "$CLI_JS"
```

Expected: A function name like `sW1` or `AV1`.

### Pattern 2: Highlighting function

Returns `[]` (no highlights) and appears right before the detection function:

```bash
DETECT=$(grep -oP 'function \K[a-zA-Z0-9]+(?=\(A\)\{return\{isBtw:!1,question:""\}\})' "$CLI_JS")
grep -oP 'function \K[a-zA-Z0-9]+(?=\(A\)\{return\[\]\}function '"$DETECT"')' "$CLI_JS"
```

Expected: A function name like `nl2` or `Ba2`.

### Pattern 3: Verify the btw regex exists

The regex for matching "btw" should exist but be unused:

```bash
grep -oP '/\^btw\\b/[gi]+' "$CLI_JS"
```

Expected: `/^btw\b/gi` or similar.

## Step 3: Apply patches

Replace DETECT_FUNC and HIGHLIGHT_FUNC with names from Step 2:

```python
cli_js = "CLI_JS_PATH"  # Replace with path from Step 1

DETECT_FUNC = "DETECT_FUNC"      # From Step 2 Pattern 1
HIGHLIGHT_FUNC = "HIGHLIGHT_FUNC" # From Step 2 Pattern 2

with open(cli_js, 'r') as f:
    content = f.read()

# Patch 1: Detection - actually detect "btw" prefix
old1 = f'function {DETECT_FUNC}(A){{return{{isBtw:!1,question:""}}}}'
new1 = f'function {DETECT_FUNC}(A){{var m=A.match(/^btw\\b/i);return m?{{isBtw:!0,question:A.slice(m[0].length).trim()}}:{{isBtw:!1,question:""}}}}'
if old1 in content:
    content = content.replace(old1, new1)
    print(f"✓ Patch 1: Detection ({DETECT_FUNC}) - applied")
elif 'isBtw:!0' in content:
    print(f"○ Patch 1: Detection - already applied")
else:
    print(f"✗ Patch 1: Detection - pattern not found")

# Patch 2: Highlighting - highlight "btw" prefix
old2 = f'function {HIGHLIGHT_FUNC}(A){{return[]}}'
new2 = f'function {HIGHLIGHT_FUNC}(A){{var m=A.match(/^btw\\b/i);return m?[{{start:0,end:m[0].length}}]:[]}}'
if old2 in content:
    content = content.replace(old2, new2)
    print(f"✓ Patch 2: Highlighting ({HIGHLIGHT_FUNC}) - applied")
elif f'function {HIGHLIGHT_FUNC}(A){{var m=A.match' in content:
    print(f"○ Patch 2: Highlighting - already applied")
else:
    print(f"✗ Patch 2: Highlighting - pattern not found")

with open(cli_js, 'w') as f:
    f.write(content)
print("Done!")
```

## Step 4: Verify

```bash
grep -oP 'isBtw:!0' "$CLI_JS" && echo "Detection patch OK"
grep -oP 'start:0,end:m\[0\]\.length' "$CLI_JS" && echo "Highlighting patch OK"
```

## Usage

Restart Claude Code, then type: `btw what is a monad?`

The "btw" prefix highlights in warning color and the question is processed as a side question (no tools, quick response).

## Debugging: When patterns aren't found

If Step 2 patterns aren't found, the code structure changed. Investigate:

### Find the btw regex

```bash
grep -oP '.{30}btw.{30}' "$CLI_JS" | grep -i regex\|match\|test
```

### Find side question handling

```bash
grep -oP '.{20}side_question.{50}' "$CLI_JS" | head -3
```

### Find stub functions returning empty

Look for functions that return `!1` (false) or `[]` near btw-related code:

```bash
grep -oP 'function [a-zA-Z0-9]+\(A\)\{return(\[\]|\{[^}]*:!1)' "$CLI_JS" | head -10
```

### Find the tip definition

The btw tip still shows, find where it's defined:

```bash
grep -oP '.{20}btw-side-question.{50}' "$CLI_JS"
```

### Understand the feature

The "btw" feature lets users ask quick side questions:
1. User types `btw what is X?`
2. Detection function checks if input starts with "btw"
3. If detected, highlighting function marks "btw" for warning color
4. Side question handler processes the question without tools
5. Response appears inline without interrupting main conversation

The feature is fully implemented but disabled via stub functions that always return false/empty.

### Find the handler

```bash
grep -oP '.{50}isBtw.{100}' "$CLI_JS" | head -3
```

This shows where `isBtw` is checked - trace back to find detection function.

## Revert

```bash
cp "$BACKUP" "$CLI_JS"
```
