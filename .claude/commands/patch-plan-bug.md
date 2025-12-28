# Patch Claude Code Plan Modification Bug

Fix GitHub issue #10861 where Ctrl+G plan edits aren't passed to Claude.

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

## Step 2: Find current patterns

### Pattern 1: Permission handler

The permission handler computes whether inputs changed using `inputsEquivalent`. Find it:

```bash
grep -oP '[A-Za-z]=B\.inputsEquivalent\?!B\.inputsEquivalent\([A-Za-z],[A-Za-z]\):\!1' "$CLI_JS"
```

Expected output like: `S=B.inputsEquivalent?!B.inputsEquivalent(G,N):!1`

The variable before `=` (e.g. `S`) holds the "modified" flag.
The second argument in the call (e.g. `N`) is the input object that should carry `userModified`.

### Pattern 2: UI component

The UI passes a plan object. Find it:

```bash
grep -oP '[A-Za-z]\?\{\}:\{plan:[A-Za-z]\}' "$CLI_JS"
```

Expected output like: `V?{}:{plan:E}`

We need to add `userModified:$` to this object, where `$` is the state tracking if user modified the plan.

### Pattern 3: Verify $ is the modification state

The modification state should be a boolean useState that's set to `!0` when Ctrl+G edits happen:

```bash
grep -oP '\[\$,[A-Za-z]\]=[A-Za-z0-9]+\.useState\(!1\)' "$CLI_JS" | head -1
```

Expected: `[$,L]=XX.useState(!1)` - confirms `$` is the state, `L` is setter.

```bash
grep -oP 'L\(!0\)' "$CLI_JS" | head -1
```

Expected: `L(!0)` - confirms setter is called with `true`.

## Step 3: Apply patches

Replace the variable names below with what you found in Step 2:

```python
cli_js = "CLI_JS_PATH"  # Replace with path from Step 1

# From Step 2 Pattern 1: e.g. S=B.inputsEquivalent?!B.inputsEquivalent(G,N):!1
MODIFIED_VAR = "S"      # Variable holding modified flag
INPUT_OBJ = "N"         # Input object that carries userModified

# From Step 2 Pattern 2: e.g. V?{}:{plan:E}
PLAN_PATTERN = "V?{}:{plan:E}"

# From Step 2 Pattern 3: state variable (usually $)
STATE_VAR = "$"

with open(cli_js, 'r') as f:
    content = f.read()

# Patch 1: Permission handler - check userModified first
old1 = f'{MODIFIED_VAR}=B.inputsEquivalent?!B.inputsEquivalent(G,{INPUT_OBJ}):!1'
new1 = f'{MODIFIED_VAR}={INPUT_OBJ}.userModified??(B.inputsEquivalent?!B.inputsEquivalent(G,{INPUT_OBJ}):!1)'
if old1 in content:
    content = content.replace(old1, new1)
    print(f"✓ Patch 1: Permission handler - applied")
elif new1 in content:
    print(f"○ Patch 1: Permission handler - already applied")
else:
    print(f"✗ Patch 1: Permission handler - pattern '{old1}' not found")

# Patch 2: UI component - pass userModified state
old2 = PLAN_PATTERN
new2 = PLAN_PATTERN.replace('}', f',userModified:{STATE_VAR}}}')
if old2 in content:
    content = content.replace(old2, new2)
    print(f"✓ Patch 2: UI component - applied")
elif new2 in content:
    print(f"○ Patch 2: UI component - already applied")
else:
    print(f"✗ Patch 2: UI component - pattern '{old2}' not found")

with open(cli_js, 'w') as f:
    f.write(content)
print("Done!")
```

## Step 4: Verify

```bash
grep -o 'userModified??' "$CLI_JS" && echo "Permission patch OK"
grep -oP 'userModified:\$' "$CLI_JS" && echo "UI patch OK"
```

## Debugging: When patterns aren't found

If Step 2 patterns aren't found, the code structure changed. Investigate:

### Find the plan editing flow

```bash
# Find Ctrl+G handler (opens external editor)
grep -oP '.{50}ctrl.{100}' "$CLI_JS" | grep -i 'plan\|editor' | head -3
```

### Find where plan is submitted

```bash
# Find plan submission with telemetry
grep -oP '.{30}tengu_plan_exit.{100}' "$CLI_JS" | head -1
```

### Find inputsEquivalent usage

```bash
# All inputsEquivalent patterns
grep -oP '.{20}inputsEquivalent.{80}' "$CLI_JS"
```

### Find useState for modification tracking

```bash
# Boolean states near plan code
grep -oP '\[[A-Za-z$]+,[A-Za-z]+\]=[A-Za-z0-9]+\.useState\(!1\)' "$CLI_JS"
```

### Understand the fix

The bug: When user edits plan via Ctrl+G, Claude doesn't see the edits.

Root cause: The permission system uses `inputsEquivalent` to detect changes, but this compares tool inputs, not user modifications.

Fix:
1. UI tracks user modifications in a state variable (e.g. `$`)
2. UI passes `userModified: true` when submitting edited plan
3. Permission handler checks `userModified` before falling back to `inputsEquivalent`

Use this understanding to find equivalent patterns in updated code.

## Revert

```bash
cp "$BACKUP" "$CLI_JS"
```
