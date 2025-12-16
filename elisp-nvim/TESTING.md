# Testing Guide

## Prerequisites

- Emacs 27+ installed (`emacs --version`)
- Neovim 0.9+ installed (`nvim --version`)

## Running Tests

### Standalone Emacs Tests

Test the Emacs bridge without Neovim:

```bash
cd ~/config
emacs --batch --load elisp-nvim/test/test-standalone.el
```

Expected output:
```
=== Testing nvim-bridge.el ===

Test 1: Bridge loaded... ✓ PASS
Test 2: RPC handlers registered... ✓ PASS (2 handlers)
Test 3: Eval handler exists... ✓ PASS
Test 4: Ping handler exists... ✓ PASS
Test 5: Eval handler works... ✓ PASS
Test 6: Ping handler works... ✓ PASS
Test 7: Dired function exists... ✓ PASS
Test 8: Dired can generate listing... ✓ PASS
   Generated XX lines

=== All Tests Complete ===
```

### Testing Dired Directly

```bash
emacs --batch --eval "(progn (require 'dired) (princ (with-current-buffer (dired-noselect \".\") (buffer-string))))"
```

Should output a formatted directory listing.

### Testing Elisp Evaluation

```bash
# Basic arithmetic
emacs --batch --eval "(princ (+ 1 2 3))"

# String concatenation
emacs --batch --eval "(princ (concat \"Hello\" \" \" \"Emacs\"))"

# List operations
emacs --batch --eval "(princ (length (list 1 2 3 4 5)))"
```

## Manual Testing with Neovim

1. Start Neovim:
   ```bash
   nvim
   ```

2. In Neovim:
   ```vim
   :ElispStart
   :ElispEval (+ 1 2 3)
   :ElispEval (concat "Hello" " from " "Elisp")
   :ElispDired .
   ```

## Troubleshooting

### Bridge won't load

Check Emacs can find the file:
```bash
emacs --batch --load elisp-nvim/elisp/nvim-bridge.el --eval "(princ \"Loaded\")"
```

### Dired doesn't work

Verify Emacs has dired:
```bash
emacs --batch --eval "(require 'dired)" --eval "(princ \"OK\")"
```

### JSON parsing errors

Check JSON syntax:
```bash
echo '{"jsonrpc":"2.0","id":1,"method":"ping"}' | python3 -m json.tool
```

## Test Results (2025-12-16)

Tested on:
- **OS**: Ubuntu 24.04 LTS
- **Emacs**: GNU Emacs 29.3
- **Status**: ✅ All standalone tests passing

### What Works
- ✅ Bridge loading
- ✅ RPC handler registration
- ✅ Elisp evaluation via handlers
- ✅ Dired buffer generation
- ✅ Basic JSON-RPC message structure

### Known Issues
- ⚠️ Full stdio RPC loop needs more testing with actual Neovim
- ⚠️ `read-char` in batch mode may need adjustment for real-time RPC
- ⚠️ Buffer synchronization not yet tested end-to-end

### Next Steps
1. Test full Neovim ↔ Emacs RPC communication
2. Implement more buffer operation handlers
3. Add interactive dired keybindings
4. Test with larger/complex elisp packages
