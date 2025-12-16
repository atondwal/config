#!/bin/bash
# Test the Emacs RPC bridge

echo "Testing Emacs JSON-RPC bridge..."

# Start Emacs in background
emacs --batch --load elisp-nvim/elisp/nvim-bridge.el --eval "(nvim-bridge-start-server)" &
EMACS_PID=$!

# Wait a moment for startup
sleep 1

# Test 1: Send a ping request
echo '{"jsonrpc":"2.0","id":1,"method":"ping","params":{}}'

# Test 2: Send an eval request
echo '{"jsonrpc":"2.0","id":2,"method":"eval","params":{"code":"(+ 1 2 3)"}}'

# Wait for responses
sleep 2

# Cleanup
kill $EMACS_PID 2>/dev/null

echo "Test complete"
