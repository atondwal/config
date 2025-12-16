-- Basic tests for elisp-nvim
-- Run with: nvim --headless -u test/test-basic.lua

local elisp = require('elisp-nvim')

-- Configure
elisp.setup({
  debug = true,
  emacs_path = "emacs"
})

print("\n=== elisp-nvim Test Suite ===\n")

-- Test 1: Start daemon
print("Test 1: Starting Emacs daemon...")
local started = elisp.start_emacs()
if started then
  print("✓ Daemon started")
else
  print("✗ Failed to start daemon")
  os.exit(1)
end

-- Wait for startup
vim.wait(2000, function() return false end)

-- Test 2: Simple arithmetic
print("\nTest 2: Evaluating (+ 1 2 3)...")
elisp.eval("(+ 1 2 3)", function(result, error)
  if error then
    print("✗ Error:", vim.inspect(error))
  elseif result and result.result == "6" then
    print("✓ Got correct result:", result.result)
  else
    print("✗ Unexpected result:", vim.inspect(result))
  end
end)

vim.wait(1000, function() return false end)

-- Test 3: String evaluation
print("\nTest 3: Evaluating (concat \"Hello\" \" \" \"World\")...")
elisp.eval('(concat "Hello" " " "World")', function(result, error)
  if error then
    print("✗ Error:", vim.inspect(error))
  else
    print("✓ Result:", vim.inspect(result))
  end
end)

vim.wait(1000, function() return false end)

-- Test 4: List evaluation
print("\nTest 4: Evaluating (list 1 2 3 4)...")
elisp.eval("(list 1 2 3 4)", function(result, error)
  if error then
    print("✗ Error:", vim.inspect(error))
  else
    print("✓ Result:", vim.inspect(result))
  end
end)

vim.wait(1000, function() return false end)

-- Test 5: Buffer operations (if Neovim RPC works)
print("\nTest 5: Testing buffer operations...")
elisp.eval("(nvim-bridge-buffer-string)", function(result, error)
  if error then
    print("✗ Error:", vim.inspect(error))
  else
    print("✓ Buffer read works:", vim.inspect(result))
  end
end)

vim.wait(1000, function() return false end)

-- Cleanup
print("\nCleaning up...")
elisp.stop_emacs()

print("\n=== Tests Complete ===\n")
vim.cmd("qall!")
