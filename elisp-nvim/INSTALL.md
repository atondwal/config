# Installation

## Prerequisites

- Neovim 0.9+ (for Lua API)
- Emacs 27+ (with JSON support)
- Working `emacs` in PATH

## Installation with lazy.nvim

Add to your Neovim config:

```lua
{
  dir = "~/config/elisp-nvim", -- or path to this repo
  name = "elisp-nvim",
  config = function()
    require('elisp-nvim').setup({
      emacs_path = "emacs",  -- or "/usr/bin/emacs"
      auto_start = false,     -- set true to auto-start on launch
      debug = true,
    })
  end,
}
```

## Installation with Packer

```lua
use {
  '~/config/elisp-nvim',
  config = function()
    require('elisp-nvim').setup()
  end
}
```

## Manual Installation

1. Clone or symlink this directory to your Neovim config:
   ```bash
   ln -s ~/config/elisp-nvim ~/.config/nvim/lua/elisp-nvim
   ```

2. Add to your `init.lua`:
   ```lua
   require('elisp-nvim').setup()
   ```

## Usage

### Basic Commands

```vim
:ElispStart              " Start the Emacs daemon
:ElispStop               " Stop the Emacs daemon
:ElispEval (+ 1 2)       " Evaluate elisp expression
:ElispEval               " Prompt for elisp to evaluate
:ElispDired ~/Projects   " Open dired for directory
:ElispDired              " Open dired for current directory
```

### From Lua

```lua
local elisp = require('elisp-nvim')

-- Start daemon
elisp.start_emacs()

-- Evaluate elisp
elisp.eval('(message "Hello from Emacs!")', function(result, error)
  if error then
    print("Error:", vim.inspect(error))
  else
    print("Result:", vim.inspect(result))
  end
end)

-- Dired example
elisp.eval('(dired "/home/user")')
```

### Example init.lua Configuration

```lua
-- In your init.lua or plugin config

require('elisp-nvim').setup({
  emacs_path = "emacs",
  auto_start = true,  -- Start emacs daemon on Neovim launch
  debug = false,      -- Disable debug logging in production
})

-- Keybindings
vim.keymap.set('n', '<leader>ee', ':ElispEval<CR>', { desc = 'Eval elisp' })
vim.keymap.set('n', '<leader>ed', ':ElispDired<CR>', { desc = 'Dired current dir' })
vim.keymap.set('n', '<leader>es', ':ElispStart<CR>', { desc = 'Start elisp daemon' })

-- Load your Emacs init.el on startup
vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    require('elisp-nvim').eval('(load "~/config/init.el")')
  end
})
```

## Troubleshooting

### "Emacs not running" error

Make sure you've started the daemon:
```vim
:ElispStart
```

### Check if Emacs is working

```bash
emacs --batch --eval "(princ (+ 1 2))"
```

Should output `3`.

### Enable debug logging

```lua
require('elisp-nvim').setup({
  debug = true
})
```

Then check `:messages` in Neovim.

### Emacs process not found

Verify emacs is in PATH:
```bash
which emacs
```

If not, specify full path in config:
```lua
require('elisp-nvim').setup({
  emacs_path = "/usr/local/bin/emacs"
})
```

## What Works

- ✅ Starting headless Emacs daemon
- ✅ Bidirectional JSON-RPC communication
- ✅ Evaluating arbitrary elisp expressions
- ✅ Basic dired integration (read-only display)
- ✅ Buffer content forwarding to Neovim

## What Doesn't Work (Yet)

- ❌ Full dired interaction (keybindings, file operations)
- ❌ Emacs window management → Neovim splits
- ❌ Magit, org-mode, other complex packages
- ❌ Emacs overlays/faces → Neovim highlights
- ❌ Bidirectional buffer sync (Neovim edits → Emacs)
- ❌ Emacs keybindings

## Roadmap

See `README.md` for architecture and development plans.
