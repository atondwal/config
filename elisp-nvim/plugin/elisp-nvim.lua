-- Plugin entry point for elisp-nvim
-- Auto-loads when Neovim starts if installed via plugin manager

if vim.g.loaded_elisp_nvim then
  return
end
vim.g.loaded_elisp_nvim = 1

-- Default configuration (can be overridden via setup())
require('elisp-nvim').setup({
  auto_start = false, -- Don't auto-start, wait for :ElispStart
  debug = true,
  startup_elisp = vim.fn.stdpath('config') .. '/elisp-nvim/elisp/nvim-bridge.el',
})
