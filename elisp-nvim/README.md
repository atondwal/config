# Elisp Interpreter for Neovim

Run Emacs Lisp packages (like dired, magit, org-mode) inside Neovim by bridging to a headless Emacs daemon.

## Architecture

```
┌─────────────────────────────────────────┐
│  Neovim (UI)                            │
│  ┌─────────────────────────────────┐    │
│  │  elisp-nvim.lua                 │    │
│  │  - RPC server                   │    │
│  │  - Buffer sync to Emacs         │    │
│  │  - Command execution            │    │
│  └──────────┬──────────────────────┘    │
└─────────────┼──────────────────────────┘
              │ stdio / JSON-RPC
              ↓
┌─────────────────────────────────────────┐
│  Emacs Daemon (headless)                │
│  ┌─────────────────────────────────┐    │
│  │  nvim-bridge.el                 │    │
│  │  - Override buffer-* functions  │    │
│  │  - Override window-* functions  │    │
│  │  - Forward to Neovim            │    │
│  └─────────────────────────────────┘    │
│  ┌─────────────────────────────────┐    │
│  │  User's Elisp Code              │    │
│  │  - dired, magit, spacemacs...   │    │
│  └─────────────────────────────────┘    │
└─────────────────────────────────────────┘
```

## Protocol

Uses JSON-RPC 2.0 over stdio for bidirectional communication:
- Neovim → Emacs: Execute elisp, eval expressions
- Emacs → Neovim: Buffer operations, window management, display updates

## Key Components

1. **lua/elisp-nvim/init.lua** - Neovim plugin that starts Emacs daemon and handles RPC
2. **elisp/nvim-bridge.el** - Elisp shims that forward Emacs APIs to Neovim
3. **elisp/dired-nvim.el** - Dired integration (proof of concept)

## Status

🚧 Early proof-of-concept stage

## References

- [EPC (Emacs Procedure Call)](https://github.com/kiwanami/emacs-epc) - RPC stack for Emacs
- [Emacs JSON-RPC](https://www.gnu.org/software/emacs/manual//html_node/elisp/JSONRPC-Overview.html)
- [Python-EPC](https://python-epc.readthedocs.io/en/latest/) - Example of bridging Emacs
