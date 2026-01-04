#!/bin/sh
# needs: zsh, xmonad, urxvt, stow

set -e
cd "$(dirname "$0")"

mkdir -p ~/.config/nvim ~/.fonts ~/.claude
stow -v -t ~ .
