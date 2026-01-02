#!/bin/sh
# needs: zsh, xmonad, urxvt, git
# provides: vim, color schemes, conky

here=$(pwd)

ln -sfn "$here/bin" ~/bin
ln -sf "$here/.gitconfig" ~/.gitconfig

mkdir -p ~/.config/nvim/
ln -sf "$here/init.vim" ~/.config/nvim/init.vim
ln -sf "$here/.spacemacs" ~/.spacemacs

ln -sfn "$here/.xmonad" ~/.xmonad
ln -sf "$here/.conkyrc" ~/.conkyrc
ln -sf "$here/.Xdefaults" ~/.Xdefaults
ln -sfn "$here/.urxvt-perl" ~/.urxvt-perl
mkdir -p ~/.fonts/
ln -sf "$here/input.ttf" ~/.fonts/

# Initialize and update git submodules if not already present
git submodule init
git submodule update --recursive

ln -sfn "$here/windelicato/.colors" ~/.colors
for rcfile in .zshrc .zprofile .zshenv .zlogin .zlogout; do
  ln -sf "$here/$rcfile" ~/"$rcfile"
done

# Claude Code config
mkdir -p ~/.claude
for f in CLAUDE.md settings.json; do
  [ -f ~/.claude/$f ] && [ ! -L ~/.claude/$f ] && mv ~/.claude/$f ~/.claude/$f.bak
  ln -sf "$here/.claude/$f" ~/.claude/$f
done
ln -sfn "$here/.claude/commands" ~/.claude/commands
ln -sfn "$here/.claude/skills" ~/.claude/skills
ln -sfn "$here/.claude/hooks" ~/.claude/hooks
