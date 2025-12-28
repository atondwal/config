#!/bin/sh
# needs: zsh, xmonad, urxvt, git
# provides: vim, emacs, color schemes, conky

here=$(pwd)

ln -s "$here/bin" ~/bin
ln -s "$here/.gitconfig" ~/.gitconfig

mkdir -p ~/.config/nvim/
ln -s "$here/init.vim" ~/.config/nvim/init.vim
mkdir -p ~/.emacs/
ln -s "$here/init.el" ~/.emacs/init.el
ln -s "$here/.spacemacs" ~/.spacemacs

ln -s "$here/.xmonad" ~/.xmonad
ln -s "$here/.conkyrc" ~/.conkyrc
ln -s "$here/.Xdefaults" ~/.Xdefaults
ln -s "$here/.urxvt-perl" ~/.urxvt-perl
mkdir -p ~/.fonts/
ln -s "$here/input.ttf" ~/.fonts/

# Initialize and update git submodules if not already present
git submodule init
git submodule update --recursive

ln -s "$here/windelicato/.colors" ~/.colors
for rcfile in .zshrc .zprofile .zshenv .zlogin .zlogout; do
  ln -s "$here/$rcfile" ~/"$rcfile"
done
