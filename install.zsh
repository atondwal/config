#!/bin/zsh
# needs: zsh, xmonad, urxvt, git
# provides: vim, emacs, color schemes, conky

ln -s `pwd`/bin ~/bin
ln -s `pwd`/.gitconfig ~/.gitconfig

mkdir -p ~/.config/nvim/
ln -s `pwd`/init.vim ~/.config/nvim/init.vim
mkdir -p ~/.emacs/
ln -s `pwd`/init.el ~/.emacs/init.el

ln -s `pwd`/.xmonad ~/.xmonad
ln -s `pwd`/.conkyrc ~/.conkyrc
ln -s `pwd`/.Xdefaults ~/.Xdefaults
ln -s `pwd`/.urxvt-perl ~/.urxvt-perl
mkdir -p ~/.fonts/
ln -s `pwd`/input.ttf ~/.fonts/

# Initialize and update git submodules if not already present
git submodule init
git submodule update --recursive

ln -s `pwd`/windelicato/.colors ~/.colors
setopt EXTENDED_GLOB
for rcfile in .zprezto/runcoms/^README.md(.N); do
  ln -s `pwd`/"$rcfile" ~"/.${rcfile:t}"
done
