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

git clone https://github.com/windelicato/dotfiles windelicato
ln -s `pwd`/windelicato/.colors ~/.colors

git clone --recursive https://github.com/atondwal/prezto.git ".zprezto"
setopt EXTENDED_GLOB
for rcfile in .zprezto/runcoms/^README.md(.N); do
  ln -s "$rcfile" `pwd`"/.${rcfile:t}"
done
