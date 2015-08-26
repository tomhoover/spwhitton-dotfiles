#!/bin/sh

USB=/Volumes/SPWHITTON

# setup

if ! [ -d "$HOME/src/dotfiles/.git" ]; then
    git clone https://github.com/spwhitton/dotfiles.git $HOME/src/dotfiles
    # this is currently out of action because GNU stow installs but
    # doesn't seem to actually do anything on Mac OS
    # $HOME/src/dotfiles/bin/bstraph.sh
    cp $HOME/src/dotfiles/{.zshrc,.shenv} $HOME # instead
fi
pkill firefox
/Applications/Firefox.app/Contents/MacOS/firefox -private-window >/dev/null 2>/dev/null &
mkdir -p $HOME/.ssh
cp $USB/lib/{id_putty,known_hosts} $HOME/.ssh
chmod 600 $HOME/.ssh/id_putty

# run the shell

cd $HOME
/bin/zsh

# cleanup

pkill firefox
echo "Please wait, securely deleting files..."
rm -rfP $HOME/.ssh/id_putty $HOME/Downloads/*
