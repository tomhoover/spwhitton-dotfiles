#!/bin/sh
. $HOME/src/dotfiles/.shenv

# Bootstrap home directory after dotfiles repository cloned.  This
# script should be POSIX.

set -e
STOWURL="http://ftp.gnu.org/gnu/stow/stow-2.2.0.tar.gz"
cd $HOME

# 1. install our two small helper scripts

if ! which stow; then
    mkdir -p $HOME/local/src
    cd $HOME/local/src
    curl -O $STOWURL
    tar xfz stow-*
    cd stow-*
    ./configure --prefix=$HOME/local && make install
    cd $HOME
fi
if ! which mr; then
    mkdir -p $HOME/local/bin
    cp $HOME/src/dotfiles/bin/mr $HOME/local/bin
fi

# 2. check those installs

hash -r
if ! which stow; then
    echo "still can't find stow :(" >&2
    exit 1
fi
if ! which mr; then
    echo "still can't find mr :(" >&2
    exit 1
fi

# 3. use those scripts to do the setup

ln -sf $HOME/src/dotfiles/home-mrconfig $HOME/.mrconfig
$HOME/src/dotfiles/bin/unskel
mr stow
mr fixups

echo "Dotfiles should be stowed into $HOME.  You should check out the host-specific branch and restow, if it exists, and then run 'mr --force co' to get all my other repos."
