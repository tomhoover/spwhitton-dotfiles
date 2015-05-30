#!/bin/sh
. $HOME/src/dotfiles/.shenv

# Bootstrap home directory after dotfiles repository cloned.  This
# script should be POSIX.

set -e
STOWURL="http://ftp.gnu.org/gnu/stow/stow-2.2.0.tar.gz"
cd $HOME

# seems to be needed for a find command run at some point; not sure
# where but I see the output of find failing to find this dir
mkdir -p $HOME/local/src

# 1. install our two small helper scripts

if ! which stow >/dev/null; then
    cd $HOME/local/src
    rm -rf stow-*
    curl -O $STOWURL
    tar xfz stow-*
    cd $(basename $STOWURL | sed -e 's/.tar.gz//')

    # we need to use gmake on BSD
    if which gmake; then
        ./configure --prefix=$HOME/local && gmake install
    else
        ./configure --prefix=$HOME/local && make install
    fi

    cd $HOME
fi
if ! which mr >/dev/null; then
    mkdir -p $HOME/local/bin
    cp $HOME/src/dotfiles/bin/mr $HOME/local/bin
fi

# 2. check those installs

hash -r
if ! which stow >/dev/null; then
    echo "still can't find stow :(" >&2
    exit 1
fi
if ! which mr >/dev/null; then
    echo "still can't find mr :(" >&2
    exit 1
fi

# 3. use those scripts to do the setup

ln -sf $HOME/src/dotfiles/home-mrconfig $HOME/.mrconfig

# remove extra stuff that unskel doesn't
rm -f $HOME/.cabal/config

# unskel can't catch these for root cos they're slightly different on
# Debian Stable
if [ "$USER" = "root" ]; then
    rm -f $HOME/.bashrc $HOME/.profile
fi

cd $HOME/src/dotfiles
mr stow
mr fixups
cd $HOME

echo "Dotfiles should be stowed into $HOME.  You should check out the host-specific branch and restow, if it exists, and then run 'mr --force co' to get all my other repos."
