#!/bin/sh

. $HOME/src/dotfiles/.shenv

# Bootstrap home directory after dotfiles repository successfully
# cloned (see ~/bin/insinuate-dotfiles).  This script should
# definitely be POSIX sh

set -e
cd "$HOME"

# ---- Perform the bootstrap

mr -t --config src/dotfiles/home-mrconfig -d $HOME/src/dotfiles fixups
mr -d src/dotfiles stow

echo "I: dotfiles bootstrap successful"

# ---- Check if there is a host-specific branch

if ! [ "$(hostname)" = "master" ] \
        && ( cd "$HOME/src/dotfiles" && git branch -a | grep origin/$(hostname) ); then
    echo "I: there is a host-specific dotfiles branch for this host"
    echo "I: consider checking it out"
fi
