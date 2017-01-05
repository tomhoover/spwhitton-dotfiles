#!/bin/sh

set -e

cd "$HOME"
# make these dirs before loading .shenv
# then .shenv adds them to our PATH
mkdir -p "$HOME/local/src" "$HOME/local/bin"
. $HOME/src/dotfiles/.shenv

# Bootstrap home directory after dotfiles repository successfully
# cloned (see ~/bin/insinuate-dotfiles).  This script should
# definitely be POSIX sh

# use http and checksum the tarball, rather than https:// which often
# fails on Debian hosts due to old ca-certificates
STOWURL="http://ftp.gnu.org/gnu/stow/stow-2.2.0.tar.gz"
STOWSHA256SUM="8b89d79939cf9ae87d2f223bb36a3b2d0c66775b62aeb9953c6d33dab40d3c2b  stow-2.2.0.tar.gz"

# ---- Handle special case hosts

# On athena, clone URLs are different as they come from /home/git (and
# pushInsteadOf won't work because that can't append the required
# `.git').  So we must change to the athena branch now in order to get
# a fixed .mrconfig.
if [ "$(hostname -f)" = "athena.silentflame.com" ]; then
    cd $HOME/src/dotfiles
    git checkout athena
    cd $HOME
fi

# ---- Ensure we have both mr(1) and stow(1) in our PATH

if ! which stow >/dev/null; then
    (
        cd $HOME/local/src
        rm -rf stow-*
        curl -O $STOWURL
        stowsha256sum=$(sha256sum stow-*.tar.gz)
        if ! [ "$STOWSHA256SUM" = "$stowsha256sum" ]; then
            echo >&2 "bstraph.sh: ERROR: stow tarball checksum mismatch"
            rm -rf stow-*
            exit 1
        fi

        tar xfz stow-*.tar.gz
        cd $(basename $STOWURL | sed -e 's/.tar.gz//')

        # we need to use gmake on BSD
        if which gmake; then
            ./configure --prefix=$HOME/local && gmake install
        else
            ./configure --prefix=$HOME/local && make install
        fi
    )
fi

if ! which mr >/dev/null; then
    mkdir -p $HOME/local/bin
    cp $HOME/src/dotfiles/bin/mr $HOME/local/bin
fi

# ---- Verify mr(1) and stow(1) in our PATH

if ! which stow >/dev/null; then
    echo "still can't find stow :(" >&2
    exit 1
fi
if ! which mr >/dev/null; then
    echo "still can't find mr :(" >&2
    exit 1
fi

# ---- Perform the bootstrap

ln -sf $HOME/src/dotfiles/home-mrconfig $HOME/.mrconfig

# deal with deficiencies in ~/bin/unskel
rm -f "$HOME/.cabal/config"
# root's .bashrc and .profile can differ from /etc/skel
[ "$USER" = "root" ] && rm -f "$HOME/.bashrc" "$HOME/.profile"

# Hand over to code in ~/.mrconfig
(
    cd $HOME/src/dotfiles
    mr stow
    mr fixups
)

echo "I: dotfiles bootstrap successful"

# ---- Check if there is a host-specific branch

if ( cd "$HOME/src/dotfiles" && git branch -a | grep origin/$(hostname) ); then
    echo "I: there is a host-specific dotfiles branch for this host"
    echo "I: consider checking it out"
fi
