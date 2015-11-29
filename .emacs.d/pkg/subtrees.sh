#!/bin/sh

# make haskell-mode autoloads
(
    # must cd to inside ~/src/dotfiles.  If change to ~/.emacs.d/blah,
    # the autoloads are not generated correctly
    cd $HOME/src/dotfiles/.emacs.d/pkg/haskell-mode
    chronic make
)

# make helm autoloads
(
    cd $HOME/src/dotfiles/.emacs.d/pkg/helm
    chronic make
)

# byte-compile anything that needs to be
emacs --batch \
      --eval '(byte-recompile-directory user-emacs-directory 0)' \
      2>&1 | tail -n1           # output only the summary line
