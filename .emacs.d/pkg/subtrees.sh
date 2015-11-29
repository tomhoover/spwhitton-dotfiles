#!/bin/sh

# make haskell-mode autoloads
(
    cd $HOME/.emacs.d/pkg/haskell-mode
    make
)

# byte-compile anything that needs to be
emacs --batch \
      --eval '(byte-recompile-directory user-emacs-directory 0)' \
      2>&1 | tail -n1           # output only the summary line
