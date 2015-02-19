#!/bin/bash

export HALCYON_BASE=$HOME/local/app
if [ -d "$HALCYON_BASE/halcyon" ]; then
    source <( HALCYON_NO_SELF_UPDATE=1 $HOME/local/app/halcyon/halcyon paths )
fi
