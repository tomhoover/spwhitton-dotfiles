#!/bin/sh

athena_cmd () {
    # here we rely on the fact that ssh already passes argument
    # through `/bin/sh -c' (note use of single-quotes in this
    # function)
    ssh athena 'cd $HOME/'"$1"' && . $HOME/.shenv && '"$2" 2>&1 | sed -e 's/^/  /'
}

win32 () {
    test "$(perl -e 'print $^O')" = "msys"
}        
