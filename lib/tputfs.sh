#!/bin/bash

# shell scripting functions using tput

status ()
{
    echo -n "["
    test "$TERM" = "dumb" || tput setaf 3
    echo -n $(basename $0)
    test "$TERM" = "dumb" || tput setaf 7
    echo -n "] "
    test "$TERM" = "dumb" || tput bold
    echo $@
    test "$TERM" = "dumb" || tput sgr0
}
