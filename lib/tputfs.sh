#!/bin/sh

# shell scripting functions using tput

status ()
{
    echo -n "["
    tput setaf 3
    echo -n $(basename $0)
    tput setaf 7
    echo -n "] "
    tput bold
    echo $@
    tput sgr0
}
