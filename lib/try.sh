#!/bin/sh

. "$HOME/lib/tputfs.sh"

zero () {
    local cmd="$*"
    while ! (output="$($cmd)" ; echo "$output" ; [ -z "$output" ]); do
        status "executing '$cmd' produced output"
        status but it should produce no output -- spawning a shell
        status "when you C-d, I will attempt to run '$cmd' again"
        eval "$SHELL"
    done
}

try () {
    local cmd="$*"
    while ! $cmd; do
        status "executing '$cmd' failed: spawning a shell"
        status "when you C-d, I will attempt to run '$cmd' again"
        eval "$SHELL"
    done
}
