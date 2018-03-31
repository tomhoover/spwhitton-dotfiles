# --- preferences

# load standard environment variables
. $HOME/.shenv

# '>' is a nice prompt char because it need not be followed by a
# space.  It is easy to distinguish the command from the prompt
PS1='$(exit_code=$?; test $exit_code -eq 0 || printf %s $exit_code " ")\u@\h:\w>'

# enable better completion
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# history settings
HISTCONTROL=ignorespace:ignoredups

# make less more friendly for non-text input files; see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# let spwhitton read and write spw's files created in develacc
# TODO need to set umask in enter-develacc-i3 too?
if in-develacc; then
    umask 002
fi

# --- aliases

alias ls="ls --color=auto --literal --classify"
alias grep="grep --colour=auto"

alias g="git"
alias ga="git annex"
alias mg="$EDITOR"
