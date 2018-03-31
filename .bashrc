# --- preferences

# load standard environment variables
. $HOME/.shenv

# '>' is a nice prompt char because it need not be followed by a
# space.  It is easy to distinguish the command from the prompt
PS1='$(exit_code=$?; test $exit_code -eq 0 || printf %s $exit_code " ")\u@\h:\w>'

# if this is an xterm set its title to user@host:dir
# (this doesn't stop the likes of ncmpcpp setting a title)
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h:\w\a\]$PS1"
        ;;
    *)
        ;;
esac

# enable better completion
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# history settings
HISTCONTROL=ignorespace:ignoredups

# make less more friendly for non-text input files; see lesspipe(1)
if [ -x /usr/bin/lesspipe ]; then
    eval "$(lesspipe)"
fi

# --- aliases

alias ls="ls --color=auto --literal --classify"
alias grep="grep --colour=auto"

alias g="git"
alias ga="git annex"
alias mg="$EDITOR"
alias mrs="mr -m status"

alias sid-build-deps='mk-build-deps -ir -s sudo -t \
      "apt-get -o Debug::pkgProblemResolver=yes -t sid --no-install-recommends"'

alias develacc='sudo machinectl shell spw@develacc \
      $(sudo enter-develacc /bin/sh -c "getent passwd spw | cut -d: -f7")'
alias develaccr='sudo machinectl shell root@develacc \
      $(sudo enter-develacc /bin/sh -c "getent passwd root | cut -d: -f7")'

# --- more powerful aliases built with shell functions

# run a package build and the full suite of checks that I can do
# locally before an upload.  Generally I use this on an UNRELEASED
# package and then use `dgit push-source` for the actual upload
sbuild-preupload() {
    local dgit=""
    local sbuild=""
    for key in "$@"; do
        case $key in
            --gbp|--dpm|--quilt=*)
                dgit="$dgit $key"
                shift
                ;;
            *)
                sbuild="$sbuild $key"
                shift
                ;;
        esac
    done
    case $(pwd) in
        *src/DHG_packages/p*)
            sbuild $sbuild \
                   --no-run-lintian --run-piuparts --run-autopkgtest
            ;;
        *)
            eval dgit $dgit sbuild $sbuild \
                 --no-run-lintian --run-piuparts --run-autopkgtest
    esac
    lintian
}

# copy files into /home/spw/tmp for use in develacc container.  cp(1)
# does not respect ACLs, so we need to set a temporary umask before
# copying, and --no-preserve=mode to ensure that group-unreadable
# files become group-readable (i.e. have cp(1) follow the umask we
# just set).  Together with the setgid perms on /home/spw and
# subdirectories, this should ensure that spw is able to manipulate
# copied files and directories
copy-to-develacc () {
    (
        umask 002
        cp --no-preserve=mode -RL "$@" /home/spw/tmp
    )
}
# copy files into /root/tmp in develacc container.  Mainly used for
# .debs I want to install and test in that container
copy-to-develaccr () {
        sudo cp -RL "$@" /var/lib/container/develacc/root/tmp/
}
