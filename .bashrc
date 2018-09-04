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
alias vi="$EDITOR"
alias e="$EDITOR"
alias mrs="mr -m status"
alias d="emacsclient -c -n -e '(dired \".\")'"
alias ta="tmux attach-session"
alias rax="screen -URaAx"
alias lid-hold="systemd-inhibit --what=sleep --who=Sean --why=manual --mode=block cat"
alias new-login-shell="exec su -l $USER" # get new UNIX group without relog
alias whitenoise="cvlc --quiet --loop ~/lib/annex/doc/sounds/R*.ogg"

alias sid-build-deps='mk-build-deps -ir -s sudo -t \
      "apt-get -o Debug::pkgProblemResolver=yes -t sid --no-install-recommends"'
alias bts-policy="bts user debian-policy@packages.debian.org \
      , package debian-policy , "
alias dak-rdeps="ssh mirror.ftp-master.debian.org dak rm -Rn"
alias madison="ssh mirror.ftp-master.debian.org dak ls"
alias b="bts --mbox show"
alias afsid="apt-file --filter-suites unstable"
alias dinstall="curl https://ftp-master.debian.org/dinstall.status;echo -n 'Right now:    ';date -u '+%a %b %d %T %Z %Y (%s)'"

alias develacc='sudo machinectl shell spw@develacc \
      $(sudo enter-develacc /bin/sh -c "getent passwd spw | cut -d: -f7")'
alias develaccr='sudo machinectl shell root@develacc \
      $(sudo enter-develacc /bin/sh -c "getent passwd root | cut -d: -f7")'

# dpkg-source unconditionally wants to chmod patches (#898010), so if
# we need to run dpkg-source inside develacc, our setgid+setfacl setup
# will not be sufficient.  In this and similar cases, just run this
# shell alias to normalise things
alias force-develacc-perms="sudo chown -R spw:spw /home/spw"

# alias does not call `git develacc` because we want manual
# verification of what is to be pushed (i.e. `git diff
# master..develacc/develacc-iris` to check patch queue is sane)
alias push-develacc-dotfiles-branch="git push \
      -f origin develacc/develacc-$(hostname -s):develacc-$(hostname -s)"

# this due to Clint Adams
alias get-my-signers='gpg --keyserver pool.sks-keyservers.net --recv-keys $(gpg --list-sigs --with-colons 0F56D0553B6D411B | awk -F: '"'"'/\[User ID not found\]/ {print $5}'"'"' | sort -u)'

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

# release process for almost all of the Debian packages I maintain
debrel () {
    (
        set -e
        local branch="$(git symbolic-ref HEAD | sed 's|refs/heads/||')"

        if ! (git diff-index --quiet --cached HEAD && \
                  git diff-files --quiet && \
                  test -z "$(git status --porcelain)" \
             ) >/dev/null 2>&1; then
            echo >&2 "must commit first"
            exit 1
        fi
        if [[ $branch =~ ^wip/ ]]; then
            echo >&2 "you don't want to upload a wip/ branch"
            exit 1
        fi
        if [ "$(dpkg-parsechangelog -SDistribution)" = "UNRELEASED" ]; then
            debchange --release # pauses for user confirmation
            git commit --include debian/changelog \
                -m"finalise changelog for $(dpkg-parsechangelog -SVersion) upload"
        fi
        dgit push-source "$@"
        git push --follow-tags
    )
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

# tidy up if I deleted files from stowed repos without properly
# restowing
kill-broken-stowed-symlinks () {
    find "$HOME" -xtype l | while read -r link; do
        if readlink "$link" | grep --quiet "^[../]*/.STOW/"; then
            rm "$link"
        fi
    done
}

# install package(s) and immediately mark as auto installed, so it
# will get cleaned up by the next autoclean.
# --no-install-recommends is needed as otherwise packages are manually
# installed beyond those specified on the command line
install-as-auto () {
    if [[ $EUID -ne 0 ]]; then
        sudo apt-get --no-install-recommends install "$@"
        sudo apt-mark auto "$@"
    else
        apt-get --no-install-recommends install "$@"
        apt-mark auto "$@"
    fi
}

# when mosh isn't installed on the server, but tmux is, use this to
# get a fairly persistent set of remote shell sessions.  Idea from
# https://coderwall.com/p/aohfrg/smux-ssh-with-auto-reconnect-tmux-a-mosh-replacement
smux () {
    autossh -M 0 -t "$@" "tmux attach-session"
}
