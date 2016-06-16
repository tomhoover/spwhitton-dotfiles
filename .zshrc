# Credits for much of the following config file: tomaw,
# <slarti@gentoo.org>, <spider@gentoo.org>, <ciaranm@gentoo.org>

# --- personal settings

# for dch
export DEBFULLNAME="Sean Whitton"
export DEBEMAIL="spwhitton@spwhitton.name"

# basic env
source ~/.shenv
export GPG_TTY=$(tty)

# colours
if ! [[ "$TERM" == "dumb" ]]; then
    autoload -U colors; colors
    export PS1="%{$fg[${1:-yellow}]%}%m %{$fg[${1:-green}]%}%~ %{$fg[${1:-blue}]%}%#%{$reset_color%} "
fi

# add newer GHC to PATH if interactive shell (can't just add in .shenv
# because xmonad needs the old one)
# if [[ $- == *i* ]]; then
#     if [ -d "$HOME/local/stow/ghc/bin" ]; then
#        export PATH=$HOME/local/stow/ghc/bin:$PATH
#     fi
# fi

# --- terminals

# From dev.gentoo.org/~ciaranm/configs/bashrc
if [[ "${TERM}" == "rxvt-unicode" ]] ; then
    export TERMTYPE="256"
elif [[ "${TERM}" != "dumb" ]] ; then
    export TERMTYPE="16"
fi

if [[ "${TERM}" == "rxvt-unicode" ]] && \
    [[ ! -f /usr/share/terminfo/r/rxvt-unicode ]] && \
    [[ ! -f ~/.terminfo/r/rxvt-unicode ]] ; then
    export TERM=rxvt
fi

# set window title

precmd() {
    #    [[ -t 1 ]] || return
    case $TERM in
        *xterm*|rxvt*) print -Pn "]2;%n@%m:%~\a"
            ;;
        # screen*) print -Pn "\"%n@%m:%~\134"
        # ;;
    esac
}

# --- aliases

alias ls="ls -F --color=always"
alias ll="ls -al"
alias g="git"
alias ga="git annex"
alias gs="git status"
# better: pa fax | grep
# alias pg="ps auxxxxxxxxxxxxxxxxxxx | grep"
alias fmr="MR_FAST=true mr"
alias d="emacsclient -c -n -e '(dired \".\")'"
alias mg=$EDITOR
alias mrs="mr -m status"

alias fixmpd="sudo invoke-rc.d mpd restart; pkill sonata"
alias radio4="mplayer \"http://wmlive-acl.bbc.co.uk/wms/bbc_ami/radio4/radio4_bb_live_eq1_sl0?BBC-UID=044be39365f98aaa88a55ca7f1aa8fc5b3569ae000708114d4dfd43698a07e8a&amp;SSO2-UID=\""
alias ws="mplayer mms://a243.l3944038972.c39440.g.lm.akamaistream.net/D/243/39440/v0001/reflector:38972"

alias screen="screen -U" # enable UTF-8
alias tmux="tmux"
alias rax="screen -URaAx"
alias ta="tmux attach"

alias blogdates="rdate.py-dir ~/html/blog/entries"
alias httpdir="sudo python -m SimpleHTTPServer 80"
alias sdfvpn="sshuttle -r ma 0/0 --dns --auto-hosts --python /usr/pkg/bin/python2.7"
alias athvpn="sshuttle -r athena 0/0 --dns"
alias calup="emacs -batch -l /home/swhitton/.emacs.d/init.el -eval \"(org-batch-store-agenda-views)\""
alias dotex="texi2dvi --pdf --clean --batch"
alias whitenoise="cvlc --quiet --loop ~/lib/annex/doc/sounds/R*.ogg"
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"
alias mailnow="mbsync -q fastmail && mutt -Z"
alias mykb="setxkbmap gb; setxkbmap -option ctrl:nocaps"
alias fixmacs="pkill -USR2 emacs"
alias myfiles="cadaver https://myfiles.messagingengine.com/"
alias unstow="stow -D"
alias sid="sudo /usr/local/bin/enter-sid /bin/sh -c \"export DISPLAY=${DISPLAY}; su - swhitton\""
alias uavpn="pkill rtorrent && sudo openconnect https://vpn.arizona.edu/ && TERM=dumb rt"
alias loungempd="ssh TallGeese /Applications/VLC.app/Contents/MacOS/VLC -I rc --loop http://192.168.1.236:8000"
alias sid-build-deps="sudo mk-build-deps -irt 'apt-get -o Debug::pkgProblemResolver=yes -t sid --no-install-recommends'"
alias testing-build-deps="sudo mk-build-deps -irt 'apt-get -o Debug::pkgProblemResolver=yes -t testing --no-install-recommends'"
alias test-package-plan="schroot -d $HOME -- /bin/sh -c \"cabal --no-require-sandbox update && cd src/package-plan && perl test-packages.pl\""
alias dgb="dgit --ignore-dirty build"
alias wnpomodoro="mplayer ~/lib/annex/doc/sounds/*pomodoro.mp3"
alias gbps="gbp buildpackage --git-builder=sbuild"

# based on gregor hermann's dh-make-perl-dev he posted on bugs.d.o
dh-make-elpa-dev () {
    PERL5LIB=~/src/dh-make-elpa/lib/ ~/src/dh-make-elpa/dh-make-elpa "$@"
}

# cd to top of git checkout
alias cg='cd $(git rev-parse --show-toplevel)'

# try to stop dpkg-buildpackage from invoking gpg2 and thereby messing
# with ~/.gnupg
alias dpkg-buildpackage="dpkg-buildpackage -pgpg"

# add a project to projectile's known projects
projectile () {
    local dir="$(pwd)"
    chronic emacsclient -c -n -e "(projectile-switch-project-by-name \"$dir\")"
}

# clone with git and open in Emacs
clone () {
    cd $HOME/src
    git clone $1
    name=$(basename $1 | cut -d: -f2)
    cd $name
    mr register
    projectile
}

# tmux is smart and it detects a changed SSH_AUTH_SOCK and any newly
# created shells use that.  But not old ones: this should pull the
# newly detected value into the old sehll.

# check tmux is running first by seeing if we're inside it
if ! [ "$TMUX" = "" ]; then
    alias fixsshagent="eval $(tmux show-environment | grep '^SSH_AUTH_SOCK')"
fi

# --- load zsh features

# Change word boundary characters. Nabbed from
# http://zshwiki.org/KeyBindings.

# by default: export WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'
# we take out the slash, period, angle brackets, dash here.
export WORDCHARS='*?_-[]~=&;!#$%^(){}'

# Follow GNU LS_COLORS for completion menus
zmodload -i zsh/complist
#eval $(dircolors -b /home/tom/.dir_colors)
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:*:kill:*' list-colors '=%*=01;31'

# Load the completion system
autoload -U compinit; compinit

# Very powerful version of mv implemented in zsh. The main feature I
# know of it that seperates it from the standard mv is that it saves you
# time by being able to use patterns which are expanded into positional
# parameters. So:
#
# slarti@pohl % zmv (*)foo ${1}bar
#
# On a series of files like onefoo, twofoo, threefoo, fivefoo would be
# renamed to onebar twobar threebar fourbar.
#
# Although that's nifty enough, I suspect there are other features I
# don't know about yet...
#
# Read $fpath/zmv for some more basic examples of usage, and also use
# run-help on it :)
autoload -U zmv

# automatically escape URLs
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Incremental completion of a word. After starting this, a list of
# completion choices can be shown after every character you type, which
# can deleted with ^H or delete. Return will accept the current
# completion. Hit tab for normal completion, ^G to get back where you
# came from and ^D to list matches.
autoload -U incremental-complete-word
zle -N incremental-complete-word
bindkey "^Xi" incremental-complete-word

# This function allows you type a file pattern, and see the results of
# the expansion at each step.  When you hit return, they will be
# inserted into the command line.
autoload -U insert-files
zle -N insert-files
bindkey "^Xf" insert-files

# This set of functions implements a sort of magic history searching.
# After predict-on, typing characters causes the editor to look backward
# in the history for the first line beginning with what you have typed so
# far.  After predict-off, editing returns to normal for the line found.
# In fact, you often don't even need to use predict-off, because if the
# line doesn't match something in the history, adding a key performs
# standard completion - though editing in the middle is liable to delete
# the rest of the line.
autoload -U predict-on
zle -N predict-on
zle -N predict-off
bindkey "^X^Z" predict-on
bindkey "^Z" predict-off

# _gnu_generic is a completion widget that parses the --help output of
# commands for options. df and feh work fine with it, however options
# are not described.
compdef _gnu_generic feh df

compdef _pkglist ecd emetadataviewer
compdef _useflaglist explainuseflag
compdef _category list_cat

compdef _nothing etc-update dispatch-conf fixpackages

# History things
HISTFILE=$HOME/.zshist
SAVEHIST=10000
HISTSIZE=10000
TMPPREFIX=$HOME/tmp

# Key bindings

# You can use:
# % autoload -U zkbd
# % zkbd
# to discover your keys.

bindkey -e

# C-x C-k to kill region
bindkey "^X^K" kill-region

# Up, down left, right.
# echotc is part of the zsh/termcap module. It outputs the termcap value
# corresponding to the capability it was given as an argument. man zshmodules.
zmodload -i zsh/termcap
bindkey "$(echotc kl)" backward-char
bindkey "$(echotc kr)" forward-char
bindkey "$(echotc ku)" up-line-or-history
bindkey "$(echotc kd)" down-line-or-history

bindkey '\e[3~' delete-char # Delete

if [[ "$TERM" == "rxvt-unicode" || "$TERM" == "screen" ]]; then
    bindkey '\e[7~' beginning-of-line # Home
    bindkey '\e[8~' end-of-line # End
elif [[ "$TERM" == "linux" ]]; then
    bindkey '\e[1~' beginning-of-line # Home
    bindkey '\e[4~' end-of-line # End
else # At least xterm; probably other terms too
    bindkey '\e[H~' beginning-of-line # Home
    bindkey '\e[F~' end-of-line # End
fi

bindkey '\e[5~' up-history # PageUp
bindkey '\e[6~' down-history # PageDown

# --- completion

# Pretty menu!
zstyle ':completion:*' menu select=1

# Completion options
zstyle ':completion:*' completer _complete _prefix
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct
zstyle ':completion:predict:*' completer _complete

# Completion caching
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

# Expand partial paths
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-slashes 'yes'

# Include non-hidden directories in globbed file completions
# for certain commands
zstyle ':completion::complete:*' '\'

# Use menuselection for pid completion
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

#  tag-order 'globbed-files directories' all-files
zstyle ':completion::complete:*:tar:directories' file-patterns '*~.*(-/)'

# Don't complete backup files as executables
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '*\~'

# Don't complete non-.tex LaTeX files with mg
zstyle ':completion:*:*:mg:*:*files' ignored-patterns '*?.aux' '*?.log' '*?.pdf' '*?.toc' '*?.bak' '*?.fdb_latexmk'

# Separate matches into groups
zstyle ':completion:*:matches' group 'yes'

# With commands like rm, it's annoying if you keep getting offered the same
# file multiple times. This fixes it. Also good for cp, et cetera..
zstyle ':completion:*:rm:*' ignore-line yes
zstyle ':completion:*:cp:*' ignore-line yes

# Describe each match group.
zstyle ':completion:*:descriptions' format "%B---- %d%b"

# Messages/warnings format
zstyle ':completion:*:messages' format '%B%U---- %d%u%b'
zstyle ':completion:*:warnings' format '%B%U---- no match for: %d%u%b'

# Describe options in full
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'

# Simulate spider's old abbrev-expand 3.0.5 patch
#zstyle ':completion:*:history-words' stop verbose
#zstyle ':completion:*:history-words' remove-all-dups yes
#zstyle ':completion:*:history-words' list false

# From the zshwiki. Hide CVS files/directores from being completed.
zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS'
zstyle ':completion:*:cd:*' ignored-patterns '(*/)#CVS'

# Also from the wiki. Hide uninteresting users from completion.
zstyle ':completion:*:*:*:users' ignored-patterns \
    adm apache bin daemon games gdm halt ident junkbust lp mail mailnull \
    named news nfsnobody nobody nscd ntp operator pcap postgres radvd \
    rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs backup  bind  \
    dictd  gnats  identd  irc  man  messagebus  postfix  proxy  sys \
    www-data alias amavis at clamav cmd5checkpw cron cyrus dhcp dnscache \
    dnslog foldingathome guest haldaemon jabber ldap mailman mpd mysql \
    nut p2p portage postmaster qmaild qmaill qmailp qmailq qmailr qmails \
    smmsp tinydns vpopmail wasabi zope

# Pull hosts from $HOME/.ssh/known_hosts, also from the wiki
# local _myhosts
_myhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
zstyle ':completion:*' hosts $_myhosts

# Approximate completion. From the wiki.
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# --- zsh options

setopt                   \
    NO_all_export        \
    always_last_prompt   \
    always_to_end        \
    append_history       \
    share_history        \
    auto_cd              \
    auto_list            \
    auto_menu            \
    auto_name_dirs       \
    auto_param_keys      \
    auto_param_slash     \
    auto_pushd           \
    auto_remove_slash    \
    NO_auto_resume       \
    bad_pattern          \
    bang_hist            \
    NO_beep              \
    brace_ccl            \
    NO_bsd_echo          \
    NO_cdable_vars       \
    NO_chase_links       \
    clobber              \
    complete_aliases     \
    complete_in_word     \
    NO_correct           \
    NO_correct_all       \
    csh_junkie_history   \
    NO_csh_junkie_loops  \
    NO_csh_junkie_quotes \
    NO_csh_null_glob     \
    equals               \
    NO_extended_glob     \
    extended_history     \
    function_argzero     \
    glob                 \
    NO_glob_assign       \
    glob_complete        \
    NO_glob_dots         \
    NO_glob_subst        \
    NO_hash_cmds         \
    NO_hash_dirs         \
    hash_list_all        \
    hist_allow_clobber   \
    hist_beep            \
    hist_ignore_dups     \
    hist_ignore_space    \
    NO_hist_no_store     \
    hist_verify          \
    NO_hup               \
    NO_ignore_braces     \
    NO_ignore_eof        \
    interactive_comments \
    inc_append_history   \
    NO_list_ambiguous    \
    NO_list_beep         \
    list_types           \
    long_list_jobs       \
    magic_equal_subst    \
    NO_mail_warning      \
    NO_mark_dirs         \
    NO_menu_complete     \
    multios              \
    nomatch              \
    notify               \
    noflowcontrol        \
    NO_null_glob         \
    numeric_glob_sort    \
    NO_overstrike        \
    path_dirs            \
    posix_builtins       \
    NO_print_exit_value  \
    NO_prompt_cr         \
    prompt_subst         \
    pushd_ignore_dups    \
    NO_pushd_minus       \
    pushd_silent         \
    pushd_to_home        \
    rc_expand_param      \
    NO_rc_quotes         \
    NO_rm_star_silent    \
    NO_sh_file_expansion \
    sh_option_letters    \
    short_loops          \
    NO_sh_word_split     \
    NO_single_line_zle   \
    NO_sun_keyboard_hack \
    NO_verbose           \
    zle

# --- compatibility

# allow zenity to be called from cron

# per http://promberger.info/linux/2009/01/02/running-x-apps-like-zenity-from-crontab-solving-cannot-open-display-problem/
if pgrep Xorg >/dev/null; then
    xhost local:${USER} > /dev/null 2> /dev/null
fi
# per http://superuser.com/questions/111771/using-either-notify-send-or-zenity-in-cron
echo $DBUS_SESSION_BUS_ADDRESS > ~/.tmp-dbus-addr

# Make BiBTeX and Org play nice together due to security change in
# TeXLive 2010.  See
# http://lists.gnu.org/archive/html/emacs-orgmode/2011-04/msg00845.html

export BIBINPUTS="$HOME/doc:$HOME/doc/papers:$BIBINPUTS"

# If we're on a weak terminal (probably non-interactive) such as
# TRAMP, kill off a bunch of the cool stuff we just set-up.
                
if [[ "$TERM" == "dumb" ]]; then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    export TERMTYPE=""
    export NOCOLOR="true"
    PS1='$ '
fi
