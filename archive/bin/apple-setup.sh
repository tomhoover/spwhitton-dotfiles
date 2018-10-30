#!/bin/sh

USB=/Volumes/SPWHITTON

key()
{
    local k="$1"
    osascript -e "tell application \"System Events\" to keystroke \"$k\""
}

code()
{
    local k="$1"
    osascript -e "tell application \"System Events\" to key code $k"
}

# activate my keyboard preferences

osascript -e 'tell application "System Preferences" to activate'
osascript -e 'tell application "System Preferences" to set current pane to pane "com.apple.preference.keyboard"'
osascript -e 'tell application "System Events" to key code 98 using {control down}' # ctrl-f7 to activate tabbing between controls
sleep 1
code 48
code 48
code 48
code 48
code 49
code 48
code 48
code 48
code 49
code 125
code 125
code 52
code 48
code 48
code 49
code 125
code 52
code 48
code 49
code 126
code 52
code 48
code 48
code 48
code 49
osascript -e 'tell application "System Events" to key code 98 using {control down}'
osascript -e 'tell application "System Events" to keystroke "q" using {command down}'

# setup

if ! [ -d "$HOME/src/dotfiles/.git" ]; then
    git clone https://git.spwhitton.name/dotfiles $HOME/src/dotfiles
    # this is currently out of action because GNU stow installs but
    # doesn't seem to actually do anything on Mac OS
    # $HOME/src/dotfiles/bin/bstraph.sh
    cp $HOME/src/dotfiles/{.zshrc,.shenv} $HOME # instead
fi
pkill firefox
/Applications/Firefox.app/Contents/MacOS/firefox -private-window http://u.arizona.edu/~spwhitton/bookmarks.shtml >/dev/null 2>/dev/null &
mkdir -p $HOME/.ssh
cp $USB/lib/{id_putty,known_hosts,config} $HOME/.ssh
chmod 600 $HOME/.ssh/id_putty

# run the shell

cd $HOME
/bin/zsh

# cleanup

pkill firefox
pkill ssh # cached connection!
echo "Please wait, securely deleting files..."
rm -rfP $HOME/.ssh/{id_putty,config} $HOME/Downloads/* $HOME/.Trash/* $HOME/.zshist $HOME/.bash_history

# reset keyboard preferences

osascript -e 'tell application "System Events" to key code 98 using {control down}'
osascript -e 'tell application "System Preferences" to activate'
osascript -e 'tell application "System Preferences" to set current pane to pane "com.apple.preference.keyboard"'
sleep 1
code 48
code 48
code 48
code 48
code 49
code 48
code 48
code 48
code 49
code 48
code 48
code 48
code 48
code 49
code 48
code 48
code 49
osascript -e 'tell application "System Events" to key code 98 using {control down}'
osascript -e 'tell application "System Events" to keystroke "q" using {command down}'

