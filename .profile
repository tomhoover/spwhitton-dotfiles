#umask 022

# --- $PATH

if [ -d ~/local/bin ] ; then
    PATH=~/local/bin:"${PATH}"
fi

if [ -d ~/bin ] ; then
    PATH=~/bin:"${PATH}"
fi
