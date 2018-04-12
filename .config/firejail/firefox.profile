include /etc/firejail/firefox.profile

whitelist ~/tmp

# for dash-haskell docsets
whitelist ~/src
whitelist ~/local/mutt
whitelist ~/local/5thsrd_offline
blacklist ~/src/athpriv
blacklist ~/src/priv
read-only ~/src

# fix ibus in Firefox on stretch
# should be removable once upgraded to buster
env GTK_IM_MODULE=xim
