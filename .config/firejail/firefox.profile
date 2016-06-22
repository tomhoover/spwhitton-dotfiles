include /etc/firejail/firefox.profile

whitelist ~/tmp
whitelist ~/bin/its-all-text-wrapper

# for dash-haskell docsets
whitelist ~/src
blacklist ~/src/athpriv
blacklist ~/src/priv
read-only ~/src
