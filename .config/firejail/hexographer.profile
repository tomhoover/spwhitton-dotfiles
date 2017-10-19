# Firejail profile for hexographer, based on upstream profile for
# terasology

include /etc/firejail/globals.local

noblacklist ${HOME}/.java

include /etc/firejail/disable-common.inc
include /etc/firejail/disable-devel.inc
include /etc/firejail/disable-passwdmgr.inc
include /etc/firejail/disable-programs.inc

mkdir ${HOME}/.java

whitelist ~/.fonts
whitelist ~/.fonts.d
whitelist ~/.fontconfig
whitelist ~/.fonts.conf
whitelist ~/.fonts.conf.d

whitelist ${HOME}/.java
whitelist ${HOME}/lib/annex/.git/annex/objects
read-only ${HOME}/lib/annex/.git/annex/objects
whitelist ${HOME}/lib/annex/big/software
read-only ${HOME}/lib/annex/big/software
# following will require `git annex unlock`
whitelist ${HOME}/lib/annex/doc/gaming
include /etc/firejail/whitelist-common.inc

caps.drop all
ipc-namespace
net none
netfilter
nodvd
nogroups
nonewprivs
noroot
notv
novideo
protocol unix,inet,inet6
seccomp
shell none

disable-mnt
private-dev
private-etc profile,fonts,alternatives,java-9-openjdk,java-8-openjdk,java-7-openjdk
private-tmp

noexec ${HOME}
