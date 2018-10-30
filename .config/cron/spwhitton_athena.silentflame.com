PATH=$HOME/local/bin:$HOME/.local/bin:$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin
MAILTO=spwhitton@spwhitton.name

*/15 * * * * chronic doccheckin
@hourly chronic nice ionice -c 3 notmuch new

# download RSS feeds at 1pm MST
0 20 * * * r2e run

# run `importfeed --relaxed` on a machine that is always turned on, to
# update podcast metadata.  Actual audio files downloaded on
# workstation
@daily cd /home/spwhitton/lib/podcasts && chronic xargs git annex importfeed --relaxed < feeds

# start up Emacs & Irssi
@reboot emacs --daemon
@reboot ii

# ikiwiki midnight maintenance: update day on calendar marked as today
# (cannot be @daily as that is not semantically guaranteed to be at
# midnight)
0 0 * * * ikiwiki --setup /home/spwhitton/src/athpriv/spwhitton.setup --refresh

# old jobs:
#@daily chronic athena-publish-org-docs
#@reboot cd /home/spwhitton/lib/dionysus && git annex watch
#@reboot mount /home/spwhitton/lib/fm
