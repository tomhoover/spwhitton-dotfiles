*/15 * * * * chronic doccheckin
@hourly chronic mbsync fastmail

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
0 0 * * * ikiwiki --setup /home/swhitton/src/athpriv/spwhitton.setup --refresh

# old jobs:
#@daily chronic athena-publish-org-docs
#@reboot cd /home/swhitton/lib/dionysus && git annex watch
#@reboot mount /home/swhitton/lib/fm
