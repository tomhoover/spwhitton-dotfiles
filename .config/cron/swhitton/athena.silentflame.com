*/15 * * * * chronic doccheckin
@hourly chronic mbsync fastmail
# 1pm MST
0 20 * * * r2e run
@daily chronic athena-publish-org-docs
@daily cd /home/swhitton/lib/podcasts && xargs git annex importfeed < feeds
@reboot emacs --daemon
@reboot ii
@reboot mount /home/swhitton/lib/fm

# ikiwiki midnight maintenance: update day on calendar marked as today
# (cannot be @daily as that is not semantically guaranteed to be at
# midnight)
0 0 * * * ikiwiki --setup /home/swhitton/src/athpriv/spwhitton.setup --refresh

#@reboot cd /home/swhitton/lib/dionysus && git annex watch
