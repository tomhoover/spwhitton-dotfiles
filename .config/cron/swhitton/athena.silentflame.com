*/15 * * * * chronic doccheckin
@hourly chronic mbsync fastmail
@daily r2e run
@daily chronic athena-publish-org-docs
@reboot emacs --daemon
@reboot ii
@reboot mount /home/swhitton/lib/fm

# ikiwiki midnight maintenance: update day on calendar marked as
# today, and fix typography (cannot be @daily as that is not
# semantically guaranteed to be at midnight)
0 0 * * * ikiwiki --setup /home/swhitton/src/athpriv/spwhitton.setup --refresh --typographyattributes=2

@reboot cd /home/swhitton/lib/dionysus && git annex watch
