# ikiwiki midnight maintenance: update day on calendar marked as today, and fix typography
0 0 * * * ikiwiki --setup /home/swhitton/src/athpriv/spwhitton.setup --refresh --typographyattributes=2

# git-annex if the repo has already been checked out (for automatic git-annex merge)
@reboot test -d /home/swhitton/lib/wikiannex/.git && git annex assistant --autostart
