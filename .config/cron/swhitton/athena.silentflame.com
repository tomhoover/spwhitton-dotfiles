# git-annex if the repo has already been checked out (for automatic git-annex merge)
@reboot test -d /home/swhitton/lib/wikiannex/.git && git annex assistant --autostart
