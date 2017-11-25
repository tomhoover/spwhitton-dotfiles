PATH=$HOME/local/bin:$HOME/.local/bin:$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin
MAILTO=spwhitton@spwhitton.name

# watch any annexes I have checked out
#@reboot cd $HOME/lib/annex 2>/dev/null && git annex watch || true
#@reboot cd $HOME/lib/dionysus 2>/dev/null && git annex watch || true
#@reboot cd $HOME/lib/rt 2>/dev/null && git annex watch || true
#@reboot cd $HOME/lib/wikiannex 2>/dev/null && git annex watch || true

* * * * *   srem --cron
0 */2 * * * srem --refresh-emacs

*/15 * * * * chronic doccheckin
*/30 * * * * chronic notmuch new
