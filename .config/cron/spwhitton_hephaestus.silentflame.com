PATH=$HOME/local/bin:$HOME/.local/bin:$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin
MAILTO=spwhitton@spwhitton.name

*/15 * * * * chronic doccheckin
*/30 * * * * chronic mbsync fastmail

@reboot rt
