* * * * *   srem --cron
0 */2 * * * srem --refresh-emacs

*/15 * * * * chronic doccheckin
*/30 * * * * chronic mbsync fastmail

*/5 * * * * sh -c "if [ $(xprintidle) -le 300000 ]; then ssh-add -D; fi" >/dev/null 2>/dev/null
