#* * * * *   srem --cron
#0 */2 * * * srem --refresh-emacs

*/15 * * * * chronic doccheckin
*/10 * * * * chronic mbsync fastmail

@reboot rt
