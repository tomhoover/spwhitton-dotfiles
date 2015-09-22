* * * * *   srem --cron
0 */2 * * * srem --refresh-emacs

*/15 * * * * chronic doccheckin
*/30 * * * * offline || chronic mbsync fastmail

@reboot rt
