#* * * * *   srem --cron
#0 */2 * * * srem --refresh-emacs

*/15 * * * * chronic doccheckin

@reboot rt
