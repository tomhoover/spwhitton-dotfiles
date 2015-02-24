#* * * * * srem cron
#0 * * * * srem emacs

*/15 * * * * chronic doccheckin
*/10 * * * * chronic offlineimap

@reboot rt
