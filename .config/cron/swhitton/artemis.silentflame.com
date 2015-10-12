* * * * *   srem --cron
0 */2 * * * srem --refresh-emacs

*/15 * * * * chronic doccheckin
*/30 * * * * offline || chronic mbsync fastmail

# alarm clock: artemis is always turned on at present and I can't get
# joeyh's systemd alarm clock working
30 5 * * 1-5 chronic goodmorning
0  6 * * 6   chronic goodmorning
30 7 * * 7   chronic goodmorning

@reboot rt
