# srem needs maintenance
# * * * * *   srem --cron
# 0 */2 * * * srem --refresh-emacs

*/15 * * * * chronic doccheckin
*/30 * * * * offline || chronic mbsync fastmail

# alarm clock: artemis is always turned on at present and I can't get
# joeyh's systemd alarm clock working.  Fifteen minutes after sunrise
30 6 * * * chronic goodmorning

0 3 * * * chronic update-recoll-db

@reboot rt

