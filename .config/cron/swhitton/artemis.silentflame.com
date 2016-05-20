# srem needs maintenance
# * * * * *   srem --cron
# 0 */2 * * * srem --refresh-emacs

*/15 * * * * chronic doccheckin
#*/30 * * * * offline || chronic mbsync fastmail
# ^ disabled for May--July 2016

@reboot rt

