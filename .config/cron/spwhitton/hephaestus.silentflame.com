*/15 * * * * chronic doccheckin
*/30 * * * * chronic mbsync fastmail

@reboot rt
@reboot sleep 120 ; mount /home/spwhitton/lib/fm
