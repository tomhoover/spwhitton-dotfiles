*/15 * * * * chronic doccheckin
*/30 * * * * chronic mbsync fastmail

@reboot rt
@reboot mount /home/spwhitton/lib/fm 2>/dev/null || true
