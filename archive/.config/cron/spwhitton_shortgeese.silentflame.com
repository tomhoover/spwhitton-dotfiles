PATH=$HOME/local/bin:$HOME/.local/bin:$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin
MAILTO=spwhitton@spwhitton.name

# until I get my hands on a VGA-to-HDMI converter, shortgeese is
# serving only mpd audio
@reboot cvlc --loop http://hephaestus.local:8000/ >/dev/null 2>&1
