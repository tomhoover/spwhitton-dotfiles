# until I get my hands on a VGA-to-HDMI converter, shortgeese is
# serving only mpd audio
@reboot cvlc --loop http://hephaestus.local:8000/ >/dev/null 2>&1
