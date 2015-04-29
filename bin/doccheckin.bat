@echo off
REM Reimplementation of my ~/bin/doccheckin script with Git on Windows

REM To run regularly, try `schtasks /Create /SC MINUTE /MO 15 /TN
REM doccheckin /TR "C:\Users\swhitton\bin\doccheckin.bat"'.  Then open up
REM Scheduled Tasks and tick EM "run whether user is logged on or not" to
REM avoid command prompt window EM flashing up.

CD %HOMEPATH%\doc
"C:\Program Files\Git\bin\sh.exe" -c "if ! git status | /bin/grep -q 'You have unmerged paths.'; then git add org/*.org playlists/*.m3u emacs-abbrevs emacs-bookmarks mutt-aliases mutt-groups || true; git commit -a -m \"auto commit on $(hostname)\" || true; fi"
