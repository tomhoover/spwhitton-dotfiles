@echo off
REM See comproc.org for when to use this script
REM ---- BEGIN WORK

CD %HOMEPATH%
mklink /J .emacs.d src\dotfiles\.emacs.d
mklink /J bin src\dotfiles\bin
mklink /J tmp Desktop
copy /y src\dotfiles\home-mrconfig .mrconfig
copy /y src\dotfiles\.gitconfig .gitconfig
copy /y src\dotfiles\.bashrc .bashrc
copy /y src\dotfiles\.shenv .shenv
mkdir %APPDATA%\cabal
copy /y src\dotfiles\.cabal\config %APPDATA%\cabal\config

REM Could generalise the following for all dirs under dotfiles/AppData/Roaming
mklink /J %APPDATA%\VirtuaWin src\dotfiles\AppData\Roaming\VirtuaWin

REM ---- END   WORK
REM ECHO.
REM ECHO That should be everything set-up.
REM pause
