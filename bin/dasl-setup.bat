@echo off
mkdir C:\%HOMEPATH%\SPW
copy /y ..\lib\putty.exe.reg C:\%HOMEPATH%\SPW
copy /y ..\lib\putty.exe-empty.reg C:\%HOMEPATH%\SPW
copy /y ..\lib\spwhitton@putty.ppk C:\%HOMEPATH%\SPW
copy /y ..\bin\ctrlswapcaps.exe C:\%HOMEPATH%\SPW
explorer "C:\%HOMEPATH%\SPW"
