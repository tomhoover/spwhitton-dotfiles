SetWorkingDir, C:\Users\swhitton

; original source: http://lifehacker.com/5468862/create-a-shortcut-key-for-restoring-a-specific-window
; but I've added TheExe parameter
ToggleWinMinimize(TheWindowTitle, TheExe)
{
SetTitleMatchMode,2
DetectHiddenWindows, Off
IfWinActive, %TheWindowTitle%
{
WinMinimize, %TheWindowTitle%
}
Else
{
IfWinExist, %TheWindowTitle%
{
WinGet, winid, ID, %TheWindowTitle%
DllCall("SwitchToThisWindow", "UInt", winid, "UInt", 1)
}
Else
{
Run, %TheExe%
}
}
Return
}

; my run-or-raise shortcuts from unix

#j::ToggleWinMinimize("emacs", "c:\emacs\bin\runemacs.exe -mm")
#k::ToggleWinMinimize("cmd", "cmd.exe")
#;::ToggleWinMinimize("spw@ma", "c:\Users\swhitton\Software\putty.exe -load ma")
#f::ToggleWinMinimize("Mozilla Firefox", "Firefox")

; swap caps lock and control, of course
Capslock::Control
Control::Capslock

; some British keyboard layout conventions

@::"
"::@
