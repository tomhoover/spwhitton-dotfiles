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

IceMessenger()
{
  CoordMode Pixel
  ; image file referenced in the below line should be a screenshot of
  ; some central pixels from the Ice Messenger tray icon
  ImageSearch, FoundX, FoundY, 0, 0, A_ScreenWidth, A_ScreenHeight, c:\Users\swhitton\Pictures\AHK\imtrayicon.png
  CoordMode, Mouse, Screen
  MouseMove, %FoundX%, %FoundY%
  Click
  WinWaitActive, Ice Messenger, , 2
  CoordMode, Mouse, Relative
  MouseMove, 130, 120
  Click
  MouseMove, 130, 370
  Click right
  Send, {Down}
}

; my run-or-raise shortcuts from unix

#j::ToggleWinMinimize("emacs", "c:\emacs\bin\runemacs.exe -mm")
;#k::ToggleWinMinimize("cmd", "cmd.exe")
#k::ToggleWinMinimize("MINGW32", "c:\Users\swhitton\Old shortcuts\Git Bash")
#h::ToggleWinMinimize("spw@ma", "c:\Users\swhitton\Software\putty.exe -load ma")
#;::ToggleWinMinimize("Mozilla Firefox", "Firefox")

; additional run-or-raise for school
; grades5and6 renamed because this autohotkey doesn't like Hangeul

#u::ToggleWinMinimize("3~4", "C:\e-Book\start\TitleList.exe")
#i::ToggleWinMinimize(" CD", "c:\Users\swhitton\Old shortcuts\grades5and6")
#o::IceMessenger()  

; empty the Recycle Bin
#^r::FileRecycleEmpty, C:\

; swap caps lock and control, of course
Capslock::Control
Control::Capslock

; some British keyboard layout conventions

@::"
"::@
