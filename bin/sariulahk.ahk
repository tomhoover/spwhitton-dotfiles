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

F9::ToggleWinMinimize("emacs", "c:\emacs\bin\runemacs.exe")
F10::ToggleWinMinimize("MINGW32", "c:\Users\swhitton\Old shortcuts\Git Bash")
F11::Send !{F4}
F12::ToggleWinMinimize("Mozilla Firefox", "Firefox")

+F9::ToggleWinMinimize("3~4", "C:\e-Book\start\TitleList.exe")
+F10::ToggleWinMinimize(" CD", "c:\Users\swhitton\Old shortcuts\grades5and6")
+F11::ToggleWinMinimize("spw@ma", "c:\Users\swhitton\Software\putty.exe -load ma")
+F12::IceMessenger()  

; empty the Recycle Bin
+#r::FileRecycleEmpty, C:\

; swap caps lock and control, of course
; Capslock::Control
; Control::Capslock

; Author: fwompner gmail com
#InstallKeybdHook
SetCapsLockState, alwaysoff
Capslock::
  SetCapsLockState, off
  Send {LControl Down}
  KeyWait, CapsLock
  Send {LControl Up}
  if ( A_PriorKey = "CapsLock" )
  {
    Send {Esc}
  }
  return
  SetCapsLockState, off
  return

; some British keyboard layout conventions

@::"
"::@

; disable my beloved RSI-inducing shortcuts
^h::Return
^m::Return
