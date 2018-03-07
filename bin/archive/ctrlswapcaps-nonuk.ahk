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

F11::Send !{F4}
F12::ToggleWinMinimize("Mozilla Firefox", "Firefox")

; for Emacs

Capslock::Ctrl
LCtrl::Capslock

; some British keyboard layout conventions

@::"
"::@
