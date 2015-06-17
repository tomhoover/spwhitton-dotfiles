import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig (additionalKeys)

myMod = mod4Mask

main = xmonad $ xfceConfig
    { terminal    = "urxvt"
    , modMask     = myMod
    -- , borderWidth = 3
    } `additionalKeys` myKeys

myEditor = "emacsclient -c -n -e '(switch-to-buffer nil)'"

myKeys = [ ((myMod, xK_e), spawn myEditor) ]
