import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks

myMod = mod4Mask

main = xmonad $ xfceConfig

    { terminal       = "urxvt"
    , modMask        = myMod
    , workspaces     = myWorkspaces
    , manageHook     = manageSpawn <+> manageHook xfceConfig
    -- , startupHook    = myStartup
    -- , borderWidth = 3
    } `additionalKeys` myKeys

myEditor = "emacsclient -c -n -e '(switch-to-buffer nil)'"
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "web"]

myKeys = [ ((myMod, xK_e), spawn myEditor)

           -- launchers
         , ((myMod, xK_f), spawnOn "web" "iceweasel")
         , ((myMod .|. shiftMask, xK_m), spawn "urxvtcd -e mutt")

           -- workspaces
         , ((myMod, xK_grave), toggleWS)
         , ((myMod, xK_0), windows $ W.greedyView "web")
         ]

-- myStartup = do

--     -- hide the Xfce panel to begin with
--     sendMessage ToggleStruts
