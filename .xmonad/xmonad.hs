import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.Submap

import Data.List (isInfixOf)
import Control.Arrow
import Data.Bits
import qualified Data.Map as M

myMod = mod4Mask

main = xmonad $ xfceConfig

    { terminal       = "urxvt"
    , modMask        = myMod
    , workspaces     = myWorkspaces
    , manageHook     = manageSpawn XMonad.<+> myManageHook XMonad.<+> manageHook xfceConfig
    -- , startupHook    = myStartup
    -- , borderWidth = 3
    , keys = addPrefix (controlMask, xK_m) (keys xfceConfig)
    } `additionalKeys` myKeys

myEditor = "emacsclient -c -n -e '(switch-to-buffer nil)'"
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "web"]

myKeys = [ ((myMod, xK_e), spawn myEditor)

           -- launchers
         , ((myMod, xK_f), spawnOn "web" "iceweasel")
         , ((myMod, xK_o), spawn "xmousetidy")
         , ((myMod .|. shiftMask, xK_m), spawn "urxvtcd -e mutt")

           -- workspaces
         , ((myMod, xK_semicolon), toggleWS)
         , ((myMod, xK_0), windows $ W.greedyView "web")
         ]

-- myStartup = do

--     -- hide the Xfce panel to begin with
--     sendMessage ToggleStruts

myManageHook = composeOne
               [ checkDock -?> doIgnore
               , isDialog               -?> doFloat
               , className =? "Gimp"    -?> doFloat
               , className =? "MPlayer" -?> doFloat
               , title =? "*Completions*" -?> doF W.focusDown
               , title =? "*Ido Completions*" -?> doF W.focusDown
               , return True -?> doF W.swapDown
               ]

-- from <http://kojevnikov.com/xmonad-metacity-gnome.html>
addPrefix p ms conf =
    M.singleton p . submap $ M.mapKeys (first chopMod) (ms conf)
  where
    mod     = modMask conf
    chopMod = (.&. complement mod)
