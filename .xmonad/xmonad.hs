import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.Submap

import Data.List (isInfixOf)
import Control.Arrow hiding ((<+>))
import Data.Bits
import qualified Data.Map as M

myMod = mod4Mask

main = xmonad $ addMyKeys $ xfceConfig

    { terminal           = "urxvtcd"
    , normalBorderColor  = "#3F3F3F"
    , focusedBorderColor = "#656555"
    , modMask            = myMod
    , workspaces         = myWorkspaces
    , manageHook         = manageSpawn
                           <+> myManageHook
                           <+> manageHook xfceConfig
    -- , startupHook     = myStartup
    , borderWidth        = 2
    }

myEditor = "emacsclient -c -n -e '(switch-to-buffer nil)'"
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "web"]

myPrefixedKeys = [ ("e", spawn myEditor)

                   -- launchers
                 , ("f", spawnOn "web" "iceweasel")
                 , ("o", windows W.focusDown)
                 , ("S-o", windows W.focusUp)
                 , ("i", spawn "xmousetidy")
                 , ("S-m", spawn "urxvtcd -e mutt")

                   -- workspaces
                 , (";", toggleWS)
                 , ("0", windows $ W.greedyView "web")
                 ]

myKeys = [ ("M4-j", windows W.focusDown)
         , ("M4-k", windows W.focusUp)
         ]

myManageHook = composeOne
               [ checkDock -?> doIgnore
               , isDialog               -?> doFloat
               , className =? "Gimp"    -?> doFloat
               , className =? "MPlayer" -?> doFloat
               , return True -?> doF W.swapDown
               ]

addMyKeys conf = conf'' `additionalKeysP` myKeys
  where
    conf'      = conf `additionalKeysP` myPrefixedKeys
    conf''     = conf' { keys = addPrefix (controlMask, xK_i) (keys conf') }

-- from <http://kojevnikov.com/xmonad-metacity-gnome.html>
addPrefix p ms conf =
    M.singleton p . submap $ M.mapKeys (first chopMod) (ms conf)
  where
    mod     = modMask conf
    chopMod = (.&. complement mod)
