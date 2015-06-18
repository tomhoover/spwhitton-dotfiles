import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Actions.CycleWS (toggleWS)
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Actions.Submap

import Data.List (isInfixOf)
import Control.Arrow hiding ((<+>))
import Data.Bits
import qualified Data.Map as M
import Control.Monad (liftM2)

myMod = mod4Mask

main = xmonad $ addMyKeys $ xfceConfig

    { terminal           = "urxvtcd"
    , normalBorderColor  = "#3F3F3F"
    , focusedBorderColor = "#656555"
    , modMask            = myMod
    , workspaces         = myWorkspaces
                           -- I would prefer to have newly created
                           -- windows always inserted at the top of
                           -- the non-master area.  But insertPosition
                           -- doesn't support that so let's always put
                           -- them at the end.
    , manageHook         = insertPosition End Newer
                           <+> myManageHook
                           <+> manageHook xfceConfig
    -- , startupHook     = myStartup
    , borderWidth        = 2
    }

myEditor = "emacsclient -c -n -e '(switch-to-buffer nil)'"
myWorkspaces = ["one", "two", "web", "misc"]

myPrefixedKeys = [ ("e", spawn myEditor)

                   -- launchers
                 , ("g w", spawn "iceweasel")
                 , ("s", spawn "urxvtcd")
                 , ("o", windows W.focusDown)
                 , ("S-o", windows W.focusUp)
                 , ("i", spawn "xmousetidy")
                 , ("S-m", spawn "urxvtcd -e mutt")

                   -- workspaces
                 , (";", toggleWS)
                 ]

myKeys = [ ("M4-j", windows W.focusDown)
         , ("M4-k", windows W.focusUp)
         ]

myManageHook = composeOne
               [ checkDock -?> doIgnore
               , isDialog               -?> doFloat
               , className =? "Gimp"    -?> doFloat
               , className =? "MPlayer" -?> doFloat
               , className =? "Iceweasel" -?> doShift "www"
               -- , return True -?> doF W.swapDown
               ]

addMyKeys conf = conf'' `additionalKeysP` myKeys
  where
    conf'      = conf `additionalKeysP` myPrefixedKeys `removeKeysP` ["M-e"]
    conf''     = conf' { keys = addPrefix (controlMask, xK_i) (keys conf') }

-- from <http://kojevnikov.com/xmonad-metacity-gnome.html>
addPrefix p ms conf =
    M.singleton p . submap $ M.mapKeys (first chopMod) (ms conf)
  where
    mod     = modMask conf
    chopMod = (.&. complement mod)
