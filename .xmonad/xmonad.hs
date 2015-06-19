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

main = xmonad $ addMyKeys $ xfceConfig

    { terminal           = myTerm
    , normalBorderColor  = "#3F3F3F"
    , focusedBorderColor = "#656555"
    , borderWidth        = 2
    , modMask            = myMod
    , workspaces         = myWorkspaces

-- I would prefer to have newly created windows always inserted at the
-- top of the non-master area.  But insertPosition doesn't support
-- that so let's always put them at the end.
    , manageHook         = insertPosition End Newer
                           <+> myManageHook
                           <+> manageHook xfceConfig
    }

-- basic preferences

myMod        = mod4Mask
myTerm       = "urxvtcd"
myEditor     = "emacsclient -c -n -e '(switch-to-buffer nil)'"
myBrowser    = "iceweasel"
myWorkspaces = ["one", "two", "www", "misc"]

-- key bindings

myPrefixedKeys = [ ("i", spawn "xmousetidy")

                   -- launchers
                 , ("g w", spawn myBrowser)
                 , ("g e", spawn myEditor)
                 , ("g g", spawn myTerm)
                 , ("g m", spawn $ inMyTerm "mutt")

                   -- window management
                 , ("o", windows W.focusDown)
                 , ("S-o", windows W.focusUp)

                   -- workspaces
                 , ("C-i", toggleWS)
                 ]

myUnprefixedKeys = [ ("M4-j", windows W.focusDown)
                   , ("M4-k", windows W.focusUp)
                   ]

myUnwantedKeys = ["M-e"]

-- hooks

myManageHook = composeOne
               [ checkDock                -?> doIgnore
               , isDialog                 -?> doFloat
               , className =? "Gimp"      -?> doFloat
               , className =? "MPlayer"   -?> doFloat
               , className =? "Iceweasel" -?> doShift "www"
               ]

-- helper functions

inMyTerm     :: String -> String
inMyTerm cmd = unwords $ [myTerm, "-e", cmd]

addMyKeys                   = addMyUnprefixed . prefixMainMap . addMyPrefixed . removeSomeDefaults
  where
    removeSomeDefaults conf = conf `removeKeysP` myUnwantedKeys
    addMyPrefixed conf      = conf `additionalKeysP` myPrefixedKeys
    prefixMainMap conf      = conf { keys = addPrefix
                                            (controlMask, xK_i)
                                            (keys conf) }
    addMyUnprefixed conf    = conf `additionalKeysP` myUnprefixedKeys

-- from <http://kojevnikov.com/xmonad-metacity-gnome.html>
addPrefix p ms conf =
    M.singleton p . submap $ M.mapKeys (first chopMod) (ms conf)
  where
    mod     = modMask conf
    chopMod = (.&. complement mod)
