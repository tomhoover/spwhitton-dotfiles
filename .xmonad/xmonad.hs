import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Actions.CycleWS (toggleWS)
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Actions.Submap
import XMonad.Actions.RotSlaves
import XMonad.Layout.LimitWindows
import XMonad.Layout.FixedColumn
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders

import Data.List (isInfixOf)
import Control.Arrow hiding ((<+>), (|||))
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
    , layoutHook = myLayoutHook
    }

-- basic preferences

myMod        = mod4Mask
myTerm       = "urxvtcd"
myEditor     = "emacsclient -c -n -e '(switch-to-buffer nil)'"
myBrowser    = "iceweasel"
myWorkspaces = ["ops", "conn", "www", "misc"]

-- key bindings

myPrefixedKeys = [ ("i", spawn "xmousetidy")

                   -- launchers
                 , ("g w", spawn myBrowser) -- could use `XMonad.Actions.WindowGo (runOrRaise)' here
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
                   , ("M4-S-j", windows W.swapDown)
                   , ("M4-S-k", windows W.swapUp)
                   , ("M1-<Tab>", rotSlavesDown)
                   , ("M1-S-<Tab>", rotSlavesUp)
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

myLayoutHook = avoidStruts $ myEditing ||| noBorders Full

-- custom layouts

-- this magnification setting should result in magnified Emacs windows
-- being 80 columns wide at 1024x

-- TODO: use XMonad.Layout.OnHost from contrib to set the limitWindows
-- and the magnification percentage on artemis and zephyr.
-- Alternatively put in branches of dotfiles repo (it's neater to have
-- it here though).
myEditing = limitWindows 5 $
            magnifiercz' 1.03 $
            FixedColumn 1 20 80 10

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
