{-

    Sean's second attempt at an XMonad configuration, after having learnt some Haskell

    Much inspiration from Joey Hess's config

-}

import           XMonad
import           XMonad.Config.Xfce
import qualified XMonad.StackSet             as W
import           XMonad.Util.EZConfig        (additionalKeysP, removeKeysP)

import           XMonad.Actions.CycleWS      (toggleWS)
import           XMonad.Actions.RotSlaves
import           XMonad.Actions.Submap
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers

import           XMonad.Layout.Dishes
import           XMonad.Layout.FixedColumn
import           XMonad.Layout.Grid
-- import           XMonad.Layout.LayoutHints
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.Magnifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.OnHost
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.ResizeScreen

import           Control.Arrow               hiding ((<+>), (|||))
import           Control.Monad               (liftM2)
import           Data.Bits
import           Data.List                   (isInfixOf)
import qualified Data.Map                    as M

main = xmonad $ addMyKeys $ xfceConfig

    { terminal           = myTerm
    , normalBorderColor  = "#656555"
    , focusedBorderColor = "#94BFF3"
    , borderWidth        = 1
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
myEditor     = "emacscd"
myBrowser    = "iceweasel"
myWorkspaces = ["ops", "conn", "www", "comm", "view", "tail"]

-- key bindings

myPrefixedKeys = [ ("i", spawn "xmousetidy")

                   -- launchers
                 , ("g w", spawn myBrowser) -- could use `XMonad.Actions.WindowGo (runOrRaise)' here
                 , ("g e", spawn myEditor)
                 , ("g g", spawn myTerm)
                 -- , ("g m", spawn $ inMyTerm "sh -c 'offline || mbsync fastmail; mutt'")
                 , ("g m", spawn $ inMyTerm "mutt")
                 , ("g t", spawn $ inMyTerm "softbeep ssh -t ma /usr/pkg/bin/tmux attach")
                 , ("g c", spawn $ inMyTerm "ncmpcpp") -- 'c' for chaones
                 -- , ("g f", spawn "sh -c 'wmctrl -a Messenger || messengerfordesktop'")

                   -- window management
                 , ("o", windows W.focusDown)
                 , ("S-o", windows W.focusUp)

                   -- workspaces
                 , ("C-i", toggleWS)
                 ]

myUnprefixedKeys = [ ("M4-j", windows W.focusDown)
                   , ("M4-k", windows W.focusUp)

                     -- When locking the screen, also clear out my SSH
                     -- key.  Otherwise it lasts until I log off.  See
                     -- GNOME bugzilla bug #525574.  Note that PGP
                     -- keys may be set to timeout, but SSH keys can't
                     -- be.
                   , ("M4-l", spawn "sh -c 'ssh-add -D && xscreensaver-command -lock'")

                   , ("M4-S-j", windows W.swapDown)
                   , ("M4-S-k", windows W.swapUp)
                   , ("M1-<Tab>", rotSlavesDown)
                   , ("M1-S-<Tab>", rotSlavesUp)
                   ]

myUnwantedKeys = ["M-e"]

-- hooks

myFloatClasses = ["Gimp"
                 , "feh"
                 , "MPlayer"
                 , "Xfrun4"]

myManageHook = composeOne $
               [ checkDock                              -?> doIgnore
               , isDialog                               -?> doFloat
               , className     =? "Iceweasel"           -?> doShift "www"
               , className     =? "libreoffice-impress" -?> doShift "view"
               , className     =? "Vlc"                 -?> doShift "view"
               , className     =? "Evince"              -?> doShift "view"
               , title         =? "Dominion"            -?> doShift "view"
               , title         =? "Messenger"           -?> doShift "view"
               , title         =? "softbeep"            -?> doShift "comm"
               ] ++ [className =? c -?> doFloat | c <- myFloatClasses]

myLayoutHook = modHost "artemis" (avoidStrutsOn []) $
               modHost "zephyr" avoidStruts $
               smartBorders $
               -- layoutHintsWithPlacement (0.5, 0.5) $
               onWorkspace "www" (myWebLayout ||| Full) $
               onWorkspace "tail" (myDish ||| Full) $
               onWorkspace "view" (Grid ||| Full) $
               myEditing ||| Grid ||| myReadWriting ||| Full -- default for other workspaces

-- custom layouts

-- Set the fixed column layout to 90 columns to allow for 88 columns
-- of quoted e-mail text in mutt and the '> ' prefix.  Then the
-- magnification setting of 1.31 allows slave mutt windows to display
-- their 90 columns properly on artemis' 1280x screen.

myEditing = modHost "zephyr" (limitWindows 7) $
            modHost "artemis" (limitWindows 5) $
            modHost "artemis" (magnifiercz' 1.31) $
            onHost "artemis" (FixedColumn 1 20 90 10) $
            Tall 1 0.03 0.55

myReadWriting = modHost "zephyr" (resizeHorizontal 600) $
                modHost "zephyr" (resizeHorizontalRight 600) $
                modHost "artemis" (resizeHorizontal 300) $
                modHost "artemis" (resizeHorizontalRight 300) $
                limitWindows 3 $
                Dishes 1 (1/6)

myWebLayout = Mirror $ Tall 1 0.03 0.8

-- logs, compiles, tails etc.
myDish = limitWindows 5 $ Dishes 1 (1/5)

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
