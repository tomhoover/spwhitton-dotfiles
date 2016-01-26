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
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers

import           XMonad.Layout.Dishes
import           XMonad.Layout.FixedColumn
import           XMonad.Layout.Grid
-- import           XMonad.Layout.LayoutHints
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.Magnifier
import           XMonad.Layout.Maximize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.ResizeScreen

import           Control.Arrow               hiding ((<+>), (|||))
import           Data.Bits
import qualified Data.Map                    as M

main = xmonad $ xfceConfig

    { terminal           = myTerm
    , normalBorderColor  = "#656555"
    , focusedBorderColor = "#94BFF3"
    , borderWidth        = 1
    , modMask            = myMod
    , workspaces         = myWorkspaces
    , manageHook         = myManageHook
                           <+> manageHook xfceConfig
                           -- A more nuanced version of
                           -- XMonad.Hooks.InsertPosition: avoid the
                           -- master for non-dialogs, and ensure
                           -- dialogs appear on top of everything else.
                           <+> (not <$> isDialog --> doF avoidMaster)
                           <+> (isDialog --> doF W.shiftMaster)
    , layoutHook         = myLayoutHook
    } `additionalKeysP` myKeys

-- basic preferences

myMod        = mod4Mask
myTerm       = "urxvtcd"
myEditor     = "emacscd"
myBrowser    = "iceweasel"
myWorkspaces = ["ops", "conn", "www", "comm", "view", "tail"]

-- key bindings

myKeys = [ ("M4-h", spawn "xmousetidy")

           -- launchers
         , ("M4-g t", spawn $ inMyTerm "ii")
         , ("M4-g c", spawn $ inMyTerm "ncmpcpp") -- 'c' for chaones
         , ("M4-g v", spawn $ inMyTerm "alsamixer")
         , ("M4-g j", spawn "thunar")

           -- special keyboard keys
         , ("<XF86Tools>", spawn $ inMyTerm "ncmpcpp")
         , ("<XF86HomePage>", spawn "thunar")

           -- window & workspace management
         , ("M4-'", withFocused (sendMessage . maximizeRestore))
         , ("M4-;", toggleWS)

           -- When locking the screen, also clear out my SSH key.
           -- Otherwise it lasts until I log off.  See GNOME bugzilla
           -- bug #525574.  Note that PGP keys may be set to timeout,
           -- but SSH keys can't be.
         , ("M4-l", spawn "sh -c 'ssh-add -D && xscreensaver-command -lock'")

           -- TODO restore M4-l shrink keybinding to M4-S-l or something
         ]

-- hooks

myFloatClasses = [ "Gimp"
                 , "feh"
                 , "MPlayer"
                 , "Xfrun4"
                 ]

myManageHook = composeOne $
               [ checkDock                              -?> doIgnore
               , isDialog                               -?> doFloat
               , className     =? "Iceweasel"           -?> doShift "www"
               , className     =? "libreoffice-impress" -?> doShift "view"
               , className     =? "Vlc"                 -?> doShift "view"
               , className     =? "Evince"              -?> doShift "view"
               , title         =? "Dominion"            -?> doShift "view"
               , title         =? "Messenger"           -?> doShift "view"
               , title         =? "ii"                  -?> doShift "comm"
               ] ++ [className =? c -?> doFloat | c <- myFloatClasses]

-- old: `modHost "artemis" (avoidStrutsOn []) $ modHost "zephyr" avoidStruts $` instead of just `avoidStruts $`
myLayoutHook = avoidStruts $
               smartBorders $
               -- layoutHintsWithPlacement (0.5, 0.5) $
               onWorkspace "www" (maximize myWebLayout) $
               onWorkspace "tail" (myDish ||| Full) $
               onWorkspace "view" (Grid ||| Full) $
               maximize myEditing ||| maximize Grid ||| myReadWriting ||| Full -- default for other workspaces

-- custom layouts

-- Set the fixed column layout to 90 columns to allow for 88 columns
-- of quoted e-mail text in mutt and the '> ' prefix.  Then the
-- magnification setting of 1.31 allows slave mutt windows to display
-- their 90 columns properly on artemis' 1280x screen.

myEditing = limitWindows 7 $
            -- small screens: magnifiercz' 1.31 $
            -- alt: FixedColumn 1 20 90 10
            Tall 1 0.03 0.55

myReadWriting = resizeHorizontal 600 $
                resizeHorizontalRight 600 $
                limitWindows 3 $
                Dishes 1 (1/6)

myWebLayout = Mirror $ Tall 1 0.03 0.8

-- logs, compiles, tails etc.
myDish = limitWindows 5 $ Dishes 1 (1/5)

-- helper functions

inMyTerm     :: String -> String
inMyTerm cmd = unwords [myTerm, "-e", cmd]

avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) -> W.Stack t [r] rs
    _                   -> c
