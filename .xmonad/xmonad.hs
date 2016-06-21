{-

    Sean's second attempt at an XMonad configuration, after having learnt some Haskell

    Many ideas from Joey Hess's config

-}

import           XMonad
import           XMonad.Config.Xfce
import qualified XMonad.StackSet            as W
import           XMonad.Util.EZConfig       (additionalKeysP, checkKeymap)

import           XMonad.Actions.CycleWS     (toggleWS)
import           XMonad.Actions.RotSlaves
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers

import           XMonad.Layout.Dishes
-- import           XMonad.Layout.FixedColumn
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.LimitWindows
-- import           XMonad.Layout.Magnifier
import           XMonad.Layout.Maximize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Tabbed
-- import           XMonad.Layout.ResizeScreen

main = xmonad $ myConfig

myConfig = xfceConfig

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
    , startupHook        = return () >> checkKeymap myConfig myKeys
    } `additionalKeysP` myKeys

-- basic preferences

myMod        = mod4Mask
myTerm       = "urxvtcd"
myEditor     = "emacscd"
myBrowser    = "firefox"
myWorkspaces = [ "one"
               , "two"
               , "three"
               , "four"
               , "five"
               , "six"
               , "www"
               , "comm"
               , "view"
               , "tail"
               ]

-- key bindings

myKeys = [ ("M4-/", spawn "xmousetidy")

           -- launchers
         , ("M4-g t"            -- see urxvt-bell-command(1)
           , spawn "urxvtcd --bell-command \"aplay /home/swhitton/lib/annex/doc/sounds/beep.wav\" -e ii")
         , ("M4-S-;", spawn myEditor)
         , ("M4-g w", spawn myBrowser)
         , ("M4-g c", spawn $ inMyTerm "ncmpcpp") -- 'c' for chaones
         , ("M4-g v", spawn $ inMyTerm "alsamixer")
         , ("M4-g j", spawn "thunar")

           -- special keyboard keys
         , ("<XF86Tools>", spawn $ inMyTerm "ncmpcpp")
         , ("<XF86HomePage>", spawn "thunar")

           -- window & workspace management
         , ("M4-'", withFocused (sendMessage . maximizeRestore))
         , ("M4-;", toggleWS)
         , ("M4-S-i", kill)
         , ("M1-<Tab>", rotSlavesDown)
         , ("M1-S-<Tab>", rotSlavesUp)

           -- tenth workspace
         , ("M4-0", windows $ W.greedyView "tail")
         , ("M4-S-0", windows $ W.shift "tail")

           -- When locking the screen, also clear out my SSH key.
           -- Otherwise it lasts until I log off.  See GNOME bugzilla
           -- bug #525574.  Note that PGP keys may be set to timeout,
           -- but SSH keys can't be.
         , ("M4-S-l", spawn "sh -c 'ssh-add -D && xscreensaver-command -lock'")

           -- Since shutting artemis' lid to suspend crashes X and
           -- using the Xfce menu option to suspend fails to lock the
           -- screen, let's just make a binding..
         , ("M4-<F4>", spawn "sh -c 'ssh-add -D && xscreensaver-command -lock && systemctl suspend'")

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
               , className     =? "Xfce4-notifyd"       -?> doIgnore
               , isDialog                               -?> doFloat
               , className     =? "Firefox"             -?> doShift "www"
               , className     =? "LibreOffice 5.0"     -?> doShift "view"
               , className     =? "vlc"                 -?> doShift "view"
               , className     =? "Evince"              -?> doShift "view"
               , title         =? "Dominion"            -?> doShift "view"
               , title         =? "Messenger"           -?> doShift "view"
               , title         =? "ii"                  -?> doShift "comm"
               ] ++ [className =? c -?> doFloat | c <- myFloatClasses]

myLayoutHook = avoidStrutsOn [] $    -- tall screens: avoidStruts
               smartBorders $
               layoutHintsWithPlacement (0.5, 0.5) $
               onWorkspace "www" myWebLayout $
               onWorkspace "comm" myWebLayout $
               onWorkspace "tail" (myDish ||| Full) $
               onWorkspace "view" simpleTabbed $
               -- default for other workspaces:
               myEditing ||| maximize Grid ||| Full

-- custom layouts

-- Set the fixed column layout to 90 columns to allow for 88 columns
-- of quoted e-mail text in mutt and the '> ' prefix.  Then the
-- magnification setting of 1.31 allows slave mutt windows to display
-- their 90 columns properly on artemis' 1280x screen.

myEditing = maximize $
            limitWindows 7 $
            -- small screens: magnifiercz' 1.31 $
            -- alt: FixedColumn 1 20 90 10
            Tall 1 0.03 0.55

-- myReadWriting = resizeHorizontal 600 $
--                 resizeHorizontalRight 600 $
--                 limitWindows 3 $
--                 Dishes 1 (1/6)

myWebLayout = maximize $ Mirror $ Tall 1 0.03 0.7

-- logs, compiles, tails etc.
myDish = maximize $ limitWindows 5 $ Dishes 1 (1/5)

-- helper functions

inMyTerm     :: String -> String
inMyTerm cmd = unwords [myTerm, "-e", cmd]

avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) -> W.Stack t [r] rs
    _                   -> c
