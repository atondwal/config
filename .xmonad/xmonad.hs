import qualified XMonad.StackSet as W

import XMonad
import XMonad.Config.Gnome

import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLogHook)
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Spacing (spacingWithEdge, incSpacing)

import XMonad.Util.EZConfig  (additionalKeys)
import XMonad.Util.NamedScratchpad

term :: String
term = "urxvtcd"

mMask :: KeyMask
mMask = mod4Mask

main :: IO ()
main = xmonad $ def {
          terminal    = term
        , modMask     = mMask
        , borderWidth = 0
        , manageHook  = namedScratchpadManageHook scratchpads <+> myManageHook
        , layoutHook  = myLayoutHook
        -- , logHook     = ewmhDesktopsLogHook <+> fadeInactiveLogHook 0.99
        , handleEventHook = fadeWindowsEventHook <+> fullscreenEventHook
        , startupHook = startup
        } `additionalKeys` keyBindings


startup :: X ()
startup = do
  spawn "conky"
  spawn "sh ~/.fehbg"
  spawn "urxvtcd"
  -- spawn "chromium"
  -- spawn "pidgin"
  -- spawn "trayer --height 15"
  -- spawn "redshift"
  -- spawn "xrandr --output LVDS1 --gamma .8:.8:.8"
  -- spawn "xrandr --output eDP1 --gamma 1.2:1.2:1.2:"
  -- spawn "compton --vsync opengl"
  -- spawn "sleep 500; CopyConsole"
  -- spawn "sleep 300; CopyAgent"
  -- spawn "xbacklight -set 10"
  -- spawn "stopwatch"
  -- spawn "gnome-settings-daemon"
  -- spawn "sleep 3; urxvtc"
  spawn "/usr/lib/kdeconnectd"
  spawn "/usr/lib/notify-osd/notify-osd"

myManageHook = composeAll
   [ className =? "mpv"  --> doFloat
   , className =? "pidgin"  --> doFloat
   , manageDocks
   ]

myLayoutHook = mkToggle (single FULL) .
               spacingWithEdge 0 $
                   tiled ||| Grid ||| tabbed shrinkText def

-- default tiling algorithm partitions the screen into two panes
tiled = Tall nmaster delta ratio
    where
      nmaster = 1      -- The default number of windows in the master pane
      ratio   = 1/2    -- Default proportion of screen occupied by master pane
      delta   = 3/100  -- Percent of screen to increment by when resizing panes

scratchpads :: [NamedScratchpad]
scratchpads =
    [ scratch "term"   ""
    , scratch "term2"  ""-- -e mplayer '-shuffle' mp3/*/*/*"
    , scratch "ranger" " -e ranger"
    ]
    where
      scratchpadSize = W.RationalRect (1/5) (1/5) (1/5) (1/5)
      mySPFloat      = customFloating scratchpadSize
      -- Format Scratchpads
      scratch label command = NS label (term ++ " -name " ++ label ++ command)
                              (resource =? label)
                              mySPFloat

keyBindings :: [((KeyMask, KeySym), X ())]
keyBindings = [
              -- Layout
                ((mMask, xK_x), sendMessage $ Toggle FULL)
              , ((mMask, xK_bracketright),   incSpacing (-2))
              , ((mMask, xK_bracketleft),    incSpacing  2)
              -- Scratchpads
              , ((mMask, xK_q),
                 namedScratchpadAction scratchpads "term")
              , ((mMask, xK_w),
                 namedScratchpadAction scratchpads "term2")
              , ((mMask, xK_o),
                 namedScratchpadAction scratchpads "ranger")
              , ((mMask, xK_m),
                 namedScratchpadAction scratchpads "mocp")
              -- Menus
              , ((mMask, xK_p), spawn dmenu)
              -- Thunar
              , ((mMask .|. shiftMask, xK_o), spawn thunar)
              -- Backlight
              , ((mMask, xK_F6),              spawn "xbacklight -dec 10")
              , ((mMask, xK_F7),              spawn "xbacklight -inc 10")
              -- Misc
              , ((mMask, xK_m), spawn "mpv `xclip -o`")
              ]
    where
      dmenu      = concat [
                    "exe=`dmenu_path | ~/.cabal/bin/yeganesh",
                    " -x ",
                    " -- -i -b ",
                    "-sb \"#689d6a\" ",
                    "-sf \"#2d2d2d\" ",
                    "-nb \"#2d2d2d\" ",
                    "-nf grey ",
                    "-fn 'Source Code Pro-9'` ",
                    "&& eval \"$exe\""
                   ]

      thunar = "thunar"
