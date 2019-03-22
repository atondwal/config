import qualified XMonad.StackSet as W

import XMonad
import XMonad.Config.Mate (mateConfig, desktopLayoutModifiers)

import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.FadeWindows  (fadeWindowsEventHook)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLogHook)

import XMonad.Layout.Fullscreen (fullscreenEventHook)
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances (StdTransformers (..))
import XMonad.Layout.Spacing (spacingWithEdge, incSpacing)
import XMonad.Layout.NoBorders (smartBorders)

import XMonad.Util.NamedScratchpad

import Data.Map (union, fromList)

term :: String
term = "st"

mMask :: KeyMask
mMask = mod4Mask

main :: IO ()
main = xmonad $ mateConfig {
          terminal        = term
        , modMask         = mMask
        , borderWidth     = 1
        , manageHook      = manageHook mateConfig <+> myManageHook
        , layoutHook      = desktopLayoutModifiers myLayoutHook
        , logHook         = ewmhDesktopsLogHook <+> fadeInactiveLogHook 0.90
        , handleEventHook = fadeWindowsEventHook <+> fullscreenEventHook <+> handleEventHook mateConfig
        , startupHook     = startupHook mateConfig <+> startup
        , keys            = \ cfg -> fromList keyBindings `union` keys def cfg `union` keys mateConfig cfg 
        }


startup :: X ()
startup = do
  spawn "conky"
  spawn "compton"
  spawn "sh ~/.fehbg"
  spawn "setxkbmap -option caps:escape"
  spawn "synclient MaxTapTime=0"
  spawn "/usr/lib/kdeconnectd"
  spawn "/usr/lib/notify-osd/notify-osd"

myManageHook = namedScratchpadManageHook scratchpads <+> composeAll
   [ className =? "mpv"      --> doFloat
   , className =? "pidgin"   --> doFloat
   , stringProperty "WM_WINDOW_ROLE" =? "devtools" --> doFloat
   ]

myLayoutHook = smartBorders .
               mkToggle (single FULL) .
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
    [ NS "term" "st -n term"
         (resource =? "term")
         (customFloating (W.RationalRect 0 (2/3) (1/3) (1/3)))
    , NS "term2" "st -n term2"
         (resource =? "term2")
         (customFloating (W.RationalRect (1/3) (2/3) (1/3) (1/3)))
    , NS "term3" "st -n term3"
         (resource =? "term3")
         (customFloating (W.RationalRect (2/3) (2/3) (1/3) (1/3)))
    , NS "term4" "st -n term4"
         (resource =? "term4")
         (customFloating (W.RationalRect 0 0 1 (1/3)))
    , scratch "ranger" " -e ranger"
    ]
    where
      scratchpadSize = W.RationalRect (1/5) (1/5) (1/5) (1/5)
      mySPFloat      = customFloating scratchpadSize
      -- Format Scratchpads
      scratch label command = NS label ("urxvtcd"  ++ " -name " ++ label ++ command)
                              (resource =? label)
                              mySPFloat

keyBindings :: [((KeyMask, KeySym), X ())]
keyBindings = [
              -- Layout
                ((mMask, xK_x), sendMessage $ Toggle FULL)
              , ((mMask, xK_bracketright),   incSpacing (-2))
              , ((mMask, xK_bracketleft),    incSpacing  2)
              -- Scratchpads
              , ((mMask, xK_a),
                 namedScratchpadAction scratchpads "term")
              , ((mMask, xK_s),
                 namedScratchpadAction scratchpads "term2")
              , ((mMask, xK_d),
                 namedScratchpadAction scratchpads "term3")
              , ((mMask, xK_z),
                 namedScratchpadAction scratchpads "term4")
              , ((mMask, xK_o),
                 namedScratchpadAction scratchpads "ranger")
              , ((mMask, xK_m),
                 namedScratchpadAction scratchpads "mocp")
              -- Menus
              , ((mMask, xK_p), spawn dmenu)
              -- Thunar
              , ((mMask .|. shiftMask, xK_o), spawn thunar)
              -- Backlight
              , ((mMask, xK_F8),              spawn "ibacklight -dec 10")
              , ((mMask, xK_F9),              spawn "ibacklight -inc 10")
              -- Misc
              , ((mMask, xK_m), spawn "mpv `xclip -o`")
              , ((mMask .|. mod1Mask, xK_w), spawn "sh ~/bin/wall.sh")
              ]
    where
      dmenu      = concat [
                    "exe=`dmenu_path | yeganesh",
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
