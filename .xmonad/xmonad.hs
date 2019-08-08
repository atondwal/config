import XMonad
import XMonad.Config.Mate (mateConfig, desktopLayoutModifiers)

import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.FadeWindows  (fadeWindowsEventHook)
-- Make mate-panel switcher work, and make dragonfly work
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLogHook)


import XMonad.Layout.Fullscreen (fullscreenEventHook)
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances (StdTransformers (..))

import XMonad.Layout.Spacing (spacingWithEdge, incSpacing)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.LayoutModifier


import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import XMonad.Actions.FloatKeys

import Data.Map (Map, union, fromList)

main :: IO ()
main = do
  spawn "conky"
  spawn "compton"
  spawn "sh ~/.fehbg"
  spawn "setxkbmap -option caps:escape"
  spawn "synclient MaxTapTime=0"
  spawn "/usr/lib/kdeconnectd"
  spawn "/usr/lib/notify-osd/notify-osd" 
  spawn "redshift"
  spawn "st" -- preload into RAM :)
  xmonad $ mateConfig {
          terminal        = "st"
        , modMask         = mod4Mask
        , manageHook      = manageHook mateConfig <+> myManageHook
        , layoutHook      = desktopLayoutModifiers .
                            smartBorders .
                            mkToggle (single FULL) .
                            spacingWithEdge 10 $
                                tiled ||| Grid ||| tabbed shrinkText def
        , logHook         = ewmhDesktopsLogHook <+> fadeInactiveLogHook 0.90
        , handleEventHook = fadeWindowsEventHook <+>
                            fullscreenEventHook <+>
                            handleEventHook mateConfig
        , keys            = \cfg -> keyBindings cfg `union`
                                    keys def cfg `union`
                                    keys mateConfig cfg
        }
  where tiled = Tall nmaster delta ratio
          where
            nmaster = 1      -- The default number of windows in the master pane
            ratio   = 1/2    -- Default proportion of screen occupied by master pane
            delta   = 3/100  -- Percent of screen to increment by when resizing panes

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "term"  "st -n term"  (resource =? "term")  (geo 0 (2/3) (1/3) (1/3))
  , NS "term2" "st -n term2" (resource =? "term2") (geo (1/3) (2/3) (1/3) (1/3))
  , NS "term3" "st -n term3" (resource =? "term3") (geo (2/3) (2/3) (1/3) (1/3))
  , NS "term4" "st -n term4" (resource =? "term4") (geo 0 0 1 (1/3))
  , NS "ranger" "uxvtcd -name ranger -e ranger" (resource =? "ranger") (geo (1/5) (1/5) (1/5) (1/5))
  ]
  where geo a b c d = customFloating (W.RationalRect a b c d)

myManageHook = namedScratchpadManageHook scratchpads <+> composeAll
   [ className =? "mpv"      --> doFloat
   , className =? "pidgin"   --> doFloat
   , className =? "Gvim"   --> doFloat
   , stringProperty "WM_WINDOW_ROLE" =? "devtools" --> doFloat
   ]

keyBindings :: XConfig y -> Map (KeyMask, KeySym) (X ())
keyBindings XConfig{modMask = mMask, terminal = term} = fromList [
    -- Layout
      ((mMask, xK_x), sendMessage $ Toggle FULL)
    , ((mMask, xK_bracketright), incSpacing (-5))
    , ((mMask, xK_bracketleft),  incSpacing  5)
    , ((mMask, xK_Down),  withFocused $ keysMoveWindow (0,5))
    , ((mMask, xK_Up),    withFocused $ keysMoveWindow (0,-5))
    , ((mMask, xK_Right), withFocused $ keysMoveWindow (5,0))
    , ((mMask, xK_Left),  withFocused $ keysMoveWindow (-5,0))

    -- Scratchpads
    , ((mMask, xK_a), namedScratchpadAction scratchpads "term")
    , ((mMask, xK_s), namedScratchpadAction scratchpads "term2")
    , ((mMask, xK_d), namedScratchpadAction scratchpads "term3")
    , ((mMask, xK_z), namedScratchpadAction scratchpads "term4")
    , ((mMask, xK_o), namedScratchpadAction scratchpads "ranger")

    , ((mMask .|. shiftMask, xK_o), spawn "thunar")
    , ((mMask, xK_p),               spawn  dmenu)

    , ((mMask, xK_F8),              spawn "ibacklight -dec 10")
    , ((mMask, xK_F9),              spawn "ibacklight -inc 10")
    , ((mMask, xK_m),               spawn "mpv `xclip -o`")
    , ((mMask .|. mod1Mask, xK_w),  spawn "sh ~/bin/wall.sh")
    ]
 where dmenu = "exe=`dmenu_path | yeganesh -x -- -i -b -sb \"#689d6a\" -sf \"#2d2d2d\" -nb \"#2d2d2d\" -nf grey -fn 'Source Code Pro-9'` && eval \"$exe\""
