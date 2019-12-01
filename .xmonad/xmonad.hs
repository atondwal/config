import Data.Map (Map, union, fromList)

import XMonad
import XMonad.Config.Mate                  (mateConfig, desktopLayoutModifiers)

import XMonad.Layout.Grid                  (Grid(..))
import XMonad.Layout.Accordion             (Accordion(..))

import XMonad.Layout.MultiToggle           (mkToggle, single, Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(..))
import XMonad.Layout.Spacing               (spacingWithEdge, incSpacing)
import XMonad.Layout.NoBorders             (smartBorders)

import XMonad.Actions.FloatKeys
import XMonad.Actions.CopyWindow (copy)

import XMonad.Prompt.Shell
import XMonad.Prompt

import XMonad.Util.NamedScratchpad
import XMonad.StackSet (RationalRect(..), view, shift)

import Opacity (updateOpacity)
import ResizeableBorders

main :: IO ()
main = do
  -- spawn "conky"
  spawn "compton"
  spawn "sh ~/.fehbg"
  spawn "setxkbmap -option caps:escape"
  spawn "synclient MaxTapTime=0"
  spawn "/usr/lib/kdeconnectd"
  spawn "/usr/lib/notify-osd/notify-osd"
  -- spawn "redshift"
  -- preload st into RAM :)
  spawn "st -n term2 "
  xmonad $ mateConfig {
      terminal   = "st"
    , modMask    = mod4Mask -- Super
    , manageHook = mconcat [
                      manageHook mateConfig
                    , namedScratchpadManageHook scratchpads
                    , className =? "mpv"    --> doFloat
                    , className =? "Gvim"   --> doFloat
                    ]
    , layoutHook = desktopLayoutModifiers   .
                   -- hide borders if only one window is visible
                   smartBorders             .
                   -- mod+x (bound in myKeys) toggles fullscreen
                   mkToggle (single NBFULL) $
                     (spacingWithEdge 10    . -- gaps (starting at 10)
                      borderWithWidth 2     $ -- my resizable borders (see ResizeableBorders)
                       Tall 1 (3/100) (1/2)
                       ||| Grid
                     )
                     ||| borderWithWidth 0 Accordion
    , keys       = \cfg -> myKeys cfg `union`
                           keys def cfg `union`
                           keys mateConfig cfg
    , mouseBindings = \cfg -> myMouse cfg `union` mouseBindings mateConfig cfg
    }

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "term"  "st -n term"  (resource =? "term")  (geo 0 (2/3) (1/3) (1/3))
  , NS "term2" "st -n term2" (resource =? "term2") (geo (1/3) (2/3) (1/3) (1/3))
  , NS "term3" "st -n term3" (resource =? "term3") (geo (2/3) (2/3) (1/3) (1/3))
  , NS "term4" "st -n term4" (resource =? "term4") (geo 0 0 1 (1/3))
  , NS "ranger" "urxvtcd -name ranger -e ranger" (resource =? "ranger") (geo (1/5) (1/5) (7/10) (7/10))
  ]
 where
  geo a b c d = customFloating (RationalRect a b c d)

myMouse :: XConfig y -> Map (ButtonMask, Button) (Window -> X ())
myMouse XConfig{modMask = m, terminal = term} = fromList [
    ((m               , button4)         , updateOpacity (+ 0.1))
  , ((m               , button5)         , updateOpacity (subtract 0.1))
  ]

myKeys :: XConfig y -> Map (KeyMask, KeySym) (X ())
myKeys XConfig{modMask = m, terminal = term, workspaces = sps} = fromList $ [
  -- Layout
    ((m               , xK_x)            , sendMessage $ Toggle NBFULL)
  , ((m               , xK_bracketright) , incSpacing (-5))
  , ((m               , xK_bracketleft)  , incSpacing   5)
  , ((m .|. shiftMask , xK_bracketright) , sendMessage IncBorder)
  , ((m .|. shiftMask , xK_bracketleft)  , sendMessage DecBorder)
  , ((m .|. shiftMask , xK_Down)         , withFocused $ keysResizeWindow ( 0, 5) (0,0))
  , ((m .|. shiftMask , xK_Up)           , withFocused $ keysResizeWindow ( 0,-5) (0,0))
  , ((m .|. shiftMask , xK_Right)        , withFocused $ keysResizeWindow ( 5, 0) (0,0))
  , ((m .|. shiftMask , xK_Left)         , withFocused $ keysResizeWindow (-5, 0) (0,0))
  , ((m               , xK_Down)         , withFocused $ keysMoveWindow ( 0, 5))
  , ((m               , xK_Up)           , withFocused $ keysMoveWindow ( 0,-5))
  , ((m               , xK_Right)        , withFocused $ keysMoveWindow ( 5, 0))
  , ((m               , xK_Left)         , withFocused $ keysMoveWindow (-5, 0))

  -- Scratchpads
  , ((m, xK_a), namedScratchpadAction scratchpads "term")
  , ((m, xK_s), namedScratchpadAction scratchpads "term2")
  , ((m, xK_d), namedScratchpadAction scratchpads "term3")
  , ((m, xK_z), namedScratchpadAction scratchpads "term4")
  , ((m, xK_o), namedScratchpadAction scratchpads "ranger")

  , ((m .|. shiftMask, xK_o), spawn "thunar")
  , ((m              , xK_p), shellPrompt greenXPConfig)

  , ((m, xK_F8),              spawn "ibacklight -dec 10")
  , ((m, xK_F9),              spawn "ibacklight -inc 10")
  , ((m, xK_m),               spawn "mpv `xclip -o`")
  , ((m .|. mod1Mask, xK_w),  spawn "sh ~/bin/wall.sh")
  , ((m .|. mod1Mask, xK_d),  spawn "sh ~/bin/floor.sh" >> spawn "sh ~/bin/wall.sh")
  ] ++ do
    (sp, k) <- zip sps [xK_1 ..]
    (act, mod) <- [(view, 0), (shift, shiftMask), (copy, shiftMask .|. controlMask)]
    [((m .|. mod, k), windows $ act sp)]
