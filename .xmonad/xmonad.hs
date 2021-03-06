import XMonad.Util.Run
import XMonad.Util.Replace
import Language.Haskell.Interpreter

import Data.Map (Map, union, fromList)
import Data.List (nub)

import XMonad
import XMonad.Config.Mate                  (mateConfig, desktopLayoutModifiers)

import XMonad.Layout.Grid                  (Grid(..))
-- import XMonad.Layout.Accordion             (Accordion(..))

import XMonad.Layout.MultiToggle           (mkToggle, single, Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(..))
import XMonad.Layout.Spacing               (spacingWithEdge, incSpacing)
import XMonad.Layout.NoBorders             (smartBorders)
import XMonad.Layout.MouseResizableTile

import XMonad.Actions.FloatKeys
import XMonad.Actions.CopyWindow (copy)

import XMonad.Prompt.Shell
import XMonad.Prompt hiding (pasteString)
import XMonad.Prompt.Eval
import XMonad.Actions.Eval
import XMonad.Prompt.Input

import XMonad.StackSet (RationalRect(..), view, shift)
import qualified XMonad.StackSet as W

import Graphics.X11.ExtraTypes.XF86
import Opacity (updateOpacity)
import ResizeableBorders
import MyAccordion
-- import MyScratchpad as S
import XMonad.Util.NamedScratchpad as S
import System.Environment
import System.Posix.Process (executeFile)

import XMonad.Util.XSelection
import XMonad.Util.Paste
import Data.Char

a :: IO (Either InterpreterError (IO ()))
a = runInterpreter $ setImports ["Prelude"] >> interpret  "print \"hello\"" (pure ())

runAOrWriteError :: IO ()
runAOrWriteError = a >>= either ((print "FAILED" >>) . safeSpawn "/usr/bin/xmessage" . pure . show) id

myStartupHook = do
  --dynFlags <- runGhc GHC.getSessionDynFlags
  -- getEnv "PATH" >>= safeSpawn "/usr/bin/xmessage" . pure . show
  -- runAOrWriteError
  -- spawn "conky"
  spawn "compton"
  spawn "sh ~/.fehbg"
  spawn "setxkbmap -option caps:escape"
  -- spawn "synclient MaxTapTime=0"
  spawn "/usr/lib/kdeconnectd"
  spawn "/usr/lib/notify-osd/notify-osd"
  -- spawn "redshift"
  -- preload st into RAM :)
  spawn "st -n term2 "
  io. print =<< mapM (runQuery className) =<< gets (W.allWindows . windowset)

main :: IO ()
main = do
  replace
  xmonad $ mateConfig {
      terminal   = "mlterm"
    , modMask    = mod4Mask -- Super
    , startupHook = myStartupHook
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
                     (spacingWithEdge 0     . -- gaps (starting at 10)
                      borderWithWidth 2     $ -- my resizable borders (see ResizeableBorders)
                       mouseResizableTile { draggerType = BordersDragger }
                       -- Tall 1 (3/100) (1/2)
                       ||| Grid
                     )
                     ||| borderWithWidth 0 MyAccordion
    , keys       = \cfg -> myKeys cfg `union`
                           keys def cfg `union`
                           keys mateConfig cfg
    , mouseBindings = \cfg -> myMouse cfg `union` mouseBindings mateConfig cfg
    , normalBorderColor = "#222222"
    , focusedBorderColor = "#666666"
    }

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "term"  "mlterm -N term"  (className  =? "term")  (geo 0 (2/3) (1/3) (1/3))
  , NS "term2" "mlterm -N term2" (className =? "term2") (geo (1/3) (2/3) (1/3) (1/3))
  , NS "term3" "mlterm -N term3" (className  =? "term3") (geo (2/3) (2/3) (1/3) (1/3))
  , NS "term4" "mlterm -N term4" (className  =? "term4") (geo (7/9) (1/10) (3/18) (1/10))
  , NS "ranger" "urxvtcd -name ranger -e ranger" (resource =? "ranger") (geo (1/5) (1/5) (7/10) (7/10))
  ]
 where
  geo a b c d = customFloating (RationalRect a b c d)

myMouse :: XConfig y -> Map (ButtonMask, Button) (Window -> X ())
myMouse XConfig{modMask = m, terminal = term} = fromList [
    ((m               , button4)         , updateOpacity (+ 0.1))
  , ((m               , button5)         , updateOpacity (subtract 0.1))
  , ((m .|. shiftMask , button5)         , \ _ -> spawn "xbacklight -dec 3")
  , ((m .|. shiftMask , button4)         , \ _ -> spawn "xbacklight -inc 3")
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
  , ((m .|. controlMask, xK_x), evalPrompt defaultEvalConfig defaultXPConfig)

  -- Scratchpads
  , ((m, xK_a), S.namedScratchpadAction scratchpads "term")
  , ((m, xK_s), S.namedScratchpadAction scratchpads "term2")
  , ((m, xK_d), S.namedScratchpadAction scratchpads "term3")
  , ((m, xK_o), S.namedScratchpadAction scratchpads "ranger")

  , ((m, xK_z), spawn "xcalib -i -a")
  , ((m .|. shiftMask, xK_o), spawn "thunar")
  , ((m              , xK_p), shellPrompt
        (greenXPConfig {historyFilter = nub, font = "xft:terminus"}))
  , ((m, xK_m), inputPrompt defaultXPConfig "Eval" >>= flip whenJust (evalExpression defaultEvalConfig))
  , ((m              , xK_r), spawn  dmenu)

  , ((0, xF86XK_MonBrightnessUp),              spawn "ibacklight -dec 10")
  , ((0, xF86XK_MonBrightnessDown),            spawn "ibacklight -inc 10")

  , ((m, xK_n),               getSelection >>= spawn . (++"\"") . ("xdotool type --clearmodifiers -- \"" ++) . capsarcasm)
  -- , ((m, xK_m),               spawn "mpv `xclip -o`")
  , ((m .|. mod1Mask, xK_w),  spawn "sh ~/bin/wall.sh")
  , ((m .|. mod1Mask, xK_d),  spawn "sh ~/bin/floor.sh" >> spawn "sh ~/bin/wall.sh")
  ] ++ do
    (sp, k) <- zip sps [xK_1 ..]
    (act, mod) <- [(shift, shiftMask), (copy, shiftMask .|. controlMask)]
    [((m .|. mod, k), windows $ act sp)]
 where
  dmenu = "exe=`dmenu_path | yeganesh -x -- -i -b -sb \"#689d6a\" -sf \"#2d2d2d\" -nb \"#2d2d2d\" -nf grey -fn 'Source Code Pro-9'` && eval \"$exe\""

  capsarcasm (x:y:xs) = toUpper x : y : capsarcasm xs
  capsarcasm xs = xs
