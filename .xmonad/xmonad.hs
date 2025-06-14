{-# LANGUAGE ImplicitParams #-}
import XMonad.Util.Run
import XMonad.Util.Replace

import Data.Map (Map, union, fromList)
import Data.List (nub, isInfixOf)

import XMonad
import XMonad.Config.Mate                  (mateConfig, desktopLayoutModifiers)

import XMonad.Layout.Grid                  (Grid(..))
-- import XMonad.Layout.Accordion             (Accordion(..))

import XMonad.Layout.MultiToggle           (mkToggle, single, Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(..))
import XMonad.Layout.Spacing               (spacingWithEdge, incScreenWindowSpacing)
import XMonad.Layout.NoBorders             (smartBorders)
import XMonad.Layout.MouseResizableTile

import XMonad.Actions.FloatKeys
import XMonad.Actions.CopyWindow (copy)

import XMonad.Prompt.Shell
import XMonad.Prompt hiding (pasteString)
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

import XMonad.Hooks.Rescreen

import Data.IORef


myStartupHook = do
  --dynFlags <- runGhc GHC.getSessionDynFlags
  -- getEnv "PATH" >>= safeSpawn "/usr/bin/xmessage" . pure . show
  -- runAOrWriteError
  -- spawn "conky"
  spawn "pgrep firefox || (firefox -P dmodel & firefox -P default-release)"
  spawn "pgrep mate-panel || mate-panel"
  spawn "setxkbmap -option caps:escape"
  spawn "pgrep picom || picom"
  spawn "xrandr -r 60.00"
  -- spawn "fbautostart"
  spawn "sh ~/.fehbg"
  -- spawn "synclient MaxTapTime=0"
  -- spawn "/usr/lib/kdeconnectd"
  spawn "/usr/lib/notify-osd/notify-osd"
  -- spawn "redshift"
  -- spawn "pgrep chrome || google-chrome"
  io. print =<< mapM (runQuery className) =<< gets (W.allWindows . windowset)

myRandrChangeHook :: X ()
myRandrChangeHook = spawn "autorandr --change"

main :: IO ()
main = do
  ref <- newIORef 6500
  let ?temp = ref
  xmonad $ addRandrChangeHook myRandrChangeHook $ mateConfig {
      terminal   = "mlterm"
    , modMask    = mod4Mask -- Super
    , startupHook = myStartupHook
    , manageHook = mconcat [
                      manageHook mateConfig
                    , namedScratchpadManageHook scratchpads
                    , className =? "mpv"    --> doFloat
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
    , workspaces = map show [1..21]
    }

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "term"  "mlterm -N term"  (className  =? "term")  (geo 0 (2/3) (1/3) (1/3))
  , NS "term2" "mlterm -N term2" (className =? "term2") (geo (1/3) (2/3) (1/3) (1/3))
  , NS "term3" "mlterm -N term3 -w 26" (className  =? "term3") (geo (2/3) (2/3) (1/3) (1/3))
  , NS "term4" "mlterm -N term4" (className  =? "term4") (geo (7/9) (1/10) (3/18) (1/10))
  , NS "logseq" "logseq" (className =? "Logseq" <&&> (not . isInfixOf "pdf" <$> stringProperty "WM_NAME")) (geo (1/5) (1/5) (7/10) (7/10))
  , NS "pulsemixer" "mlterm -N pulsemixer -e pulsemixer" (className  =? "pulsemixer")  (geo 0 (2/3) (1/3) (1/3))
  , NS "htop" "mlterm -N htop -e htop" (className  =? "htop")  (geo (1/3) (2/3) (1/3) (1/3))
  ]
 where
  geo a b c d = customFloating (RationalRect a b c d)

myMouse :: (?temp :: IORef Int) => XConfig y -> Map (ButtonMask, Button) (Window -> X ())
myMouse XConfig{modMask = m, terminal = term} = fromList [
    ((m               , button4)                  , \ _ -> updateTemp (+ 50))
  , ((m               , button5)                  , \ _ -> updateTemp (subtract 50))
  , ((m .|. shiftMask , button5)                  , \ _ -> spawn "lux -s 3%")
  , ((m .|. shiftMask , button4)                  , \ _ -> spawn "lux -a 3%")
  ]

updateTemp :: (?temp :: IORef Int) => (Int -> Int) -> X ()
updateTemp f = do
    io $ modifyIORef ?temp f
    temp <- io $ readIORef ?temp
    -- spawn $ "notify-send " ++ (show temp)
    spawn $ "redshift -r -P -O" ++ (show temp)

myKeys :: XConfig y -> Map (KeyMask, KeySym) (X ())
myKeys XConfig{modMask = m, terminal = term, workspaces = sps} = fromList $ [
  -- Layout
    ((m               , xK_x)            , sendMessage $ Toggle NBFULL)
  , ((m               , xK_bracketright) , incScreenWindowSpacing (-5))
  , ((m               , xK_bracketleft)  , incScreenWindowSpacing   5)
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
  -- , ((m .|. controlMask, xK_x), evalPrompt defaultEvalConfig defaultXPConfig)
  -- , ((m, xK_F6), spawn "lux -a 1%")
  -- , ((m, xK_F5), spawn "lux -s 1%")
  , ((0, xF86XK_MonBrightnessUp),   spawn "lux -a 2%")
  , ((0, xF86XK_MonBrightnessDown), spawn "lux -s 2%")

  , ((m .|. shiftMask, xK_a), spawn "xrandr --auto")
  -- , ((m , xK_F7), spawn "configuredock")

  -- Scratchpads
  , ((m, xK_a), S.namedScratchpadAction scratchpads "term")
  , ((m .|. shiftMask, xK_a), S.namedScratchpadAction scratchpads "pulsemixer")
  , ((m, xK_s), S.namedScratchpadAction scratchpads "term2")
  , ((m .|. shiftMask, xK_s), S.namedScratchpadAction scratchpads "htop")
  , ((m, xK_d), S.namedScratchpadAction scratchpads "term3")
  , ((m, xK_o), S.namedScratchpadAction scratchpads "logseq")

  , ((m, xK_z), spawn "xcalib -i -a")
  , ((m .|. shiftMask, xK_o), spawn "thunar")
  -- , ((m              , xK_p), shellPrompt (greenXPConfig {historyFilter = nub}))
  , ((m              , xK_p), shellPrompt (greenXPConfig {historyFilter = nub}))
  , ((m              , xK_r), spawn  "rofi -show run")
  , ((m .|. shiftMask, xK_r), spawn  "rofi -show drun")

  , ((0, xF86XK_MonBrightnessUp),              spawn "ibacklight -dec 10")
  , ((0, xF86XK_MonBrightnessDown),            spawn "ibacklight -inc 10")

  -- , ((m, xK_n),               getSelection >>= spawn . (++"\"") . ("xdotool type --clearmodifiers -- \"" ++) . capsarcasm)
  -- , ((m, xK_m),               spawn "mpv `xclip -o`")
  , ((m .|. mod1Mask, xK_w),  spawn "sh ~/bin/wall.sh")
  , ((m .|. mod1Mask, xK_d),  spawn "sh ~/bin/floor.sh" >> spawn "sh ~/bin/wall.sh")
  ] ++ do
    (sp, k) <- zip sps ([xK_1 .. xK_9] ++ [xK_F1 ..])
    (act, mod) <- [(shift, shiftMask), (copy, shiftMask .|. controlMask), (view, 0)]
    [((m .|. mod, k), windows $ act sp)]
 where
  -- dmenu = "exe=`dmenu_path | yeganesh -- -i -b -sb \"#689d6a\" -sf \"#2d2d2d\" -nb \"#2d2d2d\" -nf grey -fn 'Source Code Pro-16'` && eval \"$exe\""
  dmenu = "rofi -combi-modi window,run,drun -show combi"

  capsarcasm (x:y:xs) = toUpper x : y : capsarcasm xs
  capsarcasm xs = xs
