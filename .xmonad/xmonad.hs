{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
import XMonad
import XMonad.Config.Mate        (mateConfig, desktopLayoutModifiers)

import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)

import XMonad.Layout.Grid                  (Grid(..))
import XMonad.Layout.Accordion             (Accordion(..))
import XMonad.Layout.MultiToggle           (mkToggle, single, Toggle(..), (??), EOT(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(..))
import XMonad.Layout.Spacing               (spacingWithEdge, incSpacing)
import XMonad.Layout.NoBorders             (smartBorders)
import XMonad.Layout.LayoutModifier

import XMonad.Util.NamedScratchpad
import XMonad.StackSet (RationalRect(..))

import XMonad.Actions.FloatKeys

import Data.Map (Map, union, fromList)
import Data.Set (toList)

import Foreign.C.Types
import Data.Word

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
  -- preload st into RAM :)
  spawn "st -n term2 "
  xmonad $ mateConfig {
      terminal   = "st"
    , modMask    = mod4Mask -- Super
    , manageHook = manageHook mateConfig <+> myManageHook
    , logHook    = logHook mateConfig    -- <+> fadeInactiveLogHook 0.90
    , layoutHook = desktopLayoutModifiers   .
                   -- hide borders if only one window is visible
                   smartBorders             .
                   -- mod+x (bound in myKeys) toggles fullscreen
                   mkToggle (single NBFULL) $
                     (spacingWithEdge 10            . -- gaps (starting at 10)
                      ModifiedLayout (WideBorder 2) $ -- my resizable borders (see BorderMessage)
                       Tall 1 (3/100) (1/2)
                       ||| Grid
                     )
                     ||| ModifiedLayout (WideBorder 0) Accordion
    , keys       = \cfg -> myKeys cfg `union`
                           keys def cfg `union`
                           keys mateConfig cfg
    , mouseBindings = \cfg -> myMouse cfg `union` mouseBindings mateConfig cfg
    }

-- increase or decrease border thickness
data BorderMessage = IncBorder | DecBorder deriving (Read, Show, Typeable)
instance Message BorderMessage

data WideBorder a = WideBorder Dimension deriving (Read, Show)
instance LayoutModifier WideBorder Window where
  unhook (WideBorder _) = do
    width <- asks (borderWidth . config)
    ws <- toList <$> gets mapped
    setBorders width ws

  redoLayout (WideBorder n) _ _ wrs = do
    setBorders n ws
    return (wrs, Just $ WideBorder n)
   where
    ws = map fst wrs

  pureMess (WideBorder n) m = doinc <$> fromMessage m
   where
    doinc IncBorder = WideBorder $ (n + 1)
    doinc DecBorder = WideBorder $ if n == 0 then 0 else (n - 1)

-- copied wholesale from NoBorders.hs (it doesn't export this fn)
setBorders :: Dimension -> [Window] -> X ()
setBorders bw ws = withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w bw) ws

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "term"  "st -n term"  (resource =? "term")  (geo 0 (2/3) (1/3) (1/3))
  , NS "term2" "st -n term2" (resource =? "term2") (geo (1/3) (2/3) (1/3) (1/3))
  , NS "term3" "st -n term3" (resource =? "term3") (geo (2/3) (2/3) (1/3) (1/3))
  , NS "term4" "st -n term4" (resource =? "term4") (geo 0 0 1 (1/3))
  , NS "ranger" "urxvtcd -name ranger -e ranger" (resource =? "ranger") (geo (1/5) (1/5) (1/5) (1/5))
  ]
 where
  geo a b c d = customFloating (RationalRect a b c d)

myManageHook = namedScratchpadManageHook scratchpads <+> composeAll
  [ className =? "mpv"      --> doFloat
  , className =? "pidgin"   --> doFloat
  , className =? "Gvim"   --> doFloat
  , stringProperty "WM_WINDOW_ROLE" =? "devtools" --> doFloat
  ]

myMouse :: XConfig y -> Map (ButtonMask, Button) (Window -> X ())
myMouse XConfig{modMask = m, terminal = term} = fromList [
    ((m               , button4)         , updateOpacity (+ 0.1))
  , ((m               , button5)         , updateOpacity (subtract 0.1))
  ]

myKeys :: XConfig y -> Map (KeyMask, KeySym) (X ())
myKeys XConfig{modMask = m, terminal = term} = fromList [
  -- Layout
    ((m               , xK_x)            , sendMessage $ Toggle NBFULL)
  , ((m               , xK_bracketright) , incSpacing (-5))
  , ((m               , xK_bracketleft)  , incSpacing   5)
  , ((m .|. shiftMask , xK_bracketright) , sendMessage $ IncBorder)
  , ((m .|. shiftMask , xK_bracketleft)  , sendMessage $ DecBorder)
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
  , ((m, xK_p),               spawn  dmenu)

  , ((m, xK_F8),              spawn "ibacklight -dec 10")
  , ((m, xK_F9),              spawn "ibacklight -inc 10")
  , ((m, xK_m),               spawn "mpv `xclip -o`")
  , ((m .|. mod1Mask, xK_w),  spawn "sh ~/bin/wall.sh")
  ]
 where
  dmenu = "exe=`dmenu_path | yeganesh -x -- -i -b -sb \"#689d6a\" -sf \"#2d2d2d\" -nb \"#2d2d2d\" -nf grey -fn 'Source Code Pro-9'` && eval \"$exe\""

-- https://github.com/yuttie/dot-xmonad/blob/master/xmonad.hs
-- http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/src/XMonad.Hooks.FadeInactive.html#setOpacity
rationalToOpacity :: Integral a => Rational -> a
rationalToOpacity r = round $ r * 0xffffffff

setOpacity :: Rational -> Window -> X ()
setOpacity r w = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WM_WINDOW_OPACITY"
    c <- getAtom "CARDINAL"
    io $ changeProperty32 dpy w a c propModeReplace [rationalToOpacity r]

opacityToRational :: Integral a => a -> Rational
opacityToRational opacity = fromIntegral opacity / 0xffffffff

getOpacity :: Window -> X Rational
getOpacity w = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WM_WINDOW_OPACITY"
    mval <- io $ getWindowProperty32 dpy a w
    return $ maybe 1 (opacityToRational . asUnsigned . head) mval
  where
    asUnsigned :: CLong -> Word32
    asUnsigned = fromIntegral

updateOpacity :: (Rational -> Rational) -> Window -> X ()
updateOpacity f w = do
    r <- getOpacity w
    let r' = max 0 $ min 1 $ f r
    setOpacity r' w
