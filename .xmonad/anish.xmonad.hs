import qualified Data.Map                     as M
import           XMonad
import           XMonad.Actions.MouseGestures
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Tabbed
import qualified XMonad.StackSet              as W
import         	 XMonad.Layout.IM   
import        	 XMonad.Layout.Grid
import        	 XMonad.Layout.Spacing
import        	 XMonad.Layout.PerWorkspace

main :: IO ()
main = xmonad $ defaultConfig {
    modMask	= mod4Mask
  , terminal	= "urxvtc"
  , borderWidth = 0
  , mouseBindings = myMouseBindings
  , layoutHook = myLayout
  , manageHook = myManageHook <+> manageHook defaultConfig 
  , startupHook = startup
  , workspaces = myWorkspaces
}

myLayout = onWorkspace "9" (spaced pidginLayout) $ tiled
                                               ||| Grid
                                               ||| spaced Grid
                                               ||| noBorders Full
                                               ||| noBorders tab
  where
     tiled   = Tall nmaster delta ratio
     tab = tabbed shrinkText defaultTheme
     pidginLayout = withIM (18/100) (Role "buddy_list") Grid
     spaced  = spacing 8
     nmaster = 1
     ratio   = 6/10
     delta   = 2/100

myWorkspaces :: [String]
myWorkspaces = map show [1 :: Integer ..9]

myManageHook = composeAll      
    [ className =? "Pidgin" --> doShift "9"
    , className =? "trayer" --> doShift "9"
    ]   


startup :: X ()
startup = do spawn "conky"
	     --spawn "pidgin"
	     spawn "sh ~/.fehbg"
	     spawn "urxvtcd"
	     spawn "trayer --height 15"
	     spawn "google-chrome"
	     spawn "nm-applet"
	     -- spawn "redshift"
	     spawn "xrandr --output LVDS1 --gamma .8:.8:.8"
	     spawn "compton --vsync opengl"
	     -- spawn "sleep 500; CopyConsole"
	     -- spawn "sleep 300; CopyAgent"
	     -- spawn "xbacklight -set 10"
	     spawn "stopwatch"
	     spawn "gnome-settings-daemon"
	     -- spawn "sleep 3; urxvtc"


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), mouseGesture gestures)
    , ((modm, button2), (\w -> focus w >> mouseMoveWindow w))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button4), \x -> windows W.swapUp)
    , ((modm, button5), \x -> windows W.swapDown)
    ]

gestures = M.fromList $
    [ ([], (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ([L],(\w -> focus w >> screenWorkspace 0
                          >>= flip whenJust (windows . W.shift)))
    , ([R],(\w -> focus w >> screenWorkspace 1
                          >>= flip whenJust (windows . W.shift)))
    ]

