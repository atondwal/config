{-# LANGUAGE PatternGuards #-}
module MyScratchpad (
  -- * Usage
  -- $usage
  NamedScratchpad(..),
  nonFloating,
  defaultFloating,
  customFloating,
  NamedScratchpads,
  namedScratchpadAction,
  allNamedScratchpadAction,
  namedScratchpadManageHook,
  namedScratchpadFilterOutWorkspace,
  namedScratchpadFilterOutWorkspacePP
  ) where

import XMonad
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Util.NamedScratchpad hiding (namedScratchpadAction, allNamedScratchpadAction)

import Control.Monad (filterM)
import Data.Maybe (listToMaybe)

import qualified XMonad.StackSet as W
--
-- | Finds named scratchpad configuration by name
findByName :: NamedScratchpads -> String -> Maybe NamedScratchpad
findByName c s = listToMaybe $ filter ((s==) . name) c

-- | Runs application which should appear in specified scratchpad
runApplication :: NamedScratchpad -> X ()
runApplication = spawn . cmd

-- | Action to pop up specified named scratchpad
namedScratchpadAction :: NamedScratchpads -- ^ Named scratchpads configuration
                      -> String           -- ^ Scratchpad name
                      -> X ()
namedScratchpadAction = someNamedScratchpadAction (\f ws -> f $ head ws)

allNamedScratchpadAction :: NamedScratchpads
                         -> String
                         -> X ()
allNamedScratchpadAction = someNamedScratchpadAction mapM_

someNamedScratchpadAction :: ((Window -> X ()) -> [Window] -> X ())
                          -> NamedScratchpads
                          -> String
                          -> X ()
someNamedScratchpadAction f confs n
    | Just conf <- findByName confs n = withWindowSet $ \s -> do
                     filterCurrent <- filterM (runQuery (query conf))
                                        ((maybe [] pure . W.peek) s)
                     filterAll <- filterM (runQuery (query conf)) (W.allWindows s)
                     case filterCurrent of
                       [] -> do
                         case filterAll of
                           [] -> runApplication conf
                           _  -> f (\w -> windows $ W.focusWindow w . W.shiftWin (W.currentTag s) w) filterAll
                       _ -> do
                         if null (filter ((== scratchpadWorkspaceTag) . W.tag) (W.workspaces s))
                             then addHiddenWorkspace scratchpadWorkspaceTag
                             else return ()
                         f (windows . W.shiftWin scratchpadWorkspaceTag) filterAll
    | otherwise = return ()


-- tag of the scratchpad workspace
scratchpadWorkspaceTag :: String
scratchpadWorkspaceTag = "NSP"
-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:foldlevel=20:
