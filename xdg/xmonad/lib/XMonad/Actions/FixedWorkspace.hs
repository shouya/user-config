{- A module that remembers which workspace should be placed on which screen.

Usage:

  - call viewWorkspace to switch to a given workspace, it switches
    screen automatically.

  - call move*WorkspaceTo*Screen to assign a workspace to a
    screen. Next time you focus on it will bring you to the specified
    screen.

Similar feature can be implemented with built-in modules:

  1. create a startup hook that assign workspaces to a screen, e.g.

    windows $ greedyViewOnScreen extraScreen "9"

  2. call W.view instead of W.greedyView to avoid accidentally
  changing the ws assignment, e.g.

    [ ("M-" ++ ws, windows $ W.view) | ws <- myWorkspaces]

There are two main differences in this module:

  1. The assignment is dynamic, meaning you can change the assignment with moveWorkspaceToScreen function.

  2. The assignment is remembered across restarts.

  3. If an external display disconnects, it will fall back to the default display. And when the external display is plugged again, it will reuse the previous assignment.

Known glitch:

  Calling moveCurrentWorkspaceToOtherScreen will swap the visible ws
  with current ws, which appears as if the current ws was replaced by
  a ws from the other screen. This is because as we put the current
  workspace to another screen, the current screen must still show some
  workspace, that's why it was swapped with the visible ws. It has no
  effect on the assignment. TODO: Improvement to be made, instead of
  swapping workspaces, switch to another workspace assigned to the
  current display.

-}

module XMonad.Actions.FixedWorkspace
  ( moveCurrentWorkspaceToOtherScreen
  , moveCurrentWorkspaceToScreen
  , moveWorkspaceToScreen
  , viewWorkspace
  , setWorkspaceScreen
  , getWorkspaceScreen
  ) where

import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Map as M

import XMonad
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace)
import XMonad.Actions.CycleWS (screenBy)
import XMonad.Actions.OnScreen (greedyViewOnScreen, viewOnScreen)
import XMonad.StackSet as W
import XMonad.Layout.IndependentScreens (countScreens)

newtype WorkspaceScreenMapping =
  WorkspaceScreenMapping { getMapping :: M.Map WorkspaceId ScreenId
                         }
  deriving (Read, Show)

instance ExtensionClass WorkspaceScreenMapping where
  initialValue = WorkspaceScreenMapping M.empty
  extensionType = PersistentExtension

moveCurrentWorkspaceToOtherScreen :: X ()
moveCurrentWorkspaceToOtherScreen = do
  ws <- gets (W.currentTag . windowset)
  s <- screenBy 1
  moveWorkspaceToScreen ws s

moveCurrentWorkspaceToScreen :: ScreenId -> X ()
moveCurrentWorkspaceToScreen s = do
  ws <- gets (W.currentTag . windowset)
  moveWorkspaceToScreen ws s

moveWorkspaceToScreen :: WorkspaceId -> ScreenId -> X ()
moveWorkspaceToScreen ws s = do
  setWorkspaceScreen ws s
  windows $ greedyViewOnScreen s ws
  viewWorkspace ws

viewWorkspace :: WorkspaceId -> X ()
viewWorkspace ws = do
  s <- getWorkspaceScreen ws
  windows $ viewOnScreen s ws

getWorkspaceScreen :: WorkspaceId -> X ScreenId
getWorkspaceScreen ws = do
  mapping <- getMapping <$> XS.get
  case M.lookup ws mapping of
    Nothing -> screenBy 0
    Just s -> do
      screenCount <- countScreens
      if s < screenCount
        then pure s
        -- restore to the default screen
        else pure 0

setWorkspaceScreen :: WorkspaceId -> ScreenId -> X ()
setWorkspaceScreen ws s = XS.modify updateRecord
  where updateRecord (WorkspaceScreenMapping mapping) =
          WorkspaceScreenMapping (M.insert ws s mapping)
