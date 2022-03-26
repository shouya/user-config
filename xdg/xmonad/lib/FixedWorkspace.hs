module FixedWorkspace where

import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Map as M

import XMonad
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace)
import XMonad.Actions.CycleWS (screenBy)
import XMonad.Actions.OnScreen (greedyViewOnScreen)
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
  viewEmptyWorkspace
  greedyViewWorkspace ws

greedyViewWorkspace :: WorkspaceId -> X ()
greedyViewWorkspace ws = do
  s <- getWorkspaceScreen ws
  windows $ greedyViewOnScreen s ws

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
