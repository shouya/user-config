{-# LANGUAGE TemplateHaskell #-}
module XMonad.Hooks.EwwLog
  ( ewwStatusBar
  , ewwLayoutLog
  , ewwTitleLog
  , ewwWorkspaceLog
  )
where

import Data.Function

import Data.Aeson
import Data.Aeson.TH

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Hooks.StatusBar
import qualified XMonad.Hooks.StatusBar.PP as PP


ewwStatusBar :: StatusBarConfig
ewwStatusBar = StatusBarConfig log (pure ()) (pure ())
  where log = ewwLayoutLog <> ewwTitleLog <> ewwWorkspaceLog

data WorkspaceDesc = WorkspaceDesc
  { index     :: Int
  , name      :: String
  , visible   :: Bool
  , current   :: Bool
  , urgent    :: Bool
  }

deriveJSON defaultOptions ''WorkspaceDesc

makePPWS :: X PP.WS
makePPWS = do
  winset <- gets windowset
  urgents <- readUrgents

  PP.WS { wsWindowSet = winset
        , wsUrgents = urgents
        , wsWS = S.workspace $ S.current winset
        , wsPP = def
        }

workspaceDescriptions :: X [WorkspaceDesc]
workspaceDescriptions = do
  ppws <- makePPWS

  s <- gets windowset

  allWorkspaceTags <- gets (workspaces . config)
  workspaces <- (S.current s : S.visible s) ++
                S.hidden s
  let wsIndex = \ws -> fromJust $ elemIndex (S.tag ws) allWorkspaceTags
  let workspaces = sortBy (comparing `on` wsIndex) workspaces

  let parseWs = \ws -> toDesc (wsIndex ws) <$> ppws {wsWs = ws}
  pure $ map parseWs workspaces

  where toDesc :: Int -> PP.WS -> WorkspaceDesc
        toDesc index ppws =
          WorkspaceDesc { index   = index
                        , name    = S.tag (PP.wsWS ppws)
                        , visible = isVisible' ppws
                        , urgent  = isUrgent ppws
                        , current = isCurrent' ppws
                        }


ewwLayoutLog :: X ()
ewwLayoutLog = pure ()
ewwTitleLog :: X ()
ewwTitleLog = pure ()
ewwWorkspaceLog :: X ()
ewwWorkspaceLog = do
  wsDescs <- workspaceDescriptions
  xmonadPropLog' "_XMONAD_WORKSPACE_LOG" (encode wsDesc)
