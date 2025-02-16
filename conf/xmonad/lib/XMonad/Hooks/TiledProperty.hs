-- This module provides functionality to set the _NET_WM_STATE_TILED
-- property on windows in XMonad. It can be used in conjunction with
-- compositors like Picom to apply different effects (e.g., shadows)
-- to tiled and floating windows.
--
-- Example Picom config:
--
-- shadow-exclude = [
--   "_NET_WM_STATE_TILED@:32c = 1"
-- ];

{-# LANGUAGE LambdaCase #-}

module XMonad.Hooks.TiledProperty (
    handleTiled,
    tiledWindowPropertyHook
) where

import qualified Data.Map as M
import Data.Monoid (All(..))
import Control.Monad (unless)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.WindowProperties (getProp32)

-- | Modifies the XConfig to include the tiled window property hook
handleTiled :: XConfig l -> XConfig l
handleTiled conf = conf
    { handleEventHook = handleEventHook conf <+> tiledWindowPropertyHook
    }

-- | Event hook to set the tiled property on windows
tiledWindowPropertyHook :: Event -> X All
tiledWindowPropertyHook = \case
    ConfigureEvent { ev_window = w, ev_override_redirect = ov } -> do
        unless ov $ do  -- only handle non-override configure events
            isFloating <- isWindowFloating w
            setTiledProperty w isFloating
        return (All True)
    _ -> return (All True)

-- | Check if a window is floating
isWindowFloating :: Window -> X Bool
isWindowFloating w = withWindowSet $ return . M.member w . W.floating

-- | Set the _NET_WM_STATE_TILED property on a window
setTiledProperty :: Window -> Bool -> X ()
setTiledProperty w isFloating = do
    dpy <- asks display
    atom <- getAtom "_NET_WM_STATE_TILED"
    c <- getAtom "CARDINAL"
    let prop = case isFloating of
            True -> 0
            False -> 1
    io $ changeProperty32 dpy w atom c propModeReplace [prop]
