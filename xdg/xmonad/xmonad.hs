{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{- External deps:

 * flameshot (prtscr)
 * slop (select window for prtscr)
 * notify-send (misc)
 * inputplug (startup hook)
 * picom (startup hook)
 * nm-applet (startup hook)
 * alacritty (app key/scratchpad/term)
 * emacs (app key)
 * malakal (app key)
 * qalculate-gtk (scratchpad)
 * stardict (scratchpad)
 * amixer (volume adjust)
 * copyq (clipboard manager)
 * autocutsel (primary/clipboard sync)
 * xscreensaver
 * rofi (app launcher with M-<space>)

  /sys/class/backlight/intel_backlight/brightness needs to be writable
  (See https://superuser.com/a/1393488)
 -}

import Data.Monoid
import Data.Maybe
import Control.Monad
import System.Exit
import qualified Data.Map as M
import Data.List
import Text.Printf
import qualified Data.Text as T

import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.NamedScratchpad
import XMonad.Util.WindowProperties ( Property (..)
                                    , propertyToQuery)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows (isFloating)
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat)
import XMonad.Hooks.StatusBar (withSB)
import XMonad.Hooks.Place (smart, underMouse, inBounds, placeHook)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.RefocusLast (refocusLastLayoutHook)
import XMonad.Config.Desktop
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWS
import XMonad.Actions.OnScreen
import XMonad.Actions.FloatKeys
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Operations

import XMonad.Layout.IndependentScreens
import XMonad.Layout.Circle
import XMonad.Layout.Spacing
import XMonad.Layout.Roledex
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.TrackFloating (trackFloating, useTransientFor)
import XMonad.Layout.Fullscreen (fullscreenSupport)

import qualified XMonad.StackSet as W

-- custom libs
import XMonad.Actions.FixedWorkspace
import XMonad.Actions.Volume
import XMonad.Actions.Backlight
import XMonad.Actions.AbsWS
import XMonad.Hooks.EwwLog

main :: IO ()
main = do
  let conf = def { modMask = mod4Mask
                 , focusFollowsMouse = False
                 , clickJustFocuses = False
                 , borderWidth = 12
                 , focusedBorderColor = "#7aa2f7"
                 , normalBorderColor = "#efeff8"
                 }
  myConf <- myConfiguration conf
  xmonad myConf

-- myConfiguration :: XConfig a -> IO (XConfig b)
myConfiguration conf = do
  pure $ myEwmh
       $ myDesktopIntegration
       $ myAppKeys
       $ myScratchpad
       $ myStartupPrograms
       $ myLayout
       $ myFloatingRules
       $ myStatusBar
       $ myWorkspaces
       $ myKeybinding
       conf


replaceKeysP :: XConfig l -> [(String, X ())] -> XConfig l
replaceKeysP conf keys = conf
                         `removeKeysP` map fst keys
                         `additionalKeysP` keys

-- myDesktopIntegration :: XConfig a -> XConfig b
myDesktopIntegration conf =
  conf { manageHook = manageHook desktopConfig <+> manageHook conf
       , startupHook = startupHook desktopConfig >> startupHook conf
       }


allWorkspaces :: [WorkspaceId]
allWorkspaces = map (:[]) "123456789"

-- myWorkspaces :: XConfig a -> XConfig a
myWorkspaces conf = conf { workspaces = allWorkspaces
                         , startupHook = startupHook conf >> wsStartupHook
                         }
                    `replaceKeysP` wsKeys
                    `replaceKeysP` wsShiftKeys
                    `replaceKeysP` screenKeys
  where wsKeys = [ ("M-" ++ ws, viewWorkspace ws) | ws <- allWorkspaces]
        wsShiftKeys = [ ("M-S-" ++ ws, windows $ W.shift ws) | ws <- allWorkspaces]
        screenKeys = [ ("M-e", nextScreenCapped)
                     , ("M-q", prevScreenCapped)
                     , ("M-S-e", shiftNextScreen)
                     -- this key is bound to kill
                     -- , ("M-S-q", shiftPrevScreen)
                     , ("C-M-w", moveCurrentWorkspaceToOtherScreen)
                     ]
        wsStartupHook = do
          _ <- getWorkspaceScreen "1"
          setWorkspaceScreen "1" 0
          setWorkspaceScreen "2" 0
          setWorkspaceScreen "3" 0
          setWorkspaceScreen "4" 0
          setWorkspaceScreen "5" 0
          setWorkspaceScreen "6" 0
          setWorkspaceScreen "7" 0
          setWorkspaceScreen "8" 0
          setWorkspaceScreen "9" 1


-- myKeybinding :: XConfig a -> XConfig a
myKeybinding conf = conf
                    `removeKeysP` map oldkey repurposedKeys
                    `replaceKeysP` map newkey repurposedKeys
                    `replaceKeysP` extraKeys
                    `replaceKeysP` floatingKeys
                    `replaceKeysP` resizeKeys
  where repurposedKeys =
          [ ("M-S-c", "M-S-q", kill)
          , ("M-S-<Return>", "M-<Return>", spawn myTerminal)
          , ("M-<Return>", "M-S-`", windows W.swapMaster)
          , ("M-m", "M-`", windows W.focusMaster)
          , ("M-q", "M-S-r", reloadXMonad)
          , ("M-p", "M-<Space>", spawn "rofi -show run")
          , ("M-<Space>", "M-0", sendMessage NextLayout)
          -- replaced by gnome-session-quit
          -- , ("M-S-q", "M-S-e", io (exitWith ExitSuccess))
          ]
        floatingKeys =
          [ ("M-<Left>",  withFocused (keysMoveWindow (-20, 0)))
          , ("M-<Right>", withFocused (keysMoveWindow (20, 0)))
          , ("M-<Up>",    withFocused (keysMoveWindow (0, -20)))
          , ("M-<Down>",  withFocused (keysMoveWindow (0, 20)))
          , ("M-S-<Left>",  withFocused (keysResizeWindow (-20, 0) (0,0)))
          , ("M-S-<Right>", withFocused (keysResizeWindow (20, 0) (0,0)))
          , ("M-S-<Up>",    withFocused (keysResizeWindow (0, -20) (0,0)))
          , ("M-S-<Down>",  withFocused (keysResizeWindow (0, 20) (0,0)))
          ]
        resizeKeys =
          [ ("M-S-h", sendMessage MirrorShrink)
          , ("M-S-l", sendMessage MirrorExpand)
          ]
        extraKeys =
          [ ("C-M-f", withFocused toggleFloat)
          , ("M-b", withFocused toggleBorder)
          , ("M-<Tab>", cycleRecentWS [xK_Super_L] xK_Tab (xK_Shift_L .|. xK_Tab))
          , ("<Print>", spawn "flameshot gui")
          , ("C-<Print>", spawn "flameshot gui --region $(slop -b 5 -p -5)")
          , ("M-S-<Space>", spawn "rofi -show window")
          , ("<XF86AudioLowerVolume>", lowerVolume 5 >> playVolume)
          , ("<XF86AudioRaiseVolume>", raiseVolume 5 >> playVolume)
          , ("<XF86AudioMute>", toggleMute >> pure ())
          , ("<XF86MonBrightnessUp>", raiseBrightness "intel_backlight" 5)
          , ("<XF86MonBrightnessDown>", lowerBrightness "intel_backlight" 5)
          , ("<XF86AudioMicMute>", toggleMuteMicrophone >> pure())
          , ("<XF86Display>", spawn "autorandr horizontal")
          ]
        oldkey (a,b,c) = a
        newkey (a,b,c) = (b,c)
        reloadXMonad = spawn "xmonad --recompile && xmonad --restart && notify-send Reloaded."
        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                                  then W.sink w s
                                  else W.float w floatingRect s)
        floatingRect = W.RationalRect (1/4) (1/4) (1/2) (1/2)
        playVolume = spawn "aplay /usr/share/sounds/sound-icons/percussion-10.wav"

myTerminal = "alacritty"

-- myStartupPrograms :: XConfig a -> XConfig a
myStartupPrograms conf = conf { startupHook = newStartupHook >> startupHook conf }
  where newStartupHook = return ()

-- myLayout :: XConfig a -> XConfig _
myLayout conf = docks $ fullscreenSupport $ conf { layoutHook = layout }
  where layout = modifier (notFull ||| full)
        notFull = smartBorders $
                  spacingWithEdge 5 $
                  avoidStruts $
                  (tallLayout ||| tabLayout)
        tallLayout = name "tall" $ ResizableTall 1 (3/100) (1/2) []
        tabLayout = name "tab" $ tabbedAlways shrinkText tabConf
        fancy = name "fancy" (Circle ||| spiral (3/4) ||| Roledex)
        full = name "full" (noBorders Full)
        name x = renamed [Replace x]
        tabConf = def
          { fontName = "xft:Noto Sans CJK TC:size=9"
          , inactiveBorderWidth = 0
          , activeBorderWidth = 0
          , decoHeight = 33
          , inactiveBorderColor = "#a9b1d6"
          , activeBorderColor = "#efeff8"
          , urgentBorderColor = "#e0af68"
          }
        modifier =
          -- keep the current window when floating window is
          -- activated, useful especially in Tabbed layout.
          refocusLastLayoutHook .
          trackFloating .
          -- useTransientFor will make it unable to switch to ws with
          -- no windows. Do not use. refocusLastLayoutHook was found
          -- to have the same effect.
          -- useTransientFor .
          id

-- myAppKeys :: XConfig a -> XConfig a
myAppKeys conf = conf
                 `replaceKeysP` map launchOrFocus appKeys
                 `replaceKeysP` map bringToCurrentWS appKeys

  where appKeys =
          [ ("<F1>", "firefox", className =? "Firefox-esr"
                           <||> className =? "Firefox"
                           <||> className =? "firefox")
          , ("<F2>", "emacs", className =? "Emacs")
          , ("<F3>", "alacritty", className =? "Alacritty")
          , ("<F4>", "malakal", className =? "malakal")
          , ("<F5>", "switch-to-slack", className =? "Slack")
          ]
        launchOrFocus (key, cmd, query) = ("M-" ++ key, runOrRaise cmd query)
        bringToCurrentWS (key, cmd, query) = ("M-S-" ++ key, runOrBring cmd query)
        runOrBring cmd query = forWindows query bringWindow (runApp cmd)
        runOrRaise cmd query = forWindows query focusWindow (runApp cmd)
        runApp cmd = spawn cmd

forWindows :: Query Bool -> (Window -> X ()) -> X () -> X ()
forWindows q f = ifWindows q (mapM_ f)

bringWindow :: Window -> X ()
bringWindow w = do
  windows $ \ss -> W.shiftWin (W.currentTag ss) w ss
  focus w

focusWindow :: Window -> X ()
focusWindow w = do
  ss <- windowset <$> get
  maybe (pure ()) viewWorkspace (W.findTag w ss)
  focus w

myStatusBar = withSB ewwStatusBar

-- myScratchpad :: XConfig a -> XConfig a
myScratchpad conf =
  conf { manageHook = manageHook conf
                   <+> namedScratchpadManageHook sps
                   <+> extraHook
       }
  `replaceKeysP` [ ("M-c", namedScratchpadAction sps "calc")
                 , ("M-d", namedScratchpadAction sps "dict")
                 , ("M-m", namedScratchpadAction sps "term")
                 ]
  where sps =  [ NS "term" "alacritty --class=scratch-term"
                    (resource =? "scratch-term" <&&> isFloating) float
               , NS "dict" "goldendict" (className =? "GoldenDict" <||>
                                         className =? "GoldenDict-ng") float
               , NS "calc" "qalculate-gtk" (className =? "Qalculate-gtk") float
               ]
        float = smartCenterFloat
        -- float = smartCenterFloat
        -- this allows me to park a scratch term to become a normal term.
        extraHook = resource =? "scratch-term" --> float

--- myFloatingRules :: XConfig a -> XConfig a
myFloatingRules conf = conf { manageHook = hooks <+> manageHook conf }
  where hooks = composeAll [ title =? "zoom " --> doFloat
                           , isDialog --> smartCenterFloat
                           , propertyToQuery (Role "About") --> smartCenterFloat
                           , isPrefixOf "About " <$> stringProperty "WM_ICON_NAME" --> smartCenterFloat
                           , className =? "flameshot" --> smartCenterFloat
                           , className =? "copyq" --> doFloat
                           , title =? "Volume Control" --> smartCenterFloat
                           , title =? "Dotcam" --> doCenterFloat
                           -- firefox popup windows (those without navigation buttons)
                           , (className =? "Firefox" <&&> firefoxPopupNormalHints)
                             --> doCenterFloat
                           ]
                where atMouse = placeHook $ inBounds $ underMouse (0.5, 0.5)
        -- normal window: program specified minimum size: 900 by 240 (or something like that)
        -- popup window: program specified minimum size: 190 by 190
        --
        -- another differentiating property is the x,y location. But
        -- these fields are obsolate and XMonad doesn't implement it.
        -- https://tronche.com/gui/x/xlib/ICC/client-to-window-manager/wm-normal-hints.html
        firefoxPopupNormalHints :: Query Bool
        firefoxPopupNormalHints = do
          w <- ask
          d <- liftX (asks display)
          sz <- liftX $ io $ getWMNormalHints d w
          case sh_min_size sz of
            Just (190, 190) -> return True
            _ -> return False

myEwmh =
  -- mark urgent (switch to workspace and focus) instead of bringing
  -- the window here.
  setEwmhActivateHook doAskUrgent .
  ewmhFullscreen .
  ewmh

smartCenterFloat :: ManageHook
smartCenterFloat = (placeHook $ inBounds $ smart (0.5, 0.5)) <+> doFloat
