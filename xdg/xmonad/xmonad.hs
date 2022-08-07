{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{- External deps:

 * dmenu (app launcher with M-<space>
 * flameshot (prtscr)
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

  /sys/class/backlight/intel_backlight/brightness needs to be writable
  (See https://superuser.com/a/1393488)
 -}

import Data.Monoid
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
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.Config.Desktop
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWS
import XMonad.Actions.OnScreen
import XMonad.Actions.FloatKeys
import XMonad.Actions.CycleRecentWS
import XMonad.Operations

import XMonad.Layout.IndependentScreens
import XMonad.Layout.Circle
import XMonad.Layout.Spacing
import XMonad.Layout.Roledex
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Renamed

import qualified XMonad.StackSet as W

-- custom libs
import XMonad.Actions.FixedWorkspace
import XMonad.Actions.Volume
import XMonad.Actions.Backlight

main :: IO ()
main = do
  let conf = def { modMask = mod4Mask
                 , focusFollowsMouse = False
                 , clickJustFocuses = False
                 , borderWidth = 5
                 , focusedBorderColor = "#01a495"
                 , normalBorderColor = "#1f2626"
                 }
  myConf <- myConfiguration conf
  xmonad myConf

-- myConfiguration :: XConfig a -> IO (XConfig b)
myConfiguration conf = do
  channel <- createPolybarChannel

  pure $ ewmh
       $ desktopIntegration
       $ myAppKeys
       $ myScratchpad
       $ myStartupPrograms
       $ myLayout
       $ myFloatingRules
       $ myPolybar channel
       $ myWorkspaces
       $ myKeybinding
       conf


replaceKeysP :: XConfig l -> [(String, X ())] -> XConfig l
replaceKeysP conf keys = conf
                         `removeKeysP` map fst keys
                         `additionalKeysP` keys

-- desktopIntegration :: XConfig a -> XConfig b
desktopIntegration conf =
  conf { manageHook = manageHook desktopConfig <+> manageHook conf
       , startupHook = startupHook desktopConfig >> startupHook conf
       , layoutHook = desktopLayoutModifiers (layoutHook conf)
       }


-- myWorkspaces :: XConfig a -> XConfig a
myWorkspaces conf = conf { workspaces = myWorkspaces
                         , startupHook = startupHook conf >> wsStartupHook
                         }
                    `replaceKeysP` wsKeys
                    `replaceKeysP` wsShiftKeys
                    `replaceKeysP` screenKeys
  where wsKeys = [ ("M-" ++ ws, viewWorkspace ws) | ws <- myWorkspaces]
        wsShiftKeys = [ ("M-S-" ++ ws, windows $ W.shift ws) | ws <- myWorkspaces]
        screenKeys = [ ("M-e", nextScreen)
                     , ("M-S-e", shiftNextScreen)
                     , ("M-w", moveCurrentWorkspaceToOtherScreen)
                     ]
        myWorkspaces = map (:[]) "123456789"
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
          , ("M-p", "M-<Space>", spawn "dmenu_run")
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
          , ("M-<Tab>", cycleRecentWS [xK_Super_L] xK_Tab (xK_Shift_L .|. xK_Tab))
          , ("<Print>", spawn "flameshot gui")
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
myLayout conf = docks $ conf { layoutHook = layout }
  where layout = avoidStruts (tallLayouts ||| tabLayout) ||| full
        tall = smartSpacingWithEdge 5 (ResizableTall 1 (3/100) (1/2) [])
        tallLayouts = name "(|)" tall ||| name "(-)" (Mirror tall)
        tabLayout = name "(T)" simpleTabbed
        fancy = name "(~)" (Circle ||| spiral (3/4) ||| Roledex)
        full = name "(F)" Full
        name x = renamed [Replace x]

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

focusWindow :: Window -> X ()
focusWindow w = do
  ss <- windowset <$> get
  maybe (pure ()) viewWorkspace (W.findTag w ss)


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
               , NS "dict" "goldendict" (className =? "GoldenDict") float
               , NS "calc" "qalculate-gtk" (className =? "Qalculate-gtk") float
               ]
        float = customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)
        -- this allows me to park a scratch term to become a normal term.
        extraHook = resource =? "scratch-term" --> float

wsName :: WorkspaceId -> String
wsName "5" = "Slack"
wsName "6" = "Email"
wsName "7" = "Conn"
wsName _ = ""

wsNameFull :: WorkspaceId -> String
wsNameFull "NSP" = ""
wsNameFull x = circledNumber x ++ "<sub>" ++ wsName x ++ "</sub>"

circledNumber :: WorkspaceId -> String
circledNumber n = ["X⓵⓶⓷⓸⓹⓺⓻⓼⓽" !! read n]

--
data PolybarChannel = PolybarChannel { titleLogPipe :: FilePath
                                     , workspaceLogPipe :: FilePath
                                     }

createPolybarChannel :: IO PolybarChannel
createPolybarChannel = do
  spawn "mkfifo /tmp/xmonad-title-log || true"
  spawn "mkfifo /tmp/xmonad-workspace-log || true"
  pure $ PolybarChannel "/tmp/xmonad-title-log" "/tmp/xmonad-workspace-log"

myPolybar :: PolybarChannel -> XConfig a -> XConfig a
myPolybar (PolybarChannel titlePipe wsPipe) conf =
  conf { logHook = dynamicLogWithPP titlePP >> dynamicLogWithPP wsPP >> logHook conf }
  where titlePP = def
                  { ppOutput = output titlePipe . fontSans
                  , ppTitle = id
                  , ppTitleSanitize = escapeTitle
                  , ppCurrent = const ""
                  , ppVisible = const ""
                  , ppHidden = const ""
                  , ppUrgent = const ""
                  , ppLayout = const ""
                  , ppSep = ""
                  , ppWsSep = ""
                  }
        wsPP = def
               { ppOutput = output wsPipe . fontMono
               , ppTitle = const ""
               , ppCurrent = color "EAE" . wrap "[" "]" . wsNameFull
               , ppVisible = color "AFA" . wsNameFull
               , ppHidden = color "AAA" . wsNameFull
               , ppUrgent = color "E00" . wsNameFull
               , ppLayout = id
               , ppSep = ""
               , ppWsSep = ""
               }

        fontMono x = printf "%%{T1}%s%%{T-}" (x :: String)
        fontSans x = printf "%%{T2}%s%%{T-}" (x :: String)

        color c t = printf "%%{F#%s}%s%%{F-}" (c :: String) (t :: String)
        output pipe str = do
          let s = str ++ "\n"
          appendFile pipe s

        wsName :: WorkspaceId -> String
        wsName "5" = "Slack"
        wsName "6" = "Email"
        wsName "7" = "Conn"
        wsName _ = ""

        wsNameFull :: WorkspaceId -> String
        wsNameFull "NSP" = ""
        wsNameFull x = case wsName x of
                         "" -> x
                         n -> x ++ ":" ++ n

        wrap l r x = l ++ x ++ r

        escapeTitle title = T.unpack $
                            T.replace "%" "\\%" $
                            T.replace "{" "\\{" $
                            T.replace "}" "\\}" $
                            T.pack title


--- myFloatingRules :: XConfig a -> XConfig a
myFloatingRules conf = conf { manageHook = hooks <+> manageHook conf }
  where hooks = composeAll [ title =? "zoom " --> doFloat
                           , isDialog --> doFloat
                           , propertyToQuery (Role "About") --> doFloat
                           , isPrefixOf "About " <$> stringProperty "WM_ICON_NAME" --> doFloat
                           , className =? "flameshot" --> doFloat
                           , className =? "copyq" --> doFloat
                           , className =? "pavucontrol" --> doFloat
                           ]
