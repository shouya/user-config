{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{- External deps:

 * gnome-session-flashback (DE)
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
import XMonad.Operations

import XMonad.Layout.IndependentScreens
import XMonad.Layout.Circle
import XMonad.Layout.Spacing
import XMonad.Layout.Roledex
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

import qualified XMonad.Config.Gnome as Gnome
import qualified XMonad.StackSet as W

import Codec.Binary.UTF8.String as UTF8

-- custom libs
import XMonad.Actions.FixedWorkspace
import XMonad.Actions.Volume
import XMonad.Actions.Backlight

main :: IO ()
main = do
  let conf = def { modMask = mod4Mask
                 -- Disabling focusFollowMouse will make first click
                 -- on unfocused window only focus it, not passing the
                 -- click through to the window.
                 --
                 , focusFollowsMouse = False
                 , borderWidth = 5
                 , focusedBorderColor = "#01a495"
                 , normalBorderColor = "#1f2626"
                 }
  myConf <- myConfiguration conf
  xmonad myConf

-- myConfiguration :: XConfig a -> IO (XConfig b)
myConfiguration conf = do
  channel <- createPolybarChannel

  pure $ gnomeIntegration
       $ myAppKeys
       $ myScratchpad
       $ myStartupPrograms
       $ myLayout
       $ myFloatingRules
       $ myPolybar channel
       $ myWorkspaces
       $ myKeybinding
       $ ewmh
       $ conf


replaceKeysP :: XConfig l -> [(String, X ())] -> XConfig l
replaceKeysP conf keys = conf
                         `removeKeysP` map fst keys
                         `additionalKeysP` keys

-- gnomeIntegration :: XConfig a -> XConfig b
gnomeIntegration conf =
  conf { manageHook = manageHook Gnome.gnomeConfig <+> manageHook conf
       , startupHook = Gnome.gnomeRegister >> startupHook conf
       , keys = gnomeKeys <+> keys conf
       }
  where gnomeKeys conf = mkKeymap conf [("M-S-e", spawn "gnome-session-quit --logout")]


-- myWorkspaces :: XConfig a -> XConfig a
myWorkspaces conf = conf { workspaces = myWorkspaces
                         , startupHook = wsStartupHook >> startupHook conf }
                    `replaceKeysP` wsKeys
                    `replaceKeysP` wsShiftKeys
                    `replaceKeysP` screenKeys
  where wsKeys = [ ("M-" ++ ws, viewWorkspace ws) | ws <- myWorkspaces]
        wsShiftKeys = [ ("M-S-" ++ ws, windows $ W.shift ws) | ws <- myWorkspaces]
        screenKeys = [ ("M-q", prevScreen)
                     , ("M-e", nextScreen)
                     , ("M-w", moveCurrentWorkspaceToOtherScreen)
                     ]
        myWorkspaces = map (:[]) "123456789"
        defaultScreen = 0
        extraScreen = 1
        wsStartupHook = do
          windows $ greedyViewOnScreen extraScreen "9"

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
          , ("<Print>", spawn "flameshot gui")
          , ("<XF86AudioLowerVolume>", lowerVolume 5 >> playVolume)
          , ("<XF86AudioRaiseVolume>", raiseVolume 5 >> playVolume)
          , ("<XF86AudioMute>", toggleMute >> pure ())
          , ("<XF86MonBrightnessUp>", raiseBrightness "intel_backlight" 5)
          , ("<XF86MonBrightnessDown>", lowerBrightness "intel_backlight" 5)
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
myStartupPrograms conf = conf { startupHook = newStartupHook }
  where newStartupHook = do
          -- X11 config
          spawnOnce "xrdb -merge ~/.Xresources"
          spawnOnce "xset b off"

          -- swap ctrl and caps lock; swap windows and alt key
          spawnOnce "inputplug -0 --command \"$HOME/.xmonad/scripts/new-keyboard.sh\""

          -- touchpad natural scroll mode
          spawnOnce "~/.xmonad/scripts/touchpad-natural-scroll.sh"

          -- compton
          spawnOnce "picom --shadow --no-dock-shadow --no-dnd-shadow --shadow-ignore-shaped --fading --backend glx --daemon"

          -- nm-applet
          spawnOnce "nm-applet &"

          -- clipboard
          spawnOnce "copyq &"
          spawnOnce "autocutsel -fork"

          -- polybar
          spawnOnce "~/.config/polybar/start.sh &"

          -- screensaver
          spawnOnce "xscreensaver --no-splash &"

          startupHook conf

-- myLayout :: XConfig a -> XConfig _
myLayout conf = docks $ conf { layoutHook = layout }
  where layout = avoidStruts (tallLayouts ||| tabLayout) ||| Full
        tall = smartSpacingWithEdge 5 (ResizableTall 1 (3/100) (1/2) [])
        tallLayouts = tall ||| Mirror tall
        tabLayout = simpleTabbed
        fancy = Circle ||| spiral (3/4) ||| Roledex

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
  conf { logHook = dynamicLogWithPP titlePP >> dynamicLogWithPP wsPP }
  where titlePP = defaultPP
                  { ppOutput = output titlePipe . fontSans
                  , ppTitle = id
                  , ppTitleSanitize = id
                  , ppCurrent = const ""
                  , ppVisible = const ""
                  , ppHidden = const ""
                  , ppUrgent = const ""
                  , ppLayout = const ""
                  , ppSep = ""
                  , ppWsSep = ""
                  }
        wsPP = defaultPP
               { ppOutput = output wsPipe . fontMono
               , ppTitle = const ""
               , ppCurrent = color "EAE" . wrap "[" "]" . wsNameFull
               , ppVisible = color "AFA" . wsNameFull
               , ppHidden = color "AAA" . wsNameFull
               , ppUrgent = color "E00" . wsNameFull
               , ppLayout = const ""
               , ppSep = ""
               , ppWsSep = ""
               }

        fontMono x = printf "%%{T1}%s%%{T-}" (x :: String)
        fontSans x = printf "%%{T2}%s%%{T-}" (x :: String)

        color c t = printf "%%{F#%s}%s%%{F-}" (c :: String) (t :: String)
        output pipe str = do
          let s = str ++ "\n"
          appendFile pipe (UTF8.decodeString s)

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

--- myFloatingRules :: XConfig a -> XConfig a
myFloatingRules conf = conf { manageHook = hooks <+> manageHook conf }
  where hooks = composeAll [ title =? "zoom" --> doFloat
                           , isDialog --> doFloat
                           , propertyToQuery (Role "About") --> doFloat
                           , isPrefixOf "About " <$> stringProperty "WM_ICON_NAME" --> doFloat
                           ]
