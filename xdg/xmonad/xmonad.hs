{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Monoid
import Control.Monad
import System.Exit
import qualified Data.Map as M

import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.WindowGo
import XMonad.Actions.WindowBringer
import XMonad.Operations


import qualified XMonad.Config.Gnome as Gnome
import qualified XMonad.StackSet as W

import qualified DBus as D
import qualified DBus.Client as D

import Codec.Binary.UTF8.String as UTF8

main :: IO ()
main = do
  let conf = def { modMask = mod4Mask, focusFollowsMouse = False }
  myConf <- myConfiguration conf
  xmonad myConf

-- myConfiguration :: XConfig a -> IO (XConfig b)
myConfiguration conf = do
  xmonadLogAppletModifier <- xmonadLogApplet

  pure $ gnomeIntegration
       $ myKeybinding
       $ myAppKeys
       $ myScratchpad
       $ myStartupPrograms
       $ myLayout
       $ xmonadLogAppletModifier
       $ conf

replaceKeysP :: XConfig l -> [(String, X ())] -> XConfig l
replaceKeysP conf keys = conf
                         `removeKeysP` (map fst keys)
                         `additionalKeysP` keys

-- gnomeIntegration :: XConfig a -> XConfig b
gnomeIntegration conf =
  conf { manageHook = manageHook Gnome.gnomeConfig <+> manageHook conf
       , startupHook = Gnome.gnomeRegister >> startupHook conf
       , keys = gnomeKeys <+> keys conf
       }
  where gnomeKeys conf = mkKeymap conf [("M-S-e", spawn "gnome-session-quit --logout")]

-- myKeybinding :: XConfig a -> XConfig a
myKeybinding conf = conf
                    `removeKeysP` (map oldkey repurposedKeys)
                    `replaceKeysP` (map newkey repurposedKeys)
                    `replaceKeysP` extraKeys
  where repurposedKeys =
          [ ("M-S-c", "M-S-q", kill)
          , ("M-S-<Return>", "M-<Return>", spawn myTerminal)
          , ("M-<Return>", "M-S-`", windows W.swapMaster)
          , ("M-m", "M-`", windows W.focusMaster)
          , ("M-q", "M-S-r", reloadXMonad)
          , ("M-p", "M-<Space>", spawn "dmenu_run")
          , ("M-<Space>", "M-0", sendMessage NextLayout)
          -- , ("M-S-q", "M-S-e", io (exitWith ExitSuccess))
          ]
        extraKeys =
          [ ("C-M-f", withFocused toggleFloat)
          ]
        oldkey (a,b,c) = a
        newkey (a,b,c) = (b,c)
        reloadXMonad = spawn "xmonad --recompile && xmonad --restart && notify-send Reloaded."
        toggleFloat w = windows (\s -> if M.member w (W.floating s)
                                  then W.sink w s
                                  else (W.float w floatingRect s))
        floatingRect = W.RationalRect (1/4) (1/4) (1/2) (1/2)

-- ifFloat :: X () -> X () -> X ()
-- ifFloat = withFocused (M.member w (W.floating s))


myTerminal = "alacritty"

-- myStartupPrograms :: XConfig a -> XConfig a
myStartupPrograms conf = conf { startupHook = newStartupHook }
  where newStartupHook = do
          -- swap ctrl and caps lock; swap windows and alt key
          spawnOnce "inputplug -0 --command \"$HOME/.xmonad/scripts/new-keyboard.sh\""
          -- touchpad natural scroll mode
          spawnOnce "~/.xmonad/scripts/touchpad-natural-scroll.sh"
          -- compton
          spawnOnce "compton --shadow --no-dock-shadow --fading --backend glx"

          startupHook conf

-- myLayout :: XConfig a -> XConfig _
myLayout conf = docks $ conf { layoutHook = avoidStruts (layoutHook conf) }

-- myAppKeys :: XConfig a -> XConfig a
myAppKeys conf = conf
                 `replaceKeysP` (map launchOrFocus appKeys)
                 `replaceKeysP` (map bringToCurrentWS appKeys)

  where appKeys =
          [ ("<F1>", "firefox", className =? "Firefox-esr")
          , ("<F2>", "alacritty", className =? "Alacritty")
          , ("<F3>", "emacs", className =? "Emacs")
          , ("<F4>", "malakal", className =? "malakal")
          ]
        launchOrFocus (key, cmd, query) = ("M-" ++ key, runOrRaise cmd query)
        bringToCurrentWS (key, cmd, query) = ("M-S-" ++ key, runOrBring cmd query)
        runOrBring cmd query = forWindows query (windows . bringWindow) (runApp cmd)
        runApp cmd = spawn cmd

forWindows :: Query Bool -> (Window -> X ()) -> X () -> X ()
forWindows q f g = ifWindows q (flip forM_ f) g

-- myScratchpad :: XConfig a -> XConfig a
myScratchpad conf =
  conf { manageHook = manageHook conf <+> namedScratchpadManageHook sps }
  `replaceKeysP` [ ("M-c", namedScratchpadAction sps "calc")
                 , ("M-d", namedScratchpadAction sps "dict")
                 , ("M-m", namedScratchpadAction sps "term")
                 ]
  where sps =  [ NS "term" "alacritty --class=scratch-term"
                    (resource =? "scratch-term") float
               , NS "dict" "stardict" (className =? "Stardict") float
               , NS "calc" "qalculate-gtk" (className =? "Qalculate-gtk") float
               ]
        float = customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)


-- copied from alexay/xmonad-log-applet
prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
  { ppOutput = dbusOutput dbus
  , ppTitle = pangoSanitize
  , ppCurrent = pangoColor "green" . wrap "[" "]" . pangoSanitize
  , ppVisible = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
  , ppHidden = const ""
  , ppUrgent = pangoColor "red"
  , ppLayout = const ""
  , ppSep = " "
  }
  where pangoSanitize = concatMap sanitize
          where sanitize '>' = "&gt;"
                sanitize '<' = "&lt;"
                sanitize '\"' = "&quot;"
                sanitize '&' = "&amp;"
                sanitize c = [c]
        pangoColor fg = wrap left right
          where left = "<span foreground=\"" ++ fg ++ "\">"
                right = "</span>"
        dbusOutput dbus str = do
          let body = [D.toVariant ("<b>" ++ UTF8.decodeString str ++ "</b>")]
          let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update")
                       { D.signalBody = body }
          D.emit dbus signal


dbusSession :: IO D.Client
dbusSession = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log") [ D.nameAllowReplacement
                                                   , D.nameReplaceExisting
                                                   , D.nameDoNotQueue
                                                   ]
  pure dbus

-- xmonadLogApplet :: IO (XConfig a -> XConfig a)
xmonadLogApplet = do
  dbus <- dbusSession
  pure $ \conf -> conf { logHook = dynamicLogWithPP (prettyPrinter dbus) }
