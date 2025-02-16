module XMonad.Actions.Backlight
  ( raiseBrightness,
    lowerBrightness,
  )
where

import Text.Printf (printf)
import XMonad (MonadIO, spawn)

raiseBrightness :: MonadIO m => String -> Int -> m ()
raiseBrightness dev n = spawn (printf "echo $(( $(cat %s) + %d )) | tee %s" devP n devP)
  where devP = devicePath dev

lowerBrightness :: MonadIO m => String -> Int -> m ()
lowerBrightness dev n = spawn (printf "echo $(( $(cat %s) - %d )) | tee %s" devP n devP)
  where devP = devicePath dev

devicePath :: String -> String
devicePath = printf "/sys/class/backlight/%s/brightness"
