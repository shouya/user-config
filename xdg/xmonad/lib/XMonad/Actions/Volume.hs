--- Requires amixer installed
module XMonad.Actions.Volume
  ( raiseVolume
  , lowerVolume
  , toggleMute
  , toggleMuteMicrophone
  )
where

import Text.Printf (printf)
import XMonad (MonadIO, spawn)

raiseVolume :: MonadIO m => Int -> m ()
raiseVolume n = spawn (printf "amixer set Master playback %d%%+" n)

lowerVolume :: MonadIO m => Int -> m ()
lowerVolume n = spawn (printf "amixer set Master playback %d%%-" n)

toggleMute :: MonadIO m => m ()
toggleMute = spawn "amixer set Master playback toggle"

toggleMuteMicrophone :: MonadIO m => m ()
toggleMuteMicrophone = spawn "amixer set Capture toggle"
