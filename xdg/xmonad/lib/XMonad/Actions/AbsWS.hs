module XMonad.Actions.AbsWS
  ( nextScreenCapped
  , prevScreenCapped
  )
where

import XMonad
import XMonad.StackSet


-- Mostly copied from XMonad.Actions.CycleWS
-- Except that the *Capped actions will stop at the limit instead of cycling.

-- | View next screen
nextScreenCapped :: X ()
nextScreenCapped = switchScreen 1

-- | View prev screen
prevScreenCapped :: X ()
prevScreenCapped = switchScreen (-1)

switchScreen :: Int -> X ()
switchScreen d = do s <- screenBy d
                    mws <- screenWorkspace s
                    case mws of
                         Nothing -> return ()
                         Just ws -> windows (view ws)

{- | Get the 'ScreenId' /d/ places over. Example usage is a variation of the
the default screen keybindings:

>     -- mod-{w,e}, Switch to previous/next Xinerama screen
>     -- mod-shift-{w,e}, Move client to previous/next Xinerama screen
>     --
>     [((m .|. modm, key), sc >>= screenWorkspace >>= flip whenJust (windows . f))
>         | (key, sc) <- zip [xK_w, xK_e] [(screenBy (-1)),(screenBy 1)]
>         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-}
screenBy :: Int -> X ScreenId
screenBy d = do ws <- gets windowset
                --let ss = sortBy screen (screens ws)
                let now = screen (current ws)
                return $
                  (now + fromIntegral d) `min`
                  fromIntegral (length (screens ws)) `max`
                  0
