module XMonad.Hooks.SmartFloat
  ( smartCenterFloat
  )
where

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))

import XMonad.Hooks.Place (smart, inBounds, purePlaceWindow)
import XMonad.Hooks.ManageHelpers (doRectFloat)

-- type ManageHook = Query (Endo WindowSet)
-- type Query a = ReaderT Window X a
--        which is Window -> (X a)
-- type Endo a = a -> a
--
-- so overall, type ManageHook = Window -> (X (WindowSet -> WindowSet))

-- This one will try to use XMonad.Hooks.Place's purePlaceWindow
-- function to calculate a rectangle for the window. The window will
-- have the maximum size of (1/3) by (1/3) of the screen. The window
-- will be placed at the center, or if it's occupied, a calculated
-- place.
--
-- | Compute the new position of a window according to a placement policy.
-- purePlaceWindow :: Placement -- ^ The placement strategy
--                 -> Rectangle -- ^ The screen
--                 -> [Rectangle] -- ^ The other visible windows
--                 -> (Position, Position) -- ^ The pointer's position.
--                 -> Rectangle -- ^ The window to be placed
--                 -> Rectangle
--
-- I should call the helper functions defined below.
smartCenterFloat :: ManageHook
smartCenterFloat = ask >>= \window -> do
  -- step 1: find the size of the current screen
  screen <- gets $ screenRect . W.screenDetail . W.current . windowset
  -- step 2: find the size of the new window, and calculate the max size
  window' <- liftX $ getWindowRectangle window
  let targetRect = maxRect (toRationalRect window' screen) (1%3, 1%3)
  -- step 3: find rectangles for all floating windows (in current workspace)
  floats <- gets $ W.floating . windowset
  let floatRects = map (`fromRationalRect` screen) $ mapMaybe (`M.lookup` floats) $ W.index . windowset $ xs
  -- step 4: calculate the place for the new window (placement should be "smart (0.5, 0.5)")
  let targetPlace = purePlaceWindow (smart (0.5, 0.5)) screen floatRects (0,0) (fromRationalRect targetRect screen xs)
  -- step 5: call doRectFloat with the new rectangle
  doRectFloat (toRationalRect targetPlace screen) window


fromRationalRect :: W.RationalRect -> Rectangle -> Rectangle
fromRationalRect r screen = let Rectangle sx sy sw sh = screen
                                W.RationalRect rx ry rw rh = r
                                x = floor $ fromIntegral sx + fromIntegral sw * rx
                                y = floor $ fromIntegral sy + fromIntegral sh * ry
                                w = floor $ fromIntegral sw * rw
                                h = floor $ fromIntegral sh * rh
                            in Rectangle x y w h

toRationalRect :: Rectangle -> Rectangle -> W.RationalRect
toRationalRect (Rectangle sx sy sw sh) screen = W.RationalRect rx ry rw rh
  where rx = toRational (sx - sx') / toRational sw'
        ry = toRational (sy - sy') / toRational sh'
        rw = toRational sw / toRational sw'
        rh = toRational sh / toRational sh'
        Rectangle sx' sy' sw' sh' = screen

getWindowRectangle :: Window -> X Rectangle
getWindowRectangle window
  = do d <- asks display
       (_, x, y, w, h, _, _) <- io $ getGeometry d window
       b <- asks $ borderWidth . config
       return $ Rectangle x y (w + 2*b) (h + 2*b)

maxRect :: W.RationalRect -> (Rational, Rational) -> W.RationalRect
maxRect (W.RationalRect x y w h) (maxW, maxH) = W.RationalRect x' y' w' h'
  where x' = if w > maxW then x else x + (w - maxW) / 2
        y' = if h > maxH then y else y + (h - maxH) / 2
        w' = min w maxW
        h' = min h maxH
