module XMonad.Hooks.SmartFloat
  ( smartCenterFloat
  , smartCenterFloatWithMax
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

smartCenterFloatWithMax :: Maybe (Rational, Rational) -> ManageHook
smartCenterFloatWithMax maxSize = ask >>= \window -> do
  -- Step 1: Find the size of the current screen
  winset <- liftX $ gets windowset
  let screen = screenRect . W.screenDetail . W.current $ winset
  -- Step 2: Find the size of the new window
  window' <- liftX $ getWindowRectangle window
  let targetRect = maybe id (flip maxRect) maxSize
                         (toRationalRect window' screen)
  -- Step 3: Find rectangles for all floating windows
  let floats = W.floating winset
  let floatRects = map (`fromRationalRect` screen) $
                     mapMaybe (`M.lookup` floats) $
                     W.index winset
  -- Step 4: Calculate the place for the new window
  let targetPlace = purePlaceWindow
                      (smart (0.5, 0.5))
                      screen
                      floatRects
                      (0,0)
                      (fromRationalRect targetRect screen)
  -- Step 5: Call doRectFloat with the new rectangle
  doRectFloat (toRationalRect targetPlace screen)

smartCenterFloat :: ManageHook
smartCenterFloat = smartCenterFloatWithMax (Just $ (1%3, 1%3))

fromRationalRect :: W.RationalRect -> Rectangle -> Rectangle
fromRationalRect r screen
  = let Rectangle sx sy sw sh = screen
        W.RationalRect rx ry rw rh = r
        x = floor $ fromIntegral sx + fromIntegral sw * rx
        y = floor $ fromIntegral sy + fromIntegral sh * ry
        w = floor $ fromIntegral sw * rw
        h = floor $ fromIntegral sh * rh
    in Rectangle x y w h

toRationalRect :: Rectangle -> Rectangle -> W.RationalRect
toRationalRect (Rectangle sx sy sw sh) screen
  = W.RationalRect rx ry rw rh
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
maxRect (W.RationalRect x y w h) (maxW, maxH)
  = W.RationalRect x' y' w' h'
  where x' = if w > maxW then x else x + (w - maxW) / 2
        y' = if h > maxH then y else y + (h - maxH) / 2
        w' = min w maxW
        h' = min h maxH
