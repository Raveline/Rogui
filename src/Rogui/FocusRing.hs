-- | This code is shamelessly stolen from Brick library.
-- (https://hackage-content.haskell.org/package/brick-2.10/docs/Brick-Focus.html)
module Rogui.FocusRing
  ( FocusRing (..),
    focusRing,
    focusNext,
    focusPrev,
    focusSetCurrent,
    focusRingLength,
    focusRingToList,
    focusGetCurrent,
  )
where

import qualified Data.CircularList as C

-- | Focus rings keep track of what is focus, and what components should receive it next.
newtype FocusRing n = FocusRing (C.CList n)
  deriving (Show)

focusRing :: [n] -> FocusRing n
focusRing = FocusRing . C.fromList

focusNext :: FocusRing n -> FocusRing n
focusNext r@(FocusRing l)
  | C.isEmpty l = r
  | otherwise = FocusRing $ C.rotR l

focusPrev :: FocusRing n -> FocusRing n
focusPrev r@(FocusRing l)
  | C.isEmpty l = r
  | otherwise = FocusRing $ C.rotL l

focusSetCurrent :: (Eq n) => n -> FocusRing n -> FocusRing n
focusSetCurrent n r@(FocusRing l) =
  maybe r FocusRing $ C.rotateTo n l

focusRingLength :: FocusRing n -> Int
focusRingLength (FocusRing l) = C.size l

focusRingToList :: FocusRing n -> [n]
focusRingToList (FocusRing l) = C.rightElements l

focusGetCurrent :: FocusRing n -> Maybe n
focusGetCurrent (FocusRing l) = C.focus l
