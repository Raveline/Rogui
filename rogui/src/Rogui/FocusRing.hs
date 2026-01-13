-- | This code is shamelessly stolen from [Brick
-- library](https://hackage-content.haskell.org/package/brick-2.10/docs/Brick-Focus.html),
-- because why improve on something that is already perfect ?
--
-- Your application state is responsible for storing focus (and distributing
-- events to focused components). Use the `Names` parameter of `Rogui` and `Component`
-- as parameter for your focus ring.
--
-- Here is a very simplified example of how you leverage the focus ring,
-- using wiring to two `Rogui.Components.TextInput.textInput` components as an example:
--
-- @
-- data Names = PlayerNameInput | SavefileInput
-- data MyState =
--    { playerName :: String
--    , saveFileName :: String
--    , ring :: FocusRing Names
--    }
--
-- bnw :: Colours
-- bnw = Colours (Just white) (Just black)
--
-- render :: ConsoleDrawers rc rb Names MyState
-- render _brushes st = (Nothing, Nothing, hBox
--   [ textInput PlayerNameInput (playerName st) bnw (focusGetCurrent (ring st) == PlayerNameInput)
--   , textInput SaveFileInput (saveFileName st) bnw (focusGetCurrent (ring st) == SaveFileInput)
--   ])
--
-- onEvents :: EventHandler MyState () Names
-- onEvents = focusEventHandling <||> otherEventsHandling
--
-- focusEventHandling :: EventHandler MyState () Names
-- focusEventHandling _state = \case
--   (FocusNext) -> modifyState (\s -> s { ring = focusNext (ring s) }) >> redraw
--   (FocusPrev) -> modifyState (\s -> s { ring = focusPrev (ring s) }) >> redraw
--
-- otherEventsHandling :: EventHandler MyState () Names
-- otherEventsHandling s e = case focusGetCurrent (ring s) of
--   (Just PlayerNameInput) -> handleTextInputEvent e (playerName s) (\t s' -> s' { playerName = t })
--   (Just SaveFileInput) -> handleTextInputEvent e (saveFileName s) (\t s' -> s' { saveFileName = t })
--   _ -> unhandled
-- @
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

-- | Build a focus ring from a list of names
focusRing :: [n] -> FocusRing n
focusRing = FocusRing . C.fromList

-- | Alter a focus ring to pick the next component, or the first
-- item of the ring if the last one of the ring was focused before.
focusNext :: FocusRing n -> FocusRing n
focusNext r@(FocusRing l)
  | C.isEmpty l = r
  | otherwise = FocusRing $ C.rotR l

-- | Alter a focus ring to pick the previous component, or the last
-- item of the ring if the first one of the ring was focused before.
focusPrev :: FocusRing n -> FocusRing n
focusPrev r@(FocusRing l)
  | C.isEmpty l = r
  | otherwise = FocusRing $ C.rotL l

-- | Set the current focus of the ring to the given element,
-- provided it is part of the ring.
focusSetCurrent :: (Eq n) => n -> FocusRing n -> FocusRing n
focusSetCurrent n r@(FocusRing l) =
  maybe r FocusRing $ C.rotateTo n l

-- | Return the focus ring length
focusRingLength :: FocusRing n -> Int
focusRingLength (FocusRing l) = C.size l

-- | Morph the focus ring to a simple list
focusRingToList :: FocusRing n -> [n]
focusRingToList (FocusRing l) = C.rightElements l

-- | Returns the currently selected item
focusGetCurrent :: FocusRing n -> Maybe n
focusGetCurrent (FocusRing l) = C.focus l
