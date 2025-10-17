{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Components.List
  ( list,
    ListState (..),
    mkListState,
    handleListEvent,
  )
where

import Rogui.Application.Event (Event (..), EventHandlingM, KeyDownDetails (..), fireEvent, modifyState)
import Rogui.Components.Types (Component (..), emptyComponent)
import Rogui.Graphics.Console (TextAlign)
import Rogui.Graphics.DSL.Instructions (Colours, setColours, strLn)
import qualified SDL

data ListState = ListState
  { selection :: Maybe Int,
    scrollOffset :: Int -- For later when you add scrolling
  }

mkListState :: ListState
mkListState = ListState Nothing 0

list :: [a] -> (a -> String) -> TextAlign -> Colours -> Colours -> ListState -> Component n
list items toText baseAlignment baseColour highlightedColours ListState {..} =
  let displayItem (item, idx) = do
        if (Just idx == selection)
          then setColours highlightedColours
          else setColours baseColour
        strLn baseAlignment (toText item)
   in emptyComponent {draw = \_ _ -> mapM_ displayItem (zip items [0 ..])}

handleListEvent :: Int -> Event e -> ListState -> (ListState -> s -> s) -> EventHandlingM s e ()
handleListEvent len event state@ListState {selection} modifier = case event of
  KeyDown KeyDownDetails {key} -> case SDL.keysymKeycode key of
    SDL.KeycodeDown ->
      let newIndex = maybe 0 (+ 1) selection
       in if newIndex >= len
            then do
              fireEvent FocusNext
              modifyState . modifier $ state {selection = Nothing}
            else modifyState (modifier $ state {selection = Just newIndex})
    SDL.KeycodeUp ->
      let newIndex = maybe (len - 1) (\n -> (n - 1)) selection
       in if newIndex >= 0
            then modifyState (modifier $ state {selection = Just newIndex})
            else do
              fireEvent FocusPrev
              modifyState . modifier $ state {selection = Nothing}
    _ -> pure ()
  _ -> pure ()