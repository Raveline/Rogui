{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Components.List
  ( list,
    ListState (..),
    mkListState,
    handleListEvent,
  )
where

import Rogui.Application.Event (Event (..), KeyDownDetails (..))
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

handleListEvent :: Int -> Event -> ListState -> ListState
handleListEvent len event state@ListState {selection} = case event of
  KeyDown KeyDownDetails {key} -> case SDL.keysymKeycode key of
    SDL.KeycodeDown -> state {selection = Just $ min (len - 1) (maybe 0 (+ 1) selection)}
    SDL.KeycodeUp -> state {selection = Just $ max 0 (maybe 0 (\n -> (n - 1)) selection)}
    _ -> state
  _ -> state