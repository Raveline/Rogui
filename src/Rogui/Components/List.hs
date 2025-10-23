{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Components.List
  ( list,
    ListState (..),
    mkListState,
    handleListEvent,
    handleClickOnList,
  )
where

import Rogui.Application.Event (Event (..), EventHandlingM, KeyDownDetails (..), MouseClickDetails (..), fireEvent, getExtentPosition, getExtentSize, modifyState, redraw)
import Rogui.Components.Types (Component (..), contextCellHeight, emptyComponent, recordExtent)
import Rogui.Graphics.Console (TextAlign)
import Rogui.Graphics.DSL.Instructions (Colours, setColours, strLn)
import Rogui.Graphics.Types (Cell (..))
import SDL (V2 (..))
import qualified SDL

data ListState = ListState
  { selection :: Maybe Int,
    scrollOffset :: Int -- For later when you add scrolling
  }

mkListState :: ListState
mkListState = ListState Nothing 0

list :: (Ord n) => n -> [a] -> (a -> String) -> TextAlign -> Colours -> Colours -> ListState -> Component n
list n items toText baseAlignment baseColour highlightedColours ListState {..} =
  let displayItem (item, idx) = do
        if (Just idx == selection)
          then setColours highlightedColours
          else setColours baseColour
        strLn baseAlignment (toText item)
      draw' = do
        recordExtent n
        visibleHeight <- contextCellHeight
        let visibleItems =
              take (getCell visibleHeight) $
                drop scrollOffset $
                  zip items [0 ..]
        mapM_ displayItem visibleItems
   in emptyComponent {draw = draw'}

handleClickOnList :: (Ord n) => n -> Int -> ListState -> (ListState -> s -> s) -> MouseClickDetails -> EventHandlingM s e n ()
handleClickOnList n listLength ls@ListState {..} modifier (MouseClickDetails _ (SDL.V2 _ mcy) SDL.ButtonLeft) = do
  (SDL.V2 _ py) <- getExtentPosition n
  let lineClicked = getCell $ mcy - py
      newSelection = lineClicked - scrollOffset
  modifyState . modifier $ ls {selection = if newSelection >= 0 && newSelection < listLength then Just newSelection else Nothing}
handleClickOnList _ _ _ _ _ = pure ()

handleListEvent :: (Ord n) => n -> Int -> Event e -> ListState -> (ListState -> s -> s) -> EventHandlingM s e n ()
handleListEvent listName listLength event state@ListState {selection, scrollOffset} modifier = do
  V2 _ visibleHeight <- getExtentSize listName

  let autoScroll sel
        | sel < scrollOffset = sel
        | sel >= scrollOffset + getCell visibleHeight =
            sel - getCell visibleHeight + 1
        | otherwise = scrollOffset

  case event of
    KeyDown KeyDownDetails {key} -> case SDL.keysymKeycode key of
      SDL.KeycodeDown ->
        let newIndex = maybe 0 (+ 1) selection
         in if newIndex >= listLength
              then do
                fireEvent FocusNext
                modifyState . modifier $ state {selection = Nothing}
              else do
                let newScroll = autoScroll newIndex
                redraw . modifyState . modifier $ state {selection = Just newIndex, scrollOffset = newScroll}
      SDL.KeycodeUp ->
        let newIndex = maybe (listLength - 1) (\n -> (n - 1)) selection
         in if newIndex >= 0
              then do
                let newScroll = autoScroll newIndex
                modifyState (modifier $ state {selection = Just newIndex, scrollOffset = newScroll})
              else do
                fireEvent FocusPrev
                redraw . modifyState . modifier $ state {selection = Nothing}
      _ -> pure ()
    _ -> pure ()
