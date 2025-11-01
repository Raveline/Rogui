{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Components.List
  ( labelList,
    list,
    ListState (..),
    ListDefinition (..),
    FocusOrigin (..),
    mkListState,
    listReceiveFocus,
    labelListReceiveFocus,
    handleListEvent,
    handleLabelListEvent,
    handleClickOnList,
    handleClickOnLabelList,
    getCurrentSelection,
  )
where

import Data.List ((!?))
import Rogui.Application.Event (Event (..), EventHandlerM, KeyDownDetails (..), MouseClickDetails (..), fireEvent, getExtentPosition, getExtentSize, keycode, modifyState, redraw, unhandled)
import Rogui.Components
import Rogui.Components.Label (label)
import Rogui.Graphics (Colours, TextAlign)
import Rogui.Graphics.Types (Cell (..))
import SDL (V2 (..))
import qualified SDL

data ListState = ListState
  { selection :: Maybe Int,
    scrollOffset :: Int -- For later when you add scrolling
  }

-- | Indicates from which direction focus is entering the list
data FocusOrigin = FromNext | FromPrev
  deriving (Eq)

data ListDefinition n a = ListDefinition
  { -- | Name of the list, for extent management
    name :: n,
    -- | Content to display. The list will scroll automatically if there are too many.
    items :: [a],
    -- | How to render a single item, and should it be displayed as selected.
    renderItem :: a -> Bool -> Component n,
    -- | Height of an item
    itemHeight :: Cell,
    -- | Should the list fire focusNext / focusPrev when reaching limits,
    -- or should it wrap ?
    wrapAround :: Bool
  }

mkListState :: ListState
mkListState = ListState Nothing 0

getCurrentSelection :: ListDefinition n a -> ListState -> Maybe a
getCurrentSelection ListDefinition {..} ListState {..} = selection >>= (items !?)

-- | Update list state when it receives focus, initializing selection and scrolling appropriately.
-- This handles the common pattern of selecting the first/last item when focus enters from different directions.
listReceiveFocus :: (Ord n) => ListDefinition n a -> ListState -> FocusOrigin -> (ListState -> s -> s) -> EventHandlerM s e n ()
listReceiveFocus ListDefinition {name, items, itemHeight} ls origin modifier = do
  V2 _ visibleHeight <- getExtentSize name
  let listLength = length items
      visibleItems = getCell (visibleHeight `div` itemHeight)
  case (selection ls, origin) of
    -- Already has a selection, keep it
    (Just _, _) -> pure ()
    -- No selection, entering from next -> select first item
    (Nothing, FromNext) -> modifyState $ modifier ls {selection = Just 0, scrollOffset = 0}
    -- No selection, entering from prev -> select last item and scroll to show it
    (Nothing, FromPrev) ->
      let lastIdx = listLength - 1
          -- Scroll so the last item is visible (at the bottom of the viewport)
          newScroll = max 0 (lastIdx - visibleItems + 1)
       in modifyState $ modifier ls {selection = Just lastIdx, scrollOffset = newScroll}

-- | A specialized version of labelListReceiveFocus that doesn't need a ListDefinition
labelListReceiveFocus :: (Ord n) => n -> [a] -> ListState -> FocusOrigin -> (ListState -> s -> s) -> EventHandlerM s e n ()
labelListReceiveFocus name items ls origin modifier = do
  listReceiveFocus
    (ListDefinition {name = name, items = items, renderItem = \_ _ -> emptyComponent, itemHeight = 1, wrapAround = False})
    ls
    origin
    modifier

-- | A simple list made of labels.
labelList :: (Ord n) => n -> [a] -> (a -> String) -> TextAlign -> Colours -> Colours -> ListState -> Component n
labelList n items toText baseAlignment baseColour highlightedColours ls =
  let renderLabel item selected = do
        label (toText item) baseAlignment (if selected then highlightedColours else baseColour)
   in list ListDefinition {name = n, items = items, renderItem = renderLabel, itemHeight = 1, wrapAround = False} ls

-- | A list of arbitrary components. Pay attention that the list does not support
-- interactive components (so you can't have a list in a list, or a grid in a list, for instance),
-- though some modicum of interaction is possible by chaining handleListEvent with another handler.
list :: (Ord n) => ListDefinition n a -> ListState -> Component n
list ListDefinition {..} ListState {..} =
  let draw' = do
        recordExtent name
        visibleHeight <- contextCellHeight
        let visibleItems =
              take (getCell $ visibleHeight `div` itemHeight) $
                drop scrollOffset $
                  zip items [0 ..]
            content = vBox $ (\(item, idx) -> vSize (Fixed itemHeight) $ renderItem item (Just idx == selection)) <$> visibleItems
        draw content
   in emptyComponent {draw = draw'}

handleClickOnLabelList :: (Ord n) => n -> [a] -> ListState -> (ListState -> s -> s) -> MouseClickDetails -> EventHandlerM s e n ()
handleClickOnLabelList n items ls modifier mcd =
  handleClickOnList ListDefinition {name = n, items = items, renderItem = \_ _ -> emptyComponent, itemHeight = 1, wrapAround = False} Nothing ls modifier mcd

handleClickOnList ::
  (Ord n) =>
  ListDefinition n a ->
  Maybe (V2 Cell -> a -> EventHandlerM s e n ()) ->
  ListState ->
  (ListState -> s -> s) ->
  MouseClickDetails ->
  EventHandlerM s e n ()
handleClickOnList ListDefinition {name, items, itemHeight} onSelectedClick ls@ListState {..} modifier (MouseClickDetails _ (SDL.V2 mcx mcy) SDL.ButtonLeft) = do
  (SDL.V2 px py) <- getExtentPosition name
  let clickedCellY = getCell $ mcy - py
      clickedItemIndex = scrollOffset + (clickedCellY `div` getCell itemHeight)
      listLength = length items
  if clickedItemIndex >= 0 && clickedItemIndex < listLength
    then case (selection, onSelectedClick) of
      -- Clicking on already-selected item with action defined
      (Just selIdx, Just action) | selIdx == clickedItemIndex -> do
        let relativeX = getCell $ mcx - px
            relativeY = clickedCellY `mod` getCell itemHeight
        action (Cell <$> V2 relativeX relativeY) (items !! clickedItemIndex)
      -- Normal click: select the item
      _ -> modifyState . modifier $ ls {selection = Just clickedItemIndex}
    else unhandled
handleClickOnList _ _ _ _ _ = unhandled

handleLabelListEvent :: (Ord n) => n -> [a] -> Bool -> Event e -> ListState -> (ListState -> s -> s) -> EventHandlerM s e n ()
handleLabelListEvent n items wrapAround e ls modifier =
  handleListEvent (ListDefinition {name = n, items = items, renderItem = \_ _ -> emptyComponent, itemHeight = 1, wrapAround}) e ls modifier

handleListEvent :: (Ord n) => ListDefinition n a -> Event e -> ListState -> (ListState -> s -> s) -> EventHandlerM s e n ()
handleListEvent ListDefinition {..} event state@ListState {selection, scrollOffset} modifier = do
  V2 _ visibleHeight <- getExtentSize name
  let listLength = length items
      visibleItems = getCell (visibleHeight `div` itemHeight)
      autoScroll sel
        | sel < scrollOffset = sel
        | sel >= scrollOffset + visibleItems =
            sel - visibleItems + 1
        | otherwise = scrollOffset

  case event of
    KeyDown KeyDownDetails {key} -> case keycode key of
      SDL.KeycodeDown ->
        let newIndex = maybe 0 (+ 1) selection
         in if newIndex >= listLength
              then do
                if wrapAround
                  then redraw . modifyState . modifier $ state {selection = Just 0, scrollOffset = autoScroll 0}
                  else (modifyState . modifier $ state {selection = Nothing}) >> fireEvent FocusNext
              else do
                redraw . modifyState . modifier $ state {selection = Just newIndex, scrollOffset = autoScroll newIndex}
      SDL.KeycodeUp ->
        let newIndex = maybe (listLength - 1) (\n -> (n - 1)) selection
         in if newIndex >= 0
              then do
                let newScroll = autoScroll newIndex
                modifyState (modifier $ state {selection = Just newIndex, scrollOffset = newScroll})
              else do
                if wrapAround
                  then redraw . modifyState . modifier $ state {selection = Just (listLength - 1), scrollOffset = autoScroll (listLength - 1)}
                  else (modifyState . modifier $ state {selection = Nothing}) >> fireEvent FocusPrev
      _ -> unhandled
    _ -> unhandled
