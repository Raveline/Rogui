{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This module exposes simple list components.
--
-- Lists are offered as _vertical_ components (no horizontal lists, but you
-- should be able to implement them by taking this module as an example),
-- in two flavours:
--
-- - A simple `labelList` for basic list of texts;
-- - A more involved `list` for arbitrary components.
--
-- Like all default Rogui components, it is assumed that you do not
-- put interactive components inside other interactive components. So,
-- a list inside of a list, for instance, is not supported.
--
-- Both components come with their own handlers for events and for
-- mouse click management.
--
-- Finally, both components are scrollable if their content is too
-- big to be displayed entirely on the available console space they
-- received in the layout (or if you used a `Fixed` vSize that is
-- smaller than the amount of items to display).
-- As a result, you do not need to compose `list` with the `viewport`
-- component.
module Rogui.Components.List
  ( -- * Main components
    labelList,
    list,

    -- * State and configuration utilities
    ListState (..),
    ListDefinition (..),
    mkListState,

    -- * Focus helpers
    listReceiveFocus,
    labelListReceiveFocus,

    -- * Event handlers
    handleListEvent,
    handleListEvent',
    handleLabelListEvent,
    handleClickOnList,
    handleClickOnLabelList,
    ListAction (..),
    defaultListAction,

    -- * Utilities
    getCurrentSelection,
  )
where

import Data.Bifunctor
import Data.List ((!?))
import Linear (V2 (..))
import Rogui.Application.Event
import Rogui.Components.Core
import Rogui.Components.Label
import Rogui.Graphics (Colours, TextAlign)
import Rogui.Graphics.Types (Cell (..))

-- | A list state, storing the index of the current selection
-- and the offset for scrolling.
data ListState = ListState
  { selection :: Maybe Int,
    scrollOffset :: Int -- For later when you add scrolling
  }

-- | A configuration object for creating lists.
-- This is mostly here to avoid having a long and obscure series
-- of parameters when defining a list.
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

-- | A default list state maker. Careful, if the list is the first
-- element of your focus ring, this might not be suitable as it
-- initialises the list as having no selection.
mkListState :: ListState
mkListState = ListState Nothing 0

-- | A utility to extract the currently selected item,
-- if any.
getCurrentSelection ::
  -- | The definition for the list
  ListDefinition n a ->
  -- | The current list state
  ListState ->
  Maybe a
getCurrentSelection ListDefinition {..} ListState {..} = selection >>= (items !?)

-- | Update list state when it receives focus, initialising selection and
-- scrolling appropriately.  This handles the common pattern of selecting the
-- first/last item when focus enters from different directions.
listReceiveFocus ::
  (Monad m, Ord n) =>
  -- | The definition for the list
  ListDefinition n a ->
  -- | The current list state
  ListState ->
  -- | A function to update the application state, given an updated ListState.
  (ListState -> s -> s) ->
  -- | Was the focus a FocusNext or a FocusPrev event
  FocusDestination ->
  EventHandlerM m s e n ()
listReceiveFocus ListDefinition {name, items, itemHeight} ls modifier destination = do
  V2 _ visibleHeight <- getExtentSize name
  let listLength = length items
      visibleItems = getCell (visibleHeight `div` itemHeight)
  case (selection ls, destination) of
    -- Already has a selection, keep it
    (Just _, _) -> pure ()
    -- No selection, entering from next -> select first item
    (Nothing, FocusNext) -> modifyState $ modifier ls {selection = Just 0, scrollOffset = 0}
    -- No selection, entering from prev -> select last item and scroll to show it
    (Nothing, FocusPrev) ->
      let lastIdx = listLength - 1
          -- Scroll so the last item is visible (at the bottom of the viewport)
          newScroll = max 0 (lastIdx - visibleItems + 1)
       in modifyState $ modifier ls {selection = Just lastIdx, scrollOffset = newScroll}

-- | A specialized version of labelListReceiveFocus that doesn't need a ListDefinition
labelListReceiveFocus :: (Monad m, Ord n) => n -> [a] -> ListState -> (ListState -> s -> s) -> FocusDestination -> EventHandlerM m s e n ()
labelListReceiveFocus name items =
  listReceiveFocus
    (ListDefinition {name = name, items = items, renderItem = \_ _ -> emptyComponent, itemHeight = 1, wrapAround = False})

-- | A simple list made of labels.
labelList ::
  (Ord n) =>
  -- | Name of this component. Lists do register their extent, so a name is always needed.
  n ->
  -- | List of items
  [a] ->
  -- | Item to string conversion (e.g., `show` if suitable).
  (a -> String) ->
  -- | Expected alignment for the items
  TextAlign ->
  -- | Colours when not selected
  Colours ->
  -- | Colours when selected
  Colours ->
  -- | Current list state
  ListState ->
  Component n
labelList n items toText baseAlignment baseColour highlightedColours ls =
  let renderLabel item selected =
        label (toText item) baseAlignment (if selected then highlightedColours else baseColour)
   in list ListDefinition {name = n, items = items, renderItem = renderLabel, itemHeight = 1, wrapAround = False} ls

-- | A list of arbitrary components. Pay attention that the list does not support
-- interactive components (so you can't have a list in a list, or a grid in a list, for instance),
-- though some modicum of interaction is possible by chaining handleListEvent with another handler.
list ::
  (Ord n) =>
  -- | A list definition for this list.
  ListDefinition n a ->
  -- | The current list state
  ListState ->
  Component n
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

-- | Utility to detect selection of an item in the list when the mouse
-- is clicked.
handleClickOnLabelList ::
  (Monad m, Ord n) =>
  -- | The name of the component, needed to retrieve its Extent.
  n ->
  -- | The list of items
  [a] ->
  -- | The current list state
  ListState ->
  -- | A function to modify the application state with the updated `ListState`.
  (ListState -> s -> s) ->
  -- | The details of the click event
  MouseClickDetails ->
  EventHandlerM m s e n ()
handleClickOnLabelList n items =
  handleClickOnList ListDefinition {name = n, items = items, renderItem = \_ _ -> emptyComponent, itemHeight = 1, wrapAround = False} Nothing

-- | Utility to detect selection of an item in the list when the mouse
-- is clicked. You can also require a specific behaviour if the user
-- clicked on an item that was already selected.
handleClickOnList ::
  (Monad m, Ord n) =>
  -- | The list definition
  ListDefinition n a ->
  -- | Action to perform when an item was already selected and is clicked
  Maybe (V2 Cell -> a -> EventHandlerM m s e n ()) ->
  -- | The current list state
  ListState ->
  -- A function to modify the application state with the updated `ListState`.
  (ListState -> s -> s) ->
  -- The details of the click event
  MouseClickDetails ->
  EventHandlerM m s e n ()
handleClickOnList ListDefinition {name, items, itemHeight} onSelectedClick ls@ListState {..} modifier (MouseClickDetails _ (V2 mcx mcy) LeftButton) = do
  (V2 px py) <- getExtentPosition name
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

-- | A default event handler for label list. Works like `handleListEvent`, but doesn't need a full list definition.
handleLabelListEvent ::
  (Monad m, Ord n) =>
  -- | The name of the component, needed to retrieve its Extent.
  n ->
  -- | The underlying list of items
  [a] ->
  -- | If the last item has been reached, should up / down keys return to the beginning or send a focus next / focus prev event ?
  Bool ->
  -- | The current list state
  ListState ->
  -- | A function to modify the application state with the updated `ListState`.
  (ListState -> s -> s) ->
  EventHandler m s e n
handleLabelListEvent n items wrapAround =
  handleListEvent (ListDefinition {name = n, items = items, renderItem = \_ _ -> emptyComponent, itemHeight = 1, wrapAround})

data ListAction
  = ListFocusNext
  | ListFocusPrev

defaultListAction :: [(KeyMatch, ListAction)]
defaultListAction =
  [ (IsNoMod KUp, ListFocusPrev),
    (IsNoMod KDown, ListFocusNext)
  ]

-- | A default event handler for list. Up and down move selection and
-- will send focusNext or focusPrev when you reach the limit of the list,
-- unless the listDefinition sets `wrapAround` to `True`.
-- If you want other interactions (e.g., something should happen when user
-- press enter), you should chain this eventHandler with another, custom one.
handleListEvent :: (Monad m, Ord n) => ListDefinition n a -> ListState -> (ListState -> s -> s) -> EventHandler m s e n
handleListEvent = handleListEvent' defaultListAction

-- | A default event handler for list, with customisable key presses.
-- Will handle going up and down the list. By default, focus is passed
-- when reaching the end of the list, unless the listDefinition
-- sets `wrapAround` to `True`.
-- If you want other interactions (e.g., something should happen when user
-- press enter), you should chain this eventHandler with another, custom one.
handleListEvent' :: (Monad m, Ord n) => [(KeyMatch, ListAction)] -> ListDefinition n a -> ListState -> (ListState -> s -> s) -> EventHandler m s e n
handleListEvent' keyMap ListDefinition {items, name, itemHeight, wrapAround} state@ListState {scrollOffset} modifier s e = do
  V2 _ visibleHeight <- getExtentSize name
  let listLength = length items
      visibleItems = getCell (visibleHeight `div` itemHeight)
      autoScroll sel
        | sel < scrollOffset = sel
        | sel >= scrollOffset + visibleItems =
            sel - visibleItems + 1
        | otherwise = scrollOffset
      toEvent = \case
        ListFocusNext -> moveSelectionDown listLength autoScroll wrapAround state modifier
        ListFocusPrev -> moveSelectionUp listLength autoScroll wrapAround state modifier
  keyPressHandler (second toEvent <$> keyMap) s e

moveSelectionDown :: (Monad m) => Int -> (Int -> Int) -> Bool -> ListState -> (ListState -> s -> s) -> EventHandler m s e n
moveSelectionDown listLength autoScroll wrapAround state@ListState {selection} modifier _ _ =
  let newIndex = maybe 0 (+ 1) selection
   in if newIndex >= listLength
        then
          if wrapAround
            then redraw . modifyState . modifier $ state {selection = Just 0, scrollOffset = autoScroll 0}
            else (modifyState . modifier $ state {selection = Nothing}) >> fireEvent (Focus FocusNext)
        else
          redraw . modifyState . modifier $ state {selection = Just newIndex, scrollOffset = autoScroll newIndex}

moveSelectionUp :: (Monad m) => Int -> (Int -> Int) -> Bool -> ListState -> (ListState -> s -> s) -> EventHandler m s e n
moveSelectionUp listLength autoScroll wrapAround state@ListState {selection} modifier _ _ =
  let newIndex = maybe (listLength - 1) (\n -> n - 1) selection
   in if newIndex >= 0
        then do
          let newScroll = autoScroll newIndex
          modifyState (modifier $ state {selection = Just newIndex, scrollOffset = newScroll})
        else
          if wrapAround
            then redraw . modifyState . modifier $ state {selection = Just (listLength - 1), scrollOffset = autoScroll (listLength - 1)}
            else (modifyState . modifier $ state {selection = Nothing}) >> fireEvent (Focus FocusPrev)