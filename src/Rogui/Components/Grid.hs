{-# LANGUAGE NamedFieldPuns #-}

-- | A grid component for geometric layouts.
-- Default implementation offer selectable grid and
-- key handling to move selection and handle vertical scrolling.
module Rogui.Components.Grid
  ( grid,
    GridDefinition (..),
    GridState (..),
    mkGridState,
    selectedGridItem,
    handleGridEvent,
    handleClickOnGrid,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.State.Strict hiding (state)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Rogui.Application.Event (Event (..), EventHandlerM, KeyDetails (..), KeyDownDetails (KeyDownDetails, key), MouseClickDetails (..), fireEvent, getExtentPosition, getExtentSize, modifyState, redraw, unhandled)
import Rogui.Components.Types
import Rogui.Graphics (Brush (Brush, tileHeight, tileWidth), Cell (..), Colours (..))
import Rogui.Graphics.DSL.Instructions
import Rogui.Graphics.Types (Console (..), (.*=.))
import SDL (V2 (..))
import qualified SDL

data GridState = GridState
  { selection :: Maybe (Int, Int),
    scrollOffset :: Cell
  }

mkGridState :: GridState
mkGridState = GridState Nothing 0

data GridDefinition a n = GridDefinition
  { -- | Constant reference type used to store grid extent
    gridName :: n,
    -- | Number of rows
    gridRows :: Int,
    -- | Grid content datatype in the grid, declared from left to right and top to bottom
    gridContent :: [a],
    -- | A function to render each component from the datatype. First parameters are column and row.
    renderCell :: (Int -> Int -> Bool -> Maybe a -> Component n),
    -- | Expected width
    cellWidths :: NE.NonEmpty Cell,
    -- Expected height
    cellHeight :: Cell,
    -- | Spacing between cells, if any
    spacing :: Cell,
    highlightColours :: Colours
  }

columnPositions :: GridDefinition a n -> [Cell]
columnPositions GridDefinition {..} =
  scanl (+) 0 $
    zipWith3
      (\w s idx -> w + if idx < length cellWidths - 1 then s else 0)
      (NE.toList cellWidths)
      (repeat spacing)
      [0 ..]

-- | A component for regular grids (with various size widths but constant height).
-- Though it can be filled with any components, it will not handle inner focus;
-- in other words, if you put a grid in a grid, for instance, you won't be able
-- to navigate the inner grid. Rogui doesn't handle nested interactive
-- components.
grid ::
  (Ord n) =>
  -- | Config datatype for the grid
  GridDefinition a n ->
  -- | State holding selection and offset
  GridState ->
  Component n
grid gd@GridDefinition {..} GridState {..} =
  let nbOfColumns = NE.length cellWidths
      draw' = do
        recordExtent gridName
        Brush {..} <- gets brush
        visibleHeight <- contextCellHeight

        let cellHeightWithSpacing = cellHeight + spacing
            colPositions = columnPositions gd
            visibleRows = getCell (visibleHeight `div` cellHeightWithSpacing)
            visibleGridRows = take visibleRows $ drop (getCell scrollOffset) [0 .. gridRows - 1]
        forM_ visibleGridRows $ \row -> do
          forM_ [0 .. nbOfColumns - 1] $ \col -> do
            let renderRow = row - (getCell scrollOffset)
                cellColPosition = colPositions !! col
                cellWidth = cellWidths NE.!! col
                (V2 cpx cpy) = V2 cellColPosition (Cell renderRow * cellHeightWithSpacing)
                cellPosInPixels = V2 (tileWidth .*=. cpx) (tileHeight .*=. cpy)
                item = getItemAt gridContent nbOfColumns (col, row)
                isSelected = selection == Just (col, row)

            oldConsole@Console {..} <- gets console
            let cellConsole =
                  Console
                    { width = tileWidth .*=. cellWidth,
                      height = tileHeight .*=. cellHeight,
                      position = position + cellPosInPixels
                    }
            changeConsole cellConsole
            when isSelected $ do
              setColours highlightColours
              traverse_ setConsoleBackground (back highlightColours)
            draw (renderCell col row isSelected item)
            changeConsole oldConsole
   in emptyComponent {draw = draw'}

selectedGridItem :: GridDefinition a n -> GridState -> Maybe a
selectedGridItem GridDefinition {..} GridState {..} =
  let numCols = NE.length cellWidths
   in selection >>= getItemAt gridContent numCols

getItemAt :: [a] -> Int -> (Int, Int) -> Maybe a
getItemAt items nbCols (col, row) =
  let idx = row * nbCols + col
   in if idx >= 0 && idx < length items
        then Just (items !! idx)
        else Nothing

-- | A default handler to select an item in a grid.
handleGridEvent ::
  (Ord n) =>
  -- | The grid definition used at rendering
  GridDefinition a n ->
  -- | The event we are processing
  Event e ->
  -- | Our input grid state
  GridState ->
  -- | A function to modify the grid state in the application state
  (GridState -> s -> s) ->
  -- | An action to perform on pressing enter on the current selection (if any)
  (Maybe a -> EventHandlerM s e n ()) ->
  EventHandlerM s e n ()
handleGridEvent GridDefinition {..} event state@GridState {..} modifier onEnter = do
  V2 _ visibleHeight <- getExtentSize gridName
  let cols = NE.length cellWidths
      rows = gridRows
      cellHeightWithSpacing = cellHeight + spacing
      visibleRows = getCell visibleHeight `div` getCell cellHeightWithSpacing
      updateSelection newSelection =
        redraw $ modifyState (modifier $ state {selection = Just newSelection, scrollOffset = autoScroll newSelection})
      fireAndDeselect ev = fireEvent ev >> modifyState (modifier $ state {selection = Nothing})
      autoScroll (_, selRow) =
        let scrollRow = getCell scrollOffset
         in if selRow < scrollRow
              then Cell selRow
              else
                if selRow >= scrollRow + visibleRows
                  then Cell (selRow - visibleRows + 1)
                  else scrollOffset
  case event of
    KeyDown KeyDownDetails {key} -> case keycode key of
      SDL.KeycodeRight -> case selection of
        Nothing -> redraw $ modifyState (modifier $ state {selection = Just (0, 0)})
        Just (col, row)
          | col + 1 < cols -> updateSelection (col + 1, row)
          | row + 1 < rows -> updateSelection (0, row + 1)
          | otherwise -> fireAndDeselect FocusNext
      SDL.KeycodeLeft -> case selection of
        Nothing -> redraw $ modifyState (modifier $ state {selection = Just (cols - 1, rows - 1)})
        Just (col, row)
          | col > 0 -> updateSelection (col - 1, row)
          | row > 0 -> updateSelection (cols - 1, row - 1)
          | otherwise -> fireAndDeselect FocusPrev
      SDL.KeycodeDown -> case selection of
        Nothing -> redraw $ modifyState (modifier $ state {selection = Just (0, 0)})
        Just (col, row)
          | row + 1 < rows -> updateSelection (col, row + 1)
          | otherwise -> fireAndDeselect FocusNext
      SDL.KeycodeUp -> case selection of
        Nothing -> redraw $ modifyState (modifier $ state {selection = Just (cols - 1, rows - 1)})
        Just (col, row)
          | row > 0 -> updateSelection (col, row - 1)
          | otherwise -> fireAndDeselect FocusPrev
      SDL.KeycodeReturn ->
        onEnter (selection >>= getItemAt gridContent cols)
      _ -> pure ()
    _ -> pure ()

handleClickOnGrid ::
  (Ord n) =>
  -- | The grid definition used at rendering
  GridDefinition a n ->
  -- | The event we are processing
  MouseClickDetails ->
  -- | Our input grid state
  GridState ->
  -- | A function to modify the grid state in the application state
  (GridState -> s -> s) ->
  -- | An action to perform on clicking
  (Maybe a -> EventHandlerM s e n ()) ->
  EventHandlerM s e n ()
handleClickOnGrid gd@GridDefinition {..} (MouseClickDetails _ mousePos SDL.ButtonLeft) state modifier onCellClick = do
  gridPos <- getExtentPosition gridName
  V2 _ gridHeight <- getExtentSize gridName

  let V2 relX relY = mousePos - gridPos
      cellHeightWithSpacing = cellHeight + spacing
      nbOfColumns = NE.length cellWidths
      colPositions = columnPositions gd
      findClickedColumn pos colIdx
        | colIdx >= nbOfColumns = Nothing
        | pos < colPositions !! colIdx = Nothing
        | colIdx == nbOfColumns - 1 = Just colIdx
        | pos >= colPositions !! colIdx && pos < colPositions !! (colIdx + 1) = Just colIdx
        | otherwise = findClickedColumn pos (colIdx + 1)

      clickedCol = findClickedColumn relX 0
      clickedRow =
        if relY >= 0 && relY < gridHeight
          then Just $ getCell (relY `div` cellHeightWithSpacing) + getCell (scrollOffset state)
          else Nothing

  -- If valid cell was clicked, update selection and call action
  case (clickedCol, clickedRow) of
    (Just col, Just row) | row < gridRows -> do
      let newState = state {selection = Just (col, row)}
          item = getItemAt gridContent nbOfColumns (col, row)
      redraw $ modifyState (modifier newState)
      onCellClick item
    _ -> unhandled
handleClickOnGrid _ _ _ _ _ = unhandled -- Ignore non-left clicks