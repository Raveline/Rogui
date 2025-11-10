{-# LANGUAGE FlexibleContexts #-}

module Rogui.ConsoleSpecs
  ( SizeSpec (..),
    PositionSpec (..),
    findConsole,
    consoleRight,
    consoleBelow,
  )
where

import Control.Monad.Except
import qualified Data.Map as M
import Rogui.Application.Error (RoguiError (NoSuchConsole))
import Rogui.Graphics (Cell, Console (..), Pixel (..))
import Rogui.Types (Rogui (..))
import SDL (V2 (..))

-- | How to size a console.
data SizeSpec
  = -- | Same size as the root console, takes all window
    FullWindow
  | -- | Percentage of width and height of the root window
    SizeWindowPct Int Int
  | -- | Direct definition in width and height in cells
    TilesSize Cell Cell
  | -- | Exact pixels
    PixelsSize Pixel Pixel

-- | Where to put a console.
data PositionSpec rc
  = TopLeft
  | TopRight
  | BottomLeft
  | BottomRight
  | Center
  | -- | Percentages (x and y) from top-left
    PosWindowPct Int Int
  | -- | Position in tiles
    TilesPos Cell Cell
  | -- | Exact pixels
    PixelsPos Pixel Pixel
  | -- | Below another console (stacking)
    Below rc
  | -- | Right of another console (side-by-side)
    RightOf rc

findConsole :: (Ord rc, MonadError (RoguiError rc rb) m) => rc -> Rogui rc rb n s e m' -> m Console
findConsole consoleRef Rogui {..} =
  case consoleRef `M.lookup` consoles of
    Nothing -> throwError (NoSuchConsole consoleRef)
    Just c -> pure c

consoleBelow :: (Ord rc, MonadError (RoguiError rc rb) m) => rc -> Rogui rc rb n s e m' -> m (V2 Pixel)
consoleBelow rc rogui = do
  Console {..} <- findConsole rc rogui
  pure $ position + V2 0 height

consoleRight :: (Ord rc, MonadError (RoguiError rc rb) m) => rc -> Rogui rc rb n s e m' -> m (V2 Pixel)
consoleRight rc rogui = do
  Console {..} <- findConsole rc rogui
  pure $ position + V2 width 0
