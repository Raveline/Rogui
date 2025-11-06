{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.ConsoleSpecs
  ( SizeSpec (..),
    PositionSpec (..),
    addConsoleWithSpec,
  )
where

import Control.Monad.Except
import qualified Data.Map as M
import Rogui.Application.Error (RoguiError (NoSuchConsole))
import Rogui.Application.System (addConsole)
import Rogui.Graphics (Cell, Console (..), Pixel (..), TileSize (..), (.*=.))
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

findConsole :: (Ord rc, MonadError (RoguiError rc rb) m) => rc -> Rogui rc rb n s e -> m Console
findConsole consoleRef Rogui {..} =
  case consoleRef `M.lookup` consoles of
    Nothing -> throwError (NoSuchConsole consoleRef)
    Just c -> pure c

consoleBelow :: (Ord rc, MonadError (RoguiError rc rb) m) => rc -> Rogui rc rb n s e -> m (V2 Pixel)
consoleBelow rc rogui = do
  Console {..} <- findConsole rc rogui
  pure $ position + V2 0 height

consoleRight :: (Ord rc, MonadError (RoguiError rc rb) m) => rc -> Rogui rc rb n s e -> m (V2 Pixel)
consoleRight rc rogui = do
  Console {..} <- findConsole rc rogui
  pure $ position + V2 width 0

addConsoleWithSpec ::
  (Ord rc, MonadError (RoguiError rc rb) m) =>
  rc ->
  TileSize ->
  SizeSpec ->
  PositionSpec rc ->
  Rogui rc rb n s e ->
  m (Rogui rc rb n s e)
addConsoleWithSpec ref consoleTS sizeSpec posSpec rogui@Rogui {rootConsole} = do
  let Console {..} = rootConsole
      (w, h) = case sizeSpec of
        FullWindow -> (width, height)
        SizeWindowPct wp hp -> (width * Pixel wp `div` 100, height * Pixel hp `div` 100)
        TilesSize tw th -> (pixelWidth consoleTS .*=. tw, pixelHeight consoleTS .*=. th)
        PixelsSize pw ph -> (pw, ph)
      pos = case posSpec of
        TopLeft -> pure $ V2 0 0
        TopRight -> pure $ V2 (width - w) 0
        BottomLeft -> pure $ V2 0 (height - h)
        BottomRight -> pure $ V2 (width - w) (height - h)
        Center -> pure $ V2 ((width - w) `div` 2) ((height - h) `div` 2)
        PosWindowPct xp yp -> pure $ V2 (width * Pixel xp `div` 100) (height * Pixel yp `div` 100)
        TilesPos tx ty -> pure $ V2 (pixelWidth consoleTS .*=. tx) (pixelHeight consoleTS .*=. ty)
        PixelsPos px py -> pure $ V2 px py
        Below rc -> consoleBelow rc rogui
        RightOf rc -> consoleRight rc rogui
  console <- Console <$> pure w <*> pure h <*> pos <*> pure consoleTS
  pure $ addConsole ref console rogui