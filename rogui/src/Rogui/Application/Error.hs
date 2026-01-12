{-# LANGUAGE DeriveAnyClass #-}

module Rogui.Application.Error
  ( RoguiError (..),
    TileSizeMismatch (..),
  )
where

import Control.Exception
import Rogui.Graphics.Types (TileSize)

data RoguiError err rc rb
  = -- | Console not found. It was probably not initialised in `boot`.
    NoSuchConsole rc
  | -- | Brush not found. It was probably not initialised in `boot`.
    NoSuchBrush rb
  | -- | Exception raised when trying to load a brush (most often, a SDL error).
    CannotLoadBrush rb String
  | -- | Exception raised when trying to use a brush that doesn't match the console
    -- expected tilesize.
    BrushConsoleMismatch TileSizeMismatch
  | ApplicationError err
  | NonEndingEventLoop
  deriving (Eq, Show, Exception)

data TileSizeMismatch = TileSizeMismatch
  { expectedTileSize :: TileSize,
    actualTileSize :: TileSize
  }
  deriving (Eq, Show)