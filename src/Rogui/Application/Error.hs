{-# LANGUAGE DeriveAnyClass #-}

module Rogui.Application.Error
  ( RoguiError (..),
    TileSizeMismatch (..),
  )
where

import Control.Exception
import Rogui.Graphics.Types (TileSize)

data TileSizeMismatch = TileSizeMismatch
  { expectedTileSize :: TileSize,
    actualTileSize :: TileSize
  }
  deriving (Eq, Show)

data RoguiError rc rb
  = NoSuchConsole rc
  | NoSuchBrush rb
  | CannotLoadBrush rb String
  | BrushConsoleMismatch TileSizeMismatch
  deriving (Eq, Show, Exception)