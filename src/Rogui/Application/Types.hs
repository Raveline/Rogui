module Rogui.Application.Types
  ( RoguiConfig (..),
  )
where

import Data.Text (Text)
import Data.Word (Word32)
import Rogui.Components (TileSize)
import Rogui.Graphics (Cell)
import Rogui.Types (ConsoleDrawers, EventHandler)
import SDL (V2)

data RoguiConfig rc rb name state event = RoguiConfig
  { brushTilesize :: TileSize,
    appName :: Text,
    consoleCellSize :: V2 Cell,
    targetFPS :: Int,
    timerStep :: Word32,
    rootConsoleReference :: rc,
    defaultBrushReference :: rb,
    defaultBrushPath :: FilePath,
    drawingFunction :: ConsoleDrawers rc rb name state,
    eventFunction :: EventHandler state event name
  }