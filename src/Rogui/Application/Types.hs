module Rogui.Application.Types
  ( RoguiConfig (..),
  )
where

import qualified Data.Map as M
import Data.Text (Text)
import Rogui.Components (TileSize)
import Rogui.Components.Types (Component)
import Rogui.Graphics (Cell)
import Rogui.Graphics.Types (Brush)
import Rogui.Types (EventHandler)
import SDL (V2)

data RoguiConfig rc rb name state event = RoguiConfig
  { brushTilesize :: TileSize,
    appName :: Text,
    consoleCellSize :: V2 Cell,
    targetFPS :: Int,
    rootConsoleReference :: rc,
    defaultBrushReference :: rb,
    defaultBrushPath :: FilePath,
    drawingFunction :: M.Map rb Brush -> state -> Component name,
    eventFunction :: EventHandler state event
  }