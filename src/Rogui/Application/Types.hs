module Rogui.Application.Types
  ( RoguiConfig (..),
    ConsoleSpec,
  )
where

import Data.Text (Text)
import Data.Word (Word32)
import Rogui.ConsoleSpecs (PositionSpec, SizeSpec)
import Rogui.Graphics (Cell, TileSize)
import Rogui.Types (ConsoleDrawers, EventHandler)
import SDL (V2)

-- | Instructions on how to build a console
type ConsoleSpec rc = (rc, TileSize, SizeSpec, PositionSpec rc)

-- | A simple configuration type provided to the `boot` function.
-- To see how this can be used in practice, read `Application.System` documentation.
-- To understand drawingFunction and eventFunction, read `Rogui.Types` documentation.
data RoguiConfig rc rb name state event m = RoguiConfig
  { -- | A default brush tilesize.
    brushTilesize :: TileSize,
    -- | Name of the application (to be used as window title)
    appName :: Text,
    -- | Desired cell size of the main window.
    consoleCellSize :: V2 Cell,
    -- | Desired FPS.
    targetFPS :: Int,
    -- | Desired milliseconds before firing a "Step" event.
    stepMs :: Word32,
    -- | Constructor to be associated to the root console
    rootConsoleReference :: rc,
    -- | Constructor to be associated to the default brush
    defaultBrushReference :: rb,
    -- | Path to the default brush
    defaultBrushPath :: FilePath,
    -- | Main drawing function
    drawingFunction :: ConsoleDrawers rc rb name state,
    -- | Main event handling function
    eventFunction :: EventHandler m state event name,
    -- | Console building instructions
    consoleSpecs :: [ConsoleSpec rc]
  }