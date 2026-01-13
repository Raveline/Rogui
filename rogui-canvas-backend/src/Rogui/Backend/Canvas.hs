{-# LANGUAGE ImportQualifiedPost #-}

module Rogui.Backend.Canvas
  ( canvasBackend,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString
import Data.Text (Text)
import Linear
import Rogui.Backend.SDL.Eval
import Rogui.Backend.SDL.Events
import Rogui.Backend.Types
import Rogui.Graphics

canvasBackend :: Backend CanvasContext JSVal e
canvasBackend =
  Backend
    { loadBrush = loadBrush,
      clearFrame = undefined
      presentFrame = undefined,
      initBackend = undefined,
      evalInstructions = undefined,
      getTicks = undefined,
      pollEvents = undefined
    }

initCanvasBackend :: (MonadIO m) => Text -> V2 Pixel -> Bool -> (CanvasContext -> m a) -> m ()
initCanvasBackend appName windowSize allowResize withRenderer = undefined

loadSDLBrush :: (MonadIO m) => CanvasContext -> TileSize -> Either ByteString FilePath -> Maybe RGBA -> m (Brush, JSVal)
loadSDLBrush renderer TileSize {..} method transparency = undefined