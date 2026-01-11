{-# LANGUAGE ImportQualifiedPost #-}

module Rogui.Backend.SDL
  ( sdlBackend,
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
import SDL qualified
import SDL.Image qualified as SDL
import SDL.Internal.Numbered qualified as Numbered
import SDL.Raw qualified as Raw

sdlBackend :: Backend SDL.Renderer SDL.Texture e
sdlBackend =
  Backend
    { loadBrush = loadSDLBrush,
      clearFrame = SDL.clear,
      presentFrame = SDL.present,
      initBackend = initSDLBackend,
      evalInstructions = evalSDLInstructions,
      getTicks = SDL.ticks,
      pollEvents = getSDLEvents
    }

initSDLBackend :: (MonadIO m) => Text -> V2 Pixel -> Bool -> (SDL.Renderer -> m a) -> m ()
initSDLBackend appName windowSize allowResize withRenderer = do
  SDL.initializeAll
  window <-
    SDL.createWindow
      appName
      SDL.defaultWindow
        { SDL.windowInitialSize = fromIntegral <$> windowSize,
          SDL.windowResizable = allowResize
        }
  when allowResize
    . void
    $ SDL.windowMinimumSize window SDL.$= (fromIntegral <$> windowSize)
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  void $ withRenderer renderer
  SDL.destroyWindow window

convertSurface :: (MonadIO m) => SDL.Surface -> SDL.PixelFormat -> m SDL.Surface
convertSurface (SDL.Surface s _) pixFmt = do
  fmt <- Raw.allocFormat (Numbered.toNumber pixFmt)
  surface <- SDL.Surface <$> Raw.convertSurface s fmt 0 <*> pure Nothing
  surface <$ Raw.freeFormat fmt

loadSDLBrush :: (MonadIO m) => SDL.Renderer -> TileSize -> Either ByteString FilePath -> Maybe RGBA -> m (Brush, SDL.Texture)
loadSDLBrush renderer TileSize {..} method transparency = do
  fontSurface <- case method of
    Left bs -> SDL.decode bs
    Right fp -> SDL.load fp

  surface <- convertSurface fontSurface (SDL.RGBA8888 :: SDL.PixelFormat)
  void $ SDL.surfaceColorKey surface SDL.$= transparency
  texture <- SDL.createTextureFromSurface renderer surface
  textInfo <- SDL.queryTexture texture
  pure
    ( Brush
        { tileWidth = pixelWidth,
          tileHeight = pixelHeight,
          textureWidth = fromIntegral (SDL.textureWidth textInfo),
          textureHeight = fromIntegral (SDL.textureHeight textInfo),
          name = either show id method
        },
      texture
    )
