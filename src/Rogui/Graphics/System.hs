{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rogui.Graphics.System
  ( loadBrush,
    boot,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import Data.Word
import Rogui.Graphics.Types (Brush (..), Console (..), TileSize (..))
import Rogui.Types (Rogui (..))
import SDL qualified
import SDL.Image qualified as SDL
import SDL.Internal.Numbered qualified as Numbered
import SDL.Raw qualified as Raw

convertSurface :: (MonadIO m) => SDL.Surface -> SDL.PixelFormat -> m SDL.Surface
convertSurface (SDL.Surface s _) pixFmt = do
  fmt <- Raw.allocFormat (Numbered.toNumber pixFmt)
  surface <- SDL.Surface <$> Raw.convertSurface s fmt 0 <*> pure Nothing
  surface <$ Raw.freeFormat fmt

loadBrush :: (MonadIO m) => SDL.Renderer -> FilePath -> SDL.V2 Int -> m Brush
loadBrush renderer path (SDL.V2 tileWidth tileHeight) = do
  fontSurface <- SDL.load path
  surface <- convertSurface fontSurface SDL.RGBA8888
  let black :: SDL.V4 Word8
      black = SDL.V4 0 0 0 0x00
  void $ SDL.surfaceColorKey surface SDL.$= pure black
  brush <- SDL.createTextureFromSurface renderer surface
  textInfo <- SDL.queryTexture brush
  pure $ Brush {tileWidth, tileHeight, textureWidth = fromIntegral (SDL.textureWidth textInfo), textureHeight = fromIntegral (SDL.textureHeight textInfo), brush}

-- Initialize a SDL application and window with the provided tilesize,
-- giving a window with a size expressed in tiles.
boot :: (MonadIO m) => TileSize -> Text -> SDL.V2 Int -> (SDL.Renderer -> Console -> m (Rogui rc rb)) -> (Rogui rc rb -> m Bool) -> m ()
boot TileSize {..} title (SDL.V2 widthInTiles heightInTiles) guiBuilder appLoop = do
  SDL.initializeAll

  let windowSize@(SDL.V2 w h) = SDL.V2 (widthInTiles * pixelWidth) (heightInTiles * pixelHeight)
  window <- SDL.createWindow title SDL.defaultWindow {SDL.windowInitialSize = fromIntegral <$> windowSize}
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  let baseConsole = Console {width = w, height = h, position = SDL.V2 0 0}
  gui <- guiBuilder renderer baseConsole

  loop gui appLoop

  SDL.destroyWindow window

loop :: (MonadIO m) => Rogui rc rb -> (Rogui rc rb -> m Bool) -> m ()
loop r@Rogui {..} clientLoop = do
  SDL.clear renderer
  stop <- clientLoop r
  SDL.present renderer
  unless stop $
    loop r clientLoop
