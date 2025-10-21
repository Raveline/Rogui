module Rogui.Graphics.Primitives
  ( printCharAt,
    fillConsoleWith,
    RGB,
  )
where

import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.Word
import Foreign.C
import Rogui.Graphics.Types
import SDL (Rectangle (..), Renderer, Texture, V2 (..), V3 (..))
import qualified SDL
import SDL.Vect (Point (..))

type RGB = V3 Word8

setFrontColour :: (MonadIO m) => Texture -> RGB -> m ()
setFrontColour texture rgb = do
  SDL.textureBlendMode texture SDL.$= SDL.BlendMod
  SDL.textureColorMod texture SDL.$= rgb
  SDL.textureBlendMode texture SDL.$= SDL.BlendAlphaBlend

setBackColour :: (MonadIO m) => Renderer -> Rectangle CInt -> RGB -> m ()
setBackColour renderer dest (V3 r g b) = do
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b 255
  SDL.fillRect renderer (pure dest)
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255

-- | 0|1|2|
--  |3|4|5|
--  |6|7|8|
--  |9|10|11|
charIdToPosition :: Brush -> Int -> Rectangle CInt
charIdToPosition Brush {..} tileId =
  let numberOfColumns = getPixel $ textureWidth `div` tileWidth
      x = tileId `mod` numberOfColumns
      y = tileId `div` numberOfColumns
   in fromIntegral <$> Rectangle (P $ V2 (x * getPixel tileWidth) (y * getPixel tileHeight)) (fromIntegral <$> (V2 tileWidth tileHeight))

printCharAt ::
  (MonadIO m) =>
  SDL.Renderer ->
  -- | Where you are painting
  Console ->
  -- | With what you are painting
  Brush ->
  -- | Frontcolour of the sprite.
  Maybe RGB ->
  -- | Backcolour of the sprite.
  Maybe RGB ->
  -- | Sprite to paint. Should in theory be limited to Char, but we
  -- want to be able to support more ambitious brushes.
  Int ->
  -- | Logical position in the console (in brush size cells)
  V2 Cell ->
  m ()
printCharAt renderer Console {..} b@Brush {..} frontColour backColour n at = do
  let cInt x = fromIntegral <$> x
      getScreenPos (V2 x y) = V2 (tileWidth .*=. x) (tileHeight .*=. y)
      realRectangle = Rectangle (P $ cInt $ (position + getScreenPos at)) (cInt $ V2 tileWidth tileHeight)
  traverse_ (setBackColour renderer realRectangle) backColour
  traverse_ (setFrontColour brush) frontColour
  SDL.copy
    renderer
    brush
    (pure $ charIdToPosition b n)
    (pure $ realRectangle)

fillConsoleWith :: (MonadIO m) => Renderer -> Console -> RGB -> m ()
fillConsoleWith renderer Console {..} (V3 r g b) = do
  let dest = Rectangle (P $ fromIntegral <$> position) (fromIntegral <$> V2 width height)
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b 255
  SDL.fillRect renderer (pure dest)
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
