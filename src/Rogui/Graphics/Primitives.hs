module Rogui.Graphics.Primitives
  ( printCharAt,
    fillConsoleWith,
    clipToConsole,
    overlayRect,
    RGBA,
    Transformation (..),
    Rotation (..),
  )
where

import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word
import Foreign.C
import Rogui.Graphics.Types
import SDL (Rectangle (..), Renderer, Texture, V2 (..), V3 (..), V4 (..))
import qualified SDL
import SDL.Vect (Point (..))

type RGBA = V4 Word8

setFrontColour :: (MonadIO m) => Texture -> RGBA -> m ()
setFrontColour texture (V4 r g b a) = do
  SDL.textureBlendMode texture SDL.$= SDL.BlendAlphaBlend
  SDL.textureColorMod texture SDL.$= V3 r g b
  SDL.textureAlphaMod texture SDL.$= a

setBackColour :: (MonadIO m) => Renderer -> Rectangle CInt -> RGBA -> m ()
setBackColour renderer dest (V4 r g b _) = do
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
   in fromIntegral <$> Rectangle (P $ V2 (x * getPixel tileWidth) (y * getPixel tileHeight)) (fromIntegral <$> V2 tileWidth tileHeight)

-- | Transformations that can be applied to glyphs during rendering.
-- Multiple transformations can be combined - rotations are summed
-- (e.g., [Rotate R90, Rotate R90] = 180°) and flips are deduplicated.
-- The order of transformation doesn't matter.
--
-- Rotation is always performed from the center of the glyph.
--
-- Note that using `RArbitrary` might break the "grid" feeling,
-- it's mostly there for animation support.
--
-- Also note that rotations will clip for non square tilesets !
-- Since rotations are implemented mostly with tileset in mind,
-- we won't support rotating non square tilesets.
data Transformation
  = FlipX
  | FlipY
  | Rotate Rotation
  deriving (Eq)

data Rotation = R90 | R180 | R270 | RArbitrary Double
  deriving (Eq)

toDegree :: Transformation -> Maybe CDouble
toDegree (Rotate R90) = Just 90
toDegree (Rotate R180) = Just 180
toDegree (Rotate R270) = Just 270
toDegree (Rotate (RArbitrary d)) = Just (realToFrac d)
toDegree _ = Nothing

getScreenRectAt :: Console -> Brush -> V2 Pixel -> V2 Cell -> Rectangle CInt
getScreenRectAt Console {..} Brush {..} (V2 rectWidth rectHeight) at =
  let cInt x = fromIntegral <$> x
      getScreenPos (V2 x y) = V2 (tileWidth .*=. x) (tileHeight .*=. y)
   in Rectangle (P $ cInt (position + getScreenPos at)) (cInt $ V2 rectWidth rectHeight)

-- | Display a glyph on the renderer, with a given brush, on a given console, with a background
-- and a foreground colour, at a given position on a given console, applying optional
-- transformation over the glyph.
printCharAt ::
  (MonadIO m) =>
  SDL.Renderer ->
  -- | Where you are painting
  Console ->
  -- | With what you are painting
  Brush ->
  -- | Series of transformation to perform on the glyph
  [Transformation] ->
  -- | Frontcolour of the sprite.
  Maybe RGBA ->
  -- | Backcolour of the sprite.
  Maybe RGBA ->
  -- | Sprite to paint. Should in theory be limited to Char, but we
  -- want to be able to support more ambitious brushes.
  Int ->
  -- | Logical position in the console (in brush size cells)
  V2 Cell ->
  m ()
printCharAt renderer console b@Brush {..} trans frontColour backColour n at = do
  let realRectangle = getScreenRectAt console b (V2 tileWidth tileHeight) at
      flip' = V2 (FlipX `elem` trans) (FlipY `elem` trans)
      rotate = sum $ mapMaybe toDegree trans
  traverse_ (setBackColour renderer realRectangle) backColour
  traverse_ (setFrontColour brush) frontColour
  SDL.copyEx
    renderer
    brush
    (pure $ charIdToPosition b n)
    (pure realRectangle)
    rotate
    Nothing
    flip'

-- | Draw a rectangle with transparency over an area
overlayRect ::
  (MonadIO m) =>
  SDL.Renderer ->
  Console ->
  Brush ->
  -- | Top-left position in cells
  V2 Cell ->
  -- | Size in cells
  V2 Cell ->
  -- | Color with alpha (RGBA)
  RGBA ->
  -- | Blend mode. will BlendAlphaBlend by default.
  Maybe SDL.BlendMode ->
  m ()
overlayRect renderer console b@Brush {..} pos (V2 w h) rgba blendMode = do
  SDL.rendererDrawBlendMode renderer SDL.$= fromMaybe SDL.BlendAlphaBlend blendMode
  SDL.rendererDrawColor renderer SDL.$= rgba
  let pixelRect = getScreenRectAt console b (V2 (tileWidth .*=. w) (tileHeight .*=. h)) pos
  SDL.fillRect renderer (Just pixelRect)
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendNone

getConsoleRect :: Console -> Rectangle CInt
getConsoleRect Console {..} =
  Rectangle (P $ fromIntegral <$> position) (fromIntegral <$> V2 width height)

fillConsoleWith :: (MonadIO m) => Renderer -> Console -> RGBA -> m ()
fillConsoleWith renderer console (V4 r g b a) = do
  let dest = getConsoleRect console
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b a
  SDL.fillRect renderer (pure dest)
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255

clipToConsole :: (MonadIO m) => Renderer -> Console -> m ()
clipToConsole renderer console = do
  let dest = getConsoleRect console
  SDL.rendererClipRect renderer SDL.$= Just dest
