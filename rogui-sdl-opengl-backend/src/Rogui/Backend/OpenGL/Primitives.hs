module Rogui.Backend.OpenGL.Primitives
  (
    clipToConsole,
    fillConsoleWith,
    overlayRect,
    printCharAt
  ) where
import Control.Monad.IO.Class
import Rogui.Backend.OpenGL.Types
import Rogui.Graphics
import Graphics.GL
import Linear
import Data.IORef (readIORef)
import Data.Maybe (mapMaybe)

clipToConsole :: (MonadIO m) => GLRenderer -> Console -> m ()
clipToConsole GLRenderer{..} Console{..} = do
  let (V2 x y) = position
  (V2 _ windowH) <- liftIO $ readIORef glWindowSize
  glEnable GL_SCISSOR_TEST
  glScissor
    (fromIntegral x)
    (fromIntegral windowH - fromIntegral y - fromIntegral height)
    (fromIntegral width)
    (fromIntegral height)

-- | Convert a Word8 RGBA colour to a Float RGBA colour (0–1 range).
rgbaToFloat :: RGBA -> V4 Float
rgbaToFloat (V4 r g b a) = V4 (f r) (f g) (f b) (f a)
  where f x = fromIntegral x / 255.0

-- | Generate the 6 vertices (2 triangles) for a single glyph quad.
printCharAt ::
  -- | Where you are painting
  Console ->
  -- | With what you are painting
  Brush ->
  GLTexture ->
  -- | Series of transformation to perform on the glyph
  [Transformation] ->
  -- | Colours to apply
  Colours ->
  -- | Sprite to paint. Should in theory be limited to Char, but we
  -- want to be able to support more ambitious brushes.
  Int ->
  -- | Logical position in the console (in brush size cells)
  V2 Cell ->
  [Vertex]
printCharAt Console{..} Brush{..} GLTexture{..} trans Colours{..} tileId at =
  let -- Source rect in the tileset
      numberOfColumns = getPixel textureWidth `div` getPixel tileWidth
      col = tileId `mod` numberOfColumns
      row = tileId `div` numberOfColumns
      tw = fromIntegral (getPixel tileWidth)
      th = fromIntegral (getPixel tileHeight)
      texW = fromIntegral texWidth :: Float
      texH = fromIntegral texHeight :: Float
      u0 = fromIntegral col * tw / texW
      v0 = fromIntegral row * th / texH
      u1 = u0 + tw / texW
      v1 = v0 + th / texH

      -- Apply flip by swapping texture coord if need be
      flipX = odd . length $ filter (== FlipX) trans
      flipY = odd . length $ filter (== FlipY) trans
      (su0, su1) = if flipX then (u1, u0) else (u0, u1)
      (sv0, sv1) = if flipY then (v1, v0) else (v0, v1)

      -- Apply rotation to UV corners
      rotationDegrees = sum $ mapMaybe toDegree trans
      (uvTL, uvTR, uvBR, uvBL) = rotateUVs rotationDegrees (su0, sv0) (su1, sv1)

      -- Destination rect in screen pixels
      (V2 cx cy) = at
      (V2 ppx ppy) = position
      px = fromIntegral (getPixel ppx + getPixel tileWidth * getCell cx)
      py = fromIntegral (getPixel ppy + getPixel tileHeight * getCell cy)

      -- Per-vertex colours
      frontColour = maybe (V4 1 1 1 1) rgbaToFloat front
      backColour = maybe (V4 0 0 0 0) rgbaToFloat back

      -- 4 corners of the screen quad
      pTL = V2 px py
      pTR = V2 (px + tw) py
      pBR = V2 (px + tw) (py + th)
      pBL = V2 px (py + th)

      mkVertex p uv = Vertex p uv frontColour backColour

   in [ mkVertex pTL uvTL, mkVertex pTR uvTR, mkVertex pBR uvBR
      , mkVertex pBR uvBR, mkVertex pBL uvBL, mkVertex pTL uvTL
      ]

-- | Extract rotation degrees from a Transformation.
toDegree :: Transformation -> Maybe Double
toDegree (Rotate R90) = Just 90
toDegree (Rotate R180) = Just 180
toDegree (Rotate R270) = Just 270
toDegree (Rotate (RArbitrary d)) = Just d
toDegree _ = Nothing

-- | Rotate UV corners for 90-degree increments.
-- For arbitrary rotation, this falls back to the nearest 90-degree step.
rotateUVs :: Double -> (Float, Float) -> (Float, Float) -> (V2 Float, V2 Float, V2 Float, V2 Float)
rotateUVs degrees (u0, v0) (u1, v1) =
  let step = round @Double @Int (degrees / 90) `mod` 4
   in case step of
        0 -> (V2 u0 v0, V2 u1 v0, V2 u1 v1, V2 u0 v1)
        1 -> (V2 u0 v1, V2 u0 v0, V2 u1 v0, V2 u1 v1)
        2 -> (V2 u1 v1, V2 u0 v1, V2 u0 v0, V2 u1 v0)
        3 -> (V2 u1 v0, V2 u1 v1, V2 u0 v1, V2 u0 v0)
        _ -> error "Unreachable reached"

-- | Generate 6 vertices for a solid-colour quad covering a cell region.
solidQuad :: V2 Float -> V2 Float -> V4 Float -> [Vertex]
solidQuad (V2 x0 y0) (V2 x1 y1) colour =
  let noUV = V2 0 0
      noBack = V4 0 0 0 0
      mkVertex p = Vertex p noUV colour noBack
      pTL = V2 x0 y0
      pTR = V2 x1 y0
      pBR = V2 x1 y1
      pBL = V2 x0 y1
   in [ mkVertex pTL, mkVertex pTR, mkVertex pBR
      , mkVertex pBR, mkVertex pBL, mkVertex pTL
      ]

-- | Fill an entire console with a solid colour.
fillConsoleWith ::
  Console ->
  RGBA ->
  [Vertex]
fillConsoleWith Console{..} rgba =
  let (V2 px py) = position
      x0 = fromIntegral (getPixel px)
      y0 = fromIntegral (getPixel py)
      x1 = x0 + fromIntegral (getPixel width)
      y1 = y0 + fromIntegral (getPixel height)
   in solidQuad (V2 x0 y0) (V2 x1 y1) (rgbaToFloat rgba)

-- | Draw a rectangle with transparency over an area defined in cells.
overlayRect ::
  Console ->
  Brush ->
  -- | Top-left position in cells
  V2 Cell ->
  -- | Size in cells
  V2 Cell ->
  -- | Color with alpha
  RGBA ->
  [Vertex]
overlayRect Console{..} Brush{..} pos (V2 w h) rgba =
  let (V2 cx cy) = pos
      (V2 px py) = position
      x0 = fromIntegral (getPixel px + getPixel tileWidth * getCell cx)
      y0 = fromIntegral (getPixel py + getPixel tileHeight * getCell cy)
      x1 = x0 + fromIntegral (getPixel tileWidth * getCell w)
      y1 = y0 + fromIntegral (getPixel tileHeight * getCell h)
   in solidQuad (V2 x0 y0) (V2 x1 y1) (rgbaToFloat rgba)
