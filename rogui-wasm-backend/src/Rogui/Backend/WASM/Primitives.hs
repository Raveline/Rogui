{-# LANGUAGE RecordWildCards #-}

-- | Canvas 2D equivalents of `Rogui.Backend.SDL.Primitives`: turning a
-- `Brush` reference + tile id into pixels on screen, and loading tileset
-- images into something `drawImage` can consume.
--
-- Unlike SDL (which mutates a texture's colour/alpha mod as global state
-- before drawing it), Canvas 2D draws are stateless per-call, so
-- `printCharAt` here takes the foreground colour directly rather than
-- relying on a "set colour, then draw" two-step.
module Rogui.Backend.WASM.Primitives
  ( loadWASMBrush,
    takeWASMScreenshot,
    printCharAt,
    fillConsoleWith,
    clipToConsole,
    overlayRect,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Maybe (fromMaybe, mapMaybe)
import Foreign.Ptr (castPtr)
import Linear (V2 (..), V4 (..))
import Rogui.Backend.WASM.FFI
  ( CanvasContext (..),
    WASMTexture (..),
    js_canvasHeight,
    js_canvasWidth,
    js_clipToRect,
    js_downloadCanvas,
    js_drawGlyph,
    js_fillRect,
    js_imageToTexture,
    js_loadImageFromBytes,
    js_loadImageFromURL,
    js_overlayRect,
    toJSString,
  )
import Rogui.Graphics

packRGBA :: RGBA -> Int
packRGBA (V4 r g b a) =
  (fromIntegral r `shiftL` 24) .|. (fromIntegral g `shiftL` 16) .|. (fromIntegral b `shiftL` 8) .|. fromIntegral a

-- | Only the RGB channels matter for a colour-key match.
packRGB :: RGBA -> Int
packRGB (V4 r g b _) =
  (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

-- | 0|1|2|
--  |3|4|5|
--  |6|7|8|
--  |9|10|11|
charIdToPosition :: Brush -> Int -> (Int, Int, Int, Int)
charIdToPosition Brush {..} tileId =
  let numberOfColumns = getPixel $ textureWidth `div` tileWidth
      x = tileId `mod` numberOfColumns
      y = tileId `div` numberOfColumns
   in (x * getPixel tileWidth, y * getPixel tileHeight, getPixel tileWidth, getPixel tileHeight)

getScreenRectAt :: Console -> Brush -> V2 Pixel -> V2 Cell -> (Int, Int, Int, Int)
getScreenRectAt Console {..} Brush {..} (V2 rectWidth rectHeight) at =
  let getScreenPos (V2 x y) = V2 (tileWidth .*=. x) (tileHeight .*=. y)
      V2 px py = position + getScreenPos at
   in (getPixel px, getPixel py, getPixel rectWidth, getPixel rectHeight)

toDegree :: Transformation -> Maybe Double
toDegree (Rotate R90) = Just 90
toDegree (Rotate R180) = Just 180
toDegree (Rotate R270) = Just 270
toDegree (Rotate (RArbitrary d)) = Just d
toDegree _ = Nothing

-- | Display a glyph on the canvas, with a given brush, on a given console,
-- with a background and a foreground colour, at a given position on a
-- given console, applying optional transformations over the glyph.
printCharAt ::
  (MonadIO m) =>
  CanvasContext ->
  -- | Where you are painting
  Console ->
  -- | With what you are painting
  Brush ->
  WASMTexture ->
  -- | Series of transformation to perform on the glyph
  [Transformation] ->
  -- | Front colour of the sprite (Canvas 2D has no persistent texture
  -- colour-mod state, so this is applied per draw call).
  Maybe RGBA ->
  -- | Back colour of the sprite.
  Maybe RGBA ->
  -- | Sprite to paint.
  Int ->
  -- | Logical position in the console (in brush size cells)
  V2 Cell ->
  m ()
printCharAt (CanvasContext ctx) console b@Brush {..} (WASMTexture tex) trans frontColour backColour n at = liftIO $ do
  let (sx, sy, sw, sh) = charIdToPosition b n
      (dx, dy, dw, dh) = getScreenRectAt console b (V2 tileWidth tileHeight) at
      flipX = FlipX `elem` trans
      flipY = FlipY `elem` trans
      rotateDeg = sum $ mapMaybe toDegree trans
      (hasBack, backPacked) = maybe (False, 0) ((,) True . packRGBA) backColour
      (hasFront, frontPacked) = maybe (False, 0) ((,) True . packRGBA) frontColour
  js_drawGlyph ctx tex sx sy sw sh dx dy dw dh flipX flipY rotateDeg hasBack backPacked hasFront frontPacked

-- | Draw a rectangle with transparency over an area
overlayRect ::
  (MonadIO m) =>
  CanvasContext ->
  Console ->
  Brush ->
  -- | Top-left position in cells
  V2 Cell ->
  -- | Size in cells
  V2 Cell ->
  -- | Colour with alpha (RGBA)
  RGBA ->
  -- | Blend mode. Defaults to alpha-blending, like the SDL backend.
  Maybe BlendMode ->
  m ()
overlayRect (CanvasContext ctx) console b@Brush {..} pos (V2 w h) rgba blendMode = liftIO $ do
  let (dx, dy, dw, dh) = getScreenRectAt console b (V2 (tileWidth .*=. w) (tileHeight .*=. h)) pos
      mode = case fromMaybe AlphaBlend blendMode of
        AlphaBlend -> 0
        Add -> 1
        None -> 2
  js_overlayRect ctx dx dy dw dh (packRGBA rgba) mode

fillConsoleWith :: (MonadIO m) => CanvasContext -> Console -> RGBA -> m ()
fillConsoleWith (CanvasContext ctx) Console {..} rgba =
  let V2 px py = position
   in liftIO $ js_fillRect ctx (getPixel px) (getPixel py) (getPixel width) (getPixel height) (packRGBA rgba)

clipToConsole :: (MonadIO m) => CanvasContext -> Console -> m ()
clipToConsole (CanvasContext ctx) Console {..} =
  let V2 px py = position
   in liftIO $ js_clipToRect ctx (getPixel px) (getPixel py) (getPixel width) (getPixel height)

loadWASMBrush ::
  (MonadIO m) =>
  CanvasContext ->
  TileSize ->
  Either ByteString FilePath ->
  Maybe RGBA ->
  m (Brush, WASMTexture)
loadWASMBrush _ctx TileSize {..} source transparency = liftIO $ do
  img <- case source of
    Right url -> js_loadImageFromURL (toJSString url)
    Left bs -> unsafeUseAsCStringLen bs $ \(ptr, len) -> js_loadImageFromBytes (castPtr ptr) len
  let (hasKey, keyPacked) = maybe (False, 0) ((,) True . packRGB) transparency
  texture <- js_imageToTexture img hasKey keyPacked
  w <- js_canvasWidth texture
  h <- js_canvasHeight texture
  pure
    ( Brush
        { tileWidth = pixelWidth,
          tileHeight = pixelHeight,
          textureWidth = Pixel w,
          textureHeight = Pixel h,
          name = either (const "embedded") id source
        },
      WASMTexture texture
    )

-- | There is no filesystem in a browser: this renders the canvas via
-- `toDataURL` and triggers a client-side download instead of writing to
-- the given path (used verbatim as the suggested download file name).
takeWASMScreenshot :: (MonadIO m) => CanvasContext -> V2 Int -> FilePath -> m ()
takeWASMScreenshot (CanvasContext ctx) _size fp = liftIO $ js_downloadCanvas ctx (toJSString fp)
