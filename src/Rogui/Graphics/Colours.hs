module Rogui.Graphics.Colours
  ( Colours (..),
    invert,
    gradient,
    setAlpha,
  )
where

import Data.Fixed (mod')
import Data.Word (Word8)
import SDL (V3 (..), V4 (..))

-- | Linear interpolation for scalar values
lerpScalar :: (Num a) => a -> a -> a -> a
lerpScalar t a b = a * (1 - t) + b * t

type RGBA = V4 Word8

data Colours = Colours {front :: Maybe RGBA, back :: Maybe RGBA}
  deriving (Eq, Show)

invert :: Colours -> Colours
invert (Colours f b) = Colours b f

setAlpha :: RGBA -> Word8 -> RGBA
setAlpha (V4 r g b _) = V4 r g b

type HSV = V3 Double

rgbToHsv :: RGBA -> HSV
rgbToHsv (V4 r g b _) =
  let r' = fromIntegral r / 255.0
      g' = fromIntegral g / 255.0
      b' = fromIntegral b / 255.0
      cmax = max r' (max g' b')
      cmin = min r' (min g' b')
      delta = cmax - cmin
      h
        | delta == 0 = 0
        | cmax == r' = 60 * (((g' - b') / delta) `mod'` 6)
        | cmax == g' = 60 * (((b' - r') / delta) + 2)
        | otherwise = 60 * (((r' - g') / delta) + 4)
      s
        | cmax == 0 = 0
        | otherwise = delta / cmax
      v = cmax
   in V3 h s v

hsvToRgb :: HSV -> Word8 -> RGBA
hsvToRgb (V3 h s v) a =
  let c = v * s
      h' = h / 60.0
      x = c * (1 - abs ((h' `mod'` 2) - 1))
      m = v - c

      (r', g', b')
        | h' < 1 = (c, x, 0)
        | h' < 2 = (x, c, 0)
        | h' < 3 = (0, c, x)
        | h' < 4 = (0, x, c)
        | h' < 5 = (x, 0, c)
        | otherwise = (c, 0, x)

      toWord8 val = round ((val + m) * 255)
   in V4 (toWord8 r') (toWord8 g') (toWord8 b') a

-- | Generate a color gradient using HSV interpolation with N steps (inclusive of endpoints)
-- Interpolates hue along the shortest path around the color wheel, and alpha linearly
gradient :: RGBA -> RGBA -> Int -> [RGBA]
gradient from to steps
  | steps < 2 = [from, to] -- Degenerate case
  | otherwise = interpolate <$> [0 .. steps - 1]
  where
    (V3 h1 s1 v1) = rgbToHsv from
    (V3 h2 s2 v2) = rgbToHsv to
    (V4 _ _ _ a1) = from
    (V4 _ _ _ a2) = to
    interpolate i =
      let t = fromIntegral i / fromIntegral (steps - 1)
          lerpWord8 w1 w2 = round (lerpScalar t (fromIntegral w1) (fromIntegral w2))
          h = lerpHueShortPath h1 h2 t
          s = lerpScalar t s1 s2
          v = lerpScalar t v1 v2
          a = lerpWord8 a1 a2
       in hsvToRgb (V3 h s v) a
    lerpHueShortPath hue1 hue2 t =
      let diff = hue2 - hue1
          shortestDiff
            | diff > 180 = diff - 360
            | diff < -180 = diff + 360
            | otherwise = diff
          result = hue1 + shortestDiff * t
       in if result < 0 then result + 360 else if result >= 360 then result - 360 else result