module Rogui.Graphics.Colours
  ( Colours (..),
    invert,
    gradient,
  )
where

import Data.Fixed (mod')
import Data.Word (Word8)
import SDL (V3 (..))

data Colours = Colours {front :: Maybe RGB, back :: Maybe RGB}
  deriving (Eq, Show)

invert :: Colours -> Colours
invert (Colours f b) = Colours b f

type RGB = V3 Word8

type HSV = V3 Double

rgbToHsv :: RGB -> HSV
rgbToHsv (V3 r g b) =
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

hsvToRgb :: HSV -> RGB
hsvToRgb (V3 h s v) =
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
   in V3 (toWord8 r') (toWord8 g') (toWord8 b')

-- | Generate a color gradient using HSV interpolation with N steps (inclusive of endpoints)
-- Interpolates hue along the shortest path around the color wheel
gradient :: RGB -> RGB -> Int -> [RGB]
gradient from to steps
  | steps < 2 = [from, to] -- Degenerate case
  | otherwise = interpolate <$> [0 .. steps - 1]
  where
    (V3 h1 s1 v1) = rgbToHsv from
    (V3 h2 s2 v2) = rgbToHsv to
    interpolate i =
      let t = fromIntegral i / fromIntegral (steps - 1)
          lerp a b = a * (1 - t) + b * t
          h = lerpHueShortPath h1 h2 t
          s = lerp s1 s2
          v = lerp v1 v2
       in hsvToRgb (V3 h s v)
    lerpHueShortPath hue1 hue2 t =
      let diff = hue2 - hue1
          shortestDiff
            | diff > 180 = diff - 360
            | diff < -180 = diff + 360
            | otherwise = diff
          result = hue1 + shortestDiff * t
       in if result < 0 then result + 360 else if result >= 360 then result - 360 else result