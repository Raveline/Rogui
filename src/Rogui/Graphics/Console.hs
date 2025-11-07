module Rogui.Graphics.Console
  ( drawBorder,
    printStrAt,
    TextAlign (..),
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Char (ord)
import Data.Foldable (traverse_)
import Rogui.Graphics.Primitives (RGB, printCharAt)
import Rogui.Graphics.Types (Brush (..), Cell (..), Console (..), (./.=))
import SDL (Renderer, V2 (..), (^*))

vertical437, horizontal437, cornerTopLeft437, cornerTopRight437, cornerBottomLeft437, cornerBottomRight437 :: Int
vertical437 = 179
horizontal437 = 196
cornerTopLeft437 = 218
cornerTopRight437 = 191
cornerBottomLeft437 = 192
cornerBottomRight437 = 217

-- | Draw a border around the given console with the given brush.
-- Assumes the Brush has the standard position for border glyphs
-- on a CCSID 437 tileset.
drawBorder :: (MonadIO m) => Renderer -> Console -> Brush -> Maybe RGB -> Maybe RGB -> m ()
drawBorder renderer console@Console {..} brush'@Brush {..} front back = do
  let (w, h) = (width ./.= tileWidth - 1, height ./.= tileHeight - 1)
      draw = printCharAt renderer console brush' front back
      bottoms = [V2 x y | x <- [1 .. w - 1], y <- [h]]
      tops = [V2 x y | x <- [1 .. w - 1], y <- [0]]
      lefts = [V2 x y | x <- [0], y <- [1 .. h - 1]]
      rights = [V2 x y | x <- [w], y <- [1 .. h - 1]]
  traverse_ (draw horizontal437) tops
  traverse_ (draw horizontal437) bottoms
  traverse_ (draw vertical437) lefts
  traverse_ (draw vertical437) rights
  void $ draw cornerTopLeft437 $ V2 0 0
  void $ draw cornerBottomLeft437 $ V2 0 h
  void $ draw cornerTopRight437 $ V2 w 0
  void $ draw cornerBottomRight437 $ V2 w h

-- | Very basic, dummy string printer. Will not check for overflow.
-- Print from the position given with the alignment given.
printStrAt :: (MonadIO m) => Renderer -> Console -> Brush -> Maybe RGB -> Maybe RGB -> TextAlign -> String -> V2 Cell -> m ()
printStrAt renderer console brush front back alignment str pos =
  let draw n = printCharAt renderer console brush front back (ord n)
      basePos = case alignment of
        TCenter -> pos - V2 (Cell $ length str `div` 2) 0
        TRight -> pos - V2 (Cell $ length str) 0
        _ -> pos
      next (i, c) = draw c (basePos + (Cell <$> V2 1 0 ^* i))
      indexed = zip [0 ..] str
   in traverse_ next indexed

data TextAlign
  = TLeft
  | TRight
  | TCenter