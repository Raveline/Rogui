module Rogui.Graphics.Constants where

import Rogui.Graphics.Primitives
import SDL (V3 (..))

white :: RGB
white = V3 255 255 255

black :: RGB
black = V3 0 0 0

red :: RGB
red = V3 255 0 0

green :: RGB
green = V3 0 255 0

blue :: RGB
blue = V3 0 0 255

grey :: RGB
grey = V3 128 128 128

-- | Characters constants for CCSID 437 tileset
fullBlock :: Int
fullBlock = 219

lightShade :: Int
lightShade = 176

mediumShade :: Int
mediumShade = 177

darkShade :: Int
darkShade = 178

arrowheadRight :: Int
arrowheadRight = 10

arrowheadLeft :: Int
arrowheadLeft = 11

arrowheadTop :: Int
arrowheadTop = 30

arrowheadBottom :: Int
arrowheadBottom = 31