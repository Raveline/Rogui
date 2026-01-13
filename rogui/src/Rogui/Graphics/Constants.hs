-- | A bunch of colour constants taken from libtcod
-- (https://github.com/libtcod/libtcod/blob/main/src/libtcod/color.hpp)
module Rogui.Graphics.Constants where

import Linear (V4 (..))
import Rogui.Graphics.Colours

white :: RGBA
white = V4 255 255 255 255

black :: RGBA
black = V4 0 0 0 255

red :: RGBA
red = V4 255 0 0 255

green :: RGBA
green = V4 0 255 0 255

blue :: RGBA
blue = V4 0 0 255 255

grey :: RGBA
grey = V4 128 128 128 255

flame :: RGBA
flame = V4 255 63 0 255

orange :: RGBA
orange = V4 255 127 0 255

amber :: RGBA
amber = V4 255 191 0 255

yellow :: RGBA
yellow = V4 255 255 0 255

lime :: RGBA
lime = V4 191 255 0 255

chartreuse :: RGBA
chartreuse = V4 127 255 0 255

sea :: RGBA
sea = V4 0 255 127 255

turquoise :: RGBA
turquoise = V4 0 255 191 255

cyan :: RGBA
cyan = V4 0 255 255 255

sky :: RGBA
sky = V4 0 191 255 255

azure :: RGBA
azure = V4 0 127 255 255

han :: RGBA
han = V4 63 0 255 255

violet :: RGBA
violet = V4 127 0 255 255

purple :: RGBA
purple = V4 191 0 255 255

fuchsia :: RGBA
fuchsia = V4 255 0 255 255

magenta :: RGBA
magenta = V4 255 0 191 255

pink :: RGBA
pink = V4 255 0 127 255

crimson :: RGBA
crimson = V4 255 0 63 255

brass :: RGBA
brass = V4 191 151 96 255

copper :: RGBA
copper = V4 196 136 124 255

gold :: RGBA
gold = V4 229 191 0 255

silver :: RGBA
silver = V4 203 203 203 255

celadon :: RGBA
celadon = V4 172 255 175 255

peach :: RGBA
peach = V4 255 159 127 255

sepia :: RGBA
sepia = V4 127 101 63 255

darkestGrey :: RGBA
darkestGrey = V4 31 31 31 255

darkerGrey :: RGBA
darkerGrey = V4 63 63 63 255

darkGrey :: RGBA
darkGrey = V4 95 95 95 255

lightGrey :: RGBA
lightGrey = V4 159 159 159 255

lighterGrey :: RGBA
lighterGrey = V4 191 191 191 255

lightestGrey :: RGBA
lightestGrey = V4 223 223 223 255

darkestSepia :: RGBA
darkestSepia = V4 31 24 15 255

darkerSepia :: RGBA
darkerSepia = V4 63 50 31 255

darkSepia :: RGBA
darkSepia = V4 94 75 47 255

lightSepia :: RGBA
lightSepia = V4 158 134 100 255

lighterSepia :: RGBA
lighterSepia = V4 191 171 143 255

lightestSepia :: RGBA
lightestSepia = V4 222 211 195 255

darkRed :: RGBA
darkRed = V4 191 0 0 255

darkFlame :: RGBA
darkFlame = V4 191 47 0 255

darkOrange :: RGBA
darkOrange = V4 191 95 0 255

darkAmber :: RGBA
darkAmber = V4 191 143 0 255

darkYellow :: RGBA
darkYellow = V4 191 191 0 255

darkLime :: RGBA
darkLime = V4 143 191 0 255

darkChartreuse :: RGBA
darkChartreuse = V4 95 191 0 255

darkGreen :: RGBA
darkGreen = V4 0 191 0 255

darkSea :: RGBA
darkSea = V4 0 191 95 255

darkTurquoise :: RGBA
darkTurquoise = V4 0 191 143 255

darkCyan :: RGBA
darkCyan = V4 0 191 191 255

darkSky :: RGBA
darkSky = V4 0 143 191 255

darkAzure :: RGBA
darkAzure = V4 0 95 191 255

darkBlue :: RGBA
darkBlue = V4 0 0 191 255

darkHan :: RGBA
darkHan = V4 47 0 191 255

darkViolet :: RGBA
darkViolet = V4 95 0 191 255

darkPurple :: RGBA
darkPurple = V4 143 0 191 255

darkFuchsia :: RGBA
darkFuchsia = V4 191 0 191 255

darkMagenta :: RGBA
darkMagenta = V4 191 0 143 255

darkPink :: RGBA
darkPink = V4 191 0 95 255

darkCrimson :: RGBA
darkCrimson = V4 191 0 47 255

darkerRed :: RGBA
darkerRed = V4 127 0 0 255

darkerFlame :: RGBA
darkerFlame = V4 127 31 0 255

darkerOrange :: RGBA
darkerOrange = V4 127 63 0 255

darkerAmber :: RGBA
darkerAmber = V4 127 95 0 255

darkerYellow :: RGBA
darkerYellow = V4 127 127 0 255

darkerLime :: RGBA
darkerLime = V4 95 127 0 255

darkerChartreuse :: RGBA
darkerChartreuse = V4 63 127 0 255

darkerGreen :: RGBA
darkerGreen = V4 0 127 0 255

darkerSea :: RGBA
darkerSea = V4 0 127 63 255

darkerTurquoise :: RGBA
darkerTurquoise = V4 0 127 95 255

darkerCyan :: RGBA
darkerCyan = V4 0 127 127 255

darkerSky :: RGBA
darkerSky = V4 0 95 127 255

darkerAzure :: RGBA
darkerAzure = V4 0 63 127 255

darkerBlue :: RGBA
darkerBlue = V4 0 0 127 255

darkerHan :: RGBA
darkerHan = V4 31 0 127 255

darkerViolet :: RGBA
darkerViolet = V4 63 0 127 255

darkerPurple :: RGBA
darkerPurple = V4 95 0 127 255

darkerFuchsia :: RGBA
darkerFuchsia = V4 127 0 127 255

darkerMagenta :: RGBA
darkerMagenta = V4 127 0 95 255

darkerPink :: RGBA
darkerPink = V4 127 0 63 255

darkerCrimson :: RGBA
darkerCrimson = V4 127 0 31 255

darkestRed :: RGBA
darkestRed = V4 63 0 0 255

darkestFlame :: RGBA
darkestFlame = V4 63 15 0 255

darkestOrange :: RGBA
darkestOrange = V4 63 31 0 255

darkestAmber :: RGBA
darkestAmber = V4 63 47 0 255

darkestYellow :: RGBA
darkestYellow = V4 63 63 0 255

darkestLime :: RGBA
darkestLime = V4 47 63 0 255

darkestChartreuse :: RGBA
darkestChartreuse = V4 31 63 0 255

darkestGreen :: RGBA
darkestGreen = V4 0 63 0 255

darkestSea :: RGBA
darkestSea = V4 0 63 31 255

darkestTurquoise :: RGBA
darkestTurquoise = V4 0 63 47 255

darkestCyan :: RGBA
darkestCyan = V4 0 63 63 255

darkestSky :: RGBA
darkestSky = V4 0 47 63 255

darkestAzure :: RGBA
darkestAzure = V4 0 31 63 255

darkestBlue :: RGBA
darkestBlue = V4 0 0 63 255

darkestHan :: RGBA
darkestHan = V4 15 0 63 255

darkestViolet :: RGBA
darkestViolet = V4 31 0 63 255

darkestPurple :: RGBA
darkestPurple = V4 47 0 63 255

darkestFuchsia :: RGBA
darkestFuchsia = V4 63 0 63 255

darkestMagenta :: RGBA
darkestMagenta = V4 63 0 47 255

darkestPink :: RGBA
darkestPink = V4 63 0 31 255

darkestCrimson :: RGBA
darkestCrimson = V4 63 0 15 255

lightRed :: RGBA
lightRed = V4 255 63 63 255

lightFlame :: RGBA
lightFlame = V4 255 111 63 255

lightOrange :: RGBA
lightOrange = V4 255 159 63 255

lightAmber :: RGBA
lightAmber = V4 255 207 63 255

lightYellow :: RGBA
lightYellow = V4 255 255 63 255

lightLime :: RGBA
lightLime = V4 207 255 63 255

lightChartreuse :: RGBA
lightChartreuse = V4 159 255 63 255

lightGreen :: RGBA
lightGreen = V4 63 255 63 255

lightSea :: RGBA
lightSea = V4 63 255 159 255

lightTurquoise :: RGBA
lightTurquoise = V4 63 255 207 255

lightCyan :: RGBA
lightCyan = V4 63 255 255 255

lightSky :: RGBA
lightSky = V4 63 207 255 255

lightAzure :: RGBA
lightAzure = V4 63 159 255 255

lightBlue :: RGBA
lightBlue = V4 63 63 255 255

lightHan :: RGBA
lightHan = V4 111 63 255 255

lightViolet :: RGBA
lightViolet = V4 159 63 255 255

lightPurple :: RGBA
lightPurple = V4 207 63 255 255

lightFuchsia :: RGBA
lightFuchsia = V4 255 63 255 255

lightMagenta :: RGBA
lightMagenta = V4 255 63 207 255

lightPink :: RGBA
lightPink = V4 255 63 159 255

lightCrimson :: RGBA
lightCrimson = V4 255 63 111 255

lighterRed :: RGBA
lighterRed = V4 255 127 127 255

lighterFlame :: RGBA
lighterFlame = V4 255 159 127 255

lighterOrange :: RGBA
lighterOrange = V4 255 191 127 255

lighterAmber :: RGBA
lighterAmber = V4 255 223 127 255

lighterYellow :: RGBA
lighterYellow = V4 255 255 127 255

lighterLime :: RGBA
lighterLime = V4 223 255 127 255

lighterChartreuse :: RGBA
lighterChartreuse = V4 191 255 127 255

lighterGreen :: RGBA
lighterGreen = V4 127 255 127 255

lighterSea :: RGBA
lighterSea = V4 127 255 191 255

lighterTurquoise :: RGBA
lighterTurquoise = V4 127 255 223 255

lighterCyan :: RGBA
lighterCyan = V4 127 255 255 255

lighterSky :: RGBA
lighterSky = V4 127 223 255 255

lighterAzure :: RGBA
lighterAzure = V4 127 191 255 255

lighterBlue :: RGBA
lighterBlue = V4 127 127 255 255

lighterHan :: RGBA
lighterHan = V4 159 127 255 255

lighterViolet :: RGBA
lighterViolet = V4 191 127 255 255

lighterPurple :: RGBA
lighterPurple = V4 223 127 255 255

lighterFuchsia :: RGBA
lighterFuchsia = V4 255 127 255 255

lighterMagenta :: RGBA
lighterMagenta = V4 255 127 223 255

lighterPink :: RGBA
lighterPink = V4 255 127 191 255

lighterCrimson :: RGBA
lighterCrimson = V4 255 127 159 255

lightestRed :: RGBA
lightestRed = V4 255 191 191 255

lightestFlame :: RGBA
lightestFlame = V4 255 207 191 255

lightestOrange :: RGBA
lightestOrange = V4 255 223 191 255

lightestAmber :: RGBA
lightestAmber = V4 255 239 191 255

lightestYellow :: RGBA
lightestYellow = V4 255 255 191 255

lightestLime :: RGBA
lightestLime = V4 239 255 191 255

lightestChartreuse :: RGBA
lightestChartreuse = V4 223 255 191 255

lightestGreen :: RGBA
lightestGreen = V4 191 255 191 255

lightestSea :: RGBA
lightestSea = V4 191 255 223 255

lightestTurquoise :: RGBA
lightestTurquoise = V4 191 255 239 255

lightestCyan :: RGBA
lightestCyan = V4 191 255 255 255

lightestSky :: RGBA
lightestSky = V4 191 239 255 255

lightestAzure :: RGBA
lightestAzure = V4 191 223 255 255

lightestBlue :: RGBA
lightestBlue = V4 191 191 255 255

lightestHan :: RGBA
lightestHan = V4 207 191 255 255

lightestViolet :: RGBA
lightestViolet = V4 223 191 255 255

lightestPurple :: RGBA
lightestPurple = V4 239 191 255 255

lightestFuchsia :: RGBA
lightestFuchsia = V4 255 191 255 255

lightestMagenta :: RGBA
lightestMagenta = V4 255 191 239 255

lightestPink :: RGBA
lightestPink = V4 255 191 223 255

lightestCrimson :: RGBA
lightestCrimson = V4 255 191 207 255

desaturatedRed :: RGBA
desaturatedRed = V4 127 63 63 255

desaturatedFlame :: RGBA
desaturatedFlame = V4 127 79 63 255

desaturatedOrange :: RGBA
desaturatedOrange = V4 127 95 63 255

desaturatedAmber :: RGBA
desaturatedAmber = V4 127 111 63 255

desaturatedYellow :: RGBA
desaturatedYellow = V4 127 127 63 255

desaturatedLime :: RGBA
desaturatedLime = V4 111 127 63 255

desaturatedChartreuse :: RGBA
desaturatedChartreuse = V4 95 127 63 255

desaturatedGreen :: RGBA
desaturatedGreen = V4 63 127 63 255

desaturatedSea :: RGBA
desaturatedSea = V4 63 127 95 255

desaturatedTurquoise :: RGBA
desaturatedTurquoise = V4 63 127 111 255

desaturatedCyan :: RGBA
desaturatedCyan = V4 63 127 127 255

desaturatedSky :: RGBA
desaturatedSky = V4 63 111 127 255

desaturatedAzure :: RGBA
desaturatedAzure = V4 63 95 127 255

desaturatedBlue :: RGBA
desaturatedBlue = V4 63 63 127 255

desaturatedHan :: RGBA
desaturatedHan = V4 79 63 127 255

desaturatedViolet :: RGBA
desaturatedViolet = V4 95 63 127 255

desaturatedPurple :: RGBA
desaturatedPurple = V4 111 63 127 255

desaturatedFuchsia :: RGBA
desaturatedFuchsia = V4 127 63 127 255

desaturatedMagenta :: RGBA
desaturatedMagenta = V4 127 63 111 255

desaturatedPink :: RGBA
desaturatedPink = V4 127 63 95 255

desaturatedCrimson :: RGBA
desaturatedCrimson = V4 127 63 79 255

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

vertical437 :: Int
vertical437 = 179

horizontal437 :: Int
horizontal437 = 196

cornerTopLeft437 :: Int
cornerTopLeft437 = 218

cornerTopRight437 :: Int
cornerTopRight437 = 191

cornerBottomLeft437 :: Int
cornerBottomLeft437 = 192

cornerBottomRight437 :: Int
cornerBottomRight437 = 217