-- | A bunch of colour constants taken from libtcod
-- (https://github.com/libtcod/libtcod/blob/main/src/libtcod/color.hpp)
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

flame :: RGB
flame = V3 255 63 0

orange :: RGB
orange = V3 255 127 0

amber :: RGB
amber = V3 255 191 0

yellow :: RGB
yellow = V3 255 255 0

lime :: RGB
lime = V3 191 255 0

chartreuse :: RGB
chartreuse = V3 127 255 0

sea :: RGB
sea = V3 0 255 127

turquoise :: RGB
turquoise = V3 0 255 191

cyan :: RGB
cyan = V3 0 255 255

sky :: RGB
sky = V3 0 191 255

azure :: RGB
azure = V3 0 127 255

han :: RGB
han = V3 63 0 255

violet :: RGB
violet = V3 127 0 255

purple :: RGB
purple = V3 191 0 255

fuchsia :: RGB
fuchsia = V3 255 0 255

magenta :: RGB
magenta = V3 255 0 191

pink :: RGB
pink = V3 255 0 127

crimson :: RGB
crimson = V3 255 0 63

brass :: RGB
brass = V3 191 151 96

copper :: RGB
copper = V3 196 136 124

gold :: RGB
gold = V3 229 191 0

silver :: RGB
silver = V3 203 203 203

celadon :: RGB
celadon = V3 172 255 175

peach :: RGB
peach = V3 255 159 127

sepia :: RGB
sepia = V3 127 101 63

darkestGrey :: RGB
darkestGrey = V3 31 31 31

darkerGrey :: RGB
darkerGrey = V3 63 63 63

darkGrey :: RGB
darkGrey = V3 95 95 95

lightGrey :: RGB
lightGrey = V3 159 159 159

lighterGrey :: RGB
lighterGrey = V3 191 191 191

lightestGrey :: RGB
lightestGrey = V3 223 223 223

darkestSepia :: RGB
darkestSepia = V3 31 24 15

darkerSepia :: RGB
darkerSepia = V3 63 50 31

darkSepia :: RGB
darkSepia = V3 94 75 47

lightSepia :: RGB
lightSepia = V3 158 134 100

lighterSepia :: RGB
lighterSepia = V3 191 171 143

lightestSepia :: RGB
lightestSepia = V3 222 211 195

darkRed :: RGB
darkRed = V3 191 0 0

darkFlame :: RGB
darkFlame = V3 191 47 0

darkOrange :: RGB
darkOrange = V3 191 95 0

darkAmber :: RGB
darkAmber = V3 191 143 0

darkYellow :: RGB
darkYellow = V3 191 191 0

darkLime :: RGB
darkLime = V3 143 191 0

darkChartreuse :: RGB
darkChartreuse = V3 95 191 0

darkGreen :: RGB
darkGreen = V3 0 191 0

darkSea :: RGB
darkSea = V3 0 191 95

darkTurquoise :: RGB
darkTurquoise = V3 0 191 143

darkCyan :: RGB
darkCyan = V3 0 191 191

darkSky :: RGB
darkSky = V3 0 143 191

darkAzure :: RGB
darkAzure = V3 0 95 191

darkBlue :: RGB
darkBlue = V3 0 0 191

darkHan :: RGB
darkHan = V3 47 0 191

darkViolet :: RGB
darkViolet = V3 95 0 191

darkPurple :: RGB
darkPurple = V3 143 0 191

darkFuchsia :: RGB
darkFuchsia = V3 191 0 191

darkMagenta :: RGB
darkMagenta = V3 191 0 143

darkPink :: RGB
darkPink = V3 191 0 95

darkCrimson :: RGB
darkCrimson = V3 191 0 47

darkerRed :: RGB
darkerRed = V3 127 0 0

darkerFlame :: RGB
darkerFlame = V3 127 31 0

darkerOrange :: RGB
darkerOrange = V3 127 63 0

darkerAmber :: RGB
darkerAmber = V3 127 95 0

darkerYellow :: RGB
darkerYellow = V3 127 127 0

darkerLime :: RGB
darkerLime = V3 95 127 0

darkerChartreuse :: RGB
darkerChartreuse = V3 63 127 0

darkerGreen :: RGB
darkerGreen = V3 0 127 0

darkerSea :: RGB
darkerSea = V3 0 127 63

darkerTurquoise :: RGB
darkerTurquoise = V3 0 127 95

darkerCyan :: RGB
darkerCyan = V3 0 127 127

darkerSky :: RGB
darkerSky = V3 0 95 127

darkerAzure :: RGB
darkerAzure = V3 0 63 127

darkerBlue :: RGB
darkerBlue = V3 0 0 127

darkerHan :: RGB
darkerHan = V3 31 0 127

darkerViolet :: RGB
darkerViolet = V3 63 0 127

darkerPurple :: RGB
darkerPurple = V3 95 0 127

darkerFuchsia :: RGB
darkerFuchsia = V3 127 0 127

darkerMagenta :: RGB
darkerMagenta = V3 127 0 95

darkerPink :: RGB
darkerPink = V3 127 0 63

darkerCrimson :: RGB
darkerCrimson = V3 127 0 31

darkestRed :: RGB
darkestRed = V3 63 0 0

darkestFlame :: RGB
darkestFlame = V3 63 15 0

darkestOrange :: RGB
darkestOrange = V3 63 31 0

darkestAmber :: RGB
darkestAmber = V3 63 47 0

darkestYellow :: RGB
darkestYellow = V3 63 63 0

darkestLime :: RGB
darkestLime = V3 47 63 0

darkestChartreuse :: RGB
darkestChartreuse = V3 31 63 0

darkestGreen :: RGB
darkestGreen = V3 0 63 0

darkestSea :: RGB
darkestSea = V3 0 63 31

darkestTurquoise :: RGB
darkestTurquoise = V3 0 63 47

darkestCyan :: RGB
darkestCyan = V3 0 63 63

darkestSky :: RGB
darkestSky = V3 0 47 63

darkestAzure :: RGB
darkestAzure = V3 0 31 63

darkestBlue :: RGB
darkestBlue = V3 0 0 63

darkestHan :: RGB
darkestHan = V3 15 0 63

darkestViolet :: RGB
darkestViolet = V3 31 0 63

darkestPurple :: RGB
darkestPurple = V3 47 0 63

darkestFuchsia :: RGB
darkestFuchsia = V3 63 0 63

darkestMagenta :: RGB
darkestMagenta = V3 63 0 47

darkestPink :: RGB
darkestPink = V3 63 0 31

darkestCrimson :: RGB
darkestCrimson = V3 63 0 15

lightRed :: RGB
lightRed = V3 255 63 63

lightFlame :: RGB
lightFlame = V3 255 111 63

lightOrange :: RGB
lightOrange = V3 255 159 63

lightAmber :: RGB
lightAmber = V3 255 207 63

lightYellow :: RGB
lightYellow = V3 255 255 63

lightLime :: RGB
lightLime = V3 207 255 63

lightChartreuse :: RGB
lightChartreuse = V3 159 255 63

lightGreen :: RGB
lightGreen = V3 63 255 63

lightSea :: RGB
lightSea = V3 63 255 159

lightTurquoise :: RGB
lightTurquoise = V3 63 255 207

lightCyan :: RGB
lightCyan = V3 63 255 255

lightSky :: RGB
lightSky = V3 63 207 255

lightAzure :: RGB
lightAzure = V3 63 159 255

lightBlue :: RGB
lightBlue = V3 63 63 255

lightHan :: RGB
lightHan = V3 111 63 255

lightViolet :: RGB
lightViolet = V3 159 63 255

lightPurple :: RGB
lightPurple = V3 207 63 255

lightFuchsia :: RGB
lightFuchsia = V3 255 63 255

lightMagenta :: RGB
lightMagenta = V3 255 63 207

lightPink :: RGB
lightPink = V3 255 63 159

lightCrimson :: RGB
lightCrimson = V3 255 63 111

lighterRed :: RGB
lighterRed = V3 255 127 127

lighterFlame :: RGB
lighterFlame = V3 255 159 127

lighterOrange :: RGB
lighterOrange = V3 255 191 127

lighterAmber :: RGB
lighterAmber = V3 255 223 127

lighterYellow :: RGB
lighterYellow = V3 255 255 127

lighterLime :: RGB
lighterLime = V3 223 255 127

lighterChartreuse :: RGB
lighterChartreuse = V3 191 255 127

lighterGreen :: RGB
lighterGreen = V3 127 255 127

lighterSea :: RGB
lighterSea = V3 127 255 191

lighterTurquoise :: RGB
lighterTurquoise = V3 127 255 223

lighterCyan :: RGB
lighterCyan = V3 127 255 255

lighterSky :: RGB
lighterSky = V3 127 223 255

lighterAzure :: RGB
lighterAzure = V3 127 191 255

lighterBlue :: RGB
lighterBlue = V3 127 127 255

lighterHan :: RGB
lighterHan = V3 159 127 255

lighterViolet :: RGB
lighterViolet = V3 191 127 255

lighterPurple :: RGB
lighterPurple = V3 223 127 255

lighterFuchsia :: RGB
lighterFuchsia = V3 255 127 255

lighterMagenta :: RGB
lighterMagenta = V3 255 127 223

lighterPink :: RGB
lighterPink = V3 255 127 191

lighterCrimson :: RGB
lighterCrimson = V3 255 127 159

lightestRed :: RGB
lightestRed = V3 255 191 191

lightestFlame :: RGB
lightestFlame = V3 255 207 191

lightestOrange :: RGB
lightestOrange = V3 255 223 191

lightestAmber :: RGB
lightestAmber = V3 255 239 191

lightestYellow :: RGB
lightestYellow = V3 255 255 191

lightestLime :: RGB
lightestLime = V3 239 255 191

lightestChartreuse :: RGB
lightestChartreuse = V3 223 255 191

lightestGreen :: RGB
lightestGreen = V3 191 255 191

lightestSea :: RGB
lightestSea = V3 191 255 223

lightestTurquoise :: RGB
lightestTurquoise = V3 191 255 239

lightestCyan :: RGB
lightestCyan = V3 191 255 255

lightestSky :: RGB
lightestSky = V3 191 239 255

lightestAzure :: RGB
lightestAzure = V3 191 223 255

lightestBlue :: RGB
lightestBlue = V3 191 191 255

lightestHan :: RGB
lightestHan = V3 207 191 255

lightestViolet :: RGB
lightestViolet = V3 223 191 255

lightestPurple :: RGB
lightestPurple = V3 239 191 255

lightestFuchsia :: RGB
lightestFuchsia = V3 255 191 255

lightestMagenta :: RGB
lightestMagenta = V3 255 191 239

lightestPink :: RGB
lightestPink = V3 255 191 223

lightestCrimson :: RGB
lightestCrimson = V3 255 191 207

desaturatedRed :: RGB
desaturatedRed = V3 127 63 63

desaturatedFlame :: RGB
desaturatedFlame = V3 127 79 63

desaturatedOrange :: RGB
desaturatedOrange = V3 127 95 63

desaturatedAmber :: RGB
desaturatedAmber = V3 127 111 63

desaturatedYellow :: RGB
desaturatedYellow = V3 127 127 63

desaturatedLime :: RGB
desaturatedLime = V3 111 127 63

desaturatedChartreuse :: RGB
desaturatedChartreuse = V3 95 127 63

desaturatedGreen :: RGB
desaturatedGreen = V3 63 127 63

desaturatedSea :: RGB
desaturatedSea = V3 63 127 95

desaturatedTurquoise :: RGB
desaturatedTurquoise = V3 63 127 111

desaturatedCyan :: RGB
desaturatedCyan = V3 63 127 127

desaturatedSky :: RGB
desaturatedSky = V3 63 111 127

desaturatedAzure :: RGB
desaturatedAzure = V3 63 95 127

desaturatedBlue :: RGB
desaturatedBlue = V3 63 63 127

desaturatedHan :: RGB
desaturatedHan = V3 79 63 127

desaturatedViolet :: RGB
desaturatedViolet = V3 95 63 127

desaturatedPurple :: RGB
desaturatedPurple = V3 111 63 127

desaturatedFuchsia :: RGB
desaturatedFuchsia = V3 127 63 127

desaturatedMagenta :: RGB
desaturatedMagenta = V3 127 63 111

desaturatedPink :: RGB
desaturatedPink = V3 127 63 95

desaturatedCrimson :: RGB
desaturatedCrimson = V3 127 63 79

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