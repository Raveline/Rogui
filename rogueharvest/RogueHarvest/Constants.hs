module RogueHarvest.Constants where

import Linear (V2 (..))
import Rogui.Graphics.Colours
import Rogui.Graphics.Constants
import Rogui.Graphics.Types (Cell)

up :: V2 Cell
up = V2 0 (-1)

down :: V2 Cell
down = V2 0 1

left :: V2 Cell
left = V2 (-1) 0

right :: V2 Cell
right = V2 1 0

nw :: V2 Cell
nw = V2 (-1) (-1)

ne :: V2 Cell
ne = V2 1 (-1)

sw :: V2 Cell
sw = V2 (-1) 1

se :: V2 Cell
se = V2 1 1

bnw :: Colours
bnw = Colours (Just white) (Just black)

bny :: Colours
bny = Colours (Just yellow) (Just black)

rnb :: Colours
rnb = Colours (Just red) (Just black)