{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Rogui.Components.Core
  ( vBox,
    hBox,
    bordered,
    renderComponents,
    -- exported for tests
    Layout (..),
    layout,
    padded,
  )
where

import Control.Monad (foldM_)
import Control.Monad.Writer
import Rogui.Components.Types (Component (..), DrawingContext (..), Size (..), TileSize (..), emptyComponent)
import Rogui.Graphics.DSL.Eval (evalInstructions)
import Rogui.Graphics.DSL.Instructions (Colours, Instructions, setColours, withBorder, withBrush, withConsole)
import Rogui.Graphics.Types (Console (..), fromBrush)
import Rogui.Types (Rogui (Rogui, defaultBrush, lastTicks, renderer, rootConsole))
import SDL (V2 (..), (^*))

data Layout = Vertical | Horizontal
  deriving (Eq)

-- | Draw components aligned vertically.
-- NB: an unchecked assumption is that everything is done with the same brush
-- size. Layout won't work if you change brush size.
vBox :: [Component n] -> Component n
vBox components = emptyComponent {draw = layout Vertical components}

-- | Draw components aligned horizontally
-- NB: an unchecked assumption is that everything is done with the same brush
-- size. Layout won't work if you change brush size.
hBox :: [Component n] -> Component n
hBox components = emptyComponent {draw = layout Horizontal components}

bordered :: Colours -> Component n -> Component n
bordered colours child =
  let draw' dc = do
        setColours colours
        withBorder
        draw (padded 1 child) dc
   in emptyComponent {draw = draw'}

padded :: Int -> Component n -> Component n
padded n child =
  let draw' dc@DrawingContext {..} = do
        let TileSize {..} = tileSize
            Console {..} = console
            newConsole =
              console
                { width = width - (pixelWidth * n),
                  height = height - (pixelHeight * n),
                  position = position + V2 (pixelWidth * n) (pixelHeight * n)
                }
        withConsole newConsole
        (draw child) dc {console = newConsole}
   in emptyComponent {draw = draw'}

-- | These are used to clarify units (and avoid silly bugs)
newtype Pixel = Pixel {getPixel :: Int}

newtype Tile = Tile Int
  deriving newtype (Num, Integral, Real, Ord, Eq, Enum, Show)

tilesToPixel :: Layout -> TileSize -> Tile -> Pixel
tilesToPixel l TileSize {..} (Tile t) = case l of
  Horizontal -> Pixel $ t * pixelWidth
  Vertical -> Pixel $ t * pixelHeight

pixelToTiles :: Layout -> TileSize -> Pixel -> Tile
pixelToTiles l TileSize {..} (Pixel p) = case l of
  Horizontal -> Tile $ p `div` pixelWidth
  Vertical -> Tile $ p `div` pixelHeight

layout :: Layout -> [Component n] -> DrawingContext -> Writer Instructions ()
layout direction children dc@DrawingContext {..} =
  let root@Console {width, height} = console
      toPartition = Pixel $ if direction == Vertical then height else width
      baseStep = if direction == Vertical then V2 0 1 else V2 1 0
      toScan = if direction == Vertical then vSize else hSize
      numberGreedy = length . filter ((==) Greedy . toScan) $ children
      countSize =
        Tile . \case
          Greedy -> 0
          Fixed n -> n
      sumFixed = sum . map (countSize . toScan) $ children
      greedySize = if numberGreedy > 0 then ((pixelToTiles direction tileSize toPartition) - sumFixed) `div` (Tile numberGreedy) else 0
      getSize child = case toScan child of
        Fixed n -> Tile n
        Greedy -> greedySize
      render currentConsole child = do
        let newValue = getSize child
            drawingConsole =
              currentConsole
                { height = if direction == Vertical then (getPixel $ tilesToPixel direction tileSize newValue) else height,
                  width = if direction == Horizontal then (getPixel $ tilesToPixel direction tileSize newValue) else width
                }
        withConsole drawingConsole
        draw child $ dc {console = drawingConsole}
        pure $ currentConsole {position = (position currentConsole) + (baseStep ^* (getPixel $ tilesToPixel direction tileSize newValue))}
   in foldM_ render root children

renderComponents :: (MonadIO m) => Rogui rc rb n s e -> Component n -> m ()
renderComponents Rogui {defaultBrush, rootConsole, lastTicks, renderer} Component {..} =
  let instructions = execWriter $ do
        withConsole rootConsole
        withBrush defaultBrush
        draw DrawingContext {tileSize = fromBrush defaultBrush, console = rootConsole, ticks = lastTicks}
   in evalInstructions renderer rootConsole defaultBrush instructions