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
import Rogui.Components.Types (Component (..), Size (..), TileSize (..), emptyComponent)
import Rogui.Graphics.DSL.Eval (evalInstructions)
import Rogui.Graphics.DSL.Instructions (Colours, Instructions, setColours, withBorder, withBrush, withConsole)
import Rogui.Graphics.Types (Brush (..), Console (..))
import SDL (Renderer, V2 (..), (^*))

data Layout = Vertical | Horizontal
  deriving (Eq)

-- | Draw components aligned vertically.
-- NB: an unchecked assumption is that everything is done with the same brush
-- size. Layout won't work if you change brush size.
vBox :: [Component] -> Component
vBox components = emptyComponent {draw = \size within -> layout size within Vertical components}

-- | Draw components aligned horizontally
-- NB: an unchecked assumption is that everything is done with the same brush
-- size. Layout won't work if you change brush size.
hBox :: [Component] -> Component
hBox components = emptyComponent {draw = \size within -> layout size within Horizontal components}

bordered :: Colours -> Component -> Component
bordered colours child =
  let draw' ts console = do
        setColours colours
        withBorder
        draw (padded 1 child) ts console
   in emptyComponent {draw = \size within -> draw' size within}

padded :: Int -> Component -> Component
padded n child =
  let draw' ts@TileSize {..} console = do
        let newConsole =
              console
                { width = width console - (pixelWidth * n),
                  height = height console - (pixelHeight * n),
                  position = position console + V2 (pixelWidth * n) (pixelHeight * n)
                }
        withConsole newConsole
        (draw child) ts newConsole
   in emptyComponent {draw = \size within -> draw' size within}

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

layout :: TileSize -> Console -> Layout -> [Component] -> Writer Instructions ()
layout tileSize root@Console {width, height} direction children =
  let toPartition = Pixel $ if direction == Vertical then height else width
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
        draw child tileSize drawingConsole
        pure currentConsole {position = (position currentConsole) + (baseStep ^* (getPixel $ tilesToPixel direction tileSize newValue))}
   in foldM_ render root children

renderComponents :: (MonadIO m) => Renderer -> Console -> Brush -> Component -> m ()
renderComponents renderer console brush'@Brush {..} Component {..} =
  let instructions = execWriter $ do
        withConsole console
        withBrush brush'
        draw (TileSize tileWidth tileHeight) console
   in evalInstructions renderer console brush' instructions