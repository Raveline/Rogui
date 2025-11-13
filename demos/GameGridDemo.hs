{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Use newtype instead of data" -}

module Main where

import Control.Monad (when, (>=>))
import Data.Array
import Data.Array.IArray (genArray)
import qualified Data.Map as M
import Data.Maybe
import Linear
import Rogui.Application
import Rogui.Components
import Rogui.Components.Game
import Rogui.Components.Game.GridTile (mouseEventToWorldPos)
import Rogui.Graphics
import Rogui.Types
import qualified SDL

data Consoles = Root | GameArea | StatusBar
  deriving (Eq, Ord, Show)

data Brushes = Drawings | BigCharset | SmallCharset
  deriving (Eq, Ord, Show)

data Names = GameGrid
  deriving (Eq, Ord)

data CustomEvent = Move (V2 Cell)

data State = State
  { mousePosition :: V2 Cell,
    worldPosition :: Maybe (V2 Cell),
    playerPos :: V2 Cell
  }

fullMapSize :: V2 Cell
fullMapSize = V2 100 100

main :: IO ()
main = do
  let config =
        RoguiConfig
          { brushTilesize = TileSize 16 16,
            appName = "RoGUI example",
            consoleCellSize = V2 50 38,
            targetFPS = 60,
            rootConsoleReference = Root,
            defaultBrushReference = Drawings,
            defaultBrushPath = "punyworld-dungeon-tileset.png",
            drawingFunction = renderApp,
            stepMs = 100,
            eventFunction = baseEventHandler <||> eventHandler
          }
      ts16x16 = TileSize 16 16
      ts10x16 = TileSize 10 16
      -- The signature for this function is often complicated, so we
      -- recommend writing it as a subfunction in main.
      guiMaker =
        addBrush BigCharset "terminal_16x16.png" ts16x16
          >=> addBrush SmallCharset "terminal_10x16.png" ts10x16
          >=> addConsoleWithSpec StatusBar ts10x16 (TilesSize 100 1) TopLeft
          >=> addConsoleWithSpec GameArea ts16x16 (SizeWindowPct 100 98) (Below StatusBar)
  bootAndPrintError
    config
    guiMaker
    $ State
      { mousePosition = V2 0 0,
        worldPosition = Nothing,
        playerPos = V2 1 1
      }

data TileType
  = Floor
  | Wall

tileToGlyphId :: TileType -> Int
tileToGlyphId = \case
  Floor -> 5
  Wall -> 78

tileToGlyphInfo :: TileType -> GlyphInfo
tileToGlyphInfo t =
  GlyphInfo
    { colours = Colours Nothing Nothing,
      glyphId = tileToGlyphId t,
      transformations = []
    }

arbitraryMap :: Array (V2 Cell) TileType
arbitraryMap =
  let generator (V2 x y) =
        if (x `mod` 3 == 0) && (y `mod` 3 == 0) then Wall else Floor
   in genArray (V2 0 0, V2 99 99) generator

gameKeysHandler :: (Monad m) => EventHandler m State CustomEvent Names
gameKeysHandler =
  let keyMap =
        M.fromList
          [ ((SDL.KeycodeUp, []), \_ _ -> fireAppEvent . Move $ V2 0 (-1)),
            ((SDL.KeycodeDown, []), \_ _ -> fireAppEvent . Move $ V2 0 1),
            ((SDL.KeycodeLeft, []), \_ _ -> fireAppEvent . Move $ V2 (-1) 0),
            ((SDL.KeycodeRight, []), \_ _ -> fireAppEvent . Move $ V2 1 0)
          ]
   in keyPressHandler keyMap

eventHandler :: (Monad m) => EventHandler m State CustomEvent Names
eventHandler =
  gameKeysHandler <||> gameEventHandler <||> gridMouseHandler

gameEventHandler :: (Monad m) => EventHandler m State CustomEvent Names
gameEventHandler State {..} = \case
  (AppEvent (Move dir)) -> do
    let newPos@(V2 x y) = playerPos + dir
    when (x >= 0 && y >= 0 && x < 100 && y < 100) $
      modifyState $
        \s -> s {playerPos = newPos}
  _ -> unhandled

gridMouseHandler :: (Monad m) => EventHandler m State CustomEvent Names
gridMouseHandler State {..} = \case
  (MouseEvent (MouseClick MouseClickDetails {..})) -> do
    worldPos <- mouseEventToWorldPos GameGrid (TileSize 16 16) fullMapSize playerPos absoluteMousePosition
    redraw $ modifyState (\s -> s {worldPosition = worldPos})
  _ -> unhandled

renderApp :: M.Map Brushes Brush -> State -> ToDraw Consoles Brushes Names
renderApp brushes State {playerPos, worldPosition} =
  let baseColours = Colours (Just white) (Just black)
      charColours = Colours (Just white) Nothing
      bigCharset = brushes M.! BigCharset
      statusBar =
        hBox . catMaybes $
          [ (\wp -> Just $ label ("Clicked on " <> show wp) TLeft baseColours) =<< worldPosition
          ]
      gameArea =
        withRecordedExtent GameGrid $
          multiLayeredGrid
            fullMapSize
            playerPos
            [ gridTile (arbitraryMap !) tileToGlyphInfo,
              trySwitchBrush bigCharset . entitiesLayer ([playerPos] :: [V2 Cell]) (const $ GlyphInfo 1 charColours []) id
            ]
   in [ (Just StatusBar, Just SmallCharset, statusBar),
        (Just GameArea, Just Drawings, gameArea)
      ]
