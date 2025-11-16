{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.List.NonEmpty as NE
import Linear (V2 (..))
import Rogui.Application
import Rogui.Components.Core
import Rogui.Components.Grid
import Rogui.Components.Label (label)
import Rogui.Components.ProgressBar
import Rogui.Graphics
import Rogui.Types

data Consoles = Root
  deriving (Show, Eq, Ord)

data Brushes = Charset
  deriving (Show, Eq, Ord)

data Names = Grid
  deriving (Show, Eq, Ord)

data DemoState = DemoState
  { gridState :: GridState,
    picked :: Maybe GridContent
  }

main :: IO ()
main = do
  let config =
        RoguiConfig
          { brushTilesize = TileSize 10 16,
            appName = "RoGUI grid demo",
            consoleCellSize = V2 50 38,
            targetFPS = 60,
            rootConsoleReference = Root,
            defaultBrushReference = Charset,
            defaultBrushPath = "terminal_10x16.png",
            drawingFunction = renderApp,
            stepMs = 100,
            eventFunction = baseEventHandler <||> eventHandler,
            consoleSpecs = [],
            allowResize = False
          }
  bootAndPrintError
    config
    pure
    $ DemoState {gridState = mkGridState, picked = Nothing}

data GridContent
  = Label String
  | ProgressBar Int

eventHandler :: (Monad m) => EventHandler m DemoState () Names
eventHandler s@DemoState {..} e =
  let alterGrid gs s' = s' {gridState = gs}
      pickComponent a = redraw $ modifyState $ \s' -> s' {picked = a}
   in case e of
        -- Click events have their own handlers, being a bit different by nature.
        -- We only have one clickable component in this demo, so we won't need
        -- to use the `foundClickExtents` utility.
        (MouseEvent (MouseClickReleased mcd)) ->
          handleClickOnGrid
            demoGrid
            mcd
            gridState
            alterGrid
            pickComponent
        otherEvent ->
          handleGridEvent
            demoGrid
            gridState
            alterGrid
            pickComponent
            s
            otherEvent

bnw :: Colours
bnw = Colours (Just white) (Just black)

renderGridComponent :: Bool -> Maybe GridContent -> Component Names
renderGridComponent selected =
  let colour = if selected then invert bnw else bnw
      barColour = if selected then Colours (Just red) (Just white) else bnw
   in \case
        (Just (Label s)) -> label s TLeft colour
        (Just (ProgressBar pct)) -> progressBar (ProgressBarDefinition 0 100 pct barColour barColour fullBlock lightShade)
        Nothing -> emptyComponent

demoGrid :: GridDefinition GridContent Names
demoGrid =
  GridDefinition
    { gridName = Grid,
      gridRows = 3,
      gridContent = [Label "Graphics DSL", ProgressBar 100, Label "Components DSL", ProgressBar 50, Label "Writing a game", ProgressBar 0],
      renderCell = \_ _ s gc -> renderGridComponent s gc,
      cellWidths = NE.fromList [15, 30],
      cellHeight = 2,
      spacing = 0,
      highlightColours = invert bnw
    }

renderApp :: ConsoleDrawers Consoles Brushes Names DemoState
renderApp _ DemoState {..} =
  let renderSelection = \case
        Label s -> "\"" <> s <> "\""
        ProgressBar _ -> "a progress bar"
      labelContent = case selectedGridItem demoGrid gridState of
        (Just a) -> "You have selected " <> renderSelection a
        Nothing -> "You have selected nothing"
      pickedContent = case picked of
        (Just a) -> label ("You picked " <> renderSelection a) TLeft bnw
        Nothing -> emptyComponent
      content =
        bordered bnw
          . vBox
          $ [ vSize (Fixed 2) $ label labelContent TLeft bnw,
              vSize (Fixed 2) pickedContent,
              grid demoGrid gridState
            ]
   in [(Nothing, Nothing, content)]