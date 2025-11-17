{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Linear (V2 (..))
import Rogui.Application.Event
import Rogui.Application.System (RoguiConfig (..), bootAndPrintError)
import Rogui.Components.Core (bordered, vBox)
import Rogui.Components.MultilineText
import Rogui.Components.Viewport
import Rogui.Graphics
import Rogui.Types (ConsoleDrawers)

data Consoles = Root
  deriving (Show, Eq, Ord)

data Brushes = Charset
  deriving (Show, Eq, Ord)

data Names = LongTextViewport | LongText
  deriving (Show, Eq, Ord)

newtype DemoState = DemoState ViewportState

main :: IO ()
main = do
  let config =
        RoguiConfig
          { brushTilesize = TileSize 10 16,
            appName = "RoGUI list demo",
            consoleCellSize = V2 50 30,
            targetFPS = 60,
            rootConsoleReference = Root,
            defaultBrushReference = Charset,
            defaultBrushPath = "terminal_10x16.png",
            drawingFunction = renderApp,
            stepMs = 100,
            eventFunction = baseEventHandler <||> handleEvents,
            consoleSpecs = [],
            allowResize = True
          }
  bootAndPrintError
    config
    pure
    (DemoState $ ViewportState (V2 0 0) (V2 0 0))

handleEvents :: (Monad m) => EventHandler m DemoState () Names
handleEvents s@(DemoState viewportState) =
  handleViewportEvent LongTextViewport LongText viewportState (\d _ -> DemoState d) s

longText :: [TextChunk]
longText =
  let bnw = Colours (Just white) (Just black)
   in [ (bnw, "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque auctor tincidunt justo, eu posuere dolor auctor eu. Integer commodo dui nulla, at mattis ex dignissim sit amet. Quisque sed convallis quam. Sed id porttitor magna. Aenean fringilla semper sem a aliquam. Quisque non turpis metus. Nam sit amet cursus elit, eu sollicitudin metus. Proin consectetur, ligula id viverra ultricies, turpis lacus hendrerit metus, quis semper nibh metus eu nibh. Maecenas bibendum volutpat nisi ut luctus. Etiam gravida nulla nec mauris maximus vehicula. Nunc ut ullamcorper arcu, porttitor egestas dolor. Etiam eget neque eros. Nunc congue mi eget nulla tincidunt, at eleifend metus pellentesque. Phasellus vitae metus arcu. Morbi sagittis, tellus ut ultricies sodales, ante erat tincidunt felis, vel cursus metus nunc at magna. Nullam auctor turpis eget enim tristique, eu mattis dolor maximus"),
        (bnw, "Integer et turpis nec neque molestie faucibus. In rhoncus risus sit amet augue porttitor, vitae viverra elit vulputate. Quisque malesuada ante eget ex consectetur finibus. In tempus elementum lorem, sit amet faucibus tellus ultrices et. Nullam sit amet lectus justo. Nulla venenatis feugiat tortor, in facilisis tellus. Mauris lacinia aliquam velit non auctor. Vestibulum posuere porttitor lacus vel scelerisque.")
      ]

renderApp :: ConsoleDrawers Consoles Brushes Names DemoState
renderApp _brushes (DemoState viewportState') =
  let content =
        bordered (Colours (Just white) (Just black))
          . vBox
          $ [ viewportClipped LongTextViewport viewportState' (multilineText LongText longText)
            ]
   in [(Nothing, Nothing, content)]