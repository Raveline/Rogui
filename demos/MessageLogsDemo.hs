{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Linear (V2 (..))
import Rogui.Application.Event
import Rogui.Application.System
import Rogui.Components.Core
import Rogui.Components.MessageLog (LogMessage, messageLog)
import Rogui.Components.Viewport (ViewportState (..), handleViewportEvent, viewport)
import Rogui.Graphics
import Rogui.Types

data Consoles = Root
  deriving (Eq, Ord, Show)

data Brushes = Charset
  deriving (Eq, Ord, Show)

newtype State = State
  { logViewport :: ViewportState
  }

data Names = LogView
  deriving (Eq, Ord)

main :: IO ()
main = do
  let config =
        RoguiConfig
          { brushTilesize = TileSize 10 16,
            appName = "RoGUI example",
            consoleCellSize = V2 50 38,
            targetFPS = 60,
            rootConsoleReference = Root,
            defaultBrushReference = Charset,
            defaultBrushPath = "terminal_10x16.png",
            drawingFunction = renderApp,
            stepMs = 100,
            eventFunction = baseEventHandler <||> logEventHandler
          }
  bootAndPrintError config pure
    . State
    $ ViewportState (V2 0 0) (V2 0 (Cell $ length fakeLogs))

fakeLogs :: [LogMessage]
fakeLogs =
  let basicLogs =
        [ [(Colours (Just white) (Just black), "This is a log message")],
          [(Colours (Just white) (Just black), "This is another log message")],
          [(Colours (Just white) (Just black), "And another one")]
        ]
   in mconcat $ replicate 200 basicLogs

logEventHandler :: (Monad m) => EventHandler m State () Names
logEventHandler (State logViewport') e =
  handleViewportEvent LogView e logViewport' $ \newViewport s' -> s' {logViewport = newViewport}

renderApp :: ConsoleDrawers Consoles Brushes Names State
renderApp _ (State logViewport') =
  let logging = filled black $ bordered (Colours (Just white) (Just black)) $ vBox [viewport LogView logViewport' $ messageLog fakeLogs]
   in [(Nothing, Nothing, logging)]