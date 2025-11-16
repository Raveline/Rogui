{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Linear (V2 (..))
import Rogui.Application.Event
import Rogui.Application.System
import Rogui.Components.Core
import Rogui.Components.MessageLog (LogMessage, handleMessageLogEvent, messageLog)
import Rogui.Components.Viewport (ViewportState (..))
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
            eventFunction = baseEventHandler <||> logEventHandler,
            consoleSpecs = [],
            allowResize = True
          }
  bootAndPrintError config pure
    . State
    -- Content size is calculated dynamically, so we just initialize with zeros
    $ ViewportState (V2 0 0) (V2 0 0)

bnw, ynw, rnw :: Colours
bnw = Colours (Just white) (Just black)
rnw = Colours (Just red) (Just black)
ynw = Colours (Just yellow) (Just black)

bottleOfBeers :: Int -> [LogMessage]
bottleOfBeers n =
  let showBottle 0 = "no more"
      showBottle 1 = "1 bottle"
      showBottle n' = show n' <> " bottles "
   in [ [(ynw, showBottle n), (bnw, "of beer on the wall")],
        [(bnw, "Take one down, pass it around..."), (ynw, showBottle $ n - 1), (bnw, "of beer on the wall")]
      ]

manyBottlesOfBeers :: [LogMessage]
manyBottlesOfBeers = bottleOfBeers =<< [100, 99 .. 1]

fakeLogs :: [LogMessage]
fakeLogs =
  [ [(bnw, "Welcome, adventurer, to the dungeon of demo !")],
    [(bnw, "You are tasked with retrieving the amulet of Yendor.")],
    [(bnw, "And detect bugs.")],
    [(rnw, "Numerous bugs !")],
    [(bnw, "Speaking of bugs, it would be a good idea to try and")],
    [(rnw, "generate")],
    [(bnw, "some right now.")],
    [(bnw, "Like by changing colours"), (ynw, "mid sentence"), (bnw, "you know ?")],
    [(bnw, "Or with suuuuper long messages, that could work too. So one goblin, one troll and one elf enter a tavern...")],
    [(bnw, "Oh I know, what about doing both at the exact"), (rnw, "same"), (bnw, "time ?"), (ynw, "In polychromy, I mean !")],
    [(bnw, "Now this is fun and all but writing these messages take some time to think")],
    [(bnw, "So let's do something a bit more simple. 100 bottles of beer on the wall...")]
  ]
    <> manyBottlesOfBeers

logEventHandler :: (Monad m) => EventHandler m State () Names
logEventHandler s@(State logViewport') =
  handleMessageLogEvent LogView fakeLogs logViewport' (\newViewport s' -> s' {logViewport = newViewport}) s

renderApp :: ConsoleDrawers Consoles Brushes Names State
renderApp _ (State logViewport') =
  let logging = filled black $ bordered (Colours (Just white) (Just black)) $ vBox [messageLog LogView (Just logViewport') fakeLogs]
   in [(Nothing, Nothing, logging)]