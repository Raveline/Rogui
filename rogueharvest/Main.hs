{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A silly little application to demonstrate more advanced patterns
-- using Rogui.
--
-- Rogueharvest is a very basic harvest-like game, where you have
-- to tend a farm. It showcases:
--
-- * Inventory management;
-- * Two shop menus;
-- * A modal;
-- * Complicated state and event management;
-- * (TODO: complete this list once this is done)
module Main (main) where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Random
import Linear (V2 (..))
import RogueHarvest.Game
import RogueHarvest.Types
import Rogui.Application.Event (baseEventHandler, (<||>))
import Rogui.Application.System
import Rogui.Components
import Rogui.Types

main :: IO ()
main = do
  -- First, we setup configuration
  let ts10x16 = TileSize 10 16
      ts16x16 = TileSize 16 16
      config =
        RoguiConfig
          { brushTilesize = ts16x16,
            appName = "Rogue Harvest",
            consoleCellSize = V2 50 37,
            targetFPS = 60,
            rootConsoleReference = Root,
            defaultBrushReference = Charset,
            allowResize = True,
            defaultBrushPath = "rogueharvest/assets/terminal_16x16.png",
            -- `renderGame` is the entry point to read about rendering
            drawingFunction = renderGame,
            stepMs = 100,
            -- We use the baseEventHandler to handle the common quitting
            -- events, chained with our main event handler.
            -- `handleGameEvents` is the entry point to read about events
            eventFunction = baseEventHandler <||> handleGameEvents,
            consoleSpecs =
              [ (GridConsole, ts16x16, SizeWindowPct 100 89, TopLeft),
                (LogConsole, ts10x16, SizeWindowPct 100 8, Below GridConsole),
                (StatusBar, ts10x16, SizeWindowPct 100 3, Below LogConsole),
                (ModalShop, ts10x16, SizeWindowPct 90 90, Center),
                (ModalLayer, ts10x16, FullWindow, TopLeft)
              ]
          }
      guiMaker = addBrush SmallCharset "rogueharvest/assets/terminal_10x16.png" ts10x16

  -- Then we boot. Since we're using the actual `boot`, unlike the
  -- demos, we need to satisfy some constraints: MonadError (through ExcepT)
  -- and MonadLogger (using `withLogging` provided by Rogui).
  result <-
    runExceptT
      . withLogging LogStdout
      $ boot
        config
        guiMaker
        baseState

  -- We don't try to recover from errors - to be honest, Rogui doesn't
  -- offer much - for now - when it comes to recovery.
  case result of
    Right _ -> pure ()
    Left e -> putStrLn ("Unexpected error: " <> show e)

-- | Since LoggingT doesn't know anything about MonadRandom,
-- we are sadly going to need an orphan instance. In real life,
-- we would define this in an "Orphan" module, but this is just
-- a demo, so we'll put this here.
-- Your own monadic stack will most likely need this type of
-- instance. You can avoid this by creating a newtype to contain
-- all your stack (and writing instances for this newtype, which
-- will be just a bunch of lifts).
-- Or you can wait till we come up with an "Effectful" version
-- of rogui.
instance (MonadRandom m) => MonadRandom (LoggingT m) where
  getRandomR = lift . getRandomR
  getRandom = lift getRandom
  getRandomRs = lift . getRandomRs
  getRandoms = lift getRandoms