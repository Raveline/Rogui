-- | Main entry point for RoGUI applications. This module re-exports everything you need
-- to bootstrap and run a RoGUI application.
--
-- == (Very) Quick start
--
-- RoGUI expects you to provide a few types by yourself.
--
-- To boot, you'll need some references for consoles and for brushes:
--
-- @
-- data Consoles = Root | GameGrid | TopBar | ModalWindow
-- data Brushes = Charset | Tileset
-- @
--
-- You also need a type that represents your application state. In this example,
-- since we just want to compile, we'll keep things super simple and use `()`.
--
-- (You'll also most likely need other types for Event and Component management, see
-- "Rogui.Application.Event" and "Rogui.Components")
--
-- You'll want to begin with a `RoguiConfig` object to define your application.
--
-- @
-- let conf = RoguiConfig
--       { brushTilesize = TileSize 10 16,   -- The default brush tile size
--         appName = "My app name",          -- Name for your app, will be the window title
--         consoleCellSize = V2 50 38,       -- The full window size, given in cells with the default brush tile size.
--         targetFPS = 60,                   -- The FPS to run at. Main loop will sleep to avoid going above.
--         rootConsoleReference = Root,      -- One of the `Consoles` constructor we've defined above
--         defaultBrushReference = Charset,  -- One of the `Brushes` constructor we've defined above
--         defaultBrushPath = "tileset.png", -- Path to the resource
--         drawingFunction = const [],       -- We'll describe this one later
--         stepMs = 100,                     -- A custom step that will fire a "Step" event you can use.
--         eventFunction = baseEventHandler  -- How to react to event. `baseEventHandler` ensures you can quit the app.
--       }
-- @
--
-- The `drawingFunction` will receive the map of known brushes. And you're expected to return
-- a list of triplets: @[(Consoles, Brushes, Component n)]@. See the "Rogui.Components" module
-- for more details on the last type. Consoles will be drawn in the given order, so you
-- typically want to keep them in your expected z-order (e.g.: modals should come last).
-- In "real" applications, it is often useful to have a @catMaybes@ and a bunch of conditionals
-- to know if you want to display a given console or not. Here, we'll just return an empty
-- list so that it compiles.
--
-- Before booting, we need a function to load all the consoles and the brush we're going to use.
-- This function receives a `Rogui` object, and should return another. You will typically
-- chain calls to `addBrush` and `addConsoleWithSpec` inside, like this:
--
-- @
-- let guiMaker rogui =
--       addBrush Tileset "path_other_tileset.png" (TileSize 16 16) rogui
--       >>= addConsoleWithSpec TopBar (TileSize 10 16) (TilesSize 100 2) TopLeft
--       >>= addConsoleWithSpec GameGrid (TileSize 16 16) (SizeWindowPct 100 98) (Below TopBar)
--       >>= addConsoleWithSpec ModalWindow (TileSize 10 16) (TilesSize 80 80) Center
-- @
--
-- Pay attention to the fact that consoles have an expected tilesize. Here, we can use
-- our @Tileset@ brush on the @GameGrid@ console, but not on the other consoles (an
-- exception will be raised if we try). Conversely, we cannot use our @Charset@ brush (which
-- got automatically loaded from our `RoguiConfig` definition) on @GameGrid@.
--
-- But this doesn't matter in this example, because we're not rendering anything,
-- just showcasing how to get RoGUI to boot and display a window.
--
-- With the configuration datatype and the loading function defined, we're ready to actually boot.
-- RoGUI exposes two functions for this, we'll use the one that is designed for quick experiments.
--
-- @
-- bootAndPrintError config guiMaker ()
-- @
--
-- This will return a window with a black background. You can quit by pressing CTRL+C.
-- Reading on "Rogui.Components" will teach you how you can actually use the @drawingFunction@
-- to render stuff.
module Rogui.Application
  ( -- * Main entry point
    boot,
    bootAndPrintError,

    -- * Log wrapper
    LogOutput (..),
    withLogging,
    withoutLogging,

    -- * Other utilities
    brushLookup,

    -- * Re-exports from submodules
    module Rogui.Application.Types,
    module Rogui.Application.Event,
    module Rogui.Application.Error,
    module Rogui.Application.ConsoleSpecs,
  )
where

import Rogui.Application.ConsoleSpecs
import Rogui.Application.Error
import Rogui.Application.Event
import Rogui.Application.System
import Rogui.Application.Types
