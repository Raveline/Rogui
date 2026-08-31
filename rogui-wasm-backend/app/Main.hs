{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Minimal smoke-test/demo for the WASM backend: proves the whole pipeline
-- (compile, link, `post-link.mjs`, instantiate in a browser, draw, take
-- input) end to end. See ../../wasm.md at the repo root.
--
-- Unlike the SDL demos (`rogui-demos`), this can't call `bootAndPrintError`:
-- the browser owns the main thread, so there is no blocking game loop. The
-- two-phase `wasmInit`/`wasmTick` split and all the state plumbing it needs
-- live in `Rogui.Backend.WASM.Run`; see that module's header for why the
-- split is mandatory. This file only supplies the app-specific config and
-- the irreducible shim: one `NOINLINE` top-level `WasmApp` and the two
-- `foreign export javascript` declarations pointing at its fields.
module Main (main) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Map qualified as M
import Linear (V2 (..))
import Log (LogT)
import Rogui.Application
import Rogui.Backend.WASM (wasmBackend)
import Rogui.Backend.WASM.Run (WasmApp (..), mkWasmApp)
import Rogui.Components.Core (Component (..), emptyComponent)
import Rogui.Graphics
import System.IO.Unsafe (unsafePerformIO)

data Consoles = Root
  deriving (Show, Eq, Ord)

data Brushes = Charset
  deriving (Show, Eq, Ord)

-- | This demo never throws a custom `ApplicationError` and never fires a
-- custom `AppEvent`, so both of `RoguiConfig`'s `err`/`event` type
-- parameters are fixed to `()` here purely to pin them down for type
-- inference.
type AppM = ExceptT (RoguiError () Consoles Brushes) (LogT IO)

-- | The single piece of process-wide state: built once (lazily, on first
-- `foreign export` call), then read by both exports.
{-# NOINLINE wasmApp #-}
wasmApp :: WasmApp
wasmApp =
  unsafePerformIO $
    mkWasmApp wasmBackend (withoutLogging . runExceptT) config ()

foreign export javascript "wasmInit" hsWasmInit :: IO ()

hsWasmInit :: IO ()
hsWasmInit = wasmAppInit wasmApp

foreign export javascript "wasmTick" hsWasmTick :: IO Bool

hsWasmTick :: IO Bool
hsWasmTick = wasmAppTick wasmApp

main :: IO ()
main = pure ()

config :: RoguiConfig Consoles Brushes () () () AppM
config =
  RoguiConfig
    { brushTilesize = TileSize 16 16,
      appName = "RoGUI WASM demo",
      consoleCellSize = V2 50 38,
      allowResize = False,
      targetFPS = 60,
      rootConsoleReference = Root,
      defaultBrushReference = Charset,
      defaultBrushPath = Right "terminal_16x16.png",
      defaultBrushTransparencyColour = Just black,
      drawingFunction = renderApp,
      stepMs = 100,
      eventFunction = baseEventHandler,
      consoleSpecs = [],
      brushesSpecs = [],
      maxEventDepth = 100
    }

renderApp :: M.Map Brushes Brush -> () -> [(Maybe Consoles, Maybe Brushes, Component ())]
renderApp _ () =
  let bars =
        [ gradient red orange 40,
          gradient blue purple 40,
          gradient yellow green 40
        ]
      drawBar row colours = do
        pencilAt (V2 0 row)
        mapM_ (\col -> setColours (Colours (Just col) (Just col)) >> str TLeft " ") colours
      draw' = do
        setColours (Colours (Just white) (Just black))
        pencilAt (V2 0 0)
        strLn TLeft "Hello from the Rogui HTML5/WASM backend!"
        mapM_ (uncurry drawBar) (zip [Cell 2 ..] bars)
   in [(Nothing, Nothing, emptyComponent {draw = draw'})]
