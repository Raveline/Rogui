{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Minimal smoke-test/demo for the WASM backend: proves the whole pipeline
-- (compile, link, `post-link.mjs`, instantiate in a browser, draw, take
-- input) end to end. See ../../wasm.md at the repo root.
--
-- Unlike the SDL demos (`rogui-demos`), this can't call `bootAndPrintError`:
-- the browser owns the main thread, so there is no blocking game loop.
-- `appInit`/`appTick` are instead driven through two `foreign export
-- javascript` functions, `wasmInit` and `wasmTick`, called from
-- `index.html`/`RoguiRuntime.startLoop`.
--
-- Why two exports rather than doing it all in `main`: `appInit` loads the
-- default brush via an async (`safe`) FFI call (fetching and decoding the
-- tileset image), and that can only be awaited from a Haskell thread that
-- JS itself called and can suspend on -- i.e. a `foreign export
-- javascript`-exported function, which the generated glue lets JS `await`.
-- `main`/`_start` is WASI's synchronous, un-awaitable entry point; calling
-- an async FFI import from within it throws `WouldBlockException` (found by
-- actually running this in a browser -- see the "Implementation notes"
-- section of ../../wasm.md for the trace). So `main` does nothing, and
-- `wasmInit` (called and awaited by index.html right after `_start`) does
-- the real setup, stashing the resulting `Rogui` value in an `IORef` and
-- pointing `wasmTick` at a closure that runs one `appTick` per call.
module Main (main) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map qualified as M
import Linear (V2 (..))
import Log (LogT)
import Rogui.Application
import Rogui.Backend.WASM (wasmBackend)
import Rogui.Backend.WASM.FFI (CanvasContext, WASMTexture)
import Rogui.Components.Core (Component (..), emptyComponent)
import Rogui.Graphics
import Rogui.Types (Rogui)
import System.IO.Unsafe (unsafePerformIO)

data Consoles = Root
  deriving (Show, Eq, Ord)

data Brushes = Charset
  deriving (Show, Eq, Ord)

-- | This demo never throws a custom `ApplicationError` and never fires a
-- custom `AppEvent`, so both of `RoguiConfig`'s `err`/`event` type
-- parameters are fixed to `()` here purely to pin them down for type
-- inference (`runOneTick`/`wasmTick` would otherwise leave them ambiguous).
type AppM = ExceptT (RoguiError () Consoles Brushes) (LogT IO)

type AppRogui = Rogui Consoles Brushes () () () CanvasContext WASMTexture AppM

-- | The closure driving each frame, replaced once `appInit` has finished
-- setting things up. Starts out as a no-op that immediately halts, in case
-- `wasmTick` is somehow invoked before `main` has run.
{-# NOINLINE tickAction #-}
tickAction :: IORef (IO Bool)
tickAction = unsafePerformIO (newIORef (pure False))

foreign export javascript "wasmTick" wasmTickExport :: IO Bool

wasmTickExport :: IO Bool
wasmTickExport = readIORef tickAction >>= id

foreign export javascript "wasmInit" wasmInitExport :: IO ()

wasmInitExport :: IO ()
wasmInitExport = do
  result <-
    withoutLogging . runExceptT $
      appInit wasmBackend config $ \rogui0 -> liftIO $ do
        stateRef <- newIORef (rogui0, ())
        writeIORef tickAction (runOneTick stateRef)
  case result of
    Left err -> print err
    Right () -> pure ()

main :: IO ()
main = pure ()

runOneTick :: IORef (AppRogui, ()) -> IO Bool
runOneTick stateRef = do
  (rogui, st) <- readIORef stateRef
  outcome <- withoutLogging . runExceptT $ appTick wasmBackend rogui st
  case outcome of
    Left err -> print err >> pure False
    Right TickHalt -> pure False
    Right (TickContinue newRogui newSt) -> do
      writeIORef stateRef (newRogui, newSt)
      pure True

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
