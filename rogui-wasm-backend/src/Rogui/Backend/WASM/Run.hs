{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Driver plumbing for running a Rogui application on top of `wasmBackend`.
--
-- The browser owns the main thread, so a WASM app can't call
-- `bootAndPrintError`/`appLoop`: there is no blocking game loop. Instead the
-- host page calls two `foreign export javascript` functions - one to set
-- things up, one to run a single frame - from a `requestAnimationFrame`
-- driver (see @jsbits/rogui-runtime.js@, @RoguiRuntime.startLoop@).
--
-- A WASM app reduces to:
--
-- @
-- \{-\# NOINLINE wasmApp \#-\}
-- wasmApp :: WasmApp
-- wasmApp =
--   unsafePerformIO $
--     mkWasmApp wasmBackend (withoutLogging . runExceptT) config initialState
--
-- foreign export javascript "wasmInit" hsWasmInit :: IO ()
-- hsWasmInit = wasmAppInit wasmApp
--
-- foreign export javascript "wasmTick" hsWasmTick :: IO Bool
-- hsWasmTick = wasmAppTick wasmApp
--
-- main :: IO ()
-- main = pure ()
-- @
module Rogui.Backend.WASM.Run
  ( WasmApp (..),
    mkWasmApp,
    StopReason (..),
    WasmAppException (..),
  )
where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, tryTakeMVar)
import Control.Exception (Exception, finally, throwIO)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Log (MonadLog)
import Rogui.Application
  ( RoguiConfig,
    RoguiError,
    TickResult (..),
    appInit,
    appTick,
  )
import Rogui.Backend.Types (Backend)
import Rogui.Backend.WASM.FFI (CanvasContext, WASMTexture, consoleError)
import Rogui.Types (Rogui)

-- | The two entry points the host page drives, each wrapping one call into
-- the Rogui application loop. Store the whole value in a single top-level
-- @{-\# NOINLINE \#-}@ binding (via `unsafePerformIO`) and point both
-- `foreign export javascript` functions at its fields; that keeps the
-- process-wide mutable state this needs down to exactly one cell.
data WasmApp = WasmApp
  { -- | Runs `appInit`: initialises the backend, loads the default brush
    -- (an async fetch/decode -- see the module header), builds the initial
    -- `Rogui`, and arms `wasmAppTick`. Call and @await@ this once from JS
    -- before starting the frame loop. Throws `WasmAppException` if
    -- initialisation fails, so JS's @await@ rejects rather than the loop
    -- starting against a half-built state.
    wasmAppInit :: IO (),
    -- | Runs one `appTick`. Returns 'True' to keep looping, 'False' to stop
    -- (clean halt, or called before `wasmAppInit` finished). Throws
    -- `WasmAppException` on a tick error, surfacing in @startLoop@'s
    -- @.catch@. Re-entrant calls (a previous tick's Promise still pending)
    -- are skipped and return 'True'.
    wasmAppTick :: IO Bool
  }

-- | Why the frame loop stopped. Carried by `WasmAppException` and written to
-- the browser console.
data StopReason
  = -- | The application requested a halt (`TickHalt`); a normal exit.
    HaltRequested
  | -- | `appInit` returned an error.
    InitFailed String
  | -- | An `appTick` returned an error.
    TickFailed String
  deriving (Eq, Show)

-- | Thrown out of `wasmAppInit`/`wasmAppTick` on failure so the condition
-- reaches JS as a rejected Promise.
newtype WasmAppException = WasmAppException StopReason
  deriving (Show)

instance Exception WasmAppException

-- | Internal: what the driver's single mutable cell holds between frames.
data LoopState rc rb n s e m
  = -- | `wasmAppInit` has not completed. `wasmAppTick` no-ops (returns
    -- 'False'); the host page is expected to @await wasmInit()@ first.
    AwaitingInit
  | -- | Initialised; carries the state threaded from tick to tick.
    Ready !(Rogui rc rb n s e CanvasContext WASMTexture m) !s
  | -- | The loop has stopped for good. `wasmAppTick` no-ops (returns 'False').
    Stopped !StopReason

-- | Build the driver for a Rogui application. Allocates the driver's mutable
-- state and returns the two IO actions closing over it; run this once, at
-- the top level, through `unsafePerformIO` with a @{-\# NOINLINE \#-}@
-- pragma (see the module header for the full shim).
mkWasmApp ::
  forall rc rb n s e m err.
  ( Show rb,
    Show rc,
    Show err,
    Ord rb,
    Ord rc,
    Ord n,
    MonadIO m,
    MonadError (RoguiError err rc rb) m,
    MonadLog m
  ) =>
  Backend CanvasContext WASMTexture e ->
  -- | Discharge the application monad, e.g. @withoutLogging . runExceptT@.
  (forall a. m a -> IO (Either (RoguiError err rc rb) a)) ->
  RoguiConfig rc rb n s e m ->
  -- | Initial application state.
  s ->
  IO WasmApp
mkWasmApp backend runM config initialState = do
  stateRef <- newIORef (AwaitingInit :: LoopState rc rb n s e m)
  -- An MVar-as-mutex to ensure, if the promise mechanism of the FFI should ever
  -- fail us, that we won't try to run two ticks at the same time if the first call
  -- takes a bit too much time.
  gate <- newMVar ()
  let runInit :: IO ()
      runInit = do
        result <-
          runM $
            appInit backend config $ \rogui0 ->
              liftIO $ writeIORef stateRef (Ready rogui0 initialState)
        case result of
          Left err -> stopWith stateRef (InitFailed (show err))
          Right () ->
            readIORef stateRef >>= \case
              Ready {} -> pure ()
              _ ->
                stopWith
                  stateRef
                  (InitFailed "appInit returned without running its continuation")

      runTick :: IO Bool
      runTick =
        readIORef stateRef >>= \case
          AwaitingInit -> pure False
          Stopped _ -> pure False
          Ready rogui st -> do
            outcome <- runM $ appTick backend rogui st
            case outcome of
              Left err -> stopWith stateRef (TickFailed (show err))
              Right TickHalt -> do
                writeIORef stateRef (Stopped HaltRequested)
                pure False
              Right (TickContinue rogui' st') -> do
                writeIORef stateRef (Ready rogui' st')
                pure True
  pure WasmApp {wasmAppInit = runInit, wasmAppTick = withGate gate runTick}

-- | Run @body@ only if no other tick is in flight; otherwise skip this frame
-- but keep the loop going. Releases the gate even if @body@ throws.
withGate :: MVar () -> IO Bool -> IO Bool
withGate gate body =
  tryTakeMVar gate >>= \case
    Nothing -> pure True
    Just () -> body `finally` putMVar gate ()

-- | Record the reason, log it to the browser console, and throw so the
-- failure reaches JS rather than being silently absorbed.
stopWith :: IORef (LoopState rc rb n s e m) -> StopReason -> IO a
stopWith stateRef reason = do
  writeIORef stateRef (Stopped reason)
  consoleError ("Rogui WASM: " <> describe reason)
  throwIO (WasmAppException reason)

describe :: StopReason -> String
describe HaltRequested = "application halted"
describe (InitFailed msg) = "initialisation failed: " <> msg
describe (TickFailed msg) = "tick failed: " <> msg
