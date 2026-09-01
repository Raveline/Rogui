{-# LANGUAGE ImportQualifiedPost #-}

-- | A Rogui backend that renders to an HTML5 `<canvas>` element, targeting
-- GHC's `wasm32-wasi` cross-compiler and its `foreign import javascript`
-- FFI.
--
-- Unlike the SDL backends, this one cannot drive a blocking game loop: the
-- browser owns the main thread. Use `Rogui.Application.System.appInit` and
-- `appTick` from a `requestAnimationFrame` callback (see jsbits/) instead of
-- `boot`/`appLoop`.
module Rogui.Backend.WASM
  ( wasmBackend,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import Linear (V2 (..))
import Rogui.Backend.Types (Backend (..))
import Rogui.Backend.WASM.Eval (evalWASMInstructions)
import Rogui.Backend.WASM.Events (pollWASMEvents)
import Rogui.Backend.WASM.FFI
import Rogui.Backend.WASM.Primitives (loadWASMBrush, takeWASMScreenshot)
import Rogui.Graphics.Types (Pixel (..))

wasmBackend :: Backend CanvasContext WASMTexture e
wasmBackend =
  Backend
    { loadBrush = loadWASMBrush,
      initBackend = initWASMBackend,
      clearFrame = clearWASMFrame,
      presentFrame = presentWASMFrame,
      evalInstructions = evalWASMInstructions,
      pollEvents = pollWASMEvents,
      getTicks = getWASMTicks,
      takeScreenshot = takeWASMScreenshot,
      -- `requestAnimationFrame` already paces frames to the browser's
      -- refresh rate: we should not try to threadDelay, which in any case
      -- doesn't compose nicely and causes weird behaviour in the browser.
      frameSleep = const (pure ())
    }

initWASMBackend :: (MonadIO m) => Text -> V2 Pixel -> Bool -> (CanvasContext -> m a) -> m ()
initWASMBackend appName (V2 (Pixel w) (Pixel h)) allowResize withRenderer = do
  canvas <- liftIO $ do
    found <- js_findCanvas
    isMissing <- js_isNullOrUndefined found
    if isMissing
      then do
        created <- js_createCanvas
        js_appendToBody created
        pure created
      else pure found
  liftIO $ do
    js_setCanvasSize canvas w h
    js_setTitle (toJSString (T.unpack appName))
    js_installListeners canvas allowResize
  -- Double buffering setup
  ctx <- liftIO $ js_setupOffscreen canvas
  _ <- withRenderer (CanvasContext ctx)
  pure ()

clearWASMFrame :: (MonadIO m) => CanvasContext -> m ()
clearWASMFrame (CanvasContext ctx) = liftIO $ js_clearRect ctx

presentWASMFrame :: (MonadIO m) => CanvasContext -> m ()
presentWASMFrame (CanvasContext ctx) = liftIO $ js_presentFrame ctx

getWASMTicks :: (MonadIO m) => m Word32
getWASMTicks = liftIO $ fromIntegral <$> js_monotonicTicks
