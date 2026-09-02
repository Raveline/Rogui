{-# LANGUAGE ImportQualifiedPost #-}

-- | Raw JavaScript FFI declarations used by the rest of the backend. Nothing
-- here should carry Rogui-specific *rendering* logic; anything more than a
-- literal one-line mapping to a browser API is delegated to the
-- @RoguiRuntime@ namespace defined in @jsbits/rogui-runtime.js@ (loaded by
-- the host page before the wasm module is instantiated), and these imports
-- just call into it. That keeps the browser-side algorithms (glyph tinting,
-- the event queue, ...) in ordinary, debuggable JavaScript rather than
-- string-quoted inside Haskell source.
module Rogui.Backend.WASM.FFI
  ( CanvasContext (..),
    WASMTexture (..),

    -- * Canvas / window setup
    js_findCanvas,
    js_createCanvas,
    js_appendToBody,
    js_isNullOrUndefined,
    js_setCanvasSize,
    js_setupOffscreen,
    js_presentFrame,
    js_setTitle,
    js_clearRect,
    js_monotonicTicks,

    -- * Asset loading
    js_loadImageFromURL,
    js_loadImageFromBytes,
    js_imageToTexture,
    js_canvasWidth,
    js_canvasHeight,

    -- * Drawing primitives
    js_drawGlyph,
    js_fillRect,
    js_overlayRect,
    js_clipToRect,

    -- * Screenshot
    js_downloadCanvas,

    -- * Events
    js_installListeners,
    js_popEvent,
    js_eventKind,
    js_eventKey,
    js_eventRepeat,
    js_eventShift,
    js_eventCtrl,
    js_eventAlt,
    js_eventX,
    js_eventY,
    js_eventDX,
    js_eventDY,
    js_eventButton,
    js_eventW,
    js_eventH,

    -- * String marshalling
    toJSString,
    fromJSString,

    -- * Diagnostics
    consoleError,
  )
where

import Foreign.Ptr (Ptr)
import GHC.Wasm.Prim (JSString (..), JSVal, fromJSString, toJSString)

-- | Wraps the 2d rendering context obtained from the backing `<canvas>`.
newtype CanvasContext = CanvasContext JSVal

-- | A loaded tileset image, always normalised to an offscreen `<canvas>` (so
-- colour-key transparency can be baked in once at load time and every later
-- `drawImage` call has a uniform source type).
newtype WASMTexture = WASMTexture JSVal

foreign import javascript unsafe "document.getElementById('rogui-canvas')"
  js_findCanvas :: IO JSVal

foreign import javascript unsafe "document.createElement('canvas')"
  js_createCanvas :: IO JSVal

foreign import javascript unsafe "document.body.appendChild($1)"
  js_appendToBody :: JSVal -> IO ()

foreign import javascript unsafe "$1 === null || $1 === undefined"
  js_isNullOrUndefined :: JSVal -> IO Bool

foreign import javascript unsafe "$1.width = $2; $1.height = $3;"
  js_setCanvasSize :: JSVal -> Int -> Int -> IO ()

-- | Create an offscreen canvas the same size as the given (visible) one,
-- linked to it so `js_presentFrame` can find it later, and return its 2D
-- context. Everything this backend draws targets this offscreen context;
-- see the module note in `RoguiRuntime.present` for why.
foreign import javascript unsafe "globalThis.RoguiRuntime.setupOffscreen($1)"
  js_setupOffscreen :: JSVal -> IO JSVal

-- | Blit the offscreen context's canvas onto the visible one it was set up
-- against by `js_setupOffscreen`.
foreign import javascript unsafe "globalThis.RoguiRuntime.present($1)"
  js_presentFrame :: JSVal -> IO ()

foreign import javascript unsafe "document.title = $1"
  js_setTitle :: JSString -> IO ()

foreign import javascript unsafe "globalThis.RoguiRuntime.clearFrame($1)"
  js_clearRect :: JSVal -> IO ()

-- | Whole milliseconds from @performance.now()@, a monotonic clock (never
-- runs backwards). Backs `getTicks`; used by
-- `Rogui.Application.System.appTick` for frame timing, the step timer, and
-- @deltaTime@. See `RoguiRuntime.monotonicTicks`.
foreign import javascript unsafe "globalThis.RoguiRuntime.monotonicTicks()"
  js_monotonicTicks :: IO Int

-- | Load an image from a URL (relative to the host page). Resolves once
-- decoding has finished, so the returned `JSVal` always has real dimensions.
foreign import javascript safe "globalThis.RoguiRuntime.loadImageFromURL($1)"
  js_loadImageFromURL :: JSString -> IO JSVal

-- | Load an image from raw bytes living in wasm linear memory (e.g. an
-- embedded PNG). The bytes are copied into a `Blob` before any `await`, so
-- this is safe even if the wasm memory later grows.
foreign import javascript safe
  "globalThis.RoguiRuntime.loadImageFromBytes(__exports.memory.buffer,$1,$2)"
  js_loadImageFromBytes :: Ptr () -> Int -> IO JSVal

-- | Normalise a loaded image into an offscreen canvas, optionally applying a
-- colour-key transparency pass (`hasKey`, then packed `0xRRGGBB`).
foreign import javascript unsafe
  "globalThis.RoguiRuntime.imageToTexture($1,$2,$3)"
  js_imageToTexture :: JSVal -> Bool -> Int -> IO JSVal

foreign import javascript unsafe "$1.width" js_canvasWidth :: JSVal -> IO Int

foreign import javascript unsafe "$1.height" js_canvasHeight :: JSVal -> IO Int

-- | Draw one glyph: an optional background fill, the tile crop (optionally
-- tinted), positioned, flipped and rotated. Colours are packed as
-- `0xRRGGBBAA`. See `RoguiRuntime.drawGlyph` for the actual algorithm.
foreign import javascript unsafe
  "globalThis.RoguiRuntime.drawGlyph($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16,$17)"
  js_drawGlyph ::
    JSVal ->
    JSVal ->
    -- | source rect: sx, sy, sw, sh
    Int ->
    Int ->
    Int ->
    Int ->
    -- | dest rect: dx, dy, dw, dh
    Int ->
    Int ->
    Int ->
    Int ->
    -- | flipX, flipY, rotation in degrees
    Bool ->
    Bool ->
    Double ->
    -- | hasBack, backRGBA (packed)
    Bool ->
    Int ->
    -- | hasFront, frontRGBA (packed)
    Bool ->
    Int ->
    IO ()

-- | Plain, unblended rectangle fill (packed `0xRRGGBBAA`).
foreign import javascript unsafe "globalThis.RoguiRuntime.fillRect($1,$2,$3,$4,$5,$6)"
  js_fillRect :: JSVal -> Int -> Int -> Int -> Int -> Int -> IO ()

-- | Rectangle fill honouring a blend mode (0 = alpha blend, 1 = additive,
-- 2 = none/copy).
foreign import javascript unsafe
  "globalThis.RoguiRuntime.overlayRect($1,$2,$3,$4,$5,$6,$7)"
  js_overlayRect :: JSVal -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "globalThis.RoguiRuntime.clipToRect($1,$2,$3,$4,$5)"
  js_clipToRect :: JSVal -> Int -> Int -> Int -> Int -> IO ()

-- | Trigger a client-side download of the canvas backing the given 2d
-- context, as a PNG, suggesting the given file name.
foreign import javascript unsafe
  "globalThis.RoguiRuntime.downloadCanvas($1.canvas, $2)"
  js_downloadCanvas :: JSVal -> JSString -> IO ()

-- | Install keyboard/mouse/resize listeners on the given canvas (a no-op if
-- already installed). The second argument mirrors `allowResize`.
foreign import javascript unsafe
  "globalThis.RoguiRuntime.installListeners($1,$2)"
  js_installListeners :: JSVal -> Bool -> IO ()

-- | Pop the oldest queued event, or `null`/`undefined` if the queue is
-- empty (check with `js_isNullOrUndefined`).
foreign import javascript unsafe "globalThis.RoguiRuntime.popEvent()"
  js_popEvent :: IO JSVal

-- | 0 = keydown, 1 = keyup, 2 = mousemove, 3 = mousedown, 4 = mouseup,
-- 5 = resize. Kept in sync with `RoguiRuntime`'s event objects.
foreign import javascript unsafe "$1.kind|0" js_eventKind :: JSVal -> IO Int

foreign import javascript unsafe "$1.key || ''" js_eventKey :: JSVal -> IO JSString

foreign import javascript unsafe "!!$1.repeat" js_eventRepeat :: JSVal -> IO Bool

foreign import javascript unsafe "!!$1.shift" js_eventShift :: JSVal -> IO Bool

foreign import javascript unsafe "!!$1.ctrl" js_eventCtrl :: JSVal -> IO Bool

foreign import javascript unsafe "!!$1.alt" js_eventAlt :: JSVal -> IO Bool

foreign import javascript unsafe "$1.x|0" js_eventX :: JSVal -> IO Int

foreign import javascript unsafe "$1.y|0" js_eventY :: JSVal -> IO Int

foreign import javascript unsafe "$1.dx|0" js_eventDX :: JSVal -> IO Int

foreign import javascript unsafe "$1.dy|0" js_eventDY :: JSVal -> IO Int

foreign import javascript unsafe "$1.button|0" js_eventButton :: JSVal -> IO Int

foreign import javascript unsafe "$1.w|0" js_eventW :: JSVal -> IO Int

foreign import javascript unsafe "$1.h|0" js_eventH :: JSVal -> IO Int

-- | Write a line to the browser console's error channel. The host page also
-- wires WASI stdout/stderr to `console.*`, but that path only carries
-- output a `foreign export` actually returned through; a thrown Haskell
-- exception bypasses it, so failures worth seeing are logged here directly.
consoleError :: String -> IO ()
consoleError = js_consoleError . toJSString

foreign import javascript unsafe "console.error($1)"
  js_consoleError :: JSString -> IO ()
