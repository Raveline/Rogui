{-# LANGUAGE ImportQualifiedPost #-}

-- | Raw JavaScript FFI declarations used by the rest of the backend. Nothing
-- here should carry Rogui-specific *rendering* logic; anything more than a
-- literal one-line mapping to a browser API is delegated to the
-- @RoguiRuntime@ namespace defined in @jsbits/rogui-runtime.js@ (loaded by
-- the host page before the wasm module is instantiated), and these imports
-- just call into it. That keeps the browser-side algorithms (glyph tinting,
-- the event queue, ...) in ordinary, debuggable JavaScript rather than
-- string-quoted inside Haskell source.
--
-- One deliberate wrinkle: no declaration here uses `GHC.Wasm.Prim.JSString`.
-- The `wasm32-wasi-ghc` snapshot this backend was built against generates a
-- C stub for `JSString`-typed imports that calls @rts_mkJSString@ /
-- @rts_getJSString@, neither of which is declared by the RTS headers this
-- toolchain ships (`RtsAPI.h` has no trace of them, unlike the `JSVal`
-- counterparts, which do work). So every string crossing the FFI boundary
-- here goes as raw UTF-8 bytes in wasm linear memory instead (`Ptr () ->
-- Int`, decoded JS-side with `TextDecoder`), or, coming back, as a `JSVal`
-- read character-by-character with `js_jsStringLength`/
-- `js_jsStringCharCodeAt`. If a future toolchain fixes this, `JSString` can
-- replace this plumbing.
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

    -- * String marshalling helpers (see the module note above)
    withUtf8,
    jsValToString,

    -- * Diagnostics
    consoleError,
    js_consoleError,
  )
where

import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Foreign.Ptr (Ptr, castPtr)
import GHC.Wasm.Prim (JSVal)

-- | Wraps the 2d rendering context obtained from the backing `<canvas>`.
newtype CanvasContext = CanvasContext JSVal

-- | A loaded tileset image, always normalised to an offscreen `<canvas>` (so
-- colour-key transparency can be baked in once at load time and every later
-- `drawImage` call has a uniform source type).
newtype WASMTexture = WASMTexture JSVal

-- | Expose a `String` to a JS FFI snippet as `(pointer, byteLength)` into
-- wasm linear memory, UTF-8 encoded. The pointer is only valid for the
-- duration of the call: for a `safe` (async) import, only read it before
-- the first `await` (see `js_loadImageFromBytes` and
-- `RoguiRuntime.loadImageFromURLBytes` for the pattern of copying it out
-- synchronously).
withUtf8 :: String -> (Ptr () -> Int -> IO a) -> IO a
withUtf8 s f = unsafeUseAsCStringLen (TE.encodeUtf8 (T.pack s)) $ \(ptr, len) -> f (castPtr ptr) len

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

foreign import javascript unsafe
  "document.title = new TextDecoder('utf-8').decode(new Uint8Array(__exports.memory.buffer,$1,$2))"
  js_setTitle :: Ptr () -> Int -> IO ()

foreign import javascript unsafe "globalThis.RoguiRuntime.clearFrame($1)"
  js_clearRect :: JSVal -> IO ()

-- | Milliseconds, strictly increasing on every call. Deliberately *not*
-- just @performance.now()@ truncated to an integer -- see
-- `RoguiRuntime.monotonicTicks`'s comment for why that distinction matters
-- a lot more than it sounds like it should.
foreign import javascript unsafe "globalThis.RoguiRuntime.monotonicTicks()"
  js_monotonicTicks :: IO Int

-- | Load an image from a URL (relative to the host page), given as UTF-8
-- bytes in wasm memory. Resolves once decoding has finished, so the
-- returned `JSVal` always has real dimensions.
foreign import javascript safe
  "globalThis.RoguiRuntime.loadImageFromURLBytes(__exports.memory.buffer,$1,$2)"
  js_loadImageFromURL :: Ptr () -> Int -> IO JSVal

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
-- context, as a PNG, suggesting the given (UTF-8 encoded) file name.
foreign import javascript unsafe
  "globalThis.RoguiRuntime.downloadCanvas($1.canvas, new TextDecoder('utf-8').decode(new Uint8Array(__exports.memory.buffer,$2,$3)))"
  js_downloadCanvas :: JSVal -> Ptr () -> Int -> IO ()

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

foreign import javascript unsafe "$1.key || ''" js_eventKey :: JSVal -> IO JSVal

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

foreign import javascript unsafe "$1.length" js_jsStringLength :: JSVal -> IO Int

foreign import javascript unsafe "$1.charCodeAt($2)" js_jsStringCharCodeAt :: JSVal -> Int -> IO Int

-- | Read a JS string value (e.g. from `js_eventKey`) into a `String`, one
-- UTF-16 code unit at a time. `KeyboardEvent.key` values are always either
-- a single BMP character or an ASCII name like @"ArrowLeft"@, so the lack
-- of surrogate-pair handling here is not a real limitation for this use.
jsValToString :: JSVal -> IO String
jsValToString v = do
  n <- js_jsStringLength v
  traverse (fmap toEnum . js_jsStringCharCodeAt v) [0 .. n - 1]

-- | Write a line to the browser console's error channel. The host page also
-- wires WASI stdout/stderr to `console.*`, but that path only carries
-- output a `foreign export` actually returned through; a thrown Haskell
-- exception bypasses it, so failures worth seeing are logged here directly.
consoleError :: String -> IO ()
consoleError s = withUtf8 s js_consoleError

foreign import javascript unsafe
  "console.error(new TextDecoder('utf-8').decode(new Uint8Array(__exports.memory.buffer,$1,$2)))"
  js_consoleError :: Ptr () -> Int -> IO ()
