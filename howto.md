# Adding a WASM/browser target to your own Rogui app

This is a practical guide for someone building a game or tool with Rogui
who wants to also ship it as a browser build, using `rogui-wasm-backend`.
It assumes you already have a working native (SDL) Rogui app. For the
backend's own design and the toolchain gotchas behind the choices made
here, see `wasm.md` at the repo root — this doc just tells you how to plug
your own app into it. `rogui-wasm-backend/app/` (the backend's own smoke
test) is a complete, working example of everything below; when in doubt,
look at what it does.

## 1. Install the toolchain

You need GHC's `wasm32-wasi` cross-compiler, via
[`ghc-wasm-meta`](https://gitlab.haskell.org/ghc/ghc-wasm-meta). Follow its
bootstrap instructions; it installs into a directory (commonly
`~/.ghc-wasm`) with an `env` script that puts `wasm32-wasi-ghc` and
`wasm32-wasi-cabal` on your `PATH`:

```bash
source ~/.ghc-wasm/env
```

Run that in every shell you build from. You'll also need Node.js (used by
the toolchain's `post-link.mjs`, not by your app at runtime) and, for
serving the demo, anything that can serve static files over HTTP.

## 2. Project layout: a separate `cabal.project`

Native-only dependencies (`sdl2`, `sdl2-image`, `gl`, ...) cannot
cross-compile to `wasm32-wasi`, so your WASM target can't live in the same
`cabal.project` solve as your SDL target. Add a second project file, say
`cabal.project.wasm`, at your repo root, listing only the packages that
need to build for the browser:

```cabal
packages:
  rogui/rogui.cabal
  path/to/your-wasm-app/your-wasm-app.cabal

-- The wasm32-wasi cross GHC tends to be newer than the native GHC your
-- packages' `base` upper bounds were tuned for. Loosen only `base` here
-- rather than touching your packages' real version bounds.
allow-newer: base
```

(If you vendor `rogui`/`rogui-wasm-backend` via `source-repository-package`
or a local path instead of Hackage, list those paths here too.)

Build with:

```bash
wasm32-wasi-cabal build --project-file=cabal.project.wasm your-wasm-app
```

## 3. Depend on `rogui-wasm-backend`

In your WASM executable's `.cabal` stanza:

```cabal
executable your-wasm-app
  main-is: Main.hs
  hs-source-dirs: app
  c-sources: app/cbits/wasm_main.c
  build-depends:
    base,
    containers,
    linear,
    log-base,
    mtl,
    rogui,
    rogui-wasm-backend

  if arch(wasm32)
    build-depends: ghc-experimental
    ghc-options: -no-hs-main
  else
    buildable: False
```

The `c-sources`/`-no-hs-main`/`if arch(wasm32)` pieces are explained in
step 5 below — they're not optional, so include them from the start.

## 4. Write your app's `Main.hs`

Your native `Main.hs` almost certainly calls `bootAndPrintError` (or
`boot`), which blocks until the app quits — that model doesn't exist in a
browser, since the browser owns the main thread. Instead, drive
`Rogui.Application.System.appInit`/`appTick` directly through two
`foreign export javascript` functions that JavaScript calls: one to set
up, one to run a single frame. This template adapts directly from
`rogui-wasm-backend/app/Main.hs` — swap in your own `Consoles`/`Brushes`/
state/event types and `RoguiConfig`:

```haskell
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Log (LogT)
import Rogui.Application
import Rogui.Backend.WASM (wasmBackend)
import Rogui.Backend.WASM.FFI (CanvasContext, WASMTexture)
import Rogui.Types (Rogui)
import System.IO.Unsafe (unsafePerformIO)

-- Your own types, and your own initial application state:
-- data Consoles = ...
-- data Brushes = ...
-- data YourState = ...
-- data YourEvent = ...

type AppM = ExceptT (RoguiError () Consoles Brushes) (LogT IO)

type AppRogui = Rogui Consoles Brushes () YourState YourEvent CanvasContext WASMTexture AppM

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
        stateRef <- newIORef (rogui0, initialState) -- your own initial state value
        writeIORef tickAction (runOneTick stateRef)
  case result of
    Left err -> print err
    Right () -> pure ()

main :: IO ()
main = pure ()

runOneTick :: IORef (AppRogui, YourState) -> IO Bool
runOneTick stateRef = do
  (rogui, st) <- readIORef stateRef
  outcome <- withoutLogging . runExceptT $ appTick wasmBackend rogui st
  case outcome of
    Left err -> print err >> pure False
    Right TickHalt -> pure False
    Right (TickContinue newRogui newSt) -> do
      writeIORef stateRef (newRogui, newSt)
      pure True

config :: RoguiConfig Consoles Brushes () YourState YourEvent AppM
config = RoguiConfig { {- same fields you already have for the SDL build -} }
```

**Why two exports and not just `main`**: `appInit` loads your default
brush (an image), which — in the browser — is fetched and decoded
asynchronously. That `await` can only happen inside a Haskell thread that
JavaScript itself called and can suspend on, i.e. a `foreign export
javascript` function. `main`/`_start` is WASI's synchronous,
un-awaitable entry point; doing async work in its dynamic extent throws
`WouldBlockException` at runtime. So `main` does nothing, and `wasmInit`
(called and awaited by your HTML, right after `_start`) does the real
setup.

Everything else — `RoguiConfig`, your `drawingFunction`, your
`eventFunction`, your components — is identical to your native app. This
is the whole point of Rogui's `Backend` abstraction: only the bottom layer
changes.

## 5. The C shim (`app/cbits/wasm_main.c`)

GHC's normal auto-generated `main()` calls `hs_init()`, runs your
`Main.main`, then `hs_exit()` and `exit()` — tearing the RTS down again
right after `main` returns, before JavaScript ever gets a chance to call
`wasmInit`/`wasmTick`. Suppress it with `-no-hs-main` (already in the
cabal stanza above) and supply your own, which never calls `hs_exit`:

```c
// app/cbits/wasm_main.c
#include <Rts.h>

int main(int argc, char *argv[]) {
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  hs_init_ghc(&argc, &argv, conf);
  // Deliberately never call hs_exit(): the wasm instance keeps running,
  // driven by requestAnimationFrame calling the exported `wasmTick`, long
  // after this `_start` call returns to JS.
  return 0;
}
```

Copy this file verbatim; there's nothing app-specific in it.

## 6. Assets (tilesets)

`loadBrush`'s `Either ByteString FilePath` works the same way it does
natively, with browser-appropriate meanings for each side:

- **`Right "some.png"`** — treated as a URL relative to the page hosting
  your `.wasm` file, fetched with `fetch()`/`Image`. This is what all the
  native demos already use, so it's usually a drop-in: just make sure the
  PNG is served alongside your HTML.
- **`Left someByteString`** — embed the PNG bytes directly in your Haskell
  binary (e.g. via `file-embed`) if you'd rather not ship a separate asset
  file. Decoded through a `Blob` URL under the hood.

## 7. The browser harness (`index.html`)

Browsers don't ship a WASI runtime, so your page needs to bring a small one
along, then wire it up to the `ghc_wasm_jsffi` imports your Haskell module
needs (generated by the toolchain's `post-link.mjs`) plus
`rogui-runtime.js` (copy it from `rogui-wasm-backend/jsbits/`, it has no
app-specific content). Copy `rogui-wasm-backend/app/index.html` and its
`package.json` as your starting point — the only real dependency is
[`@bjorn3/browser_wasi_shim`](https://www.npmjs.com/package/@bjorn3/browser_wasi_shim):

```bash
cp rogui-wasm-backend/app/index.html rogui-wasm-backend/app/package.json your-wasm-app/
cd your-wasm-app && npm install
```

Then edit `index.html`'s one `import` line to point at your own compiled
`.wasm`/`.jsffi.js` file names (rename `rogui-wasm-demo.wasm` /
`rogui-wasm-demo.jsffi.js` to match your executable). Nothing else in that
file is Rogui-app-specific — it just: instantiates your module with the
WASI shim's imports plus the generated `ghc_wasm_jsffi` imports, runs
`_start`, awaits `wasmInit`, then hands `wasmTick` to
`RoguiRuntime.startLoop`.

## 8. Build and run

```bash
source ~/.ghc-wasm/env
wasm32-wasi-cabal build --project-file=cabal.project.wasm your-wasm-app
WASM_OUT=$(wasm32-wasi-cabal list-bin --project-file=cabal.project.wasm your-wasm-app)
cp "$WASM_OUT" your-wasm-app/your-wasm-app.wasm
node "$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" \
  -i your-wasm-app/your-wasm-app.wasm -o your-wasm-app/your-wasm-app.jsffi.js
cp rogui-wasm-backend/jsbits/rogui-runtime.js your-wasm-app/rogui-runtime.js

cd your-wasm-app && python3 -m http.server 8000
```

Open `http://localhost:8000`. It must be real HTTP, not `file://` —
`fetch()`-ing your tileset PNG (and, depending on your browser, loading
the WASI shim as an ES module) is blocked under `file://`.

`rogui-wasm-backend`'s own `make build-wasm-demo`/`make serve-wasm-demo`
targets are a working reference for automating exactly these steps if
you'd rather crib a Makefile than write one from scratch.

## 9. Gotchas worth knowing up front

These are explained in more depth in `wasm.md`'s "Implementation notes"
section, but in short, because they'll bite you if you improvise around
the templates above instead of following them:

- **Don't call async (`safe`) FFI from `main`.** Only from a `foreign
  export javascript` function JS awaits (see step 4). This mostly matters
  if you add your own `foreign import javascript safe` calls (e.g. to load
  additional assets) — keep them behind an exported, awaited entry point,
  not in `main`'s call graph.
- **Don't drop `-no-hs-main`/the C shim.** Without it, your module's RTS
  shuts down the instant `main` returns and every subsequent `wasmTick`
  call fails with "RTS is not initialised".
- **`JSString` doesn't work on at least some `wasm32-wasi-ghc` snapshots.**
  If you write your own `foreign import javascript` declarations (for
  custom browser APIs Rogui doesn't cover), avoid `GHC.Wasm.Prim.JSString`
  in the signature; pass strings as UTF-8 bytes (`Ptr () -> Int`, see
  `Rogui.Backend.WASM.FFI.withUtf8`) or read them back via `JSVal` +
  `charCodeAt`, mirroring that module. Check whether this is still true on
  whatever toolchain snapshot you're using before assuming you need the
  workaround.
- **Haskell `Bool` marshals to JS as `0`/`1`, not `true`/`false`.** Fine
  for truthiness checks; coerce with `!!` if you pass one into a
  strictly-typed Web API.

## 10. What you get, and what you don't

Everything that goes through Rogui's `draw`/`onEvent`/component system
works unmodified: layouts, focus handling, animations, your game logic.
What differs from the SDL backend:

- `takeScreenshot` triggers a browser download instead of writing a file
  (there's no filesystem to write to).
- Window resize (`allowResize = True`) resizes the canvas to fill its
  parent element rather than an OS window; style the page around that.
- There's no `boot`/`bootAndPrintError`/blocking `appLoop` — you own the
  two-export structure from step 4 instead.

Everything else — consoles, brushes, colours, the DSL, components,
event handling — is exactly what you already know from the native
backend.
