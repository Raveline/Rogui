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
    ghc-options:
      -no-hs-main -optl-mexec-model=reactor
      -optl-Wl,--export=wasmInit -optl-Wl,--export=wasmTick
  else
    buildable: False
```

The `ghc-options`/`if arch(wasm32)` pieces are explained in step 5 below —
they're not optional, so include them from the start. If you name your
exports something other than `wasmInit`/`wasmTick`, adjust the `--export`
flags to match.

## 4. Write your app's `Main.hs`

Your native `Main.hs` almost certainly calls `bootAndPrintError` (or
`boot`), which blocks until the app quits — that model doesn't exist in a
browser, since the browser owns the main thread. Instead, drive
`Rogui.Application.System.appInit`/`appTick` through two `foreign export
javascript` functions that JavaScript calls: one to set up, one to run a
single frame. `Rogui.Backend.WASM.Run.mkWasmApp` builds both for you (it
owns the two-phase sequencing, the frame-loop state, re-entrancy
guarding, and error propagation); your `Main.hs` supplies the config and
the irreducible shim — one `NOINLINE` top-level binding and the two
`foreign export` declarations, which have to stay in the executable.
This template adapts directly from `rogui-wasm-backend/app/Main.hs`:

```haskell
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Except (ExceptT, runExceptT)
import Log (LogT)
import Rogui.Application
import Rogui.Backend.WASM (wasmBackend)
import Rogui.Backend.WASM.Run (WasmApp (..), mkWasmApp)
import System.IO.Unsafe (unsafePerformIO)

-- Your own types, and your own initial application state:
-- data Consoles = ...
-- data Brushes = ...
-- data YourState = ...
-- data YourEvent = ...

type AppM = ExceptT (RoguiError () Consoles Brushes) (LogT IO)

{-# NOINLINE wasmApp #-}
wasmApp :: WasmApp
wasmApp =
  unsafePerformIO $
    mkWasmApp wasmBackend (withoutLogging . runExceptT) config initialState
  where
    initialState = () -- your own initial state value

foreign export javascript "wasmInit" hsWasmInit :: IO ()

hsWasmInit :: IO ()
hsWasmInit = wasmAppInit wasmApp

foreign export javascript "wasmTick" hsWasmTick :: IO Bool

hsWasmTick :: IO Bool
hsWasmTick = wasmAppTick wasmApp

main :: IO ()
main = pure ()

config :: RoguiConfig Consoles Brushes () YourState YourEvent AppM
config = RoguiConfig { {- same fields you already have for the SDL build -} }
```

**Why two exports and not just `main`**: `appInit` loads your default
brush (an image), which — in the browser — is fetched and decoded
asynchronously. That `await` can only happen inside a Haskell thread that
JavaScript itself called and can suspend on, i.e. a `foreign export
javascript` function. A reactor module has no `main`/`_start` to do it in
anyway (only `_initialize`, which just sets the RTS up). So `wasmInit`
(called and awaited by your HTML, right after `_initialize`) does the real
setup. `mkWasmApp` packages that split; see its Haddock for the details.
`main` stays defined only because the module is called `Main`; it's never
run.

If initialisation or a tick fails, `mkWasmApp` writes the reason to the
browser console and throws, so `await wasmInit()` rejects (rather than the
loop starting against a half-built state) and a tick error surfaces in
`RoguiRuntime.startLoop`'s `.catch`. The `index.html` from step 7 shows
that on the page instead of leaving a blank canvas.

Everything else — `RoguiConfig`, your `drawingFunction`, your
`eventFunction`, your components — is identical to your native app. This
is the whole point of Rogui's `Backend` abstraction: only the bottom layer
changes.

## 5. Why a reactor module (`-optl-mexec-model=reactor`)

By default GHC links a wasm32-wasi *command* module: it exports `_start`,
which runs `hs_init` → `Main.main` → `hs_exit` and tears the RTS down as
soon as `main` returns — before JavaScript ever gets to call
`wasmInit`/`wasmTick`. Every subsequent tick would then fail with "RTS is
not initialised".

`-optl-mexec-model=reactor` links a *reactor* module instead: it exports
`_initialize` (which sets the RTS up without running `main` and never
calls `hs_exit`), and the instance stays alive for the
`requestAnimationFrame` driver to keep calling into. `-no-hs-main` drops
GHC's `main()`; the `-optl-Wl,--export=` flags keep your two entry points
in the linked module. This is the standard setup for GHC wasm modules
that use the JavaScript FFI — see the "JavaScript FFI" section of the GHC
User's Guide.

There is no app-specific code here: the three `ghc-options` in step 3 are
all you need.

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
WASI shim's imports plus the generated `ghc_wasm_jsffi` imports, calls
`wasi.initialize` (the reactor module's `_initialize`), awaits `wasmInit`,
then hands `wasmTick` to `RoguiRuntime.startLoop`.

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

- **Don't call async (`safe`) FFI outside an awaited export.** Only call
  it from a `foreign export javascript` function JS awaits (see step 4).
  This mostly matters if you add your own `foreign import javascript safe`
  calls (e.g. to load additional assets) — keep them behind an exported,
  awaited entry point.
- **Don't drop `-no-hs-main -optl-mexec-model=reactor`.** Without the
  reactor model you get a command module whose RTS shuts down the instant
  `_start` returns, and every subsequent `wasmTick` call fails with "RTS
  is not initialised".
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
