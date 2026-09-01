# Browser checks for the WASM backend

`playwright-core` scripts that actually load the compiled demos in a
headless browser and drive them, rather than just reading the code. Not a
test framework — each script is a plain Node script, run directly, that
exits non-zero on failure. They exist because most of the real bugs found
while building this backend were only visible by actually running it in a
browser; see the comments in `interaction-list.mjs` for the specific ones
each check guards against.

## Running everything

From the repo root:

```bash
make test-browser
```

That builds and stages both WASM demos, serves each on a local port,
runs both scripts against a real browser, and stops the servers again.
It needs the `wasm32-wasi` toolchain (like the other `*-wasm-*` targets)
and a Chromium-based browser — see Requirements below.

## Running a single script by hand

```bash
cd rogui-wasm-backend/test-browser && npm install     # once
```

Stage and serve the demo(s) you want (each `serve-*` blocks, so use
separate terminals):

```bash
make build-wasm-demo      && make serve-wasm-demo       # http://localhost:8000
make build-wasm-list-demo && make serve-wasm-list-demo  # http://localhost:8001
```

```bash
node smoke-hello.mjs       http://localhost:8000/index.html
node interaction-list.mjs  http://localhost:8001/index.html
```

Both scripts default to `http://127.0.0.1:8000` / `:8001` if you omit the
URL.

## Requirements

- **Node 18 or newer.** `package.json` pins `playwright-core` to the 1.54
  line, the last that still runs on Node 18.
- **Google Chrome or Chromium installed.** `playwright-core` ships no
  browser of its own; `helpers.mjs` launches the system one via
  Playwright's `chrome`/`chromium`/`msedge` channels. If none is on the
  default path, point at one explicitly:

  ```bash
  ROGUI_TEST_CHROME=/usr/bin/chromium make test-browser
  ```

## What's here

- `helpers.mjs` — shared bits: launching the browser and collecting
  console/page errors, a non-black-pixel sanity check, canvas-relative
  mouse coordinates, `waitFor` (poll for an expected state instead of a
  fixed delay — how long a redraw actually takes depends on real
  scheduling, and this sandbox's shared load made fixed delays flaky in
  both directions), and `highlightedRows` (samples the list demo's known
  row geometry to read back which item the app is currently drawing as
  selected).
- `smoke-hello.mjs` — loads the static demo, checks the console is clean
  and something was actually drawn.
- `interaction-list.mjs` — keyboard navigation, mouse clicks, and window
  resize against the interactive list demo. This is the one that actually
  found bugs; see below.

## Bugs this caught (fixed)

1. **`present()`'s canvas blit used the default `"source-over"`
   compositing instead of `"copy"`.** `clearFrame()` clears the *offscreen*
   canvas to fully transparent, and `source-over` alpha-blends the source
   onto the destination — a transparent source pixel leaves the
   destination (the visible canvas, never cleared directly) untouched. Any
   area a frame left transparent (the blank line between a list item's
   title and description, anywhere a previous frame's content wasn't
   repainted with something opaque) kept showing stale opaque pixels from
   whatever frame last painted there — e.g. a superseded selection
   highlight, forever. Fixed in `jsbits/rogui-runtime.js`'s `present`.
2. **`pollWASMEvents` didn't deduplicate raw browser events the way
   `Rogui.Backend.Events.getSDLEvents` deduplicates raw SDL ones.**
   `appTick` drains and processes an entire poll's worth of events in one
   go, so structurally-identical repeated events landing in the same batch
   (held-key auto-repeat, mainly) could get processed more than once per
   physical keypress. With `wrapAround = True` on this list, that could
   walk selection past the end and wrap back near the start. Fixed in
   `Rogui.Backend.WASM.Events` (dedup on a raw, pre-conversion
   `RawEvent`, mirroring `SDL.EventPayload`).

Two test-methodology bugs were found and fixed along the way too (both
explained in `interaction-list.mjs`'s comments where they bit): a
focus-click landing inside the list's own clickable extent (so it was
*also* a real list click, racing the keypress right after it), and
`page.mouse.click` needing page coordinates, not canvas-relative ones.

## Fixed: overlapping ticks / lost or reverted selection from `threadDelay`

`interaction-list.mjs`'s keyboard-navigation loop used to fail
intermittently (roughly 1 in 5-8 runs) even with the two fixes above and
generous polling: a `KeyDown` would get correctly decoded and correctly
compute a new selection, but a *later* tick's `runOneTick` call would
overwrite `stateRef` with a stale snapshot afterward, discarding the
update. Debug-print timestamps showed ticks overlapping outright — tick
N+1 starting before tick N's own call had returned — racing on the shared
`stateRef` `IORef` with a last-writer-wins overwrite.

Root cause: `appTick` (core, shared by every backend) unconditionally
calls `threadDelay` once per tick for frame pacing. On the wasm backend,
`wasmTick` is a `foreign export javascript` function, and every call to it
runs on a bound task created specifically to service that one call and
resolve its outer `Promise` back to JS. Because this module uses JSFFI at
all, GHC links in a browser-only override of `threadDelay` that's actually
an async JSFFI import backed by `setTimeout()` (see `Note [threadDelay on
wasm]` in `GHC.Internal.Wasm.Prim.Conc` — a JS-only WASI shim generally
can't make `poll_oneoff`, which the default `threadDelay` relies on,
actually sleep). Forcing that async thunk from inside the bound task
servicing an exported call is exactly the shape the wasm backend's own
docs flag as unsupported for C/JS-exported entry points ("A Haskell
thread cannot force an async JSFFI import thunk when it represents a
Haskell function exported via C FFI. Doing so would throw
`WouldBlockException`.") — in practice this didn't surface as a clean
exception, it made `wasmTick`'s outer `Promise` hang or resolve out of
order relative to the next `requestAnimationFrame`-triggered call,
producing the overlapping-tick race above. Confirmed with an isolated
repro outside this repo: a trivial `foreign export javascript` counter
function called repeatedly via `requestAnimationFrame` resolves cleanly
every time *until* a `threadDelay 0` is added inside it, at which point
calls start hanging nondeterministically.

Fixed by adding a `frameSleep` field to the `Backend` record
(`rogui/src/Rogui/Backend/Types.hs`) and having `appTick` call
`frameSleep backend sleepMs` instead of `threadDelay` directly. The SDL
and SDL+OpenGL backends set it to real `threadDelay`-based sleeping
(unchanged behavior); the wasm backend sets it to a no-op, since
`requestAnimationFrame` already paces frames to the browser's refresh
rate and `threadDelay` was serving no purpose there while sitting on this
hazard. `interaction-list.mjs` passed 15/15 runs after the fix (previously
~1-in-5-8 failures).
