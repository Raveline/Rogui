# Browser checks for the WASM backend

`playwright-core` scripts that actually load the compiled demos in a
headless browser and drive them. 

Not a test framework — each script is a plain Node script, run directly, that
exits non-zero on failure. 

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
