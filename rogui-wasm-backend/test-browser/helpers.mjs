// Small shared helpers for the browser checks in this directory. Nothing
// here is a test framework -- each script is a plain Node script that
// exits non-zero on failure, run directly with `node`. See README.md.

import { chromium } from "playwright-core";

// We depend on `playwright-core`, not `playwright`: it has no
// browser-download postinstall (so `npm install` needs no network and
// isn't subject to that step's CVEs), and it's the last line that still
// runs on the Node 18 this repo targets. The tradeoff is that it never
// bundles a browser, so we drive one already installed on the machine:
// $ROGUI_TEST_CHROME if set, otherwise Playwright's "chrome"/"chromium"
// channels (system Google Chrome / Chromium), tried in turn.
async function launchBrowser() {
  const explicit = process.env.ROGUI_TEST_CHROME;
  if (explicit) return chromium.launch({ executablePath: explicit });

  const errors = [];
  for (const channel of ["chrome", "chromium", "msedge"]) {
    try {
      return await chromium.launch({ channel });
    } catch (e) {
      errors.push(`  ${channel}: ${e.message.split("\n")[0]}`);
    }
  }
  throw new Error(
    "could not launch a browser. Install Google Chrome or Chromium, or set " +
      "ROGUI_TEST_CHROME to a Chromium-based browser binary. Tried:\n" +
      errors.join("\n")
  );
}

// Launches a browser, collects console messages and page errors, and hands
// back a `{ browser, page, logs, unexpectedLogs }` where `unexpectedLogs`
// is `logs` filtered to drop the noisy-but-harmless WASI debug lines
// (`wasi: 0 0`, the willReadFrequently perf hint) that show up on every
// run regardless of anything this is meant to catch.
export async function launch() {
  const browser = await launchBrowser();
  const page = await browser.newPage({ viewport: { width: 900, height: 700 } });
  const logs = [];
  page.on("console", (msg) => {
    const url = msg.location()?.url;
    logs.push(`[console:${msg.type()}] ${msg.text()}${url ? ` (${url})` : ""}`);
  });
  page.on("pageerror", (err) => logs.push(`[pageerror] ${err.stack || err.message}`));
  const unexpectedLogs = () =>
    logs.filter(
      (l) =>
        // Noisy-but-harmless lines unrelated to anything these checks
        // guard: the WASI shim's own debug output, a canvas perf hint,
        // and the browser's automatic /favicon.ico probe (the demo pages
        // don't ship one, so the static server 404s it).
        !l.includes("wasi:") &&
        !l.includes("willReadFrequently") &&
        !l.includes("/favicon.ico")
    );
  return { browser, page, logs, unexpectedLogs };
}

// Fails (throws) if anything unexpected showed up in the console. Call
// this at the point in a test where you expect things to be quiet.
export function assertCleanConsole(unexpectedLogs, context) {
  const bad = unexpectedLogs();
  if (bad.length > 0) {
    throw new Error(`${context}: unexpected console output:\n${bad.join("\n")}`);
  }
}

// Counts non-black pixels on the page's canvas -- a cheap sanity check
// that *something* got drawn, without asserting exact content.
export async function nonBlackPixelCount(page) {
  return page.evaluate(() => {
    const canvas = document.querySelector("canvas");
    const ctx = canvas.getContext("2d");
    const data = ctx.getImageData(0, 0, canvas.width, canvas.height).data;
    let count = 0;
    for (let i = 0; i < data.length; i += 4) {
      if (data[i] || data[i + 1] || data[i + 2]) count++;
    }
    return count;
  });
}

// Polls `check()` (a zero-arg async function) until it returns something
// deep-equal to `expected` (compared via JSON.stringify) or `timeoutMs`
// elapses, then returns the last value seen. Prefer this over a fixed
// `waitForTimeout` before an assertion: how long a given redraw takes to
// land depends on real wall-clock scheduling (this sandbox runs many
// browser/node processes for these checks, and contention alone can push
// a redraw well past a fixed guess), and a fixed delay is either flaky
// (too short) or slow-and-still-eventually-flaky (too long, doesn't fix
// the underlying race). Polling waits exactly as long as needed and no
// longer.
export async function waitFor(check, expected, { timeoutMs = 5000, intervalMs = 50 } = {}) {
  const wantJSON = JSON.stringify(expected);
  const deadline = Date.now() + timeoutMs;
  let last;
  while (Date.now() < deadline) {
    last = await check();
    if (JSON.stringify(last) === wantJSON) return last;
    await new Promise((resolve) => setTimeout(resolve, intervalMs));
  }
  return last;
}

// Converts canvas-relative (x, y) into page coordinates suitable for
// page.mouse.*: the canvas isn't at the page origin (there's an
// instructions paragraph above it), so page.mouse.click(x, y) with raw
// canvas coordinates lands in the wrong place.
export async function canvasPoint(page, x, y) {
  const rect = await page.evaluate(() => {
    const r = document.querySelector("canvas").getBoundingClientRect();
    return { left: r.left, top: r.top };
  });
  return [rect.left + x, rect.top + y];
}

// Specific to rogui-wasm-list-demo's layout (10x16 tile brush, itemHeight
// = 3 tiles = 48px rows, first row at y=16 -- one tile of top
// border+padding). Samples the *blank middle line* of each visible item
// row (title / blank / description, so the blank line is guaranteed to
// never contain glyph pixels -- only `SetConsoleBackground`'s fill, if
// any, shows there), and returns which row indices currently render with
// a white background, i.e. which items the app is drawing as selected.
//
// This is the regression check for a real bug found while building this
// backend: `present()`'s canvas blit used to leave stale selection
// highlights on screen forever once superseded, because the default
// "source-over" canvas compositing doesn't overwrite a destination pixel
// where the source is transparent. See the comment on `present` in
// jsbits/rogui-runtime.js for the full story.
export async function highlightedRows(page, { rowStartY = 16, rowHeight = 48, sampleX = 15 } = {}) {
  return page.evaluate(
    ({ rowStartY, rowHeight, sampleX }) => {
      const canvas = document.querySelector("canvas");
      const ctx = canvas.getContext("2d");
      // Only sample rows that fully fit before the bottom border (one
      // tile's worth of margin), so a shrunk canvas doesn't have this
      // land on border glyphs (also white-on-black) and report a
      // phantom highlighted row -- rowCount is about list content, this
      // is about not sampling outside it.
      const rowCount = Math.max(0, Math.floor((canvas.height - rowStartY - 16) / rowHeight));
      const rows = [];
      for (let i = 0; i < rowCount; i++) {
        const y = rowStartY + i * rowHeight + rowHeight / 2; // middle of the blank line
        const [r, g, b] = ctx.getImageData(sampleX, y, 1, 1).data;
        if (r > 240 && g > 240 && b > 240) rows.push(i);
      }
      return rows;
    },
    { rowStartY, rowHeight, sampleX }
  );
}
