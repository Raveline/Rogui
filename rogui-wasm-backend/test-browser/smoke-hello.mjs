// Smoke test for rogui-wasm-demo (the static "hello" demo in ../app):
// loads the page in headless Chromium, waits for a real frame, and checks
// nothing errored and something actually got drawn. See README.md for how
// to run this.

import { launch, assertCleanConsole, nonBlackPixelCount } from "./helpers.mjs";

const url = process.argv[2] ?? "http://127.0.0.1:8000/index.html";

const { browser, page, unexpectedLogs } = await launch();
try {
  await page.goto(url, { waitUntil: "load" });
  await page.waitForTimeout(1500);

  assertCleanConsole(unexpectedLogs, "after load");

  const nonBlack = await nonBlackPixelCount(page);
  if (nonBlack < 1000) {
    throw new Error(`expected a real frame to be drawn, only saw ${nonBlack} non-black pixels`);
  }

  console.log(`PASS: ${url} loaded cleanly and drew ${nonBlack} non-black pixels`);
} finally {
  await browser.close();
}
