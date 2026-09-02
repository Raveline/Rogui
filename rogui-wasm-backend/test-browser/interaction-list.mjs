// Interaction test for rogui-wasm-list-demo (../app-list): keyboard
// navigation, mouse clicks, and window resize, run headlessly against a
// real browser. See README.md for how to run this.
//
// The keyboard-navigation assertions are a regression check for two bugs:
//
// 1. `pollWASMEvents` didn't deduplicate raw browser events the way
//    `Rogui.Backend.Events.getSDLEvents` deduplicates raw SDL ones, so a
//    single keypress could occasionally get processed more than once in
//    one `appTick` (which drains and processes its whole queued batch).
// 2. `present()`'s canvas blit used the default "source-over" compositing
//    instead of "copy", so a superseded selection highlight never
//    actually got erased from the visible canvas.
//
// Use `sampleX` in `highlightedRows` (see helpers.mjs) if you resize the
// list demo's layout enough to move where item rows land on screen.

import { launch, assertCleanConsole, highlightedRows, canvasPoint, waitFor } from "./helpers.mjs";

const url = process.argv[2] ?? "http://127.0.0.1:8001/index.html";

async function assertRowsEventually(page, expected, context) {
  const got = await waitFor(() => highlightedRows(page), expected);
  const a = JSON.stringify(got);
  const e = JSON.stringify(expected);
  if (a !== e) {
    throw new Error(`${context}: expected highlighted rows ${e}, got ${a}`);
  }
}

const { browser, page, unexpectedLogs } = await launch();
try {
  await page.goto(url, { waitUntil: "load" });
  await page.waitForTimeout(1500);
  assertCleanConsole(unexpectedLogs, "after load");
  await assertRowsEventually(page, [], "before any keypress");

  await page.mouse.click(...(await canvasPoint(page, 3, 3)));

  for (let i = 0; i < 5; i++) {
    await page.keyboard.press("ArrowDown");
    await assertRowsEventually(page, [i], `after ArrowDown #${i + 1}`);
  }
  assertCleanConsole(unexpectedLogs, "after keyboard navigation");

  for (let i = 0; i < 3; i++) {
    await page.keyboard.press("ArrowUp");
    await page.waitForTimeout(50);
  }
  await assertRowsEventually(page, [1], "after 3x ArrowUp from row 4");

  await page.mouse.click(...(await canvasPoint(page, 400, 16 + 5 * 48 + 24))); // middle of row 5's blank line
  await assertRowsEventually(page, [5], "after clicking row 5");
  assertCleanConsole(unexpectedLogs, "after mouse click");

  await page.setViewportSize({ width: 1200, height: 500 });
  await waitFor(
    () => page.evaluate(() => document.querySelector("canvas").width >= 1000),
    true
  );
  const canvasSize = await page.evaluate(() => {
    const c = document.querySelector("canvas");
    return { width: c.width, height: c.height };
  });
  if (canvasSize.width < 1000) {
    throw new Error(`expected the canvas to grow with the window, got ${JSON.stringify(canvasSize)}`);
  }
  await assertRowsEventually(page, [5], "after resize");
  assertCleanConsole(unexpectedLogs, "after resize");

  console.log(`PASS: ${url} keyboard nav, mouse click, and resize all behaved`);
} finally {
  await browser.close();
}
