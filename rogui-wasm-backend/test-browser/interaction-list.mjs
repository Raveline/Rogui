// Interaction test for rogui-wasm-list-demo (../app-list): keyboard
// navigation, mouse clicks, and window resize, run headlessly against a
// real browser. See README.md for how to run this.
//
// The keyboard-navigation assertions are a regression check for two real
// bugs found by running this demo in an actual browser (neither was
// obvious from reading the code):
//
// 1. `pollWASMEvents` didn't deduplicate raw browser events the way
//    `Rogui.Backend.Events.getSDLEvents` deduplicates raw SDL ones, so a
//    single keypress could occasionally get processed more than once in
//    one `appTick` (which drains and processes its whole queued batch).
//    With `wrapAround = True` on this list, that could walk selection
//    past the end and back near the start, looking like pressing "down"
//    randomly jumped back up.
// 2. `present()`'s canvas blit used the default "source-over" compositing
//    instead of "copy", so a superseded selection highlight never
//    actually got erased from the visible canvas -- see the comment on
//    `present` in jsbits/rogui-runtime.js.
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

  // page.locator(...).click() has an unrelated actionability-check quirk
  // against this canvas's flex-sized CSS box (it hangs polling
  // stability); page.mouse.click() bypasses that and is what real user
  // input looks like anyway. It needs page coordinates though, not
  // canvas-relative ones (the canvas isn't at the page origin -- there's
  // an instructions paragraph above it), hence canvasPoint().
  //
  // (3, 3) rather than somewhere in the middle: the list's own recorded
  // extent covers the whole padded content area starting at y=16, so a
  // focusing click anywhere inside it is *also* a genuine list click
  // (selecting whatever row it landed on) racing the keypress right
  // after it -- found by this test being flaky in a way no fixed delay
  // explained, until logging showed the "flaky" runs were a real,
  // consistent extra selection change from the click itself. (3, 3) sits
  // in the border/padding margin, above any row, so it only focuses.
  await page.mouse.click(...(await canvasPoint(page, 3, 3)));

  for (let i = 0; i < 5; i++) {
    await page.keyboard.press("ArrowDown");
    await assertRowsEventually(page, [i], `after ArrowDown #${i + 1}`);
  }
  assertCleanConsole(unexpectedLogs, "after keyboard navigation");

  // A real-world gap between presses, not zero: firing several
  // structurally-identical keydowns back-to-back with no delay can land
  // more than one in the same poll batch, which the dedup described above
  // will legitimately collapse -- exactly like `getSDLEvents` would for
  // the same reason (SDL.eventPayload carries no timestamp either). That
  // means "no delay" is an unrealistic edge case (well past normal human
  // input rate) to hold either backend to, not a regression to test for.
  for (let i = 0; i < 3; i++) {
    await page.keyboard.press("ArrowUp");
    await page.waitForTimeout(50);
  }
  await assertRowsEventually(page, [1], "after 3x ArrowUp from row 4");

  // Mouse click selects the clicked row (note: handleClickOnList doesn't
  // call `redraw`, so this can take up to one Step interval -- ~100ms in
  // this demo's RoguiConfig -- to actually show up).
  await page.mouse.click(...(await canvasPoint(page, 400, 16 + 5 * 48 + 24))); // middle of row 5's blank line
  await assertRowsEventually(page, [5], "after clicking row 5");
  assertCleanConsole(unexpectedLogs, "after mouse click");

  // allowResize: resizing the window should resize the canvas and keep
  // rendering (and the current selection) intact, not break anything.
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
