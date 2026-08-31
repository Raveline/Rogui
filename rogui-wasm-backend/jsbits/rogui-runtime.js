// Browser-side runtime for the Rogui WASM backend (rogui-wasm-backend).
//
// This is loaded as a plain <script> (not an ES module) BEFORE the compiled
// wasm module is instantiated, so it can install `globalThis.RoguiRuntime`
// ahead of time: the Haskell-side `foreign import javascript` snippets in
// Rogui/Backend/WASM/FFI.hs call straight into it (e.g.
// `globalThis.RoguiRuntime.drawGlyph(...)`), keeping anything more
// interesting than a one-line browser API call out of Haskell source and in
// ordinary, debuggable JavaScript.
//
// See ../../wasm.md at the repo root for the overall design.
(function (global) {
  "use strict";

  // Compositing operations for OverlayAt / FullConsoleOverlay, matching
  // Rogui.Graphics.DSL.Instructions.BlendMode. Kept in sync with the
  // integer codes written by Rogui.Backend.WASM.Primitives.overlayRect.
  const BLEND_MODES = ["source-over", "lighter", "copy"];

  function unpackRGBA(packed) {
    // packed as 0xRRGGBBAA, see Rogui.Backend.WASM.Primitives.packRGBA
    const r = (packed >>> 24) & 0xff;
    const g = (packed >>> 16) & 0xff;
    const b = (packed >>> 8) & 0xff;
    const a = packed & 0xff;
    return [r, g, b, a];
  }

  function rgba(packed) {
    const [r, g, b, a] = unpackRGBA(packed);
    return `rgba(${r},${g},${b},${a / 255})`;
  }

  const RoguiRuntime = {
    // ---------------------------------------------------------------
    // Timing
    // ---------------------------------------------------------------

    // The millisecond clock behind `getTicks` (Rogui.Backend.WASM.getWASMTicks),
    // used by Rogui.Application.System.appTick for frame-duration measurement,
    // the step timer, and `deltaTime`. `performance.now()` is a monotonic
    // clock (never runs backwards, immune to system clock adjustments), so
    // flooring it to whole milliseconds is a safe source for the Word32
    // subtractions appTick does on these values -- `frameEnd - frameStart`
    // and friends can be 0 on a fast frame but never underflow.
    //
    // Frame pacing itself is not handled here: `requestAnimationFrame`
    // (see startLoop) already paces to the display refresh rate, and
    // `wasmBackend` sets `frameSleep` to a no-op so appTick never sleeps.
    monotonicTicks() {
      return Math.floor(performance.now());
    },

    // ---------------------------------------------------------------
    // Frame lifecycle
    // ---------------------------------------------------------------

    // Clears the whole backing canvas. Also drops any clip region left
    // over from a previous OnConsole, since clipToRect() below never pairs
    // its ctx.save()/clip() with a matching restore() until the *next*
    // clip or the next clearFrame() -- there is exactly one active clip at
    // a time, mirroring SDL's rendererClipRect (a single rect, not a
    // stack).
    clearFrame(ctx) {
      if (ctx.__roguiClipped) {
        ctx.restore();
        ctx.__roguiClipped = false;
      }
      ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
    },

    // Everything this backend draws targets an offscreen canvas rather
    // than the visible one directly: a full clear-then-redraw of the
    // *visible* canvas can flicker if the browser composites mid-frame
    // (slow frame, GC pause, ...), since Canvas 2D is otherwise a single
    // buffer with no vsync-aligned swap of its own. Drawing off-screen and
    // blitting the result in one `drawImage` call (present(), below) makes
    // each visible frame change atomically instead.
    //
    // The offscreen canvas is created once and resized in place alongside
    // the visible one (see installListeners' resize handler) rather than
    // recreated, so the 2D context reference Haskell holds onto for the
    // whole app lifetime (in a `CanvasContext`) never goes stale.
    setupOffscreen(canvas) {
      const off = document.createElement("canvas");
      off.width = canvas.width;
      off.height = canvas.height;
      const ctx = off.getContext("2d");
      // `alpha: false` on the *visible* canvas only (never re-fetched with
      // options after this first call, so cache it): it tells the browser
      // this canvas is always fully opaque, so it never needs to
      // alpha-composite it against the page behind it. That composite step
      // is a plausible source of the tearing/flicker this is working
      // around -- one less place a partially-updated canvas can show
      // through to. The offscreen canvas being drawn to keeps real alpha,
      // since overlays/background fills still need to blend against it
      // while a frame is being built.
      canvas.__roguiVisibleCtx = canvas.getContext("2d", { alpha: false });
      ctx.__roguiPresentCanvas = canvas;
      canvas.__roguiOffscreenCanvas = off;
      return ctx;
    },

    // A single drawImage() call, deliberately not preceded by a clearRect
    // (offscreen and visible canvas are always the same size, see the
    // resize handler below, so a full-canvas drawImage covers every
    // destination pixel already) -- but critically, with
    // globalCompositeOperation set to "copy", not the default
    // "source-over".
    //
    // This was a real, found-by-actually-testing-it bug, not a
    // precaution: clearFrame() clears the *offscreen* canvas to fully
    // transparent (clearRect(), not a black fill), and with the default
    // "source-over" compositing, drawImage() alpha-*blends* the source
    // onto the destination -- a transparent source pixel leaves the
    // destination (the visible canvas, never cleared directly) completely
    // untouched, not overwritten. So any area a frame's redraw left
    // transparent on the offscreen canvas (the gap between a list item's
    // title and body text, anywhere a previous frame's now-stale content
    // wasn't repainted with something opaque this frame, ...) silently
    // kept showing whatever *previous* frame last painted opaque pixels
    // there -- e.g. a list item's selection highlight, one item out of
    // date, forever, because nothing ever drew there again to overwrite
    // it. "copy" compositing makes this a true replace: every destination
    // pixel (including alpha) becomes exactly the source pixel,
    // regardless of the source's own alpha, which is what a "present the
    // finished frame" blit actually needs to be.
    present(ctx) {
      const dest = ctx.__roguiPresentCanvas.__roguiVisibleCtx;
      dest.globalCompositeOperation = "copy";
      dest.drawImage(ctx.canvas, 0, 0);
      dest.globalCompositeOperation = "source-over";
    },

    // ---------------------------------------------------------------
    // Asset loading
    // ---------------------------------------------------------------

    loadImageFromURL(url) {
      return new Promise((resolve, reject) => {
        const img = new Image();
        img.onload = () => resolve(img);
        img.onerror = () => reject(new Error(`Rogui: failed to load image ${url}`));
        img.src = url;
      });
    },

    // `buffer`/`ptr`/`len` locate a UTF-8 encoded URL string in wasm linear
    // memory (see Rogui.Backend.WASM.FFI.withUtf8). Decoded synchronously,
    // before any `await`, for the same reason as loadImageFromBytes below.
    loadImageFromURLBytes(buffer, ptr, len) {
      const url = new TextDecoder("utf-8").decode(new Uint8Array(buffer, ptr, len));
      return this.loadImageFromURL(url);
    },

    // `buffer` is the wasm instance's memory ArrayBuffer, `ptr`/`len`
    // locate the raw (already-decoded, e.g. PNG file bytes) asset within
    // it. The Uint8Array view is only read synchronously by `new Blob([...])`
    // (which copies), so this remains safe even though wasm memory can grow
    // (and so `buffer` can be detached/replaced) across the `await` below.
    loadImageFromBytes(buffer, ptr, len) {
      const bytes = new Uint8Array(buffer, ptr, len);
      const blob = new Blob([bytes]);
      const url = URL.createObjectURL(blob);
      return this.loadImageFromURL(url).finally(() => URL.revokeObjectURL(url));
    },

    // Normalises a loaded <img> into an offscreen <canvas> (so every later
    // drawImage() source is uniform), optionally punching out a colour-key
    // as real alpha=0 transparency (Canvas 2D has no native colour-key, so
    // this is done once here rather than per-draw).
    imageToTexture(img, hasKey, keyPacked) {
      const w = img.naturalWidth || img.width;
      const h = img.naturalHeight || img.height;
      const canvas = document.createElement("canvas");
      canvas.width = w;
      canvas.height = h;
      // GHC's wasm FFI marshals Haskell `Bool` as 0/1, not a real JS
      // boolean; most uses below only need truthiness, but this Web API
      // strictly validates the type, hence the explicit coercion.
      const ctx = canvas.getContext("2d", { willReadFrequently: !!hasKey });
      ctx.drawImage(img, 0, 0);
      if (hasKey) {
        const kr = (keyPacked >>> 16) & 0xff;
        const kg = (keyPacked >>> 8) & 0xff;
        const kb = keyPacked & 0xff;
        const imageData = ctx.getImageData(0, 0, w, h);
        const data = imageData.data;
        for (let i = 0; i < data.length; i += 4) {
          if (data[i] === kr && data[i + 1] === kg && data[i + 2] === kb) {
            data[i + 3] = 0;
          }
        }
        ctx.putImageData(imageData, 0, 0);
      }
      return canvas;
    },

    // ---------------------------------------------------------------
    // Drawing primitives
    // ---------------------------------------------------------------

    // Cache of already-tinted glyph crops, per texture (a WeakMap so a
    // dropped/replaced texture's cache entries can be collected too), keyed
    // by source rect + colour. Text-heavy screens redraw the same handful
    // of (glyph, colour) combinations hundreds of times a frame -- e.g.
    // rogui-wasm-list-demo's item descriptions are prose, where the same
    // letters in the same colour repeat constantly -- so this turns most
    // `drawGlyph` calls into a plain drawImage instead of the 4-operation
    // tint recipe below. This is not just an optimisation: redoing that
    // recipe from scratch for every glyph of every redraw was expensive
    // enough to visibly slow down input handling (everything runs on one
    // thread; a slow redraw delays the next frame's event processing too).
    _tintCache: new WeakMap(),

    // The standard Canvas 2D sprite-tinting recipe: draw the crop, multiply
    // a solid colour over it, then clip back down to the crop's original
    // alpha shape with destination-in (otherwise the multiply fill would
    // paint the whole crop opaque, including previously-transparent
    // pixels). Cached per (texture, source rect, colour) -- see above.
    _tintCrop(tex, sx, sy, sw, sh, r, g, b) {
      let perTexture = this._tintCache.get(tex);
      if (!perTexture) {
        perTexture = new Map();
        this._tintCache.set(tex, perTexture);
      }
      const key = `${sx},${sy},${sw},${sh}:${r},${g},${b}`;
      const cached = perTexture.get(key);
      if (cached) return cached;

      const crop = document.createElement("canvas");
      crop.width = sw;
      crop.height = sh;
      const tctx = crop.getContext("2d");
      tctx.drawImage(tex, sx, sy, sw, sh, 0, 0, sw, sh);
      tctx.globalCompositeOperation = "multiply";
      tctx.fillStyle = `rgb(${r},${g},${b})`;
      tctx.fillRect(0, 0, sw, sh);
      tctx.globalCompositeOperation = "destination-in";
      tctx.drawImage(tex, sx, sy, sw, sh, 0, 0, sw, sh);

      perTexture.set(key, crop);
      return crop;
    },

    drawGlyph(
      ctx,
      tex,
      sx,
      sy,
      sw,
      sh,
      dx,
      dy,
      dw,
      dh,
      flipX,
      flipY,
      rotDeg,
      hasBack,
      backPacked,
      hasFront,
      frontPacked
    ) {
      if (hasBack) {
        ctx.fillStyle = rgba(backPacked);
        ctx.fillRect(dx, dy, dw, dh);
      }

      let src = tex;
      let ssx = sx;
      let ssy = sy;
      let alpha = 1;
      if (hasFront) {
        const [r, g, b, a] = unpackRGBA(frontPacked);
        src = this._tintCrop(tex, sx, sy, sw, sh, r, g, b);
        ssx = 0;
        ssy = 0;
        alpha = a / 255;
      }

      // The common case (plain text, no rotation or flip) skips the
      // save/transform/restore dance entirely -- a plain drawImage is
      // meaningfully cheaper, and this runs per glyph.
      if (!flipX && !flipY && !rotDeg && alpha === 1) {
        ctx.drawImage(src, ssx, ssy, sw, sh, dx, dy, dw, dh);
        return;
      }

      ctx.save();
      ctx.globalAlpha = alpha;
      ctx.translate(dx + dw / 2, dy + dh / 2);
      if (rotDeg) ctx.rotate((rotDeg * Math.PI) / 180);
      ctx.scale(flipX ? -1 : 1, flipY ? -1 : 1);
      ctx.drawImage(src, ssx, ssy, sw, sh, -dw / 2, -dh / 2, dw, dh);
      ctx.restore();
    },

    fillRect(ctx, x, y, w, h, packed) {
      ctx.fillStyle = rgba(packed);
      ctx.fillRect(x, y, w, h);
    },

    overlayRect(ctx, x, y, w, h, packed, blendModeCode) {
      const prevOp = ctx.globalCompositeOperation;
      ctx.globalCompositeOperation = BLEND_MODES[blendModeCode] || "source-over";
      ctx.fillStyle = rgba(packed);
      ctx.fillRect(x, y, w, h);
      ctx.globalCompositeOperation = prevOp;
    },

    // Replaces the active clip region, like SDL's rendererClipRect (not a
    // stack: a second call supersedes the first, and clearFrame() drops it
    // entirely at the start of the next frame).
    clipToRect(ctx, x, y, w, h) {
      if (ctx.__roguiClipped) {
        ctx.restore();
      }
      ctx.save();
      ctx.beginPath();
      ctx.rect(x, y, w, h);
      ctx.clip();
      ctx.__roguiClipped = true;
    },

    // ---------------------------------------------------------------
    // Screenshot
    // ---------------------------------------------------------------

    downloadCanvas(canvas, filename) {
      const a = document.createElement("a");
      a.href = canvas.toDataURL("image/png");
      a.download = filename;
      document.body.appendChild(a);
      a.click();
      a.remove();
    },

    // ---------------------------------------------------------------
    // Events
    // ---------------------------------------------------------------

    _events: [],

    popEvent() {
      return this._events.length ? this._events.shift() : null;
    },

    installListeners(canvas, allowResize) {
      if (canvas.__roguiListenersInstalled) return;
      canvas.__roguiListenersInstalled = true;

      // Let the canvas receive keyboard focus/events.
      if (!canvas.hasAttribute("tabindex")) canvas.tabIndex = 0;

      const push = (e) => this._events.push(e);
      const canvasPos = (e) => {
        const r = canvas.getBoundingClientRect();
        return [Math.round(e.clientX - r.left), Math.round(e.clientY - r.top)];
      };

      // Kind codes must match Rogui.Backend.WASM.FFI.js_eventKind's haddock:
      // 0 keydown, 1 keyup, 2 mousemove, 3 mousedown, 4 mouseup, 5 resize.
      canvas.addEventListener("keydown", (e) => {
        e.preventDefault();
        push({
          kind: 0,
          key: e.key,
          repeat: e.repeat,
          shift: e.shiftKey,
          ctrl: e.ctrlKey,
          alt: e.altKey,
        });
      });
      canvas.addEventListener("keyup", (e) => {
        e.preventDefault();
        push({ kind: 1, key: e.key, shift: e.shiftKey, ctrl: e.ctrlKey, alt: e.altKey });
      });
      canvas.addEventListener("mousemove", (e) => {
        const [x, y] = canvasPos(e);
        push({ kind: 2, x, y, dx: Math.round(e.movementX || 0), dy: Math.round(e.movementY || 0) });
      });
      canvas.addEventListener("mousedown", (e) => {
        const [x, y] = canvasPos(e);
        push({ kind: 3, x, y, button: e.button });
      });
      canvas.addEventListener("mouseup", (e) => {
        const [x, y] = canvasPos(e);
        push({ kind: 4, x, y, button: e.button });
      });

      if (allowResize) {
        global.addEventListener("resize", () => {
          // The canvas's own rendered CSS box, not its parent's: this way
          // it works regardless of how the host page's CSS actually sizes
          // the canvas (flex, grid, explicit width/height, ...), as long
          // as that CSS gives it a box independent of its width/height
          // attributes (the bitmap resolution being set here).
          const rect = canvas.getBoundingClientRect();
          const w = Math.round(rect.width) || global.innerWidth;
          const h = Math.round(rect.height) || global.innerHeight;
          canvas.width = w;
          canvas.height = h;
          // Keep the offscreen canvas (see setupOffscreen/present above)
          // the same size as the visible one it gets blitted onto.
          if (canvas.__roguiOffscreenCanvas) {
            canvas.__roguiOffscreenCanvas.width = w;
            canvas.__roguiOffscreenCanvas.height = h;
          }
          push({ kind: 5, w, h });
        });
      }
    },

    // ---------------------------------------------------------------
    // requestAnimationFrame driver
    //
    // `tick` is expected to be a zero-argument function (typically a
    // `foreign export javascript` from the Haskell app, wrapping
    // Rogui.Application.System.appTick) returning a boolean: true to keep
    // looping, false to stop.
    //
    // Treating its result as a Promise (`Promise.resolve(tick()).then(...)`)
    // is not a style choice: a `foreign export javascript` call into this
    // GHC wasm RTS always returns a *Promise*, even for a plain `IO Bool`
    // export with no async FFI calls in its body anywhere -- there is no
    // synchronous return path. Found by logging `typeof tick()` here: it
    // was a Promise on every single call. Skipping that (calling it like a
    // normal synchronous function, which is what it looks like from the
    // type signature) doesn't error -- `if (aPromise)` is just always
    // truthy -- so the loop schedules the *next* requestAnimationFrame
    // immediately, without waiting for the current call's Haskell
    // continuation to actually finish. Two (or more) calls into the RTS
    // then genuinely overlap: the next tick's `stateRef` read can happen
    // before the previous tick's `stateRef` write, and whichever finishes
    // last wins, silently discarding the other's state update (observed as
    // an occasional dropped keystroke: a KeyDown event that correctly
    // computed a new selection, immediately overwritten back to the old
    // state by a concurrently-running tick that started from a stale
    // snapshot).
    //
    // `.then()`, specifically, not `await` in an `async` step function:
    // tried that first, and it hung completely (never rendered a single
    // frame, in an actual browser, not just this test's headless one).
    // Spec-wise `await p` and `p.then(...)` are supposed to schedule the
    // continuation identically, but empirically only the explicit `.then()`
    // form let this particular RTS's promise resolve at all. Since the
    // difference isn't understood, don't "clean this up" back to
    // async/await without re-verifying an actual frame gets drawn, not
    // just that the code looks more idiomatic.
    // ---------------------------------------------------------------

    startLoop(tick) {
      const step = () => {
        Promise.resolve(tick())
          .then((cont) => {
            if (cont) global.requestAnimationFrame(step);
          })
          .catch((e) => {
            // An uncaught Haskell exception surfaces here as a rejected
            // Promise. Log clearly and stop, rather than leaving the page
            // silently frozen with no indication why.
            console.error("Rogui: tick() threw, stopping the loop:", e);
          });
      };
      global.requestAnimationFrame(step);
    },
  };

  global.RoguiRuntime = RoguiRuntime;
})(globalThis);
