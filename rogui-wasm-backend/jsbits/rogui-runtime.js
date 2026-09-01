// Browser-side runtime for the Rogui WASM backend (rogui-wasm-backend).
//
// This is loaded as a plain <script> (not an ES module) BEFORE the compiled
// wasm module is instantiated, so it can install `globalThis.RoguiRuntime`
// ahead of time: the Haskell-side `foreign import javascript` snippets in
// Rogui/Backend/WASM/FFI.hs call straight into it (e.g.
// `globalThis.RoguiRuntime.drawGlyph(...)`), keeping anything more
// interesting than a one-line browser API call out of Haskell source and in
// ordinary, debuggable JavaScript.
(function (global) {
  "use strict";

  const BLEND_MODES = ["source-over", "lighter", "copy"];

  function unpackRGBA(packed) {
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

    monotonicTicks() {
      return Math.floor(performance.now());
    },

    // ---------------------------------------------------------------
    // Frame lifecycle
    // ---------------------------------------------------------------

    clearFrame(ctx) {
      if (ctx.__roguiClipped) {
        ctx.restore();
        ctx.__roguiClipped = false;
      }
      ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
    },

   setupOffscreen(canvas) {
      const off = document.createElement("canvas");
      off.width = canvas.width;
      off.height = canvas.height;
      const ctx = off.getContext("2d");
      // `alpha: false` protects against experienced tearing
      // in previous versions.
      canvas.__roguiVisibleCtx = canvas.getContext("2d", { alpha: false });
      ctx.__roguiPresentCanvas = canvas;
      canvas.__roguiOffscreenCanvas = off;
      return ctx;
    },

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

    loadImageFromBytes(buffer, ptr, len) {
      const bytes = new Uint8Array(buffer, ptr, len);
      const blob = new Blob([bytes]);
      const url = URL.createObjectURL(blob);
      return this.loadImageFromURL(url).finally(() => URL.revokeObjectURL(url));
    },

    imageToTexture(img, hasKey, keyPacked) {
      const w = img.naturalWidth || img.width;
      const h = img.naturalHeight || img.height;
      const canvas = document.createElement("canvas");
      canvas.width = w;
      canvas.height = h;
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
    // of (glyph, colour) combinations hundreds of times a frame - so this turns
    // most `drawGlyph` calls into a plain drawImage instead of the 4-operation
    // tint recipe below. This is not just an optimisation: redoing that
    // recipe from scratch for every glyph of every redraw was expensive
    // enough to visibly slow down input handling.
    _tintCache: new WeakMap(),

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
      // save/transform/restore dance entirely - a plain drawImage is
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
          // The canvas's own rendered CSS box, not its parent's.
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
    // `tick` had to return a Promise (`Promise.resolve(tick()).then(...)`),
    // not by choice: a `foreign export javascript` call into this
    // GHC wasm RTS always returns a *Promise*, even for a plain `IO Bool`
    // export with no async FFI calls in its body anywhere - there is no
    // synchronous return path.
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
