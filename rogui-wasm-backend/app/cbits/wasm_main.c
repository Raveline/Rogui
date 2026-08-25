// Replaces GHC's usual auto-generated `main()`, which would call
// `hs_init()`, run `Main.main`, then `hs_exit()` and `exit()` -- tearing
// the RTS down again before JS ever gets a chance to call the
// `foreign export javascript` functions (`wasmInit`, `wasmTick`) this
// module exists to expose. See the note atop app/Main.hs for how this was
// found (running the "normal" build in an actual browser).
//
// Only paired with `ghc-options: -no-hs-main` on the executable stanza,
// which suppresses GHC's own `main()`.
#include <Rts.h>

int main(int argc, char *argv[]) {
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  hs_init_ghc(&argc, &argv, conf);
  // Deliberately never call hs_exit(): the wasm instance is meant to keep
  // running (driven by requestAnimationFrame calling the exported
  // `wasmTick`) long after this `_start` call returns to JS.
  return 0;
}
