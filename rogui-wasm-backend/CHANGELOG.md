# Revision history for rogui-wasm-backend

## 0.1.0.0 -- YYYY-MM-DD

* First version.
* Add `Rogui.Backend.WASM.Run` (`mkWasmApp`/`WasmApp`): builds the
  `wasmInit`/`wasmTick` entry points from a `RoguiConfig`, owning the
  two-phase init sequencing, frame-loop state, re-entrancy guard, and
  error propagation that every WASM app would otherwise hand-roll.
* Add `Rogui.Backend.WASM.FFI.consoleError` for surfacing failures to the
  browser console.
