# Slop Runner - WASM Edition

A 2D platformer where **the whole game simulation runs in WebAssembly**, and you pick the language it was
written in from a dropdown on the title screen. Rendering (Three.js), audio, input and UI stay in JavaScript;
physics, collision, enemy AI, the boss fight, pickups, checkpoints and scoring are compiled from:

| Core | Toolchain | Notes |
| --- | --- | --- |
| JavaScript | (reference) | readable original, `core/js/core.js` |
| C | clang `wasm32`, freestanding | no libc |
| Rust | `wasm32-unknown-unknown` | `cdylib` |
| Zig | `wasm32-freestanding` | |
| Go | `GOOS=wasip1`, `//go:wasmexport` | reactor module + tiny WASI shim in the host |
| Nim | Nim -> C (`--os:standalone --gc:none`) -> clang | |
| OCaml | `wasm_of_ocaml` (WasmGC) | |
| Haskell | GHC WebAssembly backend | reactor module |
| F* | F* -> OCaml extraction -> `wasm_of_ocaml` | only the `Prim` float/memory primitives are trusted |

## The ABI (why nine ports stay in lockstep)

Every core exposes the same six functions - `init`, `load_level`, `advance(dt)`, `mem_get(i)`, `mem_set(i, v)`,
`mem_ptr()` - and keeps *all* state in **one flat array of `f64`**. `tools/abi.py` is the single source of truth
for that array's layout and emits the index constants for every language (`core/*/abi.*`). The host writes input and
level data into the array, calls `advance`, and reads state and an event queue (sounds, particles, popups) back.
Cores that expose linear memory are read directly through a `Float64Array` view; the WasmGC ones (OCaml, F*) go
through `mem_get`/`mem_set`.

Because everything is plain IEEE-754 double arithmetic (with a hand-written `sin`/`cos` and RNG so no libm is
needed), **all nine cores produce bit-identical simulations**. `tools/difftest.js` proves it: it runs the same bot
scenarios through every backend in headless Chromium and compares full state traces with the JS reference.

## Layout

```
web/index.html      game host: Three.js renderer, audio, input, UI, level generator, backend dropdown
web/backends.js     loads a core (raw wasm / WASI / wasm_of_ocaml glue) behind one interface
core/<lang>/        the simulation core in each language
core/js/core.js     JS reference implementation
tools/abi.py        ABI layout generator          tools/difftest.js  differential test in Chromium
tools/build_*.sh    per-language builds           tools/ol_*.{js,sh} toolchain-free native trace comparison
.github/workflows/ci.yml   builds every core, runs the differential test, deploys to GitHub Pages
```

## Play

https://primaryatias-oss.github.io/Lets-Do-Some-Slop-WASM/ (add `?core=rust`, `?core=haskell`, ... to preselect a core)

| Key | Action |
| --- | --- |
| A / D, arrows | Run |
| Space / W | Jump (again in the air for a double jump; works off walls too) |
| Shift / X | Dash (kills slimes, dodges bullets) |
| S + Jump | Drop through thin platforms |
| P / R / M | Pause / Restart / Mute |

The corner badge shows the active core and its measured simulation cost per frame.

Copy of [Lets-Do-Some-Slop](https://github.com/primaryatias-oss/Lets-Do-Some-Slop) (the pure-JS version).
