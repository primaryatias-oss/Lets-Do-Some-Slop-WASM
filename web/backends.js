/* Host-side adapters: turn "some compiled thing" into the uniform backend interface
     { id, name, note, bytes, init(), loadLevel(), advance(dt) -> steps, get(i), set(i,v), view() }
   view() returns a live Float64Array over the core memory when the module exposes its
   linear memory (fast path), otherwise null and the host falls back to get()/set().   */
(function (root) {
'use strict';

const list = [
  { id: 'js',      name: 'JavaScript (reference)', kind: 'js',      note: 'readable reference implementation' },
  { id: 'c',       name: 'C (clang)',              kind: 'raw',     file: 'wasm/c.wasm',       note: 'freestanding wasm32, no libc' },
  { id: 'rust',    name: 'Rust',                   kind: 'raw',     file: 'wasm/rust.wasm',    note: 'wasm32-unknown-unknown, std' },
  { id: 'zig',     name: 'Zig',                    kind: 'raw',     file: 'wasm/zig.wasm',     note: 'wasm32-freestanding' },
  { id: 'go',      name: 'Go',                     kind: 'wasi',    file: 'wasm/go.wasm',      note: 'wasip1 reactor + //go:wasmexport' },
  { id: 'nim',     name: 'Nim',                    kind: 'raw',     file: 'wasm/nim.wasm',     note: 'Nim -> C -> clang, no GC' },
  { id: 'ocaml',   name: 'OCaml',                  kind: 'glue',    file: 'wasm/ocaml/core.js', note: 'wasm_of_ocaml (WasmGC)' },
  { id: 'haskell', name: 'Haskell',                kind: 'wasi',    file: 'wasm/haskell.wasm', note: 'GHC WebAssembly backend', hs: true },
  { id: 'fstar',   name: 'F*',                     kind: 'glue',    file: 'wasm/fstar/core.js', note: 'F* -> OCaml -> wasm_of_ocaml' },
];

// -------- minimal WASI shim (enough for Go / GHC reactors) --------
function makeWasi(ref) {
  const dv = () => new DataView(ref.mem.buffer);
  const t = {
    proc_exit(code) { throw new Error('wasm proc_exit(' + code + ')'); },
    fd_write(fd, iovs, n, out) {
      const d = dv(); let len = 0;
      for (let i = 0; i < n; i++) len += d.getUint32(iovs + i * 8 + 4, true);
      d.setUint32(out, len, true);
      return 0;
    },
    fd_read(fd, iovs, n, out) { dv().setUint32(out, 0, true); return 0; },
    fd_close() { return 0; }, fd_seek() { return 70; }, fd_sync() { return 0; },
    fd_fdstat_get() { return 8; }, fd_fdstat_set_flags() { return 0; },
    fd_prestat_get() { return 8; }, fd_prestat_dir_name() { return 8; },
    fd_filestat_get() { return 8; }, path_open() { return 44; }, path_filestat_get() { return 44; },
    args_sizes_get(c, s) { const d = dv(); d.setUint32(c, 0, true); d.setUint32(s, 0, true); return 0; },
    args_get() { return 0; },
    environ_sizes_get(c, s) { const d = dv(); d.setUint32(c, 0, true); d.setUint32(s, 0, true); return 0; },
    environ_get() { return 0; },
    clock_time_get(id, prec, out) { dv().setBigUint64(out, BigInt(Math.floor(performance.now() * 1e6)), true); return 0; },
    clock_res_get(id, out) { dv().setBigUint64(out, 1000n, true); return 0; },
    random_get(buf, len) { crypto.getRandomValues(new Uint8Array(ref.mem.buffer, buf, len)); return 0; },
    sched_yield() { return 0; },
    poll_oneoff(i, o, n, out) { dv().setUint32(out, 0, true); return 0; },
  };
  return new Proxy(t, { get: (o, k) => (k in o ? o[k] : () => 52) });
}

async function fetchBytes(url) {
  const r = await fetch(url);
  if (!r.ok) throw new Error('HTTP ' + r.status + ' for ' + url);
  return new Uint8Array(await r.arrayBuffer());
}

function wrapExports(d, ex, bytes) {
  let M = null, lastBuf = null, ptr = 0;
  const be = {
    id: d.id, name: d.name, note: d.note, bytes,
    init() { ex.init(); ptr = ex.mem_ptr(); lastBuf = null; },
    loadLevel() { ex.load_level(); },
    advance(dt) { return ex.advance(dt); },
    get: (i) => ex.mem_get(i),
    set: (i, v) => ex.mem_set(i, v),
    view() {
      if (!ptr || !ex.memory) return null;
      if (ex.memory.buffer !== lastBuf) { lastBuf = ex.memory.buffer; M = new Float64Array(lastBuf, ptr, MEM_SIZE); }
      return M;
    },
  };
  return be;
}

async function loadRaw(d) {
  const bytes = await fetchBytes(d.file);
  const ref = { mem: null };
  const { instance } = await WebAssembly.instantiate(bytes, { wasi_snapshot_preview1: makeWasi(ref), env: new Proxy({}, { get: () => () => 0 }) });
  const ex = instance.exports;
  ref.mem = ex.memory;
  if (ex._initialize) ex._initialize();
  if (d.hs && ex.hs_init) ex.hs_init(0, 0);
  return wrapExports(d, ex, bytes.length);
}

// wasm_of_ocaml output: a script that registers `globalThis.SlopCore_<id>` with the same functions
function loadGlue(d) {
  return new Promise((resolve, reject) => {
    const key = 'SlopCore_' + d.id;
    const s = document.createElement('script');
    s.src = d.file;
    s.onerror = () => reject(new Error('could not load ' + d.file));
    document.head.appendChild(s);
    const t0 = performance.now();
    (function poll() {
      const c = root[key];
      if (c && c.ready) {
        resolve({
          id: d.id, name: d.name, note: d.note, bytes: c.bytes || 0,
          init: () => c.init(), loadLevel: () => c.load_level(), advance: (dt) => c.advance(dt),
          get: (i) => c.mem_get(i), set: (i, v) => c.mem_set(i, v), view: () => null,
        });
      } else if (performance.now() - t0 > 20000) reject(new Error('timeout waiting for ' + d.file));
      else setTimeout(poll, 30);
    })();
  });
}

const cache = new Map();
root.Backends = {
  list,
  get(id) {
    if (!cache.has(id)) {
      const d = list.find((x) => x.id === id);
      if (!d) return Promise.reject(new Error('unknown core ' + id));
      let p;
      if (d.kind === 'js') {
        const j = root.SlopBackends.js;
        p = j.load().then(() => ({ id: d.id, name: d.name, note: d.note, bytes: 0, init: j.init, loadLevel: j.loadLevel, advance: j.advance, get: j.get, set: j.set, view: () => j.M }));
      } else if (d.kind === 'glue') p = loadGlue(d);
      else p = loadRaw(d);
      p.catch(() => cache.delete(id));
      cache.set(id, p);
    }
    return cache.get(id);
  },
};
})(typeof window !== 'undefined' ? window : globalThis);
