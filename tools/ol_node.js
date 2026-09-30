#!/usr/bin/env node
// Open-loop scenario on the JS reference core (node).  Prints the same trace format as the
// native OCaml / Haskell drivers so they can be diffed line by line.
//   node tools/ol_node.js <level 0..2> <frames> [core.js path]
const fs = require('fs'), path = require('path'), vm = require('vm');
const lvl = +process.argv[2] || 0, frames = +process.argv[3] || 3600;
const dir = path.join(__dirname, '..', 'core');
const ctx = { console, globalThis: null };
ctx.globalThis = ctx; ctx.window = undefined;
vm.createContext(ctx);
vm.runInContext(fs.readFileSync(path.join(dir, 'js', 'abi.js'), 'utf8').replace(/^const /gm, 'var ') + '\n' +
  fs.readFileSync(path.join(dir, 'js', 'core.js'), 'utf8'), ctx);
const C = ctx.SlopBackends.js, M = C.M, A = ctx;
const tok = fs.readFileSync(path.join(dir, 'test', `level${lvl}.txt`), 'utf8').split(/\s+/).filter(Boolean);
let t = 0; const nx = () => tok[t++];
C.init();
const W = +(nx(), nx()); M[A.G_LW] = W;
nx(); M[A.G_SPAWNX] = +nx(); M[A.G_SPAWNY] = +nx();
nx(); { const x = +nx(), y = +nx(), on = +nx(); if (on) { M[A.G_GOALON] = 1; M[A.G_GOALX] = x; M[A.G_GOALY] = y; } }
nx(); { const x = +nx(), y0 = +nx(), y1 = +nx(), on = +nx(); if (on) { M[A.G_DOORON] = 1; M[A.G_DOORX] = x; M[A.G_DOORY0] = y0; M[A.G_DOORY1] = y1; } }
nx(); { const x = +nx(), y = +nx(), tr = +nx(), on = +nx(); if (on) { M[A.G_BSPECX] = x; M[A.G_BSPECY] = y; M[A.G_BTRIG] = tr; } }
nx(); M[A.G_RNG] = +nx();
nx(); { const n = +nx(); for (let i = 0; i < n; i++) { const x = +nx(), y = +nx(), v = +nx(); M[A.TILE_BASE + y * W + x] = v; } }
nx(); { const n = +nx(); for (let i = 0; i < n; i++) { const k = +nx(), x = +nx(), y = +nx(); M[A.SPEC_BASE + i * 3] = k; M[A.SPEC_BASE + i * 3 + 1] = x; M[A.SPEC_BASE + i * 3 + 2] = y; } M[A.G_NSPEC] = n; }
M[A.G_MODE] = A.MODE_PLAY; M[A.G_VIEWW] = 20;
C.loadLevel();
if (process.argv[4]) { M[A.P_X] = +process.argv[4]; M[A.P_Y] = 3; }

let s = 7;
const lcg = () => { s = s * 1664525 + 1013904223; s = s - Math.floor(s / 4294967296) * 4294967296; return s / 4294967296; };
const bits = (x) => { const b = new DataView(new ArrayBuffer(8)); b.setFloat64(0, x); return b.getBigUint64(0).toString(16).padStart(16, '0'); };
let evTotal = 0;
const out = [];
for (let f = 0; f < frames; f++) {
  let r = lcg(); const ax = r < 0.2 ? -1 : (r < 0.9 ? 1 : 0);
  r = lcg(); const jp = r < 0.05 ? 1 : 0;
  r = lcg(); const jh = r < 0.5 ? 1 : 0;
  r = lcg(); const dp = r < 0.01 ? 1 : 0;
  r = lcg(); const dn = r < 0.03 ? 1 : 0;
  M[A.IN_AX] = ax; M[A.IN_JUMPHELD] = jh; M[A.IN_DOWNHELD] = dn;
  if (jp) M[A.IN_JUMPPRESS] = 1;
  if (dp) M[A.IN_DASHPRESS] = 1;
  C.advance(1 / 60);
  evTotal += M[A.G_EVN];
  if (M[A.P_HP] < 2) M[A.P_HP] = 3;   // keep the run alive so all code paths are exercised
  if (f % 15 === 0) {
    let h = 0;
    for (let i = 0; i < A.SPEC_BASE; i++) h += M[i] * (1 + (i % 7));
    out.push([f, M[A.P_X], M[A.P_Y], M[A.P_VX], M[A.P_VY], M[A.P_HP], M[A.G_SCORE], M[A.G_COINS], M[A.G_KILLS], M[A.G_MODE], M[A.G_NSH], M[A.G_RNG], evTotal, h].map((v, i) => (i === 0 ? String(v) : bits(v))).join(' '));
  }
}
console.log(out.join('\n'));
