#!/usr/bin/env node
/* Differential test: run identical bot scenarios on every backend (inside headless
   Chromium, against ./dist served over http) and compare complete state traces with
   the JS reference.  Usage: node tools/difftest.js [backend ids...]                 */
const { chromium } = require(process.env.PLAYWRIGHT_PATH || '/opt/node22/lib/node_modules/playwright');
const fs = require('fs');
const path = require('path');
const THREE = fs.readFileSync(process.env.THREE_JS || path.join(__dirname, 'three.min.js'), 'utf8');
const ids = process.argv.slice(2);
const URL = process.env.URL || 'http://localhost:8765/';

async function run(page, id, scen) {
  return page.evaluate(async ({ id, scen }) => {
    const be = await Backends.get(id);
    simBe = be; simM = be.view();
    loadLevel(scen.lvl, false);
    G.state = 'play';
    if (scen.teleport) { wr(P_X, scen.teleport[0]); wr(P_Y, scen.teleport[1]); }
    let seed = 7;
    const rr = () => { seed = (seed * 1103515245 + 12345) % 2147483648; return seed / 2147483648; };
    const trace = [];
    let evTotal = 0, evMask = 0;
    for (let f = 0; f < scen.frames; f++) {
      const px = rd(P_X), py = rd(P_Y), vy = rd(P_VY), gnd = rd(P_GND);
      let ax = 1, jump = 0, jumpHeld = 0, dash = 0, down = 0;
      const solidAt = (dx, dy) => { const t = rd(TILE_BASE + Math.floor(py + dy) * rd(G_LW) + Math.floor(px + dx)); return t === 1 || t === 4; };
      const groundAt = (dx) => { for (let d = 0; d < 3; d++) { const t = rd(TILE_BASE + Math.floor(py - 0.2 - d) * rd(G_LW) + Math.floor(px + dx)); if (t === 1 || t === 2) return true; } return false; };
      if (scen.chase && rd(G_BOSS) >= 0) {
        const b = EN_BASE + rd(G_BOSS) * EN_N; const dx = rd(b + B_X) - px;
        ax = Math.abs(dx) > 0.6 ? Math.sign(dx) : 0;
        if (gnd && Math.abs(dx) < 3.5) { jump = 1; jumpHeld = 1; } else if (!gnd) jumpHeld = vy > 0 ? 1 : 0;
      } else {
        if (gnd) {
          if (solidAt(1.0, 0.3) || solidAt(1.0, 1.2) || !groundAt(1.3) || rr() < 0.02) { jump = 1; jumpHeld = 1; }
        } else {
          jumpHeld = vy > 0 ? 1 : 0;
          if (vy < -2 && !groundAt(0.8) && rr() < 0.3) jump = 1;
        }
        if (rr() < 0.01) dash = 1;
        if (rr() < 0.004) ax = -1;
        if (rr() < 0.004) down = 1;
      }
      wr(IN_AX, ax); wr(IN_JUMPHELD, jumpHeld); wr(IN_DOWNHELD, down);
      if (jump) wr(IN_JUMPPRESS, 1);
      if (dash) wr(IN_DASHPRESS, 1);
      be.advance(1 / 60); simM = be.view();
      evTotal += rd(G_EVN);
      for (let e = 0, n = rd(G_EVN); e < n; e++) evMask |= 1 << rd(EV_BASE + e * EV_N);
      if (scen.god && rd(P_HP) < 2) wr(P_HP, 3);
      if (f % 15 === 0) {
        let h = 0;
        for (let i = 0; i < MEM_SIZE; i++) { if (i >= SPEC_BASE && i < TILE_BASE + 4480) continue; h = (h * 31 + rd(i) * (1 + (i % 7))) % 1e15; }
        trace.push([f, rd(P_X), rd(P_Y), rd(P_VX), rd(P_VY), rd(P_HP), rd(G_SCORE), rd(G_COINS), rd(G_KILLS), rd(G_MODE), rd(G_NSH), evTotal, rd(G_RNG), h]);
      }
      if (rd(G_MODE) === MODE_CLEAR) break;
    }
    return { trace, bytes: be.bytes, evMask, state: [rd(P_X), rd(P_Y), rd(G_SCORE), rd(G_DEATHS), rd(G_KILLS), rd(G_BOSSKILLED), rd(G_MODE)] };
  }, { id, scen });
}

(async () => {
  const launchOpts = { args: ['--use-gl=swiftshader', '--no-sandbox'] };
  if (process.env.CHROME !== '') launchOpts.executablePath = process.env.CHROME || '/opt/pw-browsers/chromium-1194/chrome-linux/chrome';
  const b = await chromium.launch(launchOpts);
  const page = await b.newPage({ viewport: { width: 800, height: 450 } });
  page.on('pageerror', (e) => console.log('PAGEERROR', e.message));
  await page.route('**/three.min.js', (r) => r.fulfill({ body: THREE, contentType: 'application/javascript' }));
  await page.route('**/fonts.googleapis.com/**', (r) => r.abort());
  await page.goto(URL);
  await page.waitForFunction(() => window.__slop);
  const all = await page.evaluate(() => Backends.list.map((b) => b.id));
  const targets = ids.length ? ids : all.filter((x) => x !== 'js');
  const scens = [
    { name: 'L1 bot', lvl: 0, frames: 60 * 60 },
    { name: 'L2 bot', lvl: 1, frames: 60 * 60 },
    { name: 'L3 bot', lvl: 2, frames: 60 * 50 },
    { name: 'L3 boss chase (god)', lvl: 2, frames: 60 * 90, teleport: [null, 3], chase: true, god: true },
  ];
  const arena = await page.evaluate(() => buildLevel(2).bossSpec.x0);
  scens[3].teleport[0] = arena - 4;
  let failed = 0;
  for (const s of scens) {
    const ref = await run(page, 'js', s);
    const names = await page.evaluate((m) => Object.keys(window).length && [...Array(26).keys()].filter((i) => (m >> i) & 1).map((i) => Object.entries({JUMP:EV_JUMP,DOUBLE:EV_DOUBLE,WALLJUMP:EV_WALLJUMP,LAND:EV_LAND,DASH:EV_DASH,COIN:EV_COIN,GEM:EV_GEM,HEART:EV_HEART,KILL:EV_KILL,HURT:EV_HURT,DIE:EV_DIE,SPRING:EV_SPRING,SHOOT:EV_SHOOT,CHECK:EV_CHECK,CRUMBLE:EV_CRUMBLE,BOOM:EV_BOOM,BOSSHIT:EV_BOSSHIT,DOOR:EV_DOOR,PIT:EV_PIT,RESPAWN:EV_RESPAWN,COMPLETE:EV_COMPLETE,BOSSDEAD:EV_BOSSDEAD,SHOTHIT:EV_SHOTHIT,BOSSSTART:EV_BOSSSTART,BOSSEXPLODE:EV_BOSSEXPLODE}).find(([k, v]) => v === i)?.[0]).join(','), ref.evMask);
    console.log(`\n[${s.name}] reference: ${ref.trace.length} samples, final`, ref.state.map((v) => +v.toFixed(3)).join(' '), '\n   events seen:', names);
    for (const id of targets) {
      let res;
      try { res = await run(page, id, s); } catch (e) { console.log(`  ${id.padEnd(8)} ERROR ${e.message.split('\n')[0]}`); failed++; continue; }
      let bad = -1, col = -1;
      const n = Math.max(res.trace.length, ref.trace.length);
      for (let i = 0; i < n && bad < 0; i++) {
        const a = res.trace[i], r = ref.trace[i];
        if (!a || !r) { bad = i; col = -2; break; }
        for (let c = 0; c < r.length; c++) if (Math.abs(a[c] - r[c]) > (process.env.TOL || 0)) { bad = i; col = c; break; }
      }
      if (bad < 0) console.log(`  ${id.padEnd(8)} OK   identical trace (${res.bytes ? (res.bytes / 1024).toFixed(1) + ' KB' : 'js'})`);
      else { failed++; console.log(`  ${id.padEnd(8)} FAIL at sample ${bad} col ${col}: got ${JSON.stringify(res.trace[bad])} want ${JSON.stringify(ref.trace[bad])}`); }
    }
  }
  await b.close();
  process.exit(failed ? 1 : 0);
})();
