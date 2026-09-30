#!/usr/bin/env node
// Dump the three generated levels to plain text so toolchain-free native drivers can load them.
const { chromium } = require(process.env.PLAYWRIGHT_PATH || '/opt/node22/lib/node_modules/playwright');
const fs = require('fs'), path = require('path');
const THREE = fs.readFileSync(path.join(__dirname, 'three.min.js'), 'utf8');
(async () => {
  const b = await chromium.launch({ executablePath: process.env.CHROME || '/opt/pw-browsers/chromium-1194/chrome-linux/chrome', args: ['--use-gl=swiftshader', '--no-sandbox'] });
  const p = await b.newPage();
  await p.route('**/three.min.js', (r) => r.fulfill({ body: THREE, contentType: 'application/javascript' }));
  await p.route('**/fonts.googleapis.com/**', (r) => r.abort());
  await p.goto(process.env.URL || 'http://localhost:8765/');
  await p.waitForFunction(() => window.__slop);
  for (let i = 0; i < 3; i++) {
    const txt = await p.evaluate((i) => {
      const L = buildLevel(i), out = [];
      out.push(`W ${L.w}`);
      out.push(`SPAWN ${L.spawn.x} ${L.spawn.y}`);
      out.push(L.goal ? `GOAL ${L.goal.x} ${L.goal.y} 1` : 'GOAL 0 0 0');
      out.push(L.door ? `DOOR ${L.door.x} ${L.door.y0} ${L.door.y1} 1` : 'DOOR 0 0 0 0');
      out.push(L.bossSpec ? `BOSS ${L.bossSpec.x} ${L.bossSpec.y} ${L.bossSpec.trigger} 1` : 'BOSS 0 0 0 0');
      out.push(`SEED ${(L.def.seed * 97 + 13) % 4294967296}`);
      const tiles = [];
      for (let ty = 0; ty < 14; ty++) for (let tx = 0; tx < L.w; tx++) { const v = L.tiles[ty * L.w + tx]; if (v) tiles.push(`${tx} ${ty} ${v}`); }
      out.push(`TILES ${tiles.length}`); out.push(...tiles);
      const specs = [];
      for (const s of L.specs) if (SPEC_KIND[s.t] !== undefined) specs.push(`${SPEC_KIND[s.t]} ${s.tx + 0.5} ${s.ty}`);
      if (L.bossSpec) specs.push(`${SK_BOSS} ${L.bossSpec.x} ${L.bossSpec.y}`);
      out.push(`SPECS ${specs.length}`); out.push(...specs);
      return out.join('\n') + '\n';
    }, i);
    fs.writeFileSync(path.join(__dirname, '..', 'core', 'test', `level${i}.txt`), txt);
    console.log('level', i, txt.split('\n').length, 'lines');
  }
  await b.close();
})();
