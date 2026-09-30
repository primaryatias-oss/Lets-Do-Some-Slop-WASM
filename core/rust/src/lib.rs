//! Slop Runner simulation core - Rust port (wasm32-unknown-unknown).
//! Mirrors core/js/core.js line by line; all state lives in the flat array `m`.
#![allow(clippy::all)]
mod abi;
use abi::*;

const LEVEL_H: i32 = 14;
const DT: f64 = 1.0 / 120.0;
const PW: f64 = 0.62;
const PH: f64 = 0.86;
const MAX_SPEED: f64 = 8.2;
const ACCEL_G: f64 = 95.0;
const ACCEL_A: f64 = 62.0;
const DECEL_G: f64 = 110.0;
const DECEL_A: f64 = 26.0;
const GRAVITY: f64 = 62.0;
const FALL_GRAVITY: f64 = 74.0;
const MAX_FALL: f64 = 27.0;
const JUMP_V: f64 = 19.6;
const DOUBLE_V: f64 = 17.2;
const COYOTE: f64 = 0.11;
const BUFFER: f64 = 0.13;
const WALL_SLIDE: f64 = 3.4;
const WALL_JUMP_X: f64 = 9.5;
const WALL_JUMP_Y: f64 = 18.2;
const WALL_LOCK: f64 = 0.16;
const DASH_TIME: f64 = 0.17;
const DASH_SPEED: f64 = 24.0;
const DASH_CD: f64 = 0.75;
const INVULN: f64 = 1.5;
const STOMP_BOUNCE: f64 = 15.5;

fn sign(v: f64) -> f64 { if v > 0.0 { 1.0 } else if v < 0.0 { -1.0 } else { 0.0 } }
fn fmin(a: f64, b: f64) -> f64 { if a < b { a } else { b } }
fn fmax(a: f64, b: f64) -> f64 { if a > b { a } else { b } }
fn fabs(a: f64) -> f64 { if a < 0.0 { -a } else { a } }
fn clamp(v: f64, a: f64, b: f64) -> f64 { if v < a { a } else if v > b { b } else { v } }
fn approach(v: f64, t: f64, s: f64) -> f64 { if v < t { fmin(v + s, t) } else { fmax(v - s, t) } }
fn fi(x: f64) -> i32 { x.floor() as i32 }

fn fsin(x: f64) -> f64 {
    let k = (x * 0.15915494309189535 + 0.5).floor();
    let mut r = x - k * 6.283185307179586;
    if r > 1.5707963267948966 { r = 3.141592653589793 - r; } else if r < -1.5707963267948966 { r = -3.141592653589793 - r; }
    let r2 = r * r;
    r * (1.0 + r2 * (-0.16666666666666666 + r2 * (0.008333333333333333 + r2 * (-0.0001984126984126984 +
        r2 * (2.7557319223985893e-6 + r2 * (-2.505210838544172e-8 + r2 * 1.6059043836821613e-10))))))
}
fn fcos(x: f64) -> f64 { fsin(x + 1.5707963267948966) }

struct World { m: [f64; MEM_SIZE] }
static mut W: World = World { m: [0.0; MEM_SIZE] };

fn world() -> &'static mut World { unsafe { &mut *core::ptr::addr_of_mut!(W) } }

impl World {
    fn rnd(&mut self) -> f64 {
        let mut s = self.m[G_RNG] * 1664525.0 + 1013904223.0;
        s = s - (s / 4294967296.0).floor() * 4294967296.0;
        self.m[G_RNG] = s;
        s / 4294967296.0
    }
    fn rndr(&mut self, a: f64, b: f64) -> f64 { a + self.rnd() * (b - a) }
    fn ev(&mut self, t: usize, x: f64, y: f64, a: f64) {
        let n = self.m[G_EVN] as usize;
        if n < EV_MAX {
            let i = EV_BASE + n * EV_N;
            self.m[i] = t as f64; self.m[i + 1] = x; self.m[i + 2] = y; self.m[i + 3] = a;
            self.m[G_EVN] = (n + 1) as f64;
        }
    }
    fn tile_at(&self, tx: i32, ty: i32) -> f64 {
        let lw = self.m[G_LW] as i32;
        if tx < 0 || tx >= lw { return 1.0; }
        if ty < 0 || ty >= LEVEL_H { return 0.0; }
        self.m[TILE_BASE + (ty * lw + tx) as usize]
    }
    fn set_tile(&mut self, tx: i32, ty: i32, v: f64) {
        let lw = self.m[G_LW] as i32;
        self.m[TILE_BASE + (ty * lw + tx) as usize] = v;
    }
    fn solid(t: f64) -> bool { t == 1.0 || t == 4.0 }

    fn move_body(&mut self, b: usize, dt: f64, drop: bool) {
        self.m[b + B_HX] = 0.0; self.m[b + B_HY] = 0.0;
        let hw = self.m[b + B_W] / 2.0;
        self.m[b + B_X] += self.m[b + B_VX] * dt;
        let y0 = fi(self.m[b + B_Y] + 0.02);
        let y1 = fi(self.m[b + B_Y] + self.m[b + B_H] - 0.02);
        let vx = self.m[b + B_VX];
        if vx > 0.0 {
            let tx = fi(self.m[b + B_X] + hw);
            for ty in y0..=y1 {
                if World::solid(self.tile_at(tx, ty)) { self.m[b + B_X] = tx as f64 - hw - 1e-4; self.m[b + B_VX] = 0.0; self.m[b + B_HX] = 1.0; break; }
            }
        } else if vx < 0.0 {
            let tx = fi(self.m[b + B_X] - hw);
            for ty in y0..=y1 {
                if World::solid(self.tile_at(tx, ty)) { self.m[b + B_X] = tx as f64 + 1.0 + hw + 1e-4; self.m[b + B_VX] = 0.0; self.m[b + B_HX] = -1.0; break; }
            }
        }
        let prev_y = self.m[b + B_Y];
        self.m[b + B_Y] += self.m[b + B_VY] * dt;
        let x0 = fi(self.m[b + B_X] - hw + 1e-3);
        let x1 = fi(self.m[b + B_X] + hw - 1e-3);
        self.m[b + B_GND] = 0.0;
        if self.m[b + B_VY] <= 0.0 {
            let ty = fi(self.m[b + B_Y]);
            for tx in x0..=x1 {
                let t = self.tile_at(tx, ty);
                if World::solid(t) || (t == 2.0 && !drop && prev_y >= ty as f64 + 1.0 - 0.02) {
                    self.m[b + B_Y] = ty as f64 + 1.0; self.m[b + B_VY] = 0.0; self.m[b + B_GND] = 1.0; self.m[b + B_HY] = -1.0; break;
                }
            }
        } else {
            let ty = fi(self.m[b + B_Y] + self.m[b + B_H]);
            for tx in x0..=x1 {
                if World::solid(self.tile_at(tx, ty)) { self.m[b + B_Y] = ty as f64 - self.m[b + B_H] - 1e-4; self.m[b + B_VY] = 0.0; self.m[b + B_HY] = 1.0; break; }
            }
        }
    }
    fn box_hit(&self, a: usize, b: usize) -> bool {
        let m = &self.m;
        fabs(m[a + B_X] - m[b + B_X]) < (m[a + B_W] + m[b + B_W]) / 2.0 &&
            m[a + B_Y] < m[b + B_Y] + m[b + B_H] && m[a + B_Y] + m[a + B_H] > m[b + B_Y]
    }

    fn set_door(&mut self, closed: bool) {
        if self.m[G_DOORON] == 0.0 { return; }
        let mut ty = self.m[G_DOORY0] as i32;
        while ty <= self.m[G_DOORY1] as i32 { let x = self.m[G_DOORX] as i32; self.set_tile(x, ty, if closed { 4.0 } else { 0.0 }); ty += 1; }
        self.m[G_DOORCLOSED] = if closed { 1.0 } else { 0.0 };
        let dx = self.m[G_DOORX];
        self.ev(EV_DOOR, dx, 0.0, if closed { 1.0 } else { 0.0 });
    }

    fn add_enemy(&mut self, kind: usize, x: f64, y: f64) {
        let i = self.m[G_NEN] as usize; if i >= EN_MAX { return; }
        self.m[G_NEN] = (i + 1) as f64;
        let b = EN_BASE + i * EN_N;
        for k in 0..EN_N { self.m[b + k] = 0.0; }
        self.m[b + B_X] = x; self.m[b + B_Y] = y; self.m[b + B_W] = 0.8; self.m[b + B_H] = 0.62;
        self.m[b + E_KIND] = kind as f64; self.m[b + E_ALIVE] = 1.0; self.m[b + E_STOMP] = 1.0;
        let r = self.rnd(); self.m[b + E_DIR] = if r < 0.5 { -1.0 } else { 1.0 };
        let r = self.rnd(); self.m[b + E_T] = r * 10.0;
        self.m[b + E_OX] = x; self.m[b + E_OY] = y;
        let r = self.rnd(); self.m[b + E_CD] = 1.0 + r;
        if kind == EK_BAT { self.m[b + B_W] = 0.75; self.m[b + B_H] = 0.5; }
        else if kind == EK_SAW { self.m[b + B_W] = 0.78; self.m[b + B_H] = 0.78; self.m[b + E_STOMP] = 0.0; self.m[b + E_DIR] = 1.0; self.m[b + B_VX] = 3.0; }
        else if kind == EK_TURRET { self.m[b + B_W] = 0.9; self.m[b + B_H] = 0.8; self.m[b + E_STOMP] = 0.0; }
        else if kind == EK_BOSS {
            self.m[b + B_W] = 2.5; self.m[b + B_H] = 2.3; self.m[b + E_HP] = 8.0; self.m[b + E_HP0] = 8.0;
            self.m[b + E_STATE] = BS_SLEEP as f64; self.m[b + E_DIR] = -1.0;
            self.m[G_BOSS] = i as f64;
        }
    }
    fn add_platform(&mut self, kind: usize, x: f64, ty: f64) {
        let i = self.m[G_NPLT] as usize; if i >= PLT_MAX { return; }
        self.m[G_NPLT] = (i + 1) as f64;
        let p = PLT_BASE + i * PLT_N;
        for k in 0..PLT_N { self.m[p + k] = 0.0; }
        self.m[p + PT_KIND] = kind as f64; self.m[p + PT_X] = x; self.m[p + PT_X0] = x;
        self.m[p + PT_TOP] = ty + 0.75; self.m[p + PT_TOP0] = ty + 0.75; self.m[p + PT_PREVTOP] = ty + 0.75;
        self.m[p + PT_W] = if kind == 2 { 1.0 } else { 3.0 }; self.m[p + PT_SOLID] = 1.0;
        let r = self.rnd(); self.m[p + PT_T] = r * 6.0; self.m[p + PT_STATE] = CS_IDLE as f64;
    }
    fn add_goal(&mut self, x: f64, y: f64) { self.m[G_GOALON] = 1.0; self.m[G_GOALX] = x; self.m[G_GOALY] = y; }

    fn reset_player(&mut self, x: f64, y: f64) {
        let m = &mut self.m;
        m[P_X] = x; m[P_Y] = y; m[P_VX] = 0.0; m[P_VY] = 0.0; m[P_FACE] = 1.0; m[P_GND] = 0.0;
        m[P_COYOTE] = 0.0; m[P_BUFFER] = 0.0; m[P_JUMPS] = 1.0; m[P_WALLDIR] = 0.0; m[P_WALLGRACE] = 0.0; m[P_WALLLOCK] = 0.0;
        m[P_DASHT] = 0.0; m[P_DASHCD] = 0.0; m[P_AIRDASH] = 0.0; m[P_INV] = 0.0; m[P_DROPT] = 0.0; m[P_DEAD] = 0.0; m[P_DEADT] = 0.0;
        m[P_RIDING] = -1.0; m[P_SAFEX] = x; m[P_SAFEY] = y; m[P_SAFET] = 0.0; m[P_PREVY] = y; m[P_WASGND] = 0.0;
    }

    fn init(&mut self) {
        for i in 0..MEM_SIZE { self.m[i] = 0.0; }
        self.m[G_RNG] = 12345.0; self.m[G_VIEWW] = 20.0;
        self.m[P_W] = PW; self.m[P_H] = PH; self.m[P_MAXHP] = 3.0; self.m[P_HP] = 3.0; self.m[P_RIDING] = -1.0; self.m[G_BOSS] = -1.0;
    }

    fn load_level(&mut self) {
        self.m[G_NCOINS] = 0.0; self.m[G_NEN] = 0.0; self.m[G_NPLT] = 0.0; self.m[G_NSH] = 0.0; self.m[G_NSPR] = 0.0; self.m[G_NCK] = 0.0; self.m[G_BOSS] = -1.0;
        self.m[G_TIME] = 0.0; self.m[G_COINS] = 0.0; self.m[G_KILLS] = 0.0; self.m[G_DEATHS] = 0.0; self.m[G_SCORE] = 0.0; self.m[G_BOSSKILLED] = 0.0;
        self.m[G_BOSSACTIVE] = 0.0; self.m[G_FREEZE] = 0.0; self.m[G_COINSTOTAL] = 0.0; self.m[G_DOORCLOSED] = 0.0; self.m[G_ACC] = 0.0; self.m[G_EVN] = 0.0;
        let n = self.m[G_NSPEC] as usize;
        for s in 0..n {
            let kind = self.m[SPEC_BASE + s * SPEC_N] as usize;
            let x = self.m[SPEC_BASE + s * SPEC_N + 1];
            let y = self.m[SPEC_BASE + s * SPEC_N + 2];
            if kind <= SK_HEART {
                let i = self.m[G_NCOINS] as usize;
                if i < COIN_MAX {
                    self.m[G_NCOINS] = (i + 1) as f64;
                    let c = COIN_BASE + i * COIN_N;
                    self.m[c + C_KIND] = kind as f64; self.m[c + C_X] = x; self.m[c + C_Y] = y + 0.5; self.m[c + C_GOT] = 0.0;
                    self.m[c + C_R] = if kind == SK_COIN { 0.55 } else { 0.65 };
                    if kind == SK_COIN { self.m[G_COINSTOTAL] += 1.0; } else if kind == SK_GEM { self.m[G_COINSTOTAL] += 5.0; }
                }
            } else if kind == SK_SLIME { self.add_enemy(EK_SLIME, x, y); }
            else if kind == SK_BAT { self.add_enemy(EK_BAT, x, y + 0.5); }
            else if kind == SK_SAW { self.add_enemy(EK_SAW, x, y + 0.02); }
            else if kind == SK_TURRET { self.add_enemy(EK_TURRET, x, y); }
            else if kind == SK_SPRING {
                let i = self.m[G_NSPR] as usize;
                if i < SPR_MAX { self.m[G_NSPR] = (i + 1) as f64; let q = SPR_BASE + i * SPR_N; self.m[q + SP_X] = x; self.m[q + SP_Y] = y; self.m[q + SP_T] = 0.0; }
            } else if kind == SK_PLAT_H { self.add_platform(0, x, y); }
            else if kind == SK_PLAT_V { self.add_platform(1, x, y); }
            else if kind == SK_CRUMBLE { self.add_platform(2, x, y); }
            else if kind == SK_CHECK {
                let i = self.m[G_NCK] as usize;
                if i < CKT_MAX { self.m[G_NCK] = (i + 1) as f64; let q = CKT_BASE + i * CKT_N; self.m[q + CK_X] = x; self.m[q + CK_Y] = y; self.m[q + CK_ON] = 0.0; }
            } else if kind == SK_BOSS { self.add_enemy(EK_BOSS, x, y); }
        }
        self.m[P_MAXHP] = 3.0; self.m[P_HP] = 3.0;
        self.m[G_CHKX] = self.m[G_SPAWNX]; self.m[G_CHKY] = self.m[G_SPAWNY];
        let (sx, sy) = (self.m[G_SPAWNX], self.m[G_SPAWNY]);
        self.reset_player(sx, sy);
    }

    // ------------------------------------------------------------ player
    fn kill_player(&mut self) {
        self.m[P_DEAD] = 1.0; self.m[P_DEADT] = 0.0; self.m[P_HP] = 0.0;
        self.m[G_DEATHS] += 1.0;
        let (x, y) = (self.m[P_X], self.m[P_Y] + 0.5);
        self.ev(EV_DIE, x, y, 0.0);
    }
    fn hurt_player(&mut self, src_x: f64) -> bool {
        if self.m[P_INV] > 0.0 || self.m[P_DASHT] > 0.0 || self.m[P_DEAD] != 0.0 { return false; }
        self.m[P_HP] -= 1.0;
        self.m[P_INV] = INVULN;
        self.m[G_FREEZE] = 0.08;
        let (x, y) = (self.m[P_X], self.m[P_Y] + 0.5);
        self.ev(EV_HURT, x, y, 0.0);
        self.m[P_VX] = (if self.m[P_X] < src_x { -1.0 } else { 1.0 }) * 8.5; self.m[P_VY] = 12.0;
        self.m[P_WALLLOCK] = 0.18; self.m[P_DASHT] = 0.0;
        if self.m[P_HP] <= 0.0 { self.kill_player(); }
        true
    }
    fn pit_fall(&mut self) {
        if self.m[P_DEAD] != 0.0 { return; }
        self.m[P_HP] -= 1.0;
        let (x, y) = (self.m[P_X], self.m[P_Y]);
        self.ev(EV_PIT, x, y, 0.0);
        if self.m[P_HP] <= 0.0 { self.kill_player(); return; }
        self.m[P_X] = self.m[P_SAFEX]; self.m[P_Y] = self.m[P_SAFEY] + 0.05; self.m[P_VX] = 0.0; self.m[P_VY] = 0.0; self.m[P_INV] = 2.0;
        let (x, y) = (self.m[P_X], self.m[P_Y] + 0.4);
        self.ev(EV_RESPAWN, x, y, 1.0);
    }
    fn respawn(&mut self) {
        let (cx, cy) = (self.m[G_CHKX], self.m[G_CHKY]);
        self.reset_player(cx, cy);
        self.m[P_HP] = self.m[P_MAXHP]; self.m[P_INV] = 2.0;
        if self.m[G_BOSS] >= 0.0 && self.m[G_BOSSACTIVE] != 0.0 { self.reset_boss(); }
        let (x, y) = (self.m[P_X], self.m[P_Y] + 0.5);
        self.ev(EV_RESPAWN, x, y, 0.0);
    }
    fn stomp_bounce(&mut self, strong: bool) {
        self.m[P_VY] = if self.m[IN_JUMPHELD] != 0.0 { STOMP_BOUNCE + 3.5 } else { STOMP_BOUNCE };
        if strong { self.m[P_VY] += 2.0; }
        self.m[P_JUMPS] = 1.0; self.m[P_AIRDASH] = 0.0; self.m[P_GND] = 0.0; self.m[P_DASHT] = 0.0;
    }

    fn update_player(&mut self, dt: f64) {
        let ax = self.m[IN_AX];
        self.m[P_PREVY] = self.m[P_Y];
        self.m[P_INV] = fmax(0.0, self.m[P_INV] - dt);
        self.m[P_DASHCD] -= dt; self.m[P_WALLLOCK] -= dt; self.m[P_BUFFER] -= dt; self.m[P_DROPT] -= dt; self.m[P_WALLGRACE] -= dt;

        if self.m[IN_JUMPPRESS] != 0.0 { self.m[IN_JUMPPRESS] = 0.0; self.m[P_BUFFER] = BUFFER; }
        let want_dash = self.m[IN_DASHPRESS] != 0.0;
        if want_dash { self.m[IN_DASHPRESS] = 0.0; }

        let rid = self.m[P_RIDING] as i32;
        if rid >= 0 {
            let p = PLT_BASE + rid as usize * PLT_N;
            if self.m[p + PT_SOLID] != 0.0 && self.m[P_VY] <= 0.0 { self.m[P_X] += self.m[p + PT_DX]; self.m[P_Y] = self.m[p + PT_TOP]; }
        }

        if self.m[P_GND] != 0.0 { self.m[P_COYOTE] = COYOTE; self.m[P_JUMPS] = 1.0; self.m[P_AIRDASH] = 0.0; }
        else { self.m[P_COYOTE] -= dt; }

        if want_dash && self.m[P_DASHCD] <= 0.0 && self.m[P_AIRDASH] == 0.0 {
            self.m[P_DASHT] = DASH_TIME; self.m[P_DASHCD] = DASH_CD;
            self.m[P_DASHDIR] = if ax != 0.0 { ax } else { self.m[P_FACE] }; self.m[P_FACE] = self.m[P_DASHDIR];
            if self.m[P_GND] == 0.0 { self.m[P_AIRDASH] = 1.0; }
            let (x, y, d) = (self.m[P_X], self.m[P_Y] + 0.4, self.m[P_DASHDIR]);
            self.ev(EV_DASH, x, y, d);
        }

        if self.m[P_DASHT] > 0.0 {
            self.m[P_DASHT] -= dt;
            self.m[P_VX] = self.m[P_DASHDIR] * DASH_SPEED;
            self.m[P_VY] = 0.0;
            if self.m[P_DASHT] <= 0.0 { self.m[P_VX] = self.m[P_DASHDIR] * MAX_SPEED; }
        } else {
            if self.m[P_WALLLOCK] <= 0.0 {
                let target = ax * MAX_SPEED;
                let mut acc;
                if ax == 0.0 { acc = if self.m[P_GND] != 0.0 { DECEL_G } else { DECEL_A }; }
                else {
                    acc = if self.m[P_GND] != 0.0 { ACCEL_G } else { ACCEL_A };
                    if sign(self.m[P_VX]) == -ax { acc *= 1.6; }
                }
                self.m[P_VX] = approach(self.m[P_VX], target, acc * dt);
                if ax != 0.0 { self.m[P_FACE] = ax; }
            }

            self.m[P_WALLDIR] = 0.0;
            if self.m[P_GND] == 0.0 && ax != 0.0 {
                let tx = fi(self.m[P_X] + ax * (PW / 2.0 + 0.08));
                if World::solid(self.tile_at(tx, fi(self.m[P_Y] + 0.3))) || World::solid(self.tile_at(tx, fi(self.m[P_Y] + 0.75))) {
                    self.m[P_WALLDIR] = ax; self.m[P_WALLGRACE] = 0.1; self.m[P_WALLMEM] = ax;
                }
            }

            if self.m[P_BUFFER] > 0.0 {
                if self.m[IN_DOWNHELD] != 0.0 && self.m[P_GND] != 0.0 && self.tile_at(fi(self.m[P_X]), fi(self.m[P_Y] - 0.05)) == 2.0 {
                    self.m[P_DROPT] = 0.22; self.m[P_BUFFER] = 0.0; self.m[P_Y] -= 0.06; self.m[P_GND] = 0.0;
                } else if self.m[P_COYOTE] > 0.0 {
                    self.m[P_VY] = JUMP_V; self.m[P_BUFFER] = 0.0; self.m[P_COYOTE] = 0.0; self.m[P_GND] = 0.0;
                    let (x, y) = (self.m[P_X], self.m[P_Y] + 0.05);
                    self.ev(EV_JUMP, x, y, 0.0);
                } else if self.m[P_WALLGRACE] > 0.0 && self.m[P_WALLMEM] != 0.0 {
                    self.m[P_VX] = -self.m[P_WALLMEM] * WALL_JUMP_X; self.m[P_VY] = WALL_JUMP_Y;
                    self.m[P_WALLLOCK] = WALL_LOCK; self.m[P_FACE] = -self.m[P_WALLMEM]; self.m[P_BUFFER] = 0.0; self.m[P_WALLGRACE] = 0.0;
                    self.m[P_JUMPS] = 1.0; self.m[P_AIRDASH] = 0.0;
                    let (x, y, a) = (self.m[P_X] + self.m[P_WALLMEM] * 0.3, self.m[P_Y] + 0.5, self.m[P_WALLMEM]);
                    self.ev(EV_WALLJUMP, x, y, a);
                } else if self.m[P_JUMPS] > 0.0 {
                    self.m[P_VY] = DOUBLE_V; self.m[P_JUMPS] -= 1.0; self.m[P_BUFFER] = 0.0;
                    let (x, y) = (self.m[P_X], self.m[P_Y] + 0.05);
                    self.ev(EV_DOUBLE, x, y, 0.0);
                }
            }

            let g = if self.m[P_VY] > 0.0 { if self.m[IN_JUMPHELD] != 0.0 { GRAVITY } else { GRAVITY * 2.4 } } else { FALL_GRAVITY };
            self.m[P_VY] = fmax(self.m[P_VY] - g * dt, -MAX_FALL);
            if self.m[P_WALLDIR] != 0.0 && self.m[P_VY] < -WALL_SLIDE { self.m[P_VY] = -WALL_SLIDE; }
        }

        let vy_pre = self.m[P_VY];
        let drop = self.m[P_DROPT] > 0.0;
        self.move_body(P_X, dt, drop);

        self.m[P_RIDING] = -1.0;
        if vy_pre <= 0.0 && self.m[P_DASHT] <= 0.0 {
            let np = self.m[G_NPLT] as usize;
            for i in 0..np {
                let p = PLT_BASE + i * PLT_N;
                if self.m[p + PT_SOLID] == 0.0 { continue; }
                if fabs(self.m[P_X] - self.m[p + PT_X]) < self.m[p + PT_W] / 2.0 + PW / 2.0 - 0.08 && self.m[P_PREVY] >= self.m[p + PT_PREVTOP] - 0.12 &&
                    self.m[P_Y] <= self.m[p + PT_TOP] + 0.02 && self.m[P_Y] >= self.m[p + PT_TOP] - 0.6 {
                    self.m[P_Y] = self.m[p + PT_TOP]; self.m[P_VY] = 0.0; self.m[P_GND] = 1.0; self.m[P_RIDING] = i as f64;
                    if self.m[p + PT_KIND] == 2.0 && self.m[p + PT_STATE] == CS_IDLE as f64 { self.m[p + PT_STATE] = CS_SHAKE as f64; self.m[p + PT_TIMER] = 0.45; }
                    break;
                }
            }
        }

        if self.m[P_GND] != 0.0 && self.m[P_WASGND] == 0.0 && vy_pre < -9.0 {
            let (x, y) = (self.m[P_X], self.m[P_Y] + 0.05);
            self.ev(EV_LAND, x, y, 0.0);
        }
        if self.m[P_HY] == 1.0 && self.m[P_VY] == 0.0 { self.m[P_VY] = -1.0; }
        self.m[P_WASGND] = self.m[P_GND];

        if self.m[P_GND] != 0.0 && self.m[P_RIDING] < 0.0 {
            let ty = fi(self.m[P_Y]) - 1;
            let px = self.m[P_X];
            if World::solid(self.tile_at(fi(px - 0.6), ty)) && World::solid(self.tile_at(fi(px + 0.6), ty)) &&
                World::solid(self.tile_at(fi(px - 1.6), ty)) && World::solid(self.tile_at(fi(px + 1.6), ty)) &&
                self.tile_at(fi(px), fi(self.m[P_Y])) != 3.0 {
                self.m[P_SAFET] += dt;
                if self.m[P_SAFET] > 0.35 { self.m[P_SAFEX] = self.m[P_X]; self.m[P_SAFEY] = self.m[P_Y]; }
            } else { self.m[P_SAFET] = 0.0; }
        } else { self.m[P_SAFET] = 0.0; }

        if self.m[P_INV] <= 0.0 && self.m[P_DASHT] <= 0.0 {
            let x0 = fi(self.m[P_X] - PW / 2.0);
            let x1 = fi(self.m[P_X] + PW / 2.0);
            let y0 = fi(self.m[P_Y]);
            let y1 = fi(self.m[P_Y] + PH * 0.5);
            let mut done = false;
            let mut ty = y0;
            while ty <= y1 && !done {
                for tx in x0..=x1 {
                    if self.tile_at(tx, ty) == 3.0 {
                        if self.m[P_X] + PW / 2.0 > tx as f64 + 0.15 && self.m[P_X] - PW / 2.0 < tx as f64 + 0.85 && self.m[P_Y] < ty as f64 + 0.5 {
                            if self.hurt_player(tx as f64 + 0.5) {
                                self.m[P_VY] = 15.0;
                                self.m[P_VX] = (if self.m[P_X] < tx as f64 + 0.5 { -1.0 } else { 1.0 }) * 5.0;
                            }
                            done = true; break;
                        }
                    }
                }
                ty += 1;
            }
        }

        if self.m[P_Y] < -2.5 { self.pit_fall(); }
    }

    // -------------------------------------------------------- platforms
    fn update_platforms(&mut self, dt: f64) {
        let n = self.m[G_NPLT] as usize;
        for i in 0..n {
            let p = PLT_BASE + i * PLT_N;
            self.m[p + PT_PREVTOP] = self.m[p + PT_TOP]; self.m[p + PT_DX] = 0.0; self.m[p + PT_DY] = 0.0;
            self.m[p + PT_T] += dt;
            let kind = self.m[p + PT_KIND] as i32;
            if kind == 0 {
                let nx = self.m[p + PT_X0] + fsin(self.m[p + PT_T] * 1.05) * 2.4;
                self.m[p + PT_DX] = nx - self.m[p + PT_X]; self.m[p + PT_X] = nx;
            } else if kind == 1 {
                let nt = self.m[p + PT_TOP0] + (1.0 - fcos(self.m[p + PT_T] * 0.95)) / 2.0 * 5.0;
                self.m[p + PT_DY] = nt - self.m[p + PT_TOP]; self.m[p + PT_TOP] = nt;
            } else {
                let st = self.m[p + PT_STATE] as usize;
                if st == CS_SHAKE {
                    self.m[p + PT_TIMER] -= dt;
                    if self.m[p + PT_TIMER] <= 0.0 {
                        self.m[p + PT_STATE] = CS_FALL as f64; self.m[p + PT_SOLID] = 0.0; self.m[p + PT_VY] = 0.0;
                        let (x, y) = (self.m[p + PT_X], self.m[p + PT_TOP]);
                        self.ev(EV_CRUMBLE, x, y, 0.0);
                    }
                } else if st == CS_FALL {
                    self.m[p + PT_VY] -= 32.0 * dt; self.m[p + PT_TOP] += self.m[p + PT_VY] * dt;
                    if self.m[p + PT_TOP] < -3.0 { self.m[p + PT_STATE] = CS_GONE as f64; self.m[p + PT_TIMER] = 2.6; }
                } else if st == CS_GONE {
                    self.m[p + PT_TIMER] -= dt;
                    if self.m[p + PT_TIMER] <= 0.0 {
                        self.m[p + PT_STATE] = CS_IDLE as f64; self.m[p + PT_SOLID] = 1.0; self.m[p + PT_TOP] = self.m[p + PT_TOP0]; self.m[p + PT_PREVTOP] = self.m[p + PT_TOP];
                    }
                }
            }
        }
    }

    // ------------------------------------------------------------ shots
    fn shoot(&mut self, x: f64, y: f64, vx: f64, vy: f64, g: f64, life: f64, size: f64, purple: f64) {
        let i = self.m[G_NSH] as usize; if i >= SH_MAX { return; }
        self.m[G_NSH] = (i + 1) as f64;
        let s = SH_BASE + i * SH_N;
        self.m[s + S_X] = x; self.m[s + S_Y] = y; self.m[s + S_VX] = vx; self.m[s + S_VY] = vy; self.m[s + S_G] = g; self.m[s + S_LIFE] = life;
        self.m[s + S_R] = size * 0.36; self.m[s + S_PURPLE] = purple; self.m[s + S_SIZE] = size;
    }
    fn remove_shot(&mut self, i: usize) {
        let last = self.m[G_NSH] as usize - 1;
        if i != last {
            let a = SH_BASE + i * SH_N; let b = SH_BASE + last * SH_N;
            for k in 0..SH_N { self.m[a + k] = self.m[b + k]; }
        }
        self.m[G_NSH] = last as f64;
    }
    fn update_shots(&mut self, dt: f64) {
        let mut i = self.m[G_NSH] as i32 - 1;
        while i >= 0 {
            let s = SH_BASE + i as usize * SH_N;
            self.m[s + S_LIFE] -= dt;
            self.m[s + S_VY] -= self.m[s + S_G] * dt;
            self.m[s + S_X] += self.m[s + S_VX] * dt; self.m[s + S_Y] += self.m[s + S_VY] * dt;
            let mut dead = self.m[s + S_LIFE] <= 0.0 || self.m[s + S_Y] < -3.0;
            if !dead && World::solid(self.tile_at(fi(self.m[s + S_X]), fi(self.m[s + S_Y]))) {
                dead = true;
                let (x, y, pu) = (self.m[s + S_X], self.m[s + S_Y], self.m[s + S_PURPLE]);
                self.ev(EV_SHOTHIT, x, y, pu);
            }
            if !dead && self.m[P_DEAD] == 0.0 && fabs(self.m[P_X] - self.m[s + S_X]) < PW / 2.0 + self.m[s + S_R] &&
                self.m[s + S_Y] > self.m[P_Y] - self.m[s + S_R] && self.m[s + S_Y] < self.m[P_Y] + PH + self.m[s + S_R] {
                let sx = self.m[s + S_X];
                if self.hurt_player(sx) { dead = true; }
            }
            if dead { self.remove_shot(i as usize); }
            i -= 1;
        }
    }

    // ---------------------------------------------------------- enemies
    fn kill_enemy(&mut self, b: usize, by_dash: bool) {
        self.m[b + E_ALIVE] = 0.0;
        self.m[G_KILLS] += 1.0; self.m[G_SCORE] += 150.0;
        let (x, y, a) = (self.m[b + B_X], self.m[b + B_Y] + self.m[b + B_H] / 2.0, self.m[b + E_KIND] + if by_dash { 10.0 } else { 0.0 });
        self.ev(EV_KILL, x, y, a);
    }

    fn start_boss(&mut self) {
        let b = EN_BASE + self.m[G_BOSS] as usize * EN_N;
        self.m[G_BOSSACTIVE] = 1.0;
        self.set_door(true);
        self.m[b + B_X] = self.m[G_BSPECX]; self.m[b + B_Y] = (LEVEL_H + 1) as f64; self.m[b + B_VX] = 0.0; self.m[b + B_VY] = 0.0;
        self.m[b + E_STATE] = BS_INTRO as f64; self.m[b + E_HP] = self.m[b + E_HP0]; self.m[b + E_INV] = 0.0; self.m[b + E_ALIVE] = 1.0;
        let (x, y) = (self.m[b + B_X], self.m[b + B_Y]);
        self.ev(EV_BOSSSTART, x, y, 0.0);
    }
    fn reset_boss(&mut self) {
        let b = EN_BASE + self.m[G_BOSS] as usize * EN_N;
        self.m[G_BOSSACTIVE] = 0.0;
        self.m[b + E_STATE] = BS_SLEEP as f64; self.m[b + E_HP] = self.m[b + E_HP0]; self.m[b + E_ALIVE] = 1.0; self.m[b + E_INV] = 0.0;
        self.m[b + B_X] = self.m[G_BSPECX]; self.m[b + B_Y] = self.m[G_BSPECY];
        self.set_door(false);
        self.m[G_NSH] = 0.0;
    }
    fn damage_boss(&mut self) -> bool {
        let b = EN_BASE + self.m[G_BOSS] as usize * EN_N;
        let st = self.m[b + E_STATE] as usize;
        if self.m[b + E_INV] > 0.0 || st == BS_INTRO || st == BS_DYING { return false; }
        self.m[b + E_HP] -= 1.0; self.m[b + E_INV] = 1.1;
        self.m[G_FREEZE] = 0.1;
        let (x, y, hp) = (self.m[b + B_X], self.m[b + B_Y], self.m[b + E_HP]);
        self.ev(EV_BOSSHIT, x, y, hp);
        if self.m[b + E_HP] <= 0.0 {
            self.m[b + E_STATE] = BS_DYING as f64; self.m[b + E_ST] = 1.8; self.m[b + B_VX] = 0.0;
            self.m[G_NSH] = 0.0;
        }
        true
    }
    fn boss_land(&mut self, b: usize, variant: f64) {
        let (x, y) = (self.m[b + B_X], self.m[b + B_Y]);
        self.ev(EV_BOOM, x, y, variant);
    }

    fn update_boss(&mut self, b: usize, dt: f64) {
        let state = self.m[b + E_STATE] as usize;
        if state == BS_SLEEP { return; }
        self.m[b + E_INV] = fmax(0.0, self.m[b + E_INV] - dt);
        self.m[b + E_ST] -= dt;
        let dx = self.m[P_X] - self.m[b + B_X];
        let low_hp = self.m[b + E_HP] <= 3.0;
        if state == BS_INTRO {
            self.m[b + B_VY] = fmax(self.m[b + B_VY] - 60.0 * dt, -30.0);
            self.move_body(b, dt, false);
            if self.m[b + B_GND] != 0.0 { self.m[b + E_STATE] = BS_IDLE as f64; self.m[b + E_ST] = 1.0; self.boss_land(b, 1.0); }
        } else if state == BS_IDLE {
            let d = sign(dx); self.m[b + E_DIR] = if d != 0.0 { d } else { 1.0 };
            self.m[b + B_VX] = 0.0; self.m[b + B_VY] = -2.0;
            self.move_body(b, dt, false);
            if self.m[b + E_ST] <= 0.0 {
                let r = self.rnd();
                if r < 0.5 {
                    self.m[b + E_STATE] = BS_JUMP as f64; self.m[b + B_VY] = 25.0; self.m[b + B_VX] = clamp(dx / 1.2, -11.0, 11.0);
                    let (x, y) = (self.m[b + B_X], self.m[b + B_Y]);
                    self.ev(EV_SPRING, x, y, 1.0);
                } else if r < 0.8 {
                    self.m[b + E_STATE] = BS_SHOOT as f64; self.m[b + E_ST] = 0.5; self.m[b + E_SHOTS] = if low_hp { 5.0 } else { 3.0 };
                } else {
                    self.m[b + E_STATE] = BS_CHARGE as f64; self.m[b + E_ST] = 1.3; self.m[b + E_DIR] = if d != 0.0 { d } else { 1.0 };
                }
            }
        } else if state == BS_JUMP {
            self.m[b + B_VY] = fmax(self.m[b + B_VY] - 62.0 * dt, -30.0);
            self.move_body(b, dt, false);
            if self.m[b + B_HX] != 0.0 { self.m[b + B_VX] = 0.0; }
            if self.m[b + B_GND] != 0.0 {
                self.m[b + E_STATE] = BS_RECOVER as f64; self.m[b + E_ST] = if low_hp { 0.8 } else { 1.2 }; self.m[b + B_VX] = 0.0;
                self.boss_land(b, 0.0);
                let (x, y) = (self.m[b + B_X], self.m[b + B_Y]);
                self.shoot(x - 1.3, y + 0.35, -7.0, 0.0, 0.0, 4.0, 0.7, 1.0);
                self.shoot(x + 1.3, y + 0.35, 7.0, 0.0, 0.0, 4.0, 0.7, 1.0);
            }
        } else if state == BS_RECOVER {
            self.m[b + B_VX] = 0.0; self.m[b + B_VY] = -2.0;
            self.move_body(b, dt, false);
            if self.m[b + E_ST] <= 0.0 { self.m[b + E_STATE] = BS_IDLE as f64; self.m[b + E_ST] = if low_hp { 0.35 } else { 0.7 }; }
        } else if state == BS_SHOOT {
            self.m[b + B_VX] = 0.0; self.m[b + B_VY] = -2.0; self.move_body(b, dt, false);
            let d = sign(dx); self.m[b + E_DIR] = if d != 0.0 { d } else { 1.0 };
            if self.m[b + E_ST] <= 0.0 && self.m[b + E_SHOTS] > 0.0 {
                let mut ux = self.m[P_X] - self.m[b + B_X];
                let mut uy = self.m[P_Y] + 0.4 - (self.m[b + B_Y] + 1.4);
                let mut len = (ux * ux + uy * uy).sqrt();
                if len < 1e-6 { ux = 1.0; uy = 0.0; len = 1.0; }
                ux /= len; uy /= len;
                let off = (self.rnd() - 0.5) * 0.35;
                let mut vx = ux - uy * off;
                let mut vy = uy + ux * off;
                let l2 = (vx * vx + vy * vy).sqrt();
                vx = vx / l2 * 7.5; vy = vy / l2 * 7.5;
                let (sx, sy, dir) = (self.m[b + B_X] + self.m[b + E_DIR] * 1.1, self.m[b + B_Y] + 1.4, self.m[b + E_DIR]);
                self.shoot(sx, sy, vx, vy, 0.0, 5.0, 0.6, 1.0);
                self.ev(EV_SHOOT, sx, sy, dir);
                self.m[b + E_SHOTS] -= 1.0; self.m[b + E_ST] = 0.3;
            } else if self.m[b + E_SHOTS] <= 0.0 && self.m[b + E_ST] <= 0.0 { self.m[b + E_STATE] = BS_IDLE as f64; self.m[b + E_ST] = 0.7; }
        } else if state == BS_CHARGE {
            self.m[b + B_VY] = fmax(self.m[b + B_VY] - 60.0 * dt, -30.0);
            if self.m[b + E_ST] > 0.75 { self.m[b + B_VX] = 0.0; }
            else { self.m[b + B_VX] = self.m[b + E_DIR] * (if low_hp { 15.0 } else { 12.0 }); }
            self.move_body(b, dt, false);
            if self.m[b + E_ST] <= 0.75 && self.m[b + B_HX] != 0.0 {
                self.m[b + E_STATE] = BS_RECOVER as f64; self.m[b + E_ST] = 1.3; self.m[b + B_VX] = 0.0;
                self.boss_land(b, 2.0);
            } else if self.m[b + E_ST] <= 0.0 { self.m[b + E_STATE] = BS_RECOVER as f64; self.m[b + E_ST] = 0.8; self.m[b + B_VX] = 0.0; }
        } else if state == BS_DYING {
            self.m[b + B_VX] = 0.0;
            if self.rnd() < 0.5 {
                let rx = self.rndr(-1.2, 1.2);
                let ry = self.rndr(0.0, 2.4);
                let (x, y) = (self.m[b + B_X] + rx, self.m[b + B_Y] + ry);
                self.ev(EV_BOSSEXPLODE, x, y, 0.0);
            }
            if self.m[b + E_ST] <= 0.0 {
                self.m[b + E_ALIVE] = 0.0;
                self.m[G_SCORE] += 3000.0; self.m[G_BOSSKILLED] = 1.0; self.m[G_BOSSACTIVE] = 0.0;
                self.set_door(false);
                let (x, y) = (self.m[b + B_X], self.m[b + B_Y]);
                self.add_goal(x, 2.0);
                self.ev(EV_BOSSDEAD, x, y, 0.0);
            }
        }
    }

    fn update_enemies(&mut self, dt: f64) {
        let n = self.m[G_NEN] as usize;
        for i in 0..n {
            let b = EN_BASE + i * EN_N;
            if self.m[b + E_ALIVE] == 0.0 { continue; }
            self.m[b + E_T] += dt;
            let kind = self.m[b + E_KIND] as usize;
            if kind == EK_SLIME {
                self.m[b + B_VY] = fmax(self.m[b + B_VY] - 55.0 * dt, -25.0);
                self.m[b + B_VX] = self.m[b + E_DIR] * 1.9;
                self.move_body(b, dt, false);
                if self.m[b + B_HX] != 0.0 { self.m[b + E_DIR] = -self.m[b + E_DIR]; }
                else if self.m[b + B_GND] != 0.0 {
                    let tx = fi(self.m[b + B_X] + self.m[b + E_DIR] * (self.m[b + B_W] / 2.0 + 0.12));
                    let ty = fi(self.m[b + B_Y] - 0.1);
                    let t = self.tile_at(tx, ty);
                    if !World::solid(t) && t != 2.0 { self.m[b + E_DIR] = -self.m[b + E_DIR]; }
                    else if self.tile_at(tx, fi(self.m[b + B_Y] + 0.1)) == 3.0 { self.m[b + E_DIR] = -self.m[b + E_DIR]; }
                }
            } else if kind == EK_BAT {
                let near = fabs(self.m[P_X] - self.m[b + E_OX]) < 8.0;
                if near && self.m[P_DEAD] == 0.0 { self.m[b + E_OX] += sign(self.m[P_X] - self.m[b + E_OX]) * 0.9 * dt; }
                let px = self.m[b + B_X];
                self.m[b + B_X] = self.m[b + E_OX] + fsin(self.m[b + E_T] * 1.3) * 3.2;
                self.m[b + B_Y] = self.m[b + E_OY] + fsin(self.m[b + E_T] * 2.4) * 0.6;
                let d = sign(self.m[b + B_X] - px);
                if d != 0.0 { self.m[b + E_DIR] = d; }
            } else if kind == EK_SAW {
                self.m[b + B_X] += self.m[b + B_VX] * dt;
                let ahead = fi(self.m[b + B_X] + sign(self.m[b + B_VX]) * 0.5);
                let floor_ahead = self.tile_at(ahead, fi(self.m[b + B_Y] - 0.2));
                if World::solid(self.tile_at(ahead, fi(self.m[b + B_Y] + 0.4))) || (!World::solid(floor_ahead) && floor_ahead != 2.0) ||
                    fabs(self.m[b + B_X] - self.m[b + E_OX]) > 3.0 {
                    self.m[b + B_VX] = -self.m[b + B_VX]; self.m[b + B_X] += self.m[b + B_VX] * dt * 2.0;
                }
            } else if kind == EK_TURRET {
                let d = sign(self.m[P_X] - self.m[b + B_X]);
                self.m[b + E_DIR] = if d != 0.0 { d } else { 1.0 };
                self.m[b + E_CD] -= dt;
                let dx = fabs(self.m[P_X] - self.m[b + B_X]);
                let dy = fabs(self.m[P_Y] - self.m[b + B_Y]);
                if self.m[b + E_CD] <= 0.0 && dx < fmin(13.0, self.m[G_VIEWW] / 2.0 + 1.0) && dy < 7.0 && self.m[P_DEAD] == 0.0 {
                    self.m[b + E_CD] = 2.2;
                    let dir = self.m[b + E_DIR];
                    let (x, y) = (self.m[b + B_X], self.m[b + B_Y]);
                    self.shoot(x + dir * 0.6, y + 0.5, dir * 6.5, 0.0, 0.0, 6.0, 0.6, 0.0);
                    self.ev(EV_SHOOT, x + dir * 0.7, y + 0.5, dir);
                }
            } else if kind == EK_BOSS { self.update_boss(b, dt); }
        }
    }

    // ----------------------------------------------------- interactions
    fn update_interactions(&mut self, dt: f64) {
        if self.m[P_DEAD] != 0.0 { return; }
        let nen = self.m[G_NEN] as usize;
        for i in 0..nen {
            let e = EN_BASE + i * EN_N;
            if self.m[e + E_ALIVE] == 0.0 { continue; }
            let kind = self.m[e + E_KIND] as usize;
            if kind == EK_BOSS && (self.m[e + E_STATE] == BS_SLEEP as f64 || self.m[e + E_STATE] == BS_DYING as f64) { continue; }
            if !self.box_hit(P_X, e) { continue; }
            if kind == EK_BOSS {
                if self.m[P_VY] < 0.0 && self.m[P_PREVY] >= self.m[e + B_Y] + self.m[e + B_H] * 0.55 && self.m[P_DASHT] <= 0.0 {
                    if self.damage_boss() { self.stomp_bounce(true); }
                    else { self.m[P_VY] = fmax(self.m[P_VY], 8.0); }
                } else { let x = self.m[e + B_X]; self.hurt_player(x); }
            } else if self.m[e + E_STOMP] == 0.0 {
                let x = self.m[e + B_X]; self.hurt_player(x);
            } else if self.m[P_DASHT] > 0.0 {
                self.kill_enemy(e, true);
            } else if self.m[P_VY] < 0.0 && self.m[P_PREVY] >= self.m[e + B_Y] + self.m[e + B_H] * 0.5 {
                self.kill_enemy(e, false);
                self.stomp_bounce(false);
                self.m[G_FREEZE] = 0.04;
            } else { let x = self.m[e + B_X]; self.hurt_player(x); }
        }

        let nc = self.m[G_NCOINS] as usize;
        for i in 0..nc {
            let c = COIN_BASE + i * COIN_N;
            if self.m[c + C_GOT] != 0.0 { continue; }
            let r = self.m[c + C_R];
            if fabs(self.m[P_X] - self.m[c + C_X]) < r + PW / 2.0 && fabs(self.m[P_Y] + 0.45 - self.m[c + C_Y]) < r + 0.45 {
                let kind = self.m[c + C_KIND] as usize;
                let (cx, cy) = (self.m[c + C_X], self.m[c + C_Y]);
                if kind == SK_HEART {
                    if self.m[P_HP] >= self.m[P_MAXHP] { self.m[G_SCORE] += 250.0; self.ev(EV_HEART, cx, cy, 0.0); }
                    else { self.m[P_HP] += 1.0; self.ev(EV_HEART, cx, cy, 1.0); }
                } else if kind == SK_GEM {
                    self.m[G_COINS] += 5.0; self.m[G_SCORE] += 500.0; self.ev(EV_GEM, cx, cy, 0.0);
                } else {
                    self.m[G_COINS] += 1.0; self.m[G_SCORE] += 100.0; self.ev(EV_COIN, cx, cy, 0.0);
                }
                self.m[c + C_GOT] = 1.0;
            }
        }

        let ns = self.m[G_NSPR] as usize;
        for i in 0..ns {
            let s = SPR_BASE + i * SPR_N;
            self.m[s + SP_T] = fmax(0.0, self.m[s + SP_T] - dt);
            if fabs(self.m[P_X] - self.m[s + SP_X]) < 0.7 && self.m[P_Y] >= self.m[s + SP_Y] - 0.1 && self.m[P_Y] < self.m[s + SP_Y] + 0.55 && self.m[P_VY] <= 0.5 {
                self.m[P_VY] = 31.0; self.m[P_GND] = 0.0; self.m[P_JUMPS] = 1.0; self.m[P_AIRDASH] = 0.0; self.m[P_DASHT] = 0.0; self.m[P_COYOTE] = 0.0;
                self.m[s + SP_T] = 0.3;
                let (x, y) = (self.m[s + SP_X], self.m[s + SP_Y] + 0.4);
                self.ev(EV_SPRING, x, y, 0.0);
            }
        }

        let nk = self.m[G_NCK] as usize;
        for i in 0..nk {
            let c = CKT_BASE + i * CKT_N;
            if self.m[c + CK_ON] == 0.0 && fabs(self.m[P_X] - self.m[c + CK_X]) < 1.3 && fabs(self.m[P_Y] - self.m[c + CK_Y]) < 2.0 {
                for j in 0..nk { self.m[CKT_BASE + j * CKT_N + CK_ON] = 0.0; }
                self.m[c + CK_ON] = 1.0;
                self.m[G_CHKX] = self.m[c + CK_X]; self.m[G_CHKY] = self.m[c + CK_Y];
                let (x, y) = (self.m[c + CK_X], self.m[c + CK_Y]);
                self.ev(EV_CHECK, x, y, 0.0);
            }
        }

        let bi = self.m[G_BOSS] as i32;
        if bi >= 0 {
            let b = EN_BASE + bi as usize * EN_N;
            if self.m[b + E_STATE] == BS_SLEEP as f64 && self.m[b + E_ALIVE] != 0.0 && self.m[P_X] > self.m[G_BTRIG] { self.start_boss(); }
        }

        if self.m[G_GOALON] != 0.0 && fabs(self.m[P_X] - self.m[G_GOALX]) < 0.9 && fabs(self.m[P_Y] + 0.5 - (self.m[G_GOALY] + 1.1)) < 1.5 {
            self.m[G_MODE] = MODE_CLEAR as f64;
            let (x, y) = (self.m[P_X], self.m[P_Y]);
            self.ev(EV_COMPLETE, x, y, 0.0);
        }
    }

    fn step(&mut self, dt: f64) {
        if self.m[G_FREEZE] > 0.0 { self.m[G_FREEZE] -= dt; return; }
        if self.m[G_MODE] as usize != MODE_PLAY {
            self.update_platforms(dt); self.update_enemies(dt); self.update_shots(dt);
            return;
        }
        self.m[G_TIME] += dt;
        self.update_platforms(dt);
        if self.m[P_DEAD] == 0.0 { self.update_player(dt); }
        else { self.m[P_DEADT] += dt; if self.m[P_DEADT] > 1.4 { self.respawn(); } }
        self.update_enemies(dt);
        self.update_shots(dt);
        self.update_interactions(dt);
    }

    fn advance(&mut self, rdt: f64) -> i32 {
        self.m[G_EVN] = 0.0;
        let mut acc = self.m[G_ACC] + rdt;
        let mut n = 0;
        while acc >= DT && n < 12 { self.step(DT); acc -= DT; n += 1; }
        if n >= 12 { acc = 0.0; }
        self.m[G_ACC] = acc; self.m[G_STEPS] = n as f64;
        if n > 0 { self.m[IN_JUMPPRESS] = 0.0; self.m[IN_DASHPRESS] = 0.0; }
        n
    }
}

#[no_mangle] pub extern "C" fn init() { world().init(); }
#[no_mangle] pub extern "C" fn load_level() { world().load_level(); }
#[no_mangle] pub extern "C" fn advance(dt: f64) -> i32 { world().advance(dt) }
#[no_mangle] pub extern "C" fn mem_get(i: i32) -> f64 { world().m[i as usize] }
#[no_mangle] pub extern "C" fn mem_set(i: i32, v: f64) { world().m[i as usize] = v; }
#[no_mangle] pub extern "C" fn mem_ptr() -> i32 { world().m.as_ptr() as usize as i32 }
