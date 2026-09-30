//! Slop Runner simulation core - Zig port (wasm32-freestanding).
//! Mirrors core/js/core.js line by line; all state lives in the flat array M.
const A = @import("abi.zig");

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

var M: [A.MEM_SIZE]f64 = [_]f64{0} ** A.MEM_SIZE;

fn f(x: anytype) f64 { return @floatFromInt(x); }
fn fi(x: f64) i32 { return @intFromFloat(@floor(x)); }
fn iu(x: f64) usize { return @intFromFloat(x); }
fn ii(x: f64) i32 { return @intFromFloat(x); }
fn sign(v: f64) f64 { return if (v > 0) 1.0 else if (v < 0) -1.0 else 0.0; }
fn fmin(a: f64, b: f64) f64 { return if (a < b) a else b; }
fn fmax(a: f64, b: f64) f64 { return if (a > b) a else b; }
fn fabs(a: f64) f64 { return if (a < 0) -a else a; }
fn clamp(v: f64, a: f64, b: f64) f64 { return if (v < a) a else if (v > b) b else v; }
fn approach(v: f64, t: f64, s: f64) f64 { return if (v < t) fmin(v + s, t) else fmax(v - s, t); }
fn b2f(b: bool) f64 { return if (b) 1.0 else 0.0; }

fn fsin(x: f64) f64 {
    const k = @floor(x * 0.15915494309189535 + 0.5);
    var r = x - k * 6.283185307179586;
    if (r > 1.5707963267948966) {
        r = 3.141592653589793 - r;
    } else if (r < -1.5707963267948966) {
        r = -3.141592653589793 - r;
    }
    const r2 = r * r;
    return r * (1.0 + r2 * (-0.16666666666666666 + r2 * (0.008333333333333333 + r2 * (-0.0001984126984126984 +
        r2 * (2.7557319223985893e-6 + r2 * (-2.505210838544172e-8 + r2 * 1.6059043836821613e-10))))));
}
fn fcos(x: f64) f64 { return fsin(x + 1.5707963267948966); }

fn rnd() f64 {
    var s = M[A.G_RNG] * 1664525.0 + 1013904223.0;
    s = s - @floor(s / 4294967296.0) * 4294967296.0;
    M[A.G_RNG] = s;
    return s / 4294967296.0;
}
fn rndr(a: f64, b: f64) f64 { return a + rnd() * (b - a); }
fn ev(t: usize, x: f64, y: f64, a: f64) void {
    const n = iu(M[A.G_EVN]);
    if (n < A.EV_MAX) {
        const i = A.EV_BASE + n * A.EV_N;
        M[i] = f(t);
        M[i + 1] = x;
        M[i + 2] = y;
        M[i + 3] = a;
        M[A.G_EVN] = f(n + 1);
    }
}

fn tileAt(tx: i32, ty: i32) f64 {
    const lw = ii(M[A.G_LW]);
    if (tx < 0 or tx >= lw) return 1;
    if (ty < 0 or ty >= LEVEL_H) return 0;
    return M[A.TILE_BASE + @as(usize, @intCast(ty * lw + tx))];
}
fn isSolid(t: f64) bool { return t == 1 or t == 4; }
fn setTile(tx: i32, ty: i32, v: f64) void {
    const lw = ii(M[A.G_LW]);
    M[A.TILE_BASE + @as(usize, @intCast(ty * lw + tx))] = v;
}

fn moveBody(b: usize, dt: f64, drop: bool) void {
    M[b + A.B_HX] = 0;
    M[b + A.B_HY] = 0;
    const hw = M[b + A.B_W] / 2;
    M[b + A.B_X] += M[b + A.B_VX] * dt;
    const y0 = fi(M[b + A.B_Y] + 0.02);
    const y1 = fi(M[b + A.B_Y] + M[b + A.B_H] - 0.02);
    const vx = M[b + A.B_VX];
    if (vx > 0) {
        const tx = fi(M[b + A.B_X] + hw);
        var ty = y0;
        while (ty <= y1) : (ty += 1) {
            if (isSolid(tileAt(tx, ty))) {
                M[b + A.B_X] = f(tx) - hw - 1e-4;
                M[b + A.B_VX] = 0;
                M[b + A.B_HX] = 1;
                break;
            }
        }
    } else if (vx < 0) {
        const tx = fi(M[b + A.B_X] - hw);
        var ty = y0;
        while (ty <= y1) : (ty += 1) {
            if (isSolid(tileAt(tx, ty))) {
                M[b + A.B_X] = f(tx) + 1 + hw + 1e-4;
                M[b + A.B_VX] = 0;
                M[b + A.B_HX] = -1;
                break;
            }
        }
    }
    const prevY = M[b + A.B_Y];
    M[b + A.B_Y] += M[b + A.B_VY] * dt;
    const x0 = fi(M[b + A.B_X] - hw + 1e-3);
    const x1 = fi(M[b + A.B_X] + hw - 1e-3);
    M[b + A.B_GND] = 0;
    if (M[b + A.B_VY] <= 0) {
        const ty = fi(M[b + A.B_Y]);
        var tx = x0;
        while (tx <= x1) : (tx += 1) {
            const t = tileAt(tx, ty);
            if (isSolid(t) or (t == 2 and !drop and prevY >= f(ty) + 1 - 0.02)) {
                M[b + A.B_Y] = f(ty) + 1;
                M[b + A.B_VY] = 0;
                M[b + A.B_GND] = 1;
                M[b + A.B_HY] = -1;
                break;
            }
        }
    } else {
        const ty = fi(M[b + A.B_Y] + M[b + A.B_H]);
        var tx = x0;
        while (tx <= x1) : (tx += 1) {
            if (isSolid(tileAt(tx, ty))) {
                M[b + A.B_Y] = f(ty) - M[b + A.B_H] - 1e-4;
                M[b + A.B_VY] = 0;
                M[b + A.B_HY] = 1;
                break;
            }
        }
    }
}
fn boxHit(a: usize, b: usize) bool {
    return fabs(M[a + A.B_X] - M[b + A.B_X]) < (M[a + A.B_W] + M[b + A.B_W]) / 2 and
        M[a + A.B_Y] < M[b + A.B_Y] + M[b + A.B_H] and M[a + A.B_Y] + M[a + A.B_H] > M[b + A.B_Y];
}
fn enBase(i: usize) usize { return A.EN_BASE + i * A.EN_N; }
fn plBase(i: usize) usize { return A.PLT_BASE + i * A.PLT_N; }
fn shBase(i: usize) usize { return A.SH_BASE + i * A.SH_N; }

fn setDoor(closed: bool) void {
    if (M[A.G_DOORON] == 0) return;
    var ty = ii(M[A.G_DOORY0]);
    while (ty <= ii(M[A.G_DOORY1])) : (ty += 1) setTile(ii(M[A.G_DOORX]), ty, if (closed) 4 else 0);
    M[A.G_DOORCLOSED] = b2f(closed);
    ev(A.EV_DOOR, M[A.G_DOORX], 0, b2f(closed));
}

fn addEnemy(kind: usize, x: f64, y: f64) void {
    const i = iu(M[A.G_NEN]);
    if (i >= A.EN_MAX) return;
    M[A.G_NEN] = f(i + 1);
    const b = enBase(i);
    var k: usize = 0;
    while (k < A.EN_N) : (k += 1) M[b + k] = 0;
    M[b + A.B_X] = x;
    M[b + A.B_Y] = y;
    M[b + A.B_W] = 0.8;
    M[b + A.B_H] = 0.62;
    M[b + A.E_KIND] = f(kind);
    M[b + A.E_ALIVE] = 1;
    M[b + A.E_STOMP] = 1;
    const r0 = rnd();
    M[b + A.E_DIR] = if (r0 < 0.5) -1 else 1;
    const r1 = rnd();
    M[b + A.E_T] = r1 * 10;
    M[b + A.E_OX] = x;
    M[b + A.E_OY] = y;
    const r2 = rnd();
    M[b + A.E_CD] = 1 + r2;
    if (kind == A.EK_BAT) {
        M[b + A.B_W] = 0.75;
        M[b + A.B_H] = 0.5;
    } else if (kind == A.EK_SAW) {
        M[b + A.B_W] = 0.78;
        M[b + A.B_H] = 0.78;
        M[b + A.E_STOMP] = 0;
        M[b + A.E_DIR] = 1;
        M[b + A.B_VX] = 3;
    } else if (kind == A.EK_TURRET) {
        M[b + A.B_W] = 0.9;
        M[b + A.B_H] = 0.8;
        M[b + A.E_STOMP] = 0;
    } else if (kind == A.EK_BOSS) {
        M[b + A.B_W] = 2.5;
        M[b + A.B_H] = 2.3;
        M[b + A.E_HP] = 8;
        M[b + A.E_HP0] = 8;
        M[b + A.E_STATE] = f(A.BS_SLEEP);
        M[b + A.E_DIR] = -1;
        M[A.G_BOSS] = f(i);
    }
}
fn addPlatform(kind: usize, x: f64, ty: f64) void {
    const i = iu(M[A.G_NPLT]);
    if (i >= A.PLT_MAX) return;
    M[A.G_NPLT] = f(i + 1);
    const p = plBase(i);
    var k: usize = 0;
    while (k < A.PLT_N) : (k += 1) M[p + k] = 0;
    M[p + A.PT_KIND] = f(kind);
    M[p + A.PT_X] = x;
    M[p + A.PT_X0] = x;
    M[p + A.PT_TOP] = ty + 0.75;
    M[p + A.PT_TOP0] = ty + 0.75;
    M[p + A.PT_PREVTOP] = ty + 0.75;
    M[p + A.PT_W] = if (kind == 2) 1 else 3;
    M[p + A.PT_SOLID] = 1;
    const r = rnd();
    M[p + A.PT_T] = r * 6;
    M[p + A.PT_STATE] = f(A.CS_IDLE);
}
fn addGoal(x: f64, y: f64) void {
    M[A.G_GOALON] = 1;
    M[A.G_GOALX] = x;
    M[A.G_GOALY] = y;
}

fn resetPlayer(x: f64, y: f64) void {
    M[A.P_X] = x; M[A.P_Y] = y; M[A.P_VX] = 0; M[A.P_VY] = 0; M[A.P_FACE] = 1; M[A.P_GND] = 0;
    M[A.P_COYOTE] = 0; M[A.P_BUFFER] = 0; M[A.P_JUMPS] = 1; M[A.P_WALLDIR] = 0; M[A.P_WALLGRACE] = 0; M[A.P_WALLLOCK] = 0;
    M[A.P_DASHT] = 0; M[A.P_DASHCD] = 0; M[A.P_AIRDASH] = 0; M[A.P_INV] = 0; M[A.P_DROPT] = 0; M[A.P_DEAD] = 0; M[A.P_DEADT] = 0;
    M[A.P_RIDING] = -1; M[A.P_SAFEX] = x; M[A.P_SAFEY] = y; M[A.P_SAFET] = 0; M[A.P_PREVY] = y; M[A.P_WASGND] = 0;
}

export fn init() void {
    var i: usize = 0;
    while (i < A.MEM_SIZE) : (i += 1) M[i] = 0;
    M[A.G_RNG] = 12345;
    M[A.G_VIEWW] = 20;
    M[A.P_W] = PW;
    M[A.P_H] = PH;
    M[A.P_MAXHP] = 3;
    M[A.P_HP] = 3;
    M[A.P_RIDING] = -1;
    M[A.G_BOSS] = -1;
}

export fn load_level() void {
    M[A.G_NCOINS] = 0; M[A.G_NEN] = 0; M[A.G_NPLT] = 0; M[A.G_NSH] = 0; M[A.G_NSPR] = 0; M[A.G_NCK] = 0; M[A.G_BOSS] = -1;
    M[A.G_TIME] = 0; M[A.G_COINS] = 0; M[A.G_KILLS] = 0; M[A.G_DEATHS] = 0; M[A.G_SCORE] = 0; M[A.G_BOSSKILLED] = 0;
    M[A.G_BOSSACTIVE] = 0; M[A.G_FREEZE] = 0; M[A.G_COINSTOTAL] = 0; M[A.G_DOORCLOSED] = 0; M[A.G_ACC] = 0; M[A.G_EVN] = 0;
    const n = iu(M[A.G_NSPEC]);
    var s: usize = 0;
    while (s < n) : (s += 1) {
        const kind = iu(M[A.SPEC_BASE + s * A.SPEC_N]);
        const x = M[A.SPEC_BASE + s * A.SPEC_N + 1];
        const y = M[A.SPEC_BASE + s * A.SPEC_N + 2];
        if (kind <= A.SK_HEART) {
            const i = iu(M[A.G_NCOINS]);
            if (i < A.COIN_MAX) {
                M[A.G_NCOINS] = f(i + 1);
                const c = A.COIN_BASE + i * A.COIN_N;
                M[c + A.C_KIND] = f(kind);
                M[c + A.C_X] = x;
                M[c + A.C_Y] = y + 0.5;
                M[c + A.C_GOT] = 0;
                M[c + A.C_R] = if (kind == A.SK_COIN) 0.55 else 0.65;
                if (kind == A.SK_COIN) {
                    M[A.G_COINSTOTAL] += 1;
                } else if (kind == A.SK_GEM) {
                    M[A.G_COINSTOTAL] += 5;
                }
            }
        } else if (kind == A.SK_SLIME) {
            addEnemy(A.EK_SLIME, x, y);
        } else if (kind == A.SK_BAT) {
            addEnemy(A.EK_BAT, x, y + 0.5);
        } else if (kind == A.SK_SAW) {
            addEnemy(A.EK_SAW, x, y + 0.02);
        } else if (kind == A.SK_TURRET) {
            addEnemy(A.EK_TURRET, x, y);
        } else if (kind == A.SK_SPRING) {
            const i = iu(M[A.G_NSPR]);
            if (i < A.SPR_MAX) {
                M[A.G_NSPR] = f(i + 1);
                const q = A.SPR_BASE + i * A.SPR_N;
                M[q + A.SP_X] = x;
                M[q + A.SP_Y] = y;
                M[q + A.SP_T] = 0;
            }
        } else if (kind == A.SK_PLAT_H) {
            addPlatform(0, x, y);
        } else if (kind == A.SK_PLAT_V) {
            addPlatform(1, x, y);
        } else if (kind == A.SK_CRUMBLE) {
            addPlatform(2, x, y);
        } else if (kind == A.SK_CHECK) {
            const i = iu(M[A.G_NCK]);
            if (i < A.CKT_MAX) {
                M[A.G_NCK] = f(i + 1);
                const q = A.CKT_BASE + i * A.CKT_N;
                M[q + A.CK_X] = x;
                M[q + A.CK_Y] = y;
                M[q + A.CK_ON] = 0;
            }
        } else if (kind == A.SK_BOSS) {
            addEnemy(A.EK_BOSS, x, y);
        }
    }
    M[A.P_MAXHP] = 3;
    M[A.P_HP] = 3;
    M[A.G_CHKX] = M[A.G_SPAWNX];
    M[A.G_CHKY] = M[A.G_SPAWNY];
    resetPlayer(M[A.G_SPAWNX], M[A.G_SPAWNY]);
}

// ------------------------------------------------------------------ player
fn killPlayer() void {
    M[A.P_DEAD] = 1;
    M[A.P_DEADT] = 0;
    M[A.P_HP] = 0;
    M[A.G_DEATHS] += 1;
    ev(A.EV_DIE, M[A.P_X], M[A.P_Y] + 0.5, 0);
}
fn hurtPlayer(srcX: f64) bool {
    if (M[A.P_INV] > 0 or M[A.P_DASHT] > 0 or M[A.P_DEAD] != 0) return false;
    M[A.P_HP] -= 1;
    M[A.P_INV] = INVULN;
    M[A.G_FREEZE] = 0.08;
    ev(A.EV_HURT, M[A.P_X], M[A.P_Y] + 0.5, 0);
    M[A.P_VX] = (if (M[A.P_X] < srcX) @as(f64, -1) else 1) * 8.5;
    M[A.P_VY] = 12;
    M[A.P_WALLLOCK] = 0.18;
    M[A.P_DASHT] = 0;
    if (M[A.P_HP] <= 0) killPlayer();
    return true;
}
fn pitFall() void {
    if (M[A.P_DEAD] != 0) return;
    M[A.P_HP] -= 1;
    ev(A.EV_PIT, M[A.P_X], M[A.P_Y], 0);
    if (M[A.P_HP] <= 0) {
        killPlayer();
        return;
    }
    M[A.P_X] = M[A.P_SAFEX];
    M[A.P_Y] = M[A.P_SAFEY] + 0.05;
    M[A.P_VX] = 0;
    M[A.P_VY] = 0;
    M[A.P_INV] = 2;
    ev(A.EV_RESPAWN, M[A.P_X], M[A.P_Y] + 0.4, 1);
}
fn respawn() void {
    resetPlayer(M[A.G_CHKX], M[A.G_CHKY]);
    M[A.P_HP] = M[A.P_MAXHP];
    M[A.P_INV] = 2;
    if (M[A.G_BOSS] >= 0 and M[A.G_BOSSACTIVE] != 0) resetBoss();
    ev(A.EV_RESPAWN, M[A.P_X], M[A.P_Y] + 0.5, 0);
}
fn stompBounce(strong: bool) void {
    M[A.P_VY] = if (M[A.IN_JUMPHELD] != 0) STOMP_BOUNCE + 3.5 else STOMP_BOUNCE;
    if (strong) M[A.P_VY] += 2;
    M[A.P_JUMPS] = 1;
    M[A.P_AIRDASH] = 0;
    M[A.P_GND] = 0;
    M[A.P_DASHT] = 0;
}

fn updatePlayer(dt: f64) void {
    const ax = M[A.IN_AX];
    M[A.P_PREVY] = M[A.P_Y];
    M[A.P_INV] = fmax(0, M[A.P_INV] - dt);
    M[A.P_DASHCD] -= dt;
    M[A.P_WALLLOCK] -= dt;
    M[A.P_BUFFER] -= dt;
    M[A.P_DROPT] -= dt;
    M[A.P_WALLGRACE] -= dt;

    if (M[A.IN_JUMPPRESS] != 0) {
        M[A.IN_JUMPPRESS] = 0;
        M[A.P_BUFFER] = BUFFER;
    }
    const wantDash = M[A.IN_DASHPRESS] != 0;
    if (wantDash) M[A.IN_DASHPRESS] = 0;

    const rid = ii(M[A.P_RIDING]);
    if (rid >= 0) {
        const p = plBase(@intCast(rid));
        if (M[p + A.PT_SOLID] != 0 and M[A.P_VY] <= 0) {
            M[A.P_X] += M[p + A.PT_DX];
            M[A.P_Y] = M[p + A.PT_TOP];
        }
    }

    if (M[A.P_GND] != 0) {
        M[A.P_COYOTE] = COYOTE;
        M[A.P_JUMPS] = 1;
        M[A.P_AIRDASH] = 0;
    } else {
        M[A.P_COYOTE] -= dt;
    }

    if (wantDash and M[A.P_DASHCD] <= 0 and M[A.P_AIRDASH] == 0) {
        M[A.P_DASHT] = DASH_TIME;
        M[A.P_DASHCD] = DASH_CD;
        M[A.P_DASHDIR] = if (ax != 0) ax else M[A.P_FACE];
        M[A.P_FACE] = M[A.P_DASHDIR];
        if (M[A.P_GND] == 0) M[A.P_AIRDASH] = 1;
        ev(A.EV_DASH, M[A.P_X], M[A.P_Y] + 0.4, M[A.P_DASHDIR]);
    }

    if (M[A.P_DASHT] > 0) {
        M[A.P_DASHT] -= dt;
        M[A.P_VX] = M[A.P_DASHDIR] * DASH_SPEED;
        M[A.P_VY] = 0;
        if (M[A.P_DASHT] <= 0) M[A.P_VX] = M[A.P_DASHDIR] * MAX_SPEED;
    } else {
        if (M[A.P_WALLLOCK] <= 0) {
            const target = ax * MAX_SPEED;
            var acc: f64 = undefined;
            if (ax == 0) {
                acc = if (M[A.P_GND] != 0) DECEL_G else DECEL_A;
            } else {
                acc = if (M[A.P_GND] != 0) ACCEL_G else ACCEL_A;
                if (sign(M[A.P_VX]) == -ax) acc *= 1.6;
            }
            M[A.P_VX] = approach(M[A.P_VX], target, acc * dt);
            if (ax != 0) M[A.P_FACE] = ax;
        }

        M[A.P_WALLDIR] = 0;
        if (M[A.P_GND] == 0 and ax != 0) {
            const tx = fi(M[A.P_X] + ax * (PW / 2 + 0.08));
            if (isSolid(tileAt(tx, fi(M[A.P_Y] + 0.3))) or isSolid(tileAt(tx, fi(M[A.P_Y] + 0.75)))) {
                M[A.P_WALLDIR] = ax;
                M[A.P_WALLGRACE] = 0.1;
                M[A.P_WALLMEM] = ax;
            }
        }

        if (M[A.P_BUFFER] > 0) {
            if (M[A.IN_DOWNHELD] != 0 and M[A.P_GND] != 0 and tileAt(fi(M[A.P_X]), fi(M[A.P_Y] - 0.05)) == 2) {
                M[A.P_DROPT] = 0.22;
                M[A.P_BUFFER] = 0;
                M[A.P_Y] -= 0.06;
                M[A.P_GND] = 0;
            } else if (M[A.P_COYOTE] > 0) {
                M[A.P_VY] = JUMP_V;
                M[A.P_BUFFER] = 0;
                M[A.P_COYOTE] = 0;
                M[A.P_GND] = 0;
                ev(A.EV_JUMP, M[A.P_X], M[A.P_Y] + 0.05, 0);
            } else if (M[A.P_WALLGRACE] > 0 and M[A.P_WALLMEM] != 0) {
                M[A.P_VX] = -M[A.P_WALLMEM] * WALL_JUMP_X;
                M[A.P_VY] = WALL_JUMP_Y;
                M[A.P_WALLLOCK] = WALL_LOCK;
                M[A.P_FACE] = -M[A.P_WALLMEM];
                M[A.P_BUFFER] = 0;
                M[A.P_WALLGRACE] = 0;
                M[A.P_JUMPS] = 1;
                M[A.P_AIRDASH] = 0;
                ev(A.EV_WALLJUMP, M[A.P_X] + M[A.P_WALLMEM] * 0.3, M[A.P_Y] + 0.5, M[A.P_WALLMEM]);
            } else if (M[A.P_JUMPS] > 0) {
                M[A.P_VY] = DOUBLE_V;
                M[A.P_JUMPS] -= 1;
                M[A.P_BUFFER] = 0;
                ev(A.EV_DOUBLE, M[A.P_X], M[A.P_Y] + 0.05, 0);
            }
        }

        const g = if (M[A.P_VY] > 0) (if (M[A.IN_JUMPHELD] != 0) GRAVITY else GRAVITY * 2.4) else FALL_GRAVITY;
        M[A.P_VY] = fmax(M[A.P_VY] - g * dt, -MAX_FALL);
        if (M[A.P_WALLDIR] != 0 and M[A.P_VY] < -WALL_SLIDE) M[A.P_VY] = -WALL_SLIDE;
    }

    const vyPre = M[A.P_VY];
    moveBody(A.P_X, dt, M[A.P_DROPT] > 0);

    M[A.P_RIDING] = -1;
    if (vyPre <= 0 and M[A.P_DASHT] <= 0) {
        const np = iu(M[A.G_NPLT]);
        var i: usize = 0;
        while (i < np) : (i += 1) {
            const p = plBase(i);
            if (M[p + A.PT_SOLID] == 0) continue;
            if (fabs(M[A.P_X] - M[p + A.PT_X]) < M[p + A.PT_W] / 2 + PW / 2 - 0.08 and M[A.P_PREVY] >= M[p + A.PT_PREVTOP] - 0.12 and
                M[A.P_Y] <= M[p + A.PT_TOP] + 0.02 and M[A.P_Y] >= M[p + A.PT_TOP] - 0.6)
            {
                M[A.P_Y] = M[p + A.PT_TOP];
                M[A.P_VY] = 0;
                M[A.P_GND] = 1;
                M[A.P_RIDING] = f(i);
                if (M[p + A.PT_KIND] == 2 and M[p + A.PT_STATE] == f(A.CS_IDLE)) {
                    M[p + A.PT_STATE] = f(A.CS_SHAKE);
                    M[p + A.PT_TIMER] = 0.45;
                }
                break;
            }
        }
    }

    if (M[A.P_GND] != 0 and M[A.P_WASGND] == 0 and vyPre < -9) ev(A.EV_LAND, M[A.P_X], M[A.P_Y] + 0.05, 0);
    if (M[A.P_HY] == 1 and M[A.P_VY] == 0) M[A.P_VY] = -1;
    M[A.P_WASGND] = M[A.P_GND];

    if (M[A.P_GND] != 0 and M[A.P_RIDING] < 0) {
        const ty = fi(M[A.P_Y]) - 1;
        if (isSolid(tileAt(fi(M[A.P_X] - 0.6), ty)) and isSolid(tileAt(fi(M[A.P_X] + 0.6), ty)) and
            isSolid(tileAt(fi(M[A.P_X] - 1.6), ty)) and isSolid(tileAt(fi(M[A.P_X] + 1.6), ty)) and
            tileAt(fi(M[A.P_X]), fi(M[A.P_Y])) != 3)
        {
            M[A.P_SAFET] += dt;
            if (M[A.P_SAFET] > 0.35) {
                M[A.P_SAFEX] = M[A.P_X];
                M[A.P_SAFEY] = M[A.P_Y];
            }
        } else M[A.P_SAFET] = 0;
    } else M[A.P_SAFET] = 0;

    if (M[A.P_INV] <= 0 and M[A.P_DASHT] <= 0) {
        const x0 = fi(M[A.P_X] - PW / 2);
        const x1 = fi(M[A.P_X] + PW / 2);
        const y0 = fi(M[A.P_Y]);
        const y1 = fi(M[A.P_Y] + PH * 0.5);
        var done = false;
        var ty = y0;
        while (ty <= y1 and !done) : (ty += 1) {
            var tx = x0;
            while (tx <= x1) : (tx += 1) {
                if (tileAt(tx, ty) == 3) {
                    if (M[A.P_X] + PW / 2 > f(tx) + 0.15 and M[A.P_X] - PW / 2 < f(tx) + 0.85 and M[A.P_Y] < f(ty) + 0.5) {
                        if (hurtPlayer(f(tx) + 0.5)) {
                            M[A.P_VY] = 15;
                            M[A.P_VX] = (if (M[A.P_X] < f(tx) + 0.5) @as(f64, -1) else 1) * 5;
                        }
                        done = true;
                        break;
                    }
                }
            }
        }
    }

    if (M[A.P_Y] < -2.5) pitFall();
}

// --------------------------------------------------------------- platforms
fn updatePlatforms(dt: f64) void {
    const n = iu(M[A.G_NPLT]);
    var i: usize = 0;
    while (i < n) : (i += 1) {
        const p = plBase(i);
        M[p + A.PT_PREVTOP] = M[p + A.PT_TOP];
        M[p + A.PT_DX] = 0;
        M[p + A.PT_DY] = 0;
        M[p + A.PT_T] += dt;
        const kind = ii(M[p + A.PT_KIND]);
        if (kind == 0) {
            const nx = M[p + A.PT_X0] + fsin(M[p + A.PT_T] * 1.05) * 2.4;
            M[p + A.PT_DX] = nx - M[p + A.PT_X];
            M[p + A.PT_X] = nx;
        } else if (kind == 1) {
            const nt = M[p + A.PT_TOP0] + (1 - fcos(M[p + A.PT_T] * 0.95)) / 2 * 5.0;
            M[p + A.PT_DY] = nt - M[p + A.PT_TOP];
            M[p + A.PT_TOP] = nt;
        } else {
            const st = iu(M[p + A.PT_STATE]);
            if (st == A.CS_SHAKE) {
                M[p + A.PT_TIMER] -= dt;
                if (M[p + A.PT_TIMER] <= 0) {
                    M[p + A.PT_STATE] = f(A.CS_FALL);
                    M[p + A.PT_SOLID] = 0;
                    M[p + A.PT_VY] = 0;
                    ev(A.EV_CRUMBLE, M[p + A.PT_X], M[p + A.PT_TOP], 0);
                }
            } else if (st == A.CS_FALL) {
                M[p + A.PT_VY] -= 32 * dt;
                M[p + A.PT_TOP] += M[p + A.PT_VY] * dt;
                if (M[p + A.PT_TOP] < -3) {
                    M[p + A.PT_STATE] = f(A.CS_GONE);
                    M[p + A.PT_TIMER] = 2.6;
                }
            } else if (st == A.CS_GONE) {
                M[p + A.PT_TIMER] -= dt;
                if (M[p + A.PT_TIMER] <= 0) {
                    M[p + A.PT_STATE] = f(A.CS_IDLE);
                    M[p + A.PT_SOLID] = 1;
                    M[p + A.PT_TOP] = M[p + A.PT_TOP0];
                    M[p + A.PT_PREVTOP] = M[p + A.PT_TOP];
                }
            }
        }
    }
}

// ------------------------------------------------------------------- shots
fn shoot(x: f64, y: f64, vx: f64, vy: f64, g: f64, life: f64, size: f64, purple: f64) void {
    const i = iu(M[A.G_NSH]);
    if (i >= A.SH_MAX) return;
    M[A.G_NSH] = f(i + 1);
    const s = shBase(i);
    M[s + A.S_X] = x;
    M[s + A.S_Y] = y;
    M[s + A.S_VX] = vx;
    M[s + A.S_VY] = vy;
    M[s + A.S_G] = g;
    M[s + A.S_LIFE] = life;
    M[s + A.S_R] = size * 0.36;
    M[s + A.S_PURPLE] = purple;
    M[s + A.S_SIZE] = size;
}
fn removeShot(i: usize) void {
    const last = iu(M[A.G_NSH]) - 1;
    if (i != last) {
        const a = shBase(i);
        const b = shBase(last);
        var k: usize = 0;
        while (k < A.SH_N) : (k += 1) M[a + k] = M[b + k];
    }
    M[A.G_NSH] = f(last);
}
fn updateShots(dt: f64) void {
    var i: i32 = ii(M[A.G_NSH]) - 1;
    while (i >= 0) : (i -= 1) {
        const s = shBase(@intCast(i));
        M[s + A.S_LIFE] -= dt;
        M[s + A.S_VY] -= M[s + A.S_G] * dt;
        M[s + A.S_X] += M[s + A.S_VX] * dt;
        M[s + A.S_Y] += M[s + A.S_VY] * dt;
        var dead = M[s + A.S_LIFE] <= 0 or M[s + A.S_Y] < -3;
        if (!dead and isSolid(tileAt(fi(M[s + A.S_X]), fi(M[s + A.S_Y])))) {
            dead = true;
            ev(A.EV_SHOTHIT, M[s + A.S_X], M[s + A.S_Y], M[s + A.S_PURPLE]);
        }
        if (!dead and M[A.P_DEAD] == 0 and fabs(M[A.P_X] - M[s + A.S_X]) < PW / 2 + M[s + A.S_R] and
            M[s + A.S_Y] > M[A.P_Y] - M[s + A.S_R] and M[s + A.S_Y] < M[A.P_Y] + PH + M[s + A.S_R])
        {
            if (hurtPlayer(M[s + A.S_X])) dead = true;
        }
        if (dead) removeShot(@intCast(i));
    }
}

// ----------------------------------------------------------------- enemies
fn killEnemy(b: usize, byDash: bool) void {
    M[b + A.E_ALIVE] = 0;
    M[A.G_KILLS] += 1;
    M[A.G_SCORE] += 150;
    ev(A.EV_KILL, M[b + A.B_X], M[b + A.B_Y] + M[b + A.B_H] / 2, M[b + A.E_KIND] + (if (byDash) @as(f64, 10) else 0));
}

fn startBoss() void {
    const b = enBase(iu(M[A.G_BOSS]));
    M[A.G_BOSSACTIVE] = 1;
    setDoor(true);
    M[b + A.B_X] = M[A.G_BSPECX];
    M[b + A.B_Y] = f(LEVEL_H + 1);
    M[b + A.B_VX] = 0;
    M[b + A.B_VY] = 0;
    M[b + A.E_STATE] = f(A.BS_INTRO);
    M[b + A.E_HP] = M[b + A.E_HP0];
    M[b + A.E_INV] = 0;
    M[b + A.E_ALIVE] = 1;
    ev(A.EV_BOSSSTART, M[b + A.B_X], M[b + A.B_Y], 0);
}
fn resetBoss() void {
    const b = enBase(iu(M[A.G_BOSS]));
    M[A.G_BOSSACTIVE] = 0;
    M[b + A.E_STATE] = f(A.BS_SLEEP);
    M[b + A.E_HP] = M[b + A.E_HP0];
    M[b + A.E_ALIVE] = 1;
    M[b + A.E_INV] = 0;
    M[b + A.B_X] = M[A.G_BSPECX];
    M[b + A.B_Y] = M[A.G_BSPECY];
    setDoor(false);
    M[A.G_NSH] = 0;
}
fn damageBoss() bool {
    const b = enBase(iu(M[A.G_BOSS]));
    const st = iu(M[b + A.E_STATE]);
    if (M[b + A.E_INV] > 0 or st == A.BS_INTRO or st == A.BS_DYING) return false;
    M[b + A.E_HP] -= 1;
    M[b + A.E_INV] = 1.1;
    M[A.G_FREEZE] = 0.1;
    ev(A.EV_BOSSHIT, M[b + A.B_X], M[b + A.B_Y], M[b + A.E_HP]);
    if (M[b + A.E_HP] <= 0) {
        M[b + A.E_STATE] = f(A.BS_DYING);
        M[b + A.E_ST] = 1.8;
        M[b + A.B_VX] = 0;
        M[A.G_NSH] = 0;
    }
    return true;
}
fn bossLand(b: usize, variant: f64) void { ev(A.EV_BOOM, M[b + A.B_X], M[b + A.B_Y], variant); }

fn updateBoss(b: usize, dt: f64) void {
    const state = iu(M[b + A.E_STATE]);
    if (state == A.BS_SLEEP) return;
    M[b + A.E_INV] = fmax(0, M[b + A.E_INV] - dt);
    M[b + A.E_ST] -= dt;
    const dx = M[A.P_X] - M[b + A.B_X];
    const lowHp = M[b + A.E_HP] <= 3;
    if (state == A.BS_INTRO) {
        M[b + A.B_VY] = fmax(M[b + A.B_VY] - 60 * dt, -30);
        moveBody(b, dt, false);
        if (M[b + A.B_GND] != 0) {
            M[b + A.E_STATE] = f(A.BS_IDLE);
            M[b + A.E_ST] = 1.0;
            bossLand(b, 1);
        }
    } else if (state == A.BS_IDLE) {
        const d = sign(dx);
        M[b + A.E_DIR] = if (d != 0) d else 1;
        M[b + A.B_VX] = 0;
        M[b + A.B_VY] = -2;
        moveBody(b, dt, false);
        if (M[b + A.E_ST] <= 0) {
            const r = rnd();
            if (r < 0.5) {
                M[b + A.E_STATE] = f(A.BS_JUMP);
                M[b + A.B_VY] = 25;
                M[b + A.B_VX] = clamp(dx / 1.2, -11, 11);
                ev(A.EV_SPRING, M[b + A.B_X], M[b + A.B_Y], 1);
            } else if (r < 0.8) {
                M[b + A.E_STATE] = f(A.BS_SHOOT);
                M[b + A.E_ST] = 0.5;
                M[b + A.E_SHOTS] = if (lowHp) 5 else 3;
            } else {
                M[b + A.E_STATE] = f(A.BS_CHARGE);
                M[b + A.E_ST] = 1.3;
                M[b + A.E_DIR] = if (d != 0) d else 1;
            }
        }
    } else if (state == A.BS_JUMP) {
        M[b + A.B_VY] = fmax(M[b + A.B_VY] - 62 * dt, -30);
        moveBody(b, dt, false);
        if (M[b + A.B_HX] != 0) M[b + A.B_VX] = 0;
        if (M[b + A.B_GND] != 0) {
            M[b + A.E_STATE] = f(A.BS_RECOVER);
            M[b + A.E_ST] = if (lowHp) 0.8 else 1.2;
            M[b + A.B_VX] = 0;
            bossLand(b, 0);
            shoot(M[b + A.B_X] - 1.3, M[b + A.B_Y] + 0.35, -7, 0, 0, 4, 0.7, 1);
            shoot(M[b + A.B_X] + 1.3, M[b + A.B_Y] + 0.35, 7, 0, 0, 4, 0.7, 1);
        }
    } else if (state == A.BS_RECOVER) {
        M[b + A.B_VX] = 0;
        M[b + A.B_VY] = -2;
        moveBody(b, dt, false);
        if (M[b + A.E_ST] <= 0) {
            M[b + A.E_STATE] = f(A.BS_IDLE);
            M[b + A.E_ST] = if (lowHp) 0.35 else 0.7;
        }
    } else if (state == A.BS_SHOOT) {
        M[b + A.B_VX] = 0;
        M[b + A.B_VY] = -2;
        moveBody(b, dt, false);
        const d = sign(dx);
        M[b + A.E_DIR] = if (d != 0) d else 1;
        if (M[b + A.E_ST] <= 0 and M[b + A.E_SHOTS] > 0) {
            var ux = M[A.P_X] - M[b + A.B_X];
            var uy = M[A.P_Y] + 0.4 - (M[b + A.B_Y] + 1.4);
            var len = @sqrt(ux * ux + uy * uy);
            if (len < 1e-6) {
                ux = 1;
                uy = 0;
                len = 1;
            }
            ux /= len;
            uy /= len;
            const off = (rnd() - 0.5) * 0.35;
            var vx = ux - uy * off;
            var vy = uy + ux * off;
            const l2 = @sqrt(vx * vx + vy * vy);
            vx = vx / l2 * 7.5;
            vy = vy / l2 * 7.5;
            shoot(M[b + A.B_X] + M[b + A.E_DIR] * 1.1, M[b + A.B_Y] + 1.4, vx, vy, 0, 5, 0.6, 1);
            ev(A.EV_SHOOT, M[b + A.B_X] + M[b + A.E_DIR] * 1.1, M[b + A.B_Y] + 1.4, M[b + A.E_DIR]);
            M[b + A.E_SHOTS] -= 1;
            M[b + A.E_ST] = 0.3;
        } else if (M[b + A.E_SHOTS] <= 0 and M[b + A.E_ST] <= 0) {
            M[b + A.E_STATE] = f(A.BS_IDLE);
            M[b + A.E_ST] = 0.7;
        }
    } else if (state == A.BS_CHARGE) {
        M[b + A.B_VY] = fmax(M[b + A.B_VY] - 60 * dt, -30);
        if (M[b + A.E_ST] > 0.75) {
            M[b + A.B_VX] = 0;
        } else {
            M[b + A.B_VX] = M[b + A.E_DIR] * (if (lowHp) @as(f64, 15) else 12);
        }
        moveBody(b, dt, false);
        if (M[b + A.E_ST] <= 0.75 and M[b + A.B_HX] != 0) {
            M[b + A.E_STATE] = f(A.BS_RECOVER);
            M[b + A.E_ST] = 1.3;
            M[b + A.B_VX] = 0;
            bossLand(b, 2);
        } else if (M[b + A.E_ST] <= 0) {
            M[b + A.E_STATE] = f(A.BS_RECOVER);
            M[b + A.E_ST] = 0.8;
            M[b + A.B_VX] = 0;
        }
    } else if (state == A.BS_DYING) {
        M[b + A.B_VX] = 0;
        if (rnd() < 0.5) {
            const rx = rndr(-1.2, 1.2);
            const ry = rndr(0, 2.4);
            ev(A.EV_BOSSEXPLODE, M[b + A.B_X] + rx, M[b + A.B_Y] + ry, 0);
        }
        if (M[b + A.E_ST] <= 0) {
            M[b + A.E_ALIVE] = 0;
            M[A.G_SCORE] += 3000;
            M[A.G_BOSSKILLED] = 1;
            M[A.G_BOSSACTIVE] = 0;
            setDoor(false);
            addGoal(M[b + A.B_X], 2);
            ev(A.EV_BOSSDEAD, M[b + A.B_X], M[b + A.B_Y], 0);
        }
    }
}

fn updateEnemies(dt: f64) void {
    const n = iu(M[A.G_NEN]);
    var i: usize = 0;
    while (i < n) : (i += 1) {
        const b = enBase(i);
        if (M[b + A.E_ALIVE] == 0) continue;
        M[b + A.E_T] += dt;
        const kind = iu(M[b + A.E_KIND]);
        if (kind == A.EK_SLIME) {
            M[b + A.B_VY] = fmax(M[b + A.B_VY] - 55 * dt, -25);
            M[b + A.B_VX] = M[b + A.E_DIR] * 1.9;
            moveBody(b, dt, false);
            if (M[b + A.B_HX] != 0) {
                M[b + A.E_DIR] = -M[b + A.E_DIR];
            } else if (M[b + A.B_GND] != 0) {
                const tx = fi(M[b + A.B_X] + M[b + A.E_DIR] * (M[b + A.B_W] / 2 + 0.12));
                const ty = fi(M[b + A.B_Y] - 0.1);
                const t = tileAt(tx, ty);
                if (!isSolid(t) and t != 2) {
                    M[b + A.E_DIR] = -M[b + A.E_DIR];
                } else if (tileAt(tx, fi(M[b + A.B_Y] + 0.1)) == 3) {
                    M[b + A.E_DIR] = -M[b + A.E_DIR];
                }
            }
        } else if (kind == A.EK_BAT) {
            const near = fabs(M[A.P_X] - M[b + A.E_OX]) < 8;
            if (near and M[A.P_DEAD] == 0) M[b + A.E_OX] += sign(M[A.P_X] - M[b + A.E_OX]) * 0.9 * dt;
            const px = M[b + A.B_X];
            M[b + A.B_X] = M[b + A.E_OX] + fsin(M[b + A.E_T] * 1.3) * 3.2;
            M[b + A.B_Y] = M[b + A.E_OY] + fsin(M[b + A.E_T] * 2.4) * 0.6;
            const d = sign(M[b + A.B_X] - px);
            if (d != 0) M[b + A.E_DIR] = d;
        } else if (kind == A.EK_SAW) {
            M[b + A.B_X] += M[b + A.B_VX] * dt;
            const ahead = fi(M[b + A.B_X] + sign(M[b + A.B_VX]) * 0.5);
            const floorAhead = tileAt(ahead, fi(M[b + A.B_Y] - 0.2));
            if (isSolid(tileAt(ahead, fi(M[b + A.B_Y] + 0.4))) or (!isSolid(floorAhead) and floorAhead != 2) or
                fabs(M[b + A.B_X] - M[b + A.E_OX]) > 3)
            {
                M[b + A.B_VX] = -M[b + A.B_VX];
                M[b + A.B_X] += M[b + A.B_VX] * dt * 2;
            }
        } else if (kind == A.EK_TURRET) {
            const d = sign(M[A.P_X] - M[b + A.B_X]);
            M[b + A.E_DIR] = if (d != 0) d else 1;
            M[b + A.E_CD] -= dt;
            const dx = fabs(M[A.P_X] - M[b + A.B_X]);
            const dy = fabs(M[A.P_Y] - M[b + A.B_Y]);
            if (M[b + A.E_CD] <= 0 and dx < fmin(13, M[A.G_VIEWW] / 2 + 1) and dy < 7 and M[A.P_DEAD] == 0) {
                M[b + A.E_CD] = 2.2;
                shoot(M[b + A.B_X] + M[b + A.E_DIR] * 0.6, M[b + A.B_Y] + 0.5, M[b + A.E_DIR] * 6.5, 0, 0, 6, 0.6, 0);
                ev(A.EV_SHOOT, M[b + A.B_X] + M[b + A.E_DIR] * 0.7, M[b + A.B_Y] + 0.5, M[b + A.E_DIR]);
            }
        } else if (kind == A.EK_BOSS) {
            updateBoss(b, dt);
        }
    }
}

// ------------------------------------------------------------ interactions
fn updateInteractions(dt: f64) void {
    if (M[A.P_DEAD] != 0) return;
    const nen = iu(M[A.G_NEN]);
    var i: usize = 0;
    while (i < nen) : (i += 1) {
        const e = enBase(i);
        if (M[e + A.E_ALIVE] == 0) continue;
        const kind = iu(M[e + A.E_KIND]);
        if (kind == A.EK_BOSS and (M[e + A.E_STATE] == f(A.BS_SLEEP) or M[e + A.E_STATE] == f(A.BS_DYING))) continue;
        if (!boxHit(A.P_X, e)) continue;
        if (kind == A.EK_BOSS) {
            if (M[A.P_VY] < 0 and M[A.P_PREVY] >= M[e + A.B_Y] + M[e + A.B_H] * 0.55 and M[A.P_DASHT] <= 0) {
                if (damageBoss()) {
                    stompBounce(true);
                } else M[A.P_VY] = fmax(M[A.P_VY], 8);
            } else _ = hurtPlayer(M[e + A.B_X]);
        } else if (M[e + A.E_STOMP] == 0) {
            _ = hurtPlayer(M[e + A.B_X]);
        } else if (M[A.P_DASHT] > 0) {
            killEnemy(e, true);
        } else if (M[A.P_VY] < 0 and M[A.P_PREVY] >= M[e + A.B_Y] + M[e + A.B_H] * 0.5) {
            killEnemy(e, false);
            stompBounce(false);
            M[A.G_FREEZE] = 0.04;
        } else _ = hurtPlayer(M[e + A.B_X]);
    }

    const nc = iu(M[A.G_NCOINS]);
    i = 0;
    while (i < nc) : (i += 1) {
        const c = A.COIN_BASE + i * A.COIN_N;
        if (M[c + A.C_GOT] != 0) continue;
        const r = M[c + A.C_R];
        if (fabs(M[A.P_X] - M[c + A.C_X]) < r + PW / 2 and fabs(M[A.P_Y] + 0.45 - M[c + A.C_Y]) < r + 0.45) {
            const kind = iu(M[c + A.C_KIND]);
            if (kind == A.SK_HEART) {
                if (M[A.P_HP] >= M[A.P_MAXHP]) {
                    M[A.G_SCORE] += 250;
                    ev(A.EV_HEART, M[c + A.C_X], M[c + A.C_Y], 0);
                } else {
                    M[A.P_HP] += 1;
                    ev(A.EV_HEART, M[c + A.C_X], M[c + A.C_Y], 1);
                }
            } else if (kind == A.SK_GEM) {
                M[A.G_COINS] += 5;
                M[A.G_SCORE] += 500;
                ev(A.EV_GEM, M[c + A.C_X], M[c + A.C_Y], 0);
            } else {
                M[A.G_COINS] += 1;
                M[A.G_SCORE] += 100;
                ev(A.EV_COIN, M[c + A.C_X], M[c + A.C_Y], 0);
            }
            M[c + A.C_GOT] = 1;
        }
    }

    const ns = iu(M[A.G_NSPR]);
    i = 0;
    while (i < ns) : (i += 1) {
        const s = A.SPR_BASE + i * A.SPR_N;
        M[s + A.SP_T] = fmax(0, M[s + A.SP_T] - dt);
        if (fabs(M[A.P_X] - M[s + A.SP_X]) < 0.7 and M[A.P_Y] >= M[s + A.SP_Y] - 0.1 and M[A.P_Y] < M[s + A.SP_Y] + 0.55 and M[A.P_VY] <= 0.5) {
            M[A.P_VY] = 31;
            M[A.P_GND] = 0;
            M[A.P_JUMPS] = 1;
            M[A.P_AIRDASH] = 0;
            M[A.P_DASHT] = 0;
            M[A.P_COYOTE] = 0;
            M[s + A.SP_T] = 0.3;
            ev(A.EV_SPRING, M[s + A.SP_X], M[s + A.SP_Y] + 0.4, 0);
        }
    }

    const nk = iu(M[A.G_NCK]);
    i = 0;
    while (i < nk) : (i += 1) {
        const c = A.CKT_BASE + i * A.CKT_N;
        if (M[c + A.CK_ON] == 0 and fabs(M[A.P_X] - M[c + A.CK_X]) < 1.3 and fabs(M[A.P_Y] - M[c + A.CK_Y]) < 2) {
            var j: usize = 0;
            while (j < nk) : (j += 1) M[A.CKT_BASE + j * A.CKT_N + A.CK_ON] = 0;
            M[c + A.CK_ON] = 1;
            M[A.G_CHKX] = M[c + A.CK_X];
            M[A.G_CHKY] = M[c + A.CK_Y];
            ev(A.EV_CHECK, M[c + A.CK_X], M[c + A.CK_Y], 0);
        }
    }

    const bi = ii(M[A.G_BOSS]);
    if (bi >= 0) {
        const b = enBase(@intCast(bi));
        if (M[b + A.E_STATE] == f(A.BS_SLEEP) and M[b + A.E_ALIVE] != 0 and M[A.P_X] > M[A.G_BTRIG]) startBoss();
    }

    if (M[A.G_GOALON] != 0 and fabs(M[A.P_X] - M[A.G_GOALX]) < 0.9 and fabs(M[A.P_Y] + 0.5 - (M[A.G_GOALY] + 1.1)) < 1.5) {
        M[A.G_MODE] = f(A.MODE_CLEAR);
        ev(A.EV_COMPLETE, M[A.P_X], M[A.P_Y], 0);
    }
}

fn step(dt: f64) void {
    if (M[A.G_FREEZE] > 0) {
        M[A.G_FREEZE] -= dt;
        return;
    }
    if (iu(M[A.G_MODE]) != A.MODE_PLAY) {
        updatePlatforms(dt);
        updateEnemies(dt);
        updateShots(dt);
        return;
    }
    M[A.G_TIME] += dt;
    updatePlatforms(dt);
    if (M[A.P_DEAD] == 0) {
        updatePlayer(dt);
    } else {
        M[A.P_DEADT] += dt;
        if (M[A.P_DEADT] > 1.4) respawn();
    }
    updateEnemies(dt);
    updateShots(dt);
    updateInteractions(dt);
}

export fn advance(rdt: f64) i32 {
    M[A.G_EVN] = 0;
    var acc = M[A.G_ACC] + rdt;
    var n: i32 = 0;
    while (acc >= DT and n < 12) {
        step(DT);
        acc -= DT;
        n += 1;
    }
    if (n >= 12) acc = 0;
    M[A.G_ACC] = acc;
    M[A.G_STEPS] = f(n);
    if (n > 0) {
        M[A.IN_JUMPPRESS] = 0;
        M[A.IN_DASHPRESS] = 0;
    }
    return n;
}

export fn mem_get(i: i32) f64 { return M[@intCast(i)]; }
export fn mem_set(i: i32, v: f64) void { M[@intCast(i)] = v; }
export fn mem_ptr() i32 { return @intCast(@intFromPtr(&M)); }
