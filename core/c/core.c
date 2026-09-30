/* Slop Runner simulation core - C port (freestanding wasm32, no libc).
   Mirrors core/js/core.js line by line; all state lives in the flat array M. */
#include "abi.h"

#define EXPORT(n) __attribute__((export_name(n)))
#define LEVEL_H 14

static double M[MEM_SIZE];

static const double DT = 1.0 / 120.0;
static const double PW = 0.62, PH = 0.86;
static const double MAX_SPEED = 8.2, ACCEL_G = 95.0, ACCEL_A = 62.0, DECEL_G = 110.0, DECEL_A = 26.0;
static const double GRAVITY = 62.0, FALL_GRAVITY = 74.0, MAX_FALL = 27.0;
static const double JUMP_V = 19.6, DOUBLE_V = 17.2;
static const double COYOTE = 0.11, BUFFER = 0.13;
static const double WALL_SLIDE = 3.4, WALL_JUMP_X = 9.5, WALL_JUMP_Y = 18.2, WALL_LOCK = 0.16;
static const double DASH_TIME = 0.17, DASH_SPEED = 24.0, DASH_CD = 0.75;
static const double INVULN = 1.5, STOMP_BOUNCE = 15.5;

static double sign(double v) { return v > 0 ? 1.0 : (v < 0 ? -1.0 : 0.0); }
static double fmin_(double a, double b) { return a < b ? a : b; }
static double fmax_(double a, double b) { return a > b ? a : b; }
static double fabs_(double a) { return a < 0 ? -a : a; }
static double clamp(double v, double a, double b) { return v < a ? a : (v > b ? b : v); }
static double approach(double v, double t, double s) { return v < t ? fmin_(v + s, t) : fmax_(v - s, t); }
static double flr(double x) { return __builtin_floor(x); }
static int fi(double x) { return (int)__builtin_floor(x); }

static double fsin(double x) {
  double k = flr(x * 0.15915494309189535 + 0.5);
  double r = x - k * 6.283185307179586;
  if (r > 1.5707963267948966) r = 3.141592653589793 - r;
  else if (r < -1.5707963267948966) r = -3.141592653589793 - r;
  double r2 = r * r;
  return r * (1.0 + r2 * (-0.16666666666666666 + r2 * (0.008333333333333333 + r2 * (-0.0001984126984126984 +
         r2 * (2.7557319223985893e-6 + r2 * (-2.505210838544172e-8 + r2 * 1.6059043836821613e-10))))));
}
static double fcos(double x) { return fsin(x + 1.5707963267948966); }
static double rnd(void) {
  double s = M[G_RNG] * 1664525.0 + 1013904223.0;
  s = s - flr(s / 4294967296.0) * 4294967296.0;
  M[G_RNG] = s;
  return s / 4294967296.0;
}
static double rndr(double a, double b) { return a + rnd() * (b - a); }
static void ev(int type, double x, double y, double a) {
  int n = (int)M[G_EVN];
  if (n < EV_MAX) {
    int i = EV_BASE + n * EV_N;
    M[i] = type; M[i + 1] = x; M[i + 2] = y; M[i + 3] = a;
    M[G_EVN] = n + 1;
  }
}

static double tileAt(int tx, int ty) {
  int lw = (int)M[G_LW];
  if (tx < 0 || tx >= lw) return 1;
  if (ty < 0 || ty >= LEVEL_H) return 0;
  return M[TILE_BASE + ty * lw + tx];
}
static int isSolid(double t) { return t == 1 || t == 4; }
static void setTile(int tx, int ty, double v) { M[TILE_BASE + ty * (int)M[G_LW] + tx] = v; }

static void moveBody(int b, double dt, int drop) {
  M[b + B_HX] = 0; M[b + B_HY] = 0;
  double hw = M[b + B_W] / 2;
  M[b + B_X] += M[b + B_VX] * dt;
  int y0 = fi(M[b + B_Y] + 0.02), y1 = fi(M[b + B_Y] + M[b + B_H] - 0.02);
  double vx = M[b + B_VX];
  if (vx > 0) {
    int tx = fi(M[b + B_X] + hw);
    for (int ty = y0; ty <= y1; ty++) {
      if (isSolid(tileAt(tx, ty))) { M[b + B_X] = tx - hw - 1e-4; M[b + B_VX] = 0; M[b + B_HX] = 1; break; }
    }
  } else if (vx < 0) {
    int tx = fi(M[b + B_X] - hw);
    for (int ty = y0; ty <= y1; ty++) {
      if (isSolid(tileAt(tx, ty))) { M[b + B_X] = tx + 1 + hw + 1e-4; M[b + B_VX] = 0; M[b + B_HX] = -1; break; }
    }
  }
  double prevY = M[b + B_Y];
  M[b + B_Y] += M[b + B_VY] * dt;
  int x0 = fi(M[b + B_X] - hw + 1e-3), x1 = fi(M[b + B_X] + hw - 1e-3);
  M[b + B_GND] = 0;
  if (M[b + B_VY] <= 0) {
    int ty = fi(M[b + B_Y]);
    for (int tx = x0; tx <= x1; tx++) {
      double t = tileAt(tx, ty);
      if (isSolid(t) || (t == 2 && !drop && prevY >= ty + 1 - 0.02)) {
        M[b + B_Y] = ty + 1; M[b + B_VY] = 0; M[b + B_GND] = 1; M[b + B_HY] = -1; break;
      }
    }
  } else {
    int ty = fi(M[b + B_Y] + M[b + B_H]);
    for (int tx = x0; tx <= x1; tx++) {
      if (isSolid(tileAt(tx, ty))) { M[b + B_Y] = ty - M[b + B_H] - 1e-4; M[b + B_VY] = 0; M[b + B_HY] = 1; break; }
    }
  }
}
static int boxHit(int a, int b) {
  return fabs_(M[a + B_X] - M[b + B_X]) < (M[a + B_W] + M[b + B_W]) / 2 &&
         M[a + B_Y] < M[b + B_Y] + M[b + B_H] && M[a + B_Y] + M[a + B_H] > M[b + B_Y];
}
static int enBase(int i) { return EN_BASE + i * EN_N; }
static int plBase(int i) { return PLT_BASE + i * PLT_N; }
static int shBase(int i) { return SH_BASE + i * SH_N; }

static void setDoor(int closed) {
  if (M[G_DOORON] == 0) return;
  for (int ty = (int)M[G_DOORY0]; ty <= (int)M[G_DOORY1]; ty++) setTile((int)M[G_DOORX], ty, closed ? 4 : 0);
  M[G_DOORCLOSED] = closed ? 1 : 0;
  ev(EV_DOOR, M[G_DOORX], 0, closed ? 1 : 0);
}

static int addEnemy(int kind, double x, double y) {
  int i = (int)M[G_NEN]; if (i >= EN_MAX) return -1;
  M[G_NEN] = i + 1;
  int b = enBase(i);
  for (int k = 0; k < EN_N; k++) M[b + k] = 0;
  M[b + B_X] = x; M[b + B_Y] = y; M[b + B_W] = 0.8; M[b + B_H] = 0.62;
  M[b + E_KIND] = kind; M[b + E_ALIVE] = 1; M[b + E_STOMP] = 1;
  M[b + E_DIR] = rnd() < 0.5 ? -1 : 1;
  M[b + E_T] = rnd() * 10; M[b + E_OX] = x; M[b + E_OY] = y; M[b + E_CD] = 1 + rnd();
  if (kind == EK_BAT) { M[b + B_W] = 0.75; M[b + B_H] = 0.5; }
  else if (kind == EK_SAW) { M[b + B_W] = 0.78; M[b + B_H] = 0.78; M[b + E_STOMP] = 0; M[b + E_DIR] = 1; M[b + B_VX] = 3; }
  else if (kind == EK_TURRET) { M[b + B_W] = 0.9; M[b + B_H] = 0.8; M[b + E_STOMP] = 0; }
  else if (kind == EK_BOSS) {
    M[b + B_W] = 2.5; M[b + B_H] = 2.3; M[b + E_HP] = 8; M[b + E_HP0] = 8; M[b + E_STATE] = BS_SLEEP; M[b + E_DIR] = -1;
    M[G_BOSS] = i;
  }
  return i;
}
static void addPlatform(int kind, double x, double ty) {
  int i = (int)M[G_NPLT]; if (i >= PLT_MAX) return;
  M[G_NPLT] = i + 1;
  int p = plBase(i);
  for (int k = 0; k < PLT_N; k++) M[p + k] = 0;
  M[p + PT_KIND] = kind; M[p + PT_X] = x; M[p + PT_X0] = x;
  M[p + PT_TOP] = ty + 0.75; M[p + PT_TOP0] = ty + 0.75; M[p + PT_PREVTOP] = ty + 0.75;
  M[p + PT_W] = kind == 2 ? 1 : 3; M[p + PT_SOLID] = 1; M[p + PT_T] = rnd() * 6; M[p + PT_STATE] = CS_IDLE;
}
static void addGoal(double x, double y) { M[G_GOALON] = 1; M[G_GOALX] = x; M[G_GOALY] = y; }

static void resetPlayer(double x, double y) {
  M[P_X] = x; M[P_Y] = y; M[P_VX] = 0; M[P_VY] = 0; M[P_FACE] = 1; M[P_GND] = 0;
  M[P_COYOTE] = 0; M[P_BUFFER] = 0; M[P_JUMPS] = 1; M[P_WALLDIR] = 0; M[P_WALLGRACE] = 0; M[P_WALLLOCK] = 0;
  M[P_DASHT] = 0; M[P_DASHCD] = 0; M[P_AIRDASH] = 0; M[P_INV] = 0; M[P_DROPT] = 0; M[P_DEAD] = 0; M[P_DEADT] = 0;
  M[P_RIDING] = -1; M[P_SAFEX] = x; M[P_SAFEY] = y; M[P_SAFET] = 0; M[P_PREVY] = y; M[P_WASGND] = 0;
}

EXPORT("init") void init(void) {
  for (int i = 0; i < MEM_SIZE; i++) M[i] = 0;
  M[G_RNG] = 12345; M[G_VIEWW] = 20;
  M[P_W] = PW; M[P_H] = PH; M[P_MAXHP] = 3; M[P_HP] = 3; M[P_RIDING] = -1; M[G_BOSS] = -1;
}

EXPORT("load_level") void load_level(void) {
  M[G_NCOINS] = 0; M[G_NEN] = 0; M[G_NPLT] = 0; M[G_NSH] = 0; M[G_NSPR] = 0; M[G_NCK] = 0; M[G_BOSS] = -1;
  M[G_TIME] = 0; M[G_COINS] = 0; M[G_KILLS] = 0; M[G_DEATHS] = 0; M[G_SCORE] = 0; M[G_BOSSKILLED] = 0;
  M[G_BOSSACTIVE] = 0; M[G_FREEZE] = 0; M[G_COINSTOTAL] = 0; M[G_DOORCLOSED] = 0; M[G_ACC] = 0; M[G_EVN] = 0;
  int n = (int)M[G_NSPEC];
  for (int s = 0; s < n; s++) {
    int kind = (int)M[SPEC_BASE + s * SPEC_N];
    double x = M[SPEC_BASE + s * SPEC_N + 1], y = M[SPEC_BASE + s * SPEC_N + 2];
    if (kind <= SK_HEART) {
      int i = (int)M[G_NCOINS];
      if (i < COIN_MAX) {
        M[G_NCOINS] = i + 1;
        int c = COIN_BASE + i * COIN_N;
        M[c + C_KIND] = kind; M[c + C_X] = x; M[c + C_Y] = y + 0.5; M[c + C_GOT] = 0; M[c + C_R] = kind == SK_COIN ? 0.55 : 0.65;
        if (kind == SK_COIN) M[G_COINSTOTAL] += 1; else if (kind == SK_GEM) M[G_COINSTOTAL] += 5;
      }
    } else if (kind == SK_SLIME) addEnemy(EK_SLIME, x, y);
    else if (kind == SK_BAT) addEnemy(EK_BAT, x, y + 0.5);
    else if (kind == SK_SAW) addEnemy(EK_SAW, x, y + 0.02);
    else if (kind == SK_TURRET) addEnemy(EK_TURRET, x, y);
    else if (kind == SK_SPRING) {
      int i = (int)M[G_NSPR];
      if (i < SPR_MAX) { M[G_NSPR] = i + 1; int q = SPR_BASE + i * SPR_N; M[q + SP_X] = x; M[q + SP_Y] = y; M[q + SP_T] = 0; }
    } else if (kind == SK_PLAT_H) addPlatform(0, x, y);
    else if (kind == SK_PLAT_V) addPlatform(1, x, y);
    else if (kind == SK_CRUMBLE) addPlatform(2, x, y);
    else if (kind == SK_CHECK) {
      int i = (int)M[G_NCK];
      if (i < CKT_MAX) { M[G_NCK] = i + 1; int q = CKT_BASE + i * CKT_N; M[q + CK_X] = x; M[q + CK_Y] = y; M[q + CK_ON] = 0; }
    } else if (kind == SK_BOSS) addEnemy(EK_BOSS, x, y);
  }
  M[P_MAXHP] = 3; M[P_HP] = 3;
  M[G_CHKX] = M[G_SPAWNX]; M[G_CHKY] = M[G_SPAWNY];
  resetPlayer(M[G_SPAWNX], M[G_SPAWNY]);
}

/* ------------------------------------------------------------- player */
static void resetBoss(void);
static void killPlayer(void) {
  M[P_DEAD] = 1; M[P_DEADT] = 0; M[P_HP] = 0;
  M[G_DEATHS] += 1;
  ev(EV_DIE, M[P_X], M[P_Y] + 0.5, 0);
}
static int hurtPlayer(double srcX) {
  if (M[P_INV] > 0 || M[P_DASHT] > 0 || M[P_DEAD] != 0) return 0;
  M[P_HP] -= 1;
  M[P_INV] = INVULN;
  M[G_FREEZE] = 0.08;
  ev(EV_HURT, M[P_X], M[P_Y] + 0.5, 0);
  M[P_VX] = (M[P_X] < srcX ? -1 : 1) * 8.5; M[P_VY] = 12;
  M[P_WALLLOCK] = 0.18; M[P_DASHT] = 0;
  if (M[P_HP] <= 0) killPlayer();
  return 1;
}
static void pitFall(void) {
  if (M[P_DEAD] != 0) return;
  M[P_HP] -= 1;
  ev(EV_PIT, M[P_X], M[P_Y], 0);
  if (M[P_HP] <= 0) { killPlayer(); return; }
  M[P_X] = M[P_SAFEX]; M[P_Y] = M[P_SAFEY] + 0.05; M[P_VX] = 0; M[P_VY] = 0; M[P_INV] = 2;
  ev(EV_RESPAWN, M[P_X], M[P_Y] + 0.4, 1);
}
static void respawn(void) {
  resetPlayer(M[G_CHKX], M[G_CHKY]);
  M[P_HP] = M[P_MAXHP]; M[P_INV] = 2;
  if (M[G_BOSS] >= 0 && M[G_BOSSACTIVE] != 0) resetBoss();
  ev(EV_RESPAWN, M[P_X], M[P_Y] + 0.5, 0);
}
static void stompBounce(int strong) {
  M[P_VY] = M[IN_JUMPHELD] != 0 ? STOMP_BOUNCE + 3.5 : STOMP_BOUNCE;
  if (strong) M[P_VY] += 2;
  M[P_JUMPS] = 1; M[P_AIRDASH] = 0; M[P_GND] = 0; M[P_DASHT] = 0;
}

static void updatePlayer(double dt) {
  double ax = M[IN_AX];
  M[P_PREVY] = M[P_Y];
  M[P_INV] = fmax_(0, M[P_INV] - dt);
  M[P_DASHCD] -= dt; M[P_WALLLOCK] -= dt; M[P_BUFFER] -= dt; M[P_DROPT] -= dt; M[P_WALLGRACE] -= dt;

  if (M[IN_JUMPPRESS] != 0) { M[IN_JUMPPRESS] = 0; M[P_BUFFER] = BUFFER; }
  int wantDash = M[IN_DASHPRESS] != 0;
  if (wantDash) M[IN_DASHPRESS] = 0;

  int rid = (int)M[P_RIDING];
  if (rid >= 0) {
    int p = plBase(rid);
    if (M[p + PT_SOLID] != 0 && M[P_VY] <= 0) { M[P_X] += M[p + PT_DX]; M[P_Y] = M[p + PT_TOP]; }
  }

  if (M[P_GND] != 0) { M[P_COYOTE] = COYOTE; M[P_JUMPS] = 1; M[P_AIRDASH] = 0; }
  else M[P_COYOTE] -= dt;

  if (wantDash && M[P_DASHCD] <= 0 && M[P_AIRDASH] == 0) {
    M[P_DASHT] = DASH_TIME; M[P_DASHCD] = DASH_CD;
    M[P_DASHDIR] = ax != 0 ? ax : M[P_FACE]; M[P_FACE] = M[P_DASHDIR];
    if (M[P_GND] == 0) M[P_AIRDASH] = 1;
    ev(EV_DASH, M[P_X], M[P_Y] + 0.4, M[P_DASHDIR]);
  }

  if (M[P_DASHT] > 0) {
    M[P_DASHT] -= dt;
    M[P_VX] = M[P_DASHDIR] * DASH_SPEED;
    M[P_VY] = 0;
    if (M[P_DASHT] <= 0) M[P_VX] = M[P_DASHDIR] * MAX_SPEED;
  } else {
    if (M[P_WALLLOCK] <= 0) {
      double target = ax * MAX_SPEED;
      double acc;
      if (ax == 0) acc = M[P_GND] != 0 ? DECEL_G : DECEL_A;
      else {
        acc = M[P_GND] != 0 ? ACCEL_G : ACCEL_A;
        if (sign(M[P_VX]) == -ax) acc *= 1.6;
      }
      M[P_VX] = approach(M[P_VX], target, acc * dt);
      if (ax != 0) M[P_FACE] = ax;
    }

    M[P_WALLDIR] = 0;
    if (M[P_GND] == 0 && ax != 0) {
      int tx = fi(M[P_X] + ax * (PW / 2 + 0.08));
      if (isSolid(tileAt(tx, fi(M[P_Y] + 0.3))) || isSolid(tileAt(tx, fi(M[P_Y] + 0.75)))) {
        M[P_WALLDIR] = ax; M[P_WALLGRACE] = 0.1; M[P_WALLMEM] = ax;
      }
    }

    if (M[P_BUFFER] > 0) {
      if (M[IN_DOWNHELD] != 0 && M[P_GND] != 0 && tileAt(fi(M[P_X]), fi(M[P_Y] - 0.05)) == 2) {
        M[P_DROPT] = 0.22; M[P_BUFFER] = 0; M[P_Y] -= 0.06; M[P_GND] = 0;
      } else if (M[P_COYOTE] > 0) {
        M[P_VY] = JUMP_V; M[P_BUFFER] = 0; M[P_COYOTE] = 0; M[P_GND] = 0;
        ev(EV_JUMP, M[P_X], M[P_Y] + 0.05, 0);
      } else if (M[P_WALLGRACE] > 0 && M[P_WALLMEM] != 0) {
        M[P_VX] = -M[P_WALLMEM] * WALL_JUMP_X; M[P_VY] = WALL_JUMP_Y;
        M[P_WALLLOCK] = WALL_LOCK; M[P_FACE] = -M[P_WALLMEM]; M[P_BUFFER] = 0; M[P_WALLGRACE] = 0;
        M[P_JUMPS] = 1; M[P_AIRDASH] = 0;
        ev(EV_WALLJUMP, M[P_X] + M[P_WALLMEM] * 0.3, M[P_Y] + 0.5, M[P_WALLMEM]);
      } else if (M[P_JUMPS] > 0) {
        M[P_VY] = DOUBLE_V; M[P_JUMPS] -= 1; M[P_BUFFER] = 0;
        ev(EV_DOUBLE, M[P_X], M[P_Y] + 0.05, 0);
      }
    }

    double g = M[P_VY] > 0 ? (M[IN_JUMPHELD] != 0 ? GRAVITY : GRAVITY * 2.4) : FALL_GRAVITY;
    M[P_VY] = fmax_(M[P_VY] - g * dt, -MAX_FALL);
    if (M[P_WALLDIR] != 0 && M[P_VY] < -WALL_SLIDE) M[P_VY] = -WALL_SLIDE;
  }

  double vyPre = M[P_VY];
  moveBody(P_X, dt, M[P_DROPT] > 0);

  M[P_RIDING] = -1;
  if (vyPre <= 0 && M[P_DASHT] <= 0) {
    int np = (int)M[G_NPLT];
    for (int i = 0; i < np; i++) {
      int p = plBase(i);
      if (M[p + PT_SOLID] == 0) continue;
      if (fabs_(M[P_X] - M[p + PT_X]) < M[p + PT_W] / 2 + PW / 2 - 0.08 && M[P_PREVY] >= M[p + PT_PREVTOP] - 0.12 &&
          M[P_Y] <= M[p + PT_TOP] + 0.02 && M[P_Y] >= M[p + PT_TOP] - 0.6) {
        M[P_Y] = M[p + PT_TOP]; M[P_VY] = 0; M[P_GND] = 1; M[P_RIDING] = i;
        if (M[p + PT_KIND] == 2 && M[p + PT_STATE] == CS_IDLE) { M[p + PT_STATE] = CS_SHAKE; M[p + PT_TIMER] = 0.45; }
        break;
      }
    }
  }

  if (M[P_GND] != 0 && M[P_WASGND] == 0 && vyPre < -9) ev(EV_LAND, M[P_X], M[P_Y] + 0.05, 0);
  if (M[P_HY] == 1 && M[P_VY] == 0) M[P_VY] = -1;
  M[P_WASGND] = M[P_GND];

  if (M[P_GND] != 0 && M[P_RIDING] < 0) {
    int ty = fi(M[P_Y]) - 1;
    if (isSolid(tileAt(fi(M[P_X] - 0.6), ty)) && isSolid(tileAt(fi(M[P_X] + 0.6), ty)) &&
        isSolid(tileAt(fi(M[P_X] - 1.6), ty)) && isSolid(tileAt(fi(M[P_X] + 1.6), ty)) &&
        tileAt(fi(M[P_X]), fi(M[P_Y])) != 3) {
      M[P_SAFET] += dt;
      if (M[P_SAFET] > 0.35) { M[P_SAFEX] = M[P_X]; M[P_SAFEY] = M[P_Y]; }
    } else M[P_SAFET] = 0;
  } else M[P_SAFET] = 0;

  if (M[P_INV] <= 0 && M[P_DASHT] <= 0) {
    int x0 = fi(M[P_X] - PW / 2), x1 = fi(M[P_X] + PW / 2);
    int y0 = fi(M[P_Y]), y1 = fi(M[P_Y] + PH * 0.5);
    int done = 0;
    for (int ty = y0; ty <= y1 && !done; ty++) {
      for (int tx = x0; tx <= x1; tx++) {
        if (tileAt(tx, ty) == 3) {
          if (M[P_X] + PW / 2 > tx + 0.15 && M[P_X] - PW / 2 < tx + 0.85 && M[P_Y] < ty + 0.5) {
            if (hurtPlayer(tx + 0.5)) { M[P_VY] = 15; M[P_VX] = (M[P_X] < tx + 0.5 ? -1 : 1) * 5; }
            done = 1; break;
          }
        }
      }
    }
  }

  if (M[P_Y] < -2.5) pitFall();
}

/* ---------------------------------------------------------- platforms */
static void updatePlatforms(double dt) {
  int n = (int)M[G_NPLT];
  for (int i = 0; i < n; i++) {
    int p = plBase(i);
    M[p + PT_PREVTOP] = M[p + PT_TOP]; M[p + PT_DX] = 0; M[p + PT_DY] = 0;
    M[p + PT_T] += dt;
    int kind = (int)M[p + PT_KIND];
    if (kind == 0) {
      double nx = M[p + PT_X0] + fsin(M[p + PT_T] * 1.05) * 2.4;
      M[p + PT_DX] = nx - M[p + PT_X]; M[p + PT_X] = nx;
    } else if (kind == 1) {
      double nt = M[p + PT_TOP0] + (1 - fcos(M[p + PT_T] * 0.95)) / 2 * 5.0;
      M[p + PT_DY] = nt - M[p + PT_TOP]; M[p + PT_TOP] = nt;
    } else {
      int st = (int)M[p + PT_STATE];
      if (st == CS_SHAKE) {
        M[p + PT_TIMER] -= dt;
        if (M[p + PT_TIMER] <= 0) {
          M[p + PT_STATE] = CS_FALL; M[p + PT_SOLID] = 0; M[p + PT_VY] = 0;
          ev(EV_CRUMBLE, M[p + PT_X], M[p + PT_TOP], 0);
        }
      } else if (st == CS_FALL) {
        M[p + PT_VY] -= 32 * dt; M[p + PT_TOP] += M[p + PT_VY] * dt;
        if (M[p + PT_TOP] < -3) { M[p + PT_STATE] = CS_GONE; M[p + PT_TIMER] = 2.6; }
      } else if (st == CS_GONE) {
        M[p + PT_TIMER] -= dt;
        if (M[p + PT_TIMER] <= 0) {
          M[p + PT_STATE] = CS_IDLE; M[p + PT_SOLID] = 1; M[p + PT_TOP] = M[p + PT_TOP0]; M[p + PT_PREVTOP] = M[p + PT_TOP];
        }
      }
    }
  }
}

/* -------------------------------------------------------------- shots */
static void shoot(double x, double y, double vx, double vy, double g, double life, double size, double purple) {
  int i = (int)M[G_NSH]; if (i >= SH_MAX) return;
  M[G_NSH] = i + 1;
  int s = shBase(i);
  M[s + S_X] = x; M[s + S_Y] = y; M[s + S_VX] = vx; M[s + S_VY] = vy; M[s + S_G] = g; M[s + S_LIFE] = life;
  M[s + S_R] = size * 0.36; M[s + S_PURPLE] = purple; M[s + S_SIZE] = size;
}
static void removeShot(int i) {
  int last = (int)M[G_NSH] - 1;
  if (i != last) {
    int a = shBase(i), b = shBase(last);
    for (int k = 0; k < SH_N; k++) M[a + k] = M[b + k];
  }
  M[G_NSH] = last;
}
static void updateShots(double dt) {
  for (int i = (int)M[G_NSH] - 1; i >= 0; i--) {
    int s = shBase(i);
    M[s + S_LIFE] -= dt;
    M[s + S_VY] -= M[s + S_G] * dt;
    M[s + S_X] += M[s + S_VX] * dt; M[s + S_Y] += M[s + S_VY] * dt;
    int dead = M[s + S_LIFE] <= 0 || M[s + S_Y] < -3;
    if (!dead && isSolid(tileAt(fi(M[s + S_X]), fi(M[s + S_Y])))) {
      dead = 1;
      ev(EV_SHOTHIT, M[s + S_X], M[s + S_Y], M[s + S_PURPLE]);
    }
    if (!dead && M[P_DEAD] == 0 && fabs_(M[P_X] - M[s + S_X]) < PW / 2 + M[s + S_R] &&
        M[s + S_Y] > M[P_Y] - M[s + S_R] && M[s + S_Y] < M[P_Y] + PH + M[s + S_R]) {
      if (hurtPlayer(M[s + S_X])) dead = 1;
    }
    if (dead) removeShot(i);
  }
}

/* ------------------------------------------------------------ enemies */
static void killEnemy(int b, int byDash) {
  M[b + E_ALIVE] = 0;
  M[G_KILLS] += 1; M[G_SCORE] += 150;
  ev(EV_KILL, M[b + B_X], M[b + B_Y] + M[b + B_H] / 2, M[b + E_KIND] + (byDash ? 10 : 0));
}

static void startBoss(void) {
  int b = enBase((int)M[G_BOSS]);
  M[G_BOSSACTIVE] = 1;
  setDoor(1);
  M[b + B_X] = M[G_BSPECX]; M[b + B_Y] = LEVEL_H + 1; M[b + B_VX] = 0; M[b + B_VY] = 0;
  M[b + E_STATE] = BS_INTRO; M[b + E_HP] = M[b + E_HP0]; M[b + E_INV] = 0; M[b + E_ALIVE] = 1;
  ev(EV_BOSSSTART, M[b + B_X], M[b + B_Y], 0);
}
static void resetBoss(void) {
  int b = enBase((int)M[G_BOSS]);
  M[G_BOSSACTIVE] = 0;
  M[b + E_STATE] = BS_SLEEP; M[b + E_HP] = M[b + E_HP0]; M[b + E_ALIVE] = 1; M[b + E_INV] = 0;
  M[b + B_X] = M[G_BSPECX]; M[b + B_Y] = M[G_BSPECY];
  setDoor(0);
  M[G_NSH] = 0;
}
static int damageBoss(void) {
  int b = enBase((int)M[G_BOSS]);
  int st = (int)M[b + E_STATE];
  if (M[b + E_INV] > 0 || st == BS_INTRO || st == BS_DYING) return 0;
  M[b + E_HP] -= 1; M[b + E_INV] = 1.1;
  M[G_FREEZE] = 0.1;
  ev(EV_BOSSHIT, M[b + B_X], M[b + B_Y], M[b + E_HP]);
  if (M[b + E_HP] <= 0) {
    M[b + E_STATE] = BS_DYING; M[b + E_ST] = 1.8; M[b + B_VX] = 0;
    M[G_NSH] = 0;
  }
  return 1;
}
static void bossLand(int b, double variant) { ev(EV_BOOM, M[b + B_X], M[b + B_Y], variant); }

static void updateBoss(int b, double dt) {
  int state = (int)M[b + E_STATE];
  if (state == BS_SLEEP) return;
  M[b + E_INV] = fmax_(0, M[b + E_INV] - dt);
  M[b + E_ST] -= dt;
  double dx = M[P_X] - M[b + B_X];
  int lowHp = M[b + E_HP] <= 3;
  if (state == BS_INTRO) {
    M[b + B_VY] = fmax_(M[b + B_VY] - 60 * dt, -30);
    moveBody(b, dt, 0);
    if (M[b + B_GND] != 0) { M[b + E_STATE] = BS_IDLE; M[b + E_ST] = 1.0; bossLand(b, 1); }
  } else if (state == BS_IDLE) {
    double d = sign(dx); M[b + E_DIR] = d != 0 ? d : 1;
    M[b + B_VX] = 0; M[b + B_VY] = -2;
    moveBody(b, dt, 0);
    if (M[b + E_ST] <= 0) {
      double r = rnd();
      if (r < 0.5) { M[b + E_STATE] = BS_JUMP; M[b + B_VY] = 25; M[b + B_VX] = clamp(dx / 1.2, -11, 11); ev(EV_SPRING, M[b + B_X], M[b + B_Y], 1); }
      else if (r < 0.8) { M[b + E_STATE] = BS_SHOOT; M[b + E_ST] = 0.5; M[b + E_SHOTS] = lowHp ? 5 : 3; }
      else { M[b + E_STATE] = BS_CHARGE; M[b + E_ST] = 1.3; M[b + E_DIR] = d != 0 ? d : 1; }
    }
  } else if (state == BS_JUMP) {
    M[b + B_VY] = fmax_(M[b + B_VY] - 62 * dt, -30);
    moveBody(b, dt, 0);
    if (M[b + B_HX] != 0) M[b + B_VX] = 0;
    if (M[b + B_GND] != 0) {
      M[b + E_STATE] = BS_RECOVER; M[b + E_ST] = lowHp ? 0.8 : 1.2; M[b + B_VX] = 0;
      bossLand(b, 0);
      shoot(M[b + B_X] - 1.3, M[b + B_Y] + 0.35, -7, 0, 0, 4, 0.7, 1);
      shoot(M[b + B_X] + 1.3, M[b + B_Y] + 0.35, 7, 0, 0, 4, 0.7, 1);
    }
  } else if (state == BS_RECOVER) {
    M[b + B_VX] = 0; M[b + B_VY] = -2;
    moveBody(b, dt, 0);
    if (M[b + E_ST] <= 0) { M[b + E_STATE] = BS_IDLE; M[b + E_ST] = lowHp ? 0.35 : 0.7; }
  } else if (state == BS_SHOOT) {
    M[b + B_VX] = 0; M[b + B_VY] = -2; moveBody(b, dt, 0);
    double d = sign(dx); M[b + E_DIR] = d != 0 ? d : 1;
    if (M[b + E_ST] <= 0 && M[b + E_SHOTS] > 0) {
      double ux = M[P_X] - M[b + B_X], uy = M[P_Y] + 0.4 - (M[b + B_Y] + 1.4);
      double len = __builtin_sqrt(ux * ux + uy * uy);
      if (len < 1e-6) { ux = 1; uy = 0; len = 1; }
      ux /= len; uy /= len;
      double off = (rnd() - 0.5) * 0.35;
      double vx = ux - uy * off, vy = uy + ux * off;
      double l2 = __builtin_sqrt(vx * vx + vy * vy);
      vx = vx / l2 * 7.5; vy = vy / l2 * 7.5;
      shoot(M[b + B_X] + M[b + E_DIR] * 1.1, M[b + B_Y] + 1.4, vx, vy, 0, 5, 0.6, 1);
      ev(EV_SHOOT, M[b + B_X] + M[b + E_DIR] * 1.1, M[b + B_Y] + 1.4, M[b + E_DIR]);
      M[b + E_SHOTS] -= 1; M[b + E_ST] = 0.3;
    } else if (M[b + E_SHOTS] <= 0 && M[b + E_ST] <= 0) { M[b + E_STATE] = BS_IDLE; M[b + E_ST] = 0.7; }
  } else if (state == BS_CHARGE) {
    M[b + B_VY] = fmax_(M[b + B_VY] - 60 * dt, -30);
    if (M[b + E_ST] > 0.75) M[b + B_VX] = 0;
    else M[b + B_VX] = M[b + E_DIR] * (lowHp ? 15 : 12);
    moveBody(b, dt, 0);
    if (M[b + E_ST] <= 0.75 && M[b + B_HX] != 0) {
      M[b + E_STATE] = BS_RECOVER; M[b + E_ST] = 1.3; M[b + B_VX] = 0;
      bossLand(b, 2);
    } else if (M[b + E_ST] <= 0) { M[b + E_STATE] = BS_RECOVER; M[b + E_ST] = 0.8; M[b + B_VX] = 0; }
  } else if (state == BS_DYING) {
    M[b + B_VX] = 0;
    if (rnd() < 0.5) ev(EV_BOSSEXPLODE, M[b + B_X] + rndr(-1.2, 1.2), M[b + B_Y] + rndr(0, 2.4), 0);
    if (M[b + E_ST] <= 0) {
      M[b + E_ALIVE] = 0;
      M[G_SCORE] += 3000; M[G_BOSSKILLED] = 1; M[G_BOSSACTIVE] = 0;
      setDoor(0);
      addGoal(M[b + B_X], 2);
      ev(EV_BOSSDEAD, M[b + B_X], M[b + B_Y], 0);
    }
  }
}

static void updateEnemies(double dt) {
  int n = (int)M[G_NEN];
  for (int i = 0; i < n; i++) {
    int b = enBase(i);
    if (M[b + E_ALIVE] == 0) continue;
    M[b + E_T] += dt;
    int kind = (int)M[b + E_KIND];
    if (kind == EK_SLIME) {
      M[b + B_VY] = fmax_(M[b + B_VY] - 55 * dt, -25);
      M[b + B_VX] = M[b + E_DIR] * 1.9;
      moveBody(b, dt, 0);
      if (M[b + B_HX] != 0) M[b + E_DIR] = -M[b + E_DIR];
      else if (M[b + B_GND] != 0) {
        int tx = fi(M[b + B_X] + M[b + E_DIR] * (M[b + B_W] / 2 + 0.12)), ty = fi(M[b + B_Y] - 0.1);
        double t = tileAt(tx, ty);
        if (!isSolid(t) && t != 2) M[b + E_DIR] = -M[b + E_DIR];
        else if (tileAt(tx, fi(M[b + B_Y] + 0.1)) == 3) M[b + E_DIR] = -M[b + E_DIR];
      }
    } else if (kind == EK_BAT) {
      int near = fabs_(M[P_X] - M[b + E_OX]) < 8;
      if (near && M[P_DEAD] == 0) M[b + E_OX] += sign(M[P_X] - M[b + E_OX]) * 0.9 * dt;
      double px = M[b + B_X];
      M[b + B_X] = M[b + E_OX] + fsin(M[b + E_T] * 1.3) * 3.2;
      M[b + B_Y] = M[b + E_OY] + fsin(M[b + E_T] * 2.4) * 0.6;
      double d = sign(M[b + B_X] - px);
      if (d != 0) M[b + E_DIR] = d;
    } else if (kind == EK_SAW) {
      M[b + B_X] += M[b + B_VX] * dt;
      int ahead = fi(M[b + B_X] + sign(M[b + B_VX]) * 0.5);
      double floorAhead = tileAt(ahead, fi(M[b + B_Y] - 0.2));
      if (isSolid(tileAt(ahead, fi(M[b + B_Y] + 0.4))) || (!isSolid(floorAhead) && floorAhead != 2) ||
          fabs_(M[b + B_X] - M[b + E_OX]) > 3) {
        M[b + B_VX] = -M[b + B_VX]; M[b + B_X] += M[b + B_VX] * dt * 2;
      }
    } else if (kind == EK_TURRET) {
      double d = sign(M[P_X] - M[b + B_X]);
      M[b + E_DIR] = d != 0 ? d : 1;
      M[b + E_CD] -= dt;
      double dx = fabs_(M[P_X] - M[b + B_X]), dy = fabs_(M[P_Y] - M[b + B_Y]);
      if (M[b + E_CD] <= 0 && dx < fmin_(13, M[G_VIEWW] / 2 + 1) && dy < 7 && M[P_DEAD] == 0) {
        M[b + E_CD] = 2.2;
        shoot(M[b + B_X] + M[b + E_DIR] * 0.6, M[b + B_Y] + 0.5, M[b + E_DIR] * 6.5, 0, 0, 6, 0.6, 0);
        ev(EV_SHOOT, M[b + B_X] + M[b + E_DIR] * 0.7, M[b + B_Y] + 0.5, M[b + E_DIR]);
      }
    } else if (kind == EK_BOSS) updateBoss(b, dt);
  }
}

/* ------------------------------------------------------- interactions */
static void updateInteractions(double dt) {
  if (M[P_DEAD] != 0) return;
  int nen = (int)M[G_NEN];
  for (int i = 0; i < nen; i++) {
    int e = enBase(i);
    if (M[e + E_ALIVE] == 0) continue;
    int kind = (int)M[e + E_KIND];
    if (kind == EK_BOSS && (M[e + E_STATE] == BS_SLEEP || M[e + E_STATE] == BS_DYING)) continue;
    if (!boxHit(P_X, e)) continue;
    if (kind == EK_BOSS) {
      if (M[P_VY] < 0 && M[P_PREVY] >= M[e + B_Y] + M[e + B_H] * 0.55 && M[P_DASHT] <= 0) {
        if (damageBoss()) stompBounce(1);
        else M[P_VY] = fmax_(M[P_VY], 8);
      } else hurtPlayer(M[e + B_X]);
    } else if (M[e + E_STOMP] == 0) {
      hurtPlayer(M[e + B_X]);
    } else if (M[P_DASHT] > 0) {
      killEnemy(e, 1);
    } else if (M[P_VY] < 0 && M[P_PREVY] >= M[e + B_Y] + M[e + B_H] * 0.5) {
      killEnemy(e, 0);
      stompBounce(0);
      M[G_FREEZE] = 0.04;
    } else hurtPlayer(M[e + B_X]);
  }

  int nc = (int)M[G_NCOINS];
  for (int i = 0; i < nc; i++) {
    int c = COIN_BASE + i * COIN_N;
    if (M[c + C_GOT] != 0) continue;
    double r = M[c + C_R];
    if (fabs_(M[P_X] - M[c + C_X]) < r + PW / 2 && fabs_(M[P_Y] + 0.45 - M[c + C_Y]) < r + 0.45) {
      int kind = (int)M[c + C_KIND];
      if (kind == SK_HEART) {
        if (M[P_HP] >= M[P_MAXHP]) { M[G_SCORE] += 250; ev(EV_HEART, M[c + C_X], M[c + C_Y], 0); }
        else { M[P_HP] += 1; ev(EV_HEART, M[c + C_X], M[c + C_Y], 1); }
      } else if (kind == SK_GEM) {
        M[G_COINS] += 5; M[G_SCORE] += 500; ev(EV_GEM, M[c + C_X], M[c + C_Y], 0);
      } else {
        M[G_COINS] += 1; M[G_SCORE] += 100; ev(EV_COIN, M[c + C_X], M[c + C_Y], 0);
      }
      M[c + C_GOT] = 1;
    }
  }

  int ns = (int)M[G_NSPR];
  for (int i = 0; i < ns; i++) {
    int s = SPR_BASE + i * SPR_N;
    M[s + SP_T] = fmax_(0, M[s + SP_T] - dt);
    if (fabs_(M[P_X] - M[s + SP_X]) < 0.7 && M[P_Y] >= M[s + SP_Y] - 0.1 && M[P_Y] < M[s + SP_Y] + 0.55 && M[P_VY] <= 0.5) {
      M[P_VY] = 31; M[P_GND] = 0; M[P_JUMPS] = 1; M[P_AIRDASH] = 0; M[P_DASHT] = 0; M[P_COYOTE] = 0;
      M[s + SP_T] = 0.3;
      ev(EV_SPRING, M[s + SP_X], M[s + SP_Y] + 0.4, 0);
    }
  }

  int nk = (int)M[G_NCK];
  for (int i = 0; i < nk; i++) {
    int c = CKT_BASE + i * CKT_N;
    if (M[c + CK_ON] == 0 && fabs_(M[P_X] - M[c + CK_X]) < 1.3 && fabs_(M[P_Y] - M[c + CK_Y]) < 2) {
      for (int j = 0; j < nk; j++) M[CKT_BASE + j * CKT_N + CK_ON] = 0;
      M[c + CK_ON] = 1;
      M[G_CHKX] = M[c + CK_X]; M[G_CHKY] = M[c + CK_Y];
      ev(EV_CHECK, M[c + CK_X], M[c + CK_Y], 0);
    }
  }

  int bi = (int)M[G_BOSS];
  if (bi >= 0) {
    int b = enBase(bi);
    if (M[b + E_STATE] == BS_SLEEP && M[b + E_ALIVE] != 0 && M[P_X] > M[G_BTRIG]) startBoss();
  }

  if (M[G_GOALON] != 0 && fabs_(M[P_X] - M[G_GOALX]) < 0.9 && fabs_(M[P_Y] + 0.5 - (M[G_GOALY] + 1.1)) < 1.5) {
    M[G_MODE] = MODE_CLEAR;
    ev(EV_COMPLETE, M[P_X], M[P_Y], 0);
  }
}

static void step(double dt) {
  if (M[G_FREEZE] > 0) { M[G_FREEZE] -= dt; return; }
  if ((int)M[G_MODE] != MODE_PLAY) {
    updatePlatforms(dt); updateEnemies(dt); updateShots(dt);
    return;
  }
  M[G_TIME] += dt;
  updatePlatforms(dt);
  if (M[P_DEAD] == 0) updatePlayer(dt);
  else { M[P_DEADT] += dt; if (M[P_DEADT] > 1.4) respawn(); }
  updateEnemies(dt);
  updateShots(dt);
  updateInteractions(dt);
}

EXPORT("advance") int advance(double rdt) {
  M[G_EVN] = 0;
  double acc = M[G_ACC] + rdt;
  int n = 0;
  while (acc >= DT && n < 12) { step(DT); acc -= DT; n++; }
  if (n >= 12) acc = 0;
  M[G_ACC] = acc; M[G_STEPS] = n;
  if (n > 0) { M[IN_JUMPPRESS] = 0; M[IN_DASHPRESS] = 0; }
  return n;
}

EXPORT("mem_get") double mem_get(int i) { return M[i]; }
EXPORT("mem_set") void mem_set(int i, double v) { M[i] = v; }
EXPORT("mem_ptr") int mem_ptr(void) { return (int)(long)&M[0]; }
