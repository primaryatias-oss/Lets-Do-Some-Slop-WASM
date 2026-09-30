# Slop Runner simulation core - Nim port (Nim -> C -> clang, wasm32, no GC / no runtime).
# Mirrors core/js/core.js line by line; all state lives in the flat array M.
import abi

const
  LEVEL_H = 14
  DT = 1.0 / 120.0
  PLW = 0.62
  PLH = 0.86
  MAX_SPEED = 8.2
  ACCEL_G = 95.0
  ACCEL_A = 62.0
  DECEL_G = 110.0
  DECEL_A = 26.0
  GRAVITY = 62.0
  FALL_GRAVITY = 74.0
  MAX_FALL = 27.0
  JUMP_V = 19.6
  DOUBLE_V = 17.2
  COYOTE = 0.11
  BUFFER = 0.13
  WALL_SLIDE = 3.4
  WALL_JUMP_X = 9.5
  WALL_JUMP_Y = 18.2
  WALL_LOCK = 0.16
  DASH_TIME = 0.17
  DASH_SPEED = 24.0
  DASH_CD = 0.75
  INVULN = 1.5
  STOMP_BOUNCE = 15.5

var M: array[MEM_SIZE, float64]

proc flr(x: float64): float64 {.importc: "__builtin_floor", nodecl.}
proc sqrtC(x: float64): float64 {.importc: "__builtin_sqrt", nodecl.}

proc f(x: int): float64 {.inline.} = float64(x)
proc fi(x: float64): int {.inline.} = int(flr(x))
proc ii(x: float64): int {.inline.} = int(x)
proc b2f(b: bool): float64 {.inline.} = (if b: 1.0 else: 0.0)
proc sign(v: float64): float64 = (if v > 0: 1.0 elif v < 0: -1.0 else: 0.0)
proc fmin(a, b: float64): float64 = (if a < b: a else: b)
proc fmax(a, b: float64): float64 = (if a > b: a else: b)
proc fabs(a: float64): float64 = (if a < 0: -a else: a)
proc clamp(v, a, b: float64): float64 = (if v < a: a elif v > b: b else: v)
proc approach(v, t, s: float64): float64 = (if v < t: fmin(v + s, t) else: fmax(v - s, t))

proc fsin(x: float64): float64 =
  let k = flr(x * 0.15915494309189535 + 0.5)
  var r = x - k * 6.283185307179586
  if r > 1.5707963267948966: r = 3.141592653589793 - r
  elif r < -1.5707963267948966: r = -3.141592653589793 - r
  let r2 = r * r
  return r * (1.0 + r2 * (-0.16666666666666666 + r2 * (0.008333333333333333 + r2 * (-0.0001984126984126984 +
         r2 * (2.7557319223985893e-6 + r2 * (-2.505210838544172e-8 + r2 * 1.6059043836821613e-10))))))
proc fcos(x: float64): float64 = fsin(x + 1.5707963267948966)

proc rnd(): float64 =
  var s = M[G_RNG] * 1664525.0 + 1013904223.0
  s = s - flr(s / 4294967296.0) * 4294967296.0
  M[G_RNG] = s
  return s / 4294967296.0
proc rndr(a, b: float64): float64 = a + rnd() * (b - a)

proc ev(t: int, x, y, a: float64) =
  let n = ii(M[G_EVN])
  if n < EV_MAX:
    let i = EV_BASE + n * EV_N
    M[i] = f(t)
    M[i + 1] = x
    M[i + 2] = y
    M[i + 3] = a
    M[G_EVN] = f(n + 1)

proc tileAt(tx, ty: int): float64 =
  let lw = ii(M[G_LW])
  if tx < 0 or tx >= lw: return 1
  if ty < 0 or ty >= LEVEL_H: return 0
  return M[TILE_BASE + ty * lw + tx]
proc isSolid(t: float64): bool = (t == 1 or t == 4)
proc setTile(tx, ty: int, v: float64) =
  M[TILE_BASE + ty * ii(M[G_LW]) + tx] = v

proc moveBody(b: int, dt: float64, drop: bool) =
  M[b + B_HX] = 0
  M[b + B_HY] = 0
  let hw = M[b + B_W] / 2
  M[b + B_X] += M[b + B_VX] * dt
  let y0 = fi(M[b + B_Y] + 0.02)
  let y1 = fi(M[b + B_Y] + M[b + B_H] - 0.02)
  let vx = M[b + B_VX]
  if vx > 0:
    let tx = fi(M[b + B_X] + hw)
    for ty in y0 .. y1:
      if isSolid(tileAt(tx, ty)):
        M[b + B_X] = f(tx) - hw - 1e-4
        M[b + B_VX] = 0
        M[b + B_HX] = 1
        break
  elif vx < 0:
    let tx = fi(M[b + B_X] - hw)
    for ty in y0 .. y1:
      if isSolid(tileAt(tx, ty)):
        M[b + B_X] = f(tx) + 1 + hw + 1e-4
        M[b + B_VX] = 0
        M[b + B_HX] = -1
        break
  let prevY = M[b + B_Y]
  M[b + B_Y] += M[b + B_VY] * dt
  let x0 = fi(M[b + B_X] - hw + 1e-3)
  let x1 = fi(M[b + B_X] + hw - 1e-3)
  M[b + B_GND] = 0
  if M[b + B_VY] <= 0:
    let ty = fi(M[b + B_Y])
    for tx in x0 .. x1:
      let t = tileAt(tx, ty)
      if isSolid(t) or (t == 2 and not drop and prevY >= f(ty) + 1 - 0.02):
        M[b + B_Y] = f(ty) + 1
        M[b + B_VY] = 0
        M[b + B_GND] = 1
        M[b + B_HY] = -1
        break
  else:
    let ty = fi(M[b + B_Y] + M[b + B_H])
    for tx in x0 .. x1:
      if isSolid(tileAt(tx, ty)):
        M[b + B_Y] = f(ty) - M[b + B_H] - 1e-4
        M[b + B_VY] = 0
        M[b + B_HY] = 1
        break

proc boxHit(a, b: int): bool =
  return fabs(M[a + B_X] - M[b + B_X]) < (M[a + B_W] + M[b + B_W]) / 2 and
    M[a + B_Y] < M[b + B_Y] + M[b + B_H] and M[a + B_Y] + M[a + B_H] > M[b + B_Y]

proc enBase(i: int): int = EN_BASE + i * EN_N
proc plBase(i: int): int = PLT_BASE + i * PLT_N
proc shBase(i: int): int = SH_BASE + i * SH_N

proc setDoor(closed: bool) =
  if M[G_DOORON] == 0: return
  var ty = ii(M[G_DOORY0])
  while ty <= ii(M[G_DOORY1]):
    setTile(ii(M[G_DOORX]), ty, (if closed: 4.0 else: 0.0))
    inc ty
  M[G_DOORCLOSED] = b2f(closed)
  ev(EV_DOOR, M[G_DOORX], 0, b2f(closed))

proc addEnemy(kind: int, x, y: float64) =
  let i = ii(M[G_NEN])
  if i >= EN_MAX: return
  M[G_NEN] = f(i + 1)
  let b = enBase(i)
  for k in 0 ..< EN_N: M[b + k] = 0
  M[b + B_X] = x
  M[b + B_Y] = y
  M[b + B_W] = 0.8
  M[b + B_H] = 0.62
  M[b + E_KIND] = f(kind)
  M[b + E_ALIVE] = 1
  M[b + E_STOMP] = 1
  let r0 = rnd()
  M[b + E_DIR] = (if r0 < 0.5: -1.0 else: 1.0)
  let r1 = rnd()
  M[b + E_T] = r1 * 10
  M[b + E_OX] = x
  M[b + E_OY] = y
  let r2 = rnd()
  M[b + E_CD] = 1 + r2
  if kind == EK_BAT:
    M[b + B_W] = 0.75
    M[b + B_H] = 0.5
  elif kind == EK_SAW:
    M[b + B_W] = 0.78
    M[b + B_H] = 0.78
    M[b + E_STOMP] = 0
    M[b + E_DIR] = 1
    M[b + B_VX] = 3
  elif kind == EK_TURRET:
    M[b + B_W] = 0.9
    M[b + B_H] = 0.8
    M[b + E_STOMP] = 0
  elif kind == EK_BOSS:
    M[b + B_W] = 2.5
    M[b + B_H] = 2.3
    M[b + E_HP] = 8
    M[b + E_HP0] = 8
    M[b + E_STATE] = f(BS_SLEEP)
    M[b + E_DIR] = -1
    M[G_BOSS] = f(i)

proc addPlatform(kind: int, x, ty: float64) =
  let i = ii(M[G_NPLT])
  if i >= PLT_MAX: return
  M[G_NPLT] = f(i + 1)
  let p = plBase(i)
  for k in 0 ..< PLT_N: M[p + k] = 0
  M[p + PT_KIND] = f(kind)
  M[p + PT_X] = x
  M[p + PT_X0] = x
  M[p + PT_TOP] = ty + 0.75
  M[p + PT_TOP0] = ty + 0.75
  M[p + PT_PREVTOP] = ty + 0.75
  M[p + PT_W] = (if kind == 2: 1.0 else: 3.0)
  M[p + PT_SOLID] = 1
  let r = rnd()
  M[p + PT_T] = r * 6
  M[p + PT_STATE] = f(CS_IDLE)

proc addGoal(x, y: float64) =
  M[G_GOALON] = 1
  M[G_GOALX] = x
  M[G_GOALY] = y

proc resetPlayer(x, y: float64) =
  M[P_X] = x; M[P_Y] = y; M[P_VX] = 0; M[P_VY] = 0; M[P_FACE] = 1; M[P_GND] = 0
  M[P_COYOTE] = 0; M[P_BUFFER] = 0; M[P_JUMPS] = 1; M[P_WALLDIR] = 0; M[P_WALLGRACE] = 0; M[P_WALLLOCK] = 0
  M[P_DASHT] = 0; M[P_DASHCD] = 0; M[P_AIRDASH] = 0; M[P_INV] = 0; M[P_DROPT] = 0; M[P_DEAD] = 0; M[P_DEADT] = 0
  M[P_RIDING] = -1; M[P_SAFEX] = x; M[P_SAFEY] = y; M[P_SAFET] = 0; M[P_PREVY] = y; M[P_WASGND] = 0

proc coreInit() {.exportc: "init", cdecl.} =
  for i in 0 ..< MEM_SIZE: M[i] = 0
  M[G_RNG] = 12345
  M[G_VIEWW] = 20
  M[P_W] = PLW
  M[P_H] = PLH
  M[P_MAXHP] = 3
  M[P_HP] = 3
  M[P_RIDING] = -1
  M[G_BOSS] = -1

proc loadLevel() {.exportc: "load_level", cdecl.} =
  M[G_NCOINS] = 0; M[G_NEN] = 0; M[G_NPLT] = 0; M[G_NSH] = 0; M[G_NSPR] = 0; M[G_NCK] = 0; M[G_BOSS] = -1
  M[G_TIME] = 0; M[G_COINS] = 0; M[G_KILLS] = 0; M[G_DEATHS] = 0; M[G_SCORE] = 0; M[G_BOSSKILLED] = 0
  M[G_BOSSACTIVE] = 0; M[G_FREEZE] = 0; M[G_COINSTOTAL] = 0; M[G_DOORCLOSED] = 0; M[G_ACC] = 0; M[G_EVN] = 0
  let n = ii(M[G_NSPEC])
  for s in 0 ..< n:
    let kind = ii(M[SPEC_BASE + s * SPEC_N])
    let x = M[SPEC_BASE + s * SPEC_N + 1]
    let y = M[SPEC_BASE + s * SPEC_N + 2]
    if kind <= SK_HEART:
      let i = ii(M[G_NCOINS])
      if i < COIN_MAX:
        M[G_NCOINS] = f(i + 1)
        let c = COIN_BASE + i * COIN_N
        M[c + C_KIND] = f(kind)
        M[c + C_X] = x
        M[c + C_Y] = y + 0.5
        M[c + C_GOT] = 0
        M[c + C_R] = (if kind == SK_COIN: 0.55 else: 0.65)
        if kind == SK_COIN: M[G_COINSTOTAL] += 1
        elif kind == SK_GEM: M[G_COINSTOTAL] += 5
    elif kind == SK_SLIME: addEnemy(EK_SLIME, x, y)
    elif kind == SK_BAT: addEnemy(EK_BAT, x, y + 0.5)
    elif kind == SK_SAW: addEnemy(EK_SAW, x, y + 0.02)
    elif kind == SK_TURRET: addEnemy(EK_TURRET, x, y)
    elif kind == SK_SPRING:
      let i = ii(M[G_NSPR])
      if i < SPR_MAX:
        M[G_NSPR] = f(i + 1)
        let q = SPR_BASE + i * SPR_N
        M[q + SP_X] = x
        M[q + SP_Y] = y
        M[q + SP_T] = 0
    elif kind == SK_PLAT_H: addPlatform(0, x, y)
    elif kind == SK_PLAT_V: addPlatform(1, x, y)
    elif kind == SK_CRUMBLE: addPlatform(2, x, y)
    elif kind == SK_CHECK:
      let i = ii(M[G_NCK])
      if i < CKT_MAX:
        M[G_NCK] = f(i + 1)
        let q = CKT_BASE + i * CKT_N
        M[q + CK_X] = x
        M[q + CK_Y] = y
        M[q + CK_ON] = 0
    elif kind == SK_BOSS: addEnemy(EK_BOSS, x, y)
  M[P_MAXHP] = 3
  M[P_HP] = 3
  M[G_CHKX] = M[G_SPAWNX]
  M[G_CHKY] = M[G_SPAWNY]
  resetPlayer(M[G_SPAWNX], M[G_SPAWNY])

# ---------------------------------------------------------------- player
proc resetBoss()

proc killPlayer() =
  M[P_DEAD] = 1
  M[P_DEADT] = 0
  M[P_HP] = 0
  M[G_DEATHS] += 1
  ev(EV_DIE, M[P_X], M[P_Y] + 0.5, 0)

proc hurtPlayer(srcX: float64): bool =
  if M[P_INV] > 0 or M[P_DASHT] > 0 or M[P_DEAD] != 0: return false
  M[P_HP] -= 1
  M[P_INV] = INVULN
  M[G_FREEZE] = 0.08
  ev(EV_HURT, M[P_X], M[P_Y] + 0.5, 0)
  M[P_VX] = (if M[P_X] < srcX: -1.0 else: 1.0) * 8.5
  M[P_VY] = 12
  M[P_WALLLOCK] = 0.18
  M[P_DASHT] = 0
  if M[P_HP] <= 0: killPlayer()
  return true

proc pitFall() =
  if M[P_DEAD] != 0: return
  M[P_HP] -= 1
  ev(EV_PIT, M[P_X], M[P_Y], 0)
  if M[P_HP] <= 0:
    killPlayer()
    return
  M[P_X] = M[P_SAFEX]
  M[P_Y] = M[P_SAFEY] + 0.05
  M[P_VX] = 0
  M[P_VY] = 0
  M[P_INV] = 2
  ev(EV_RESPAWN, M[P_X], M[P_Y] + 0.4, 1)

proc respawn() =
  resetPlayer(M[G_CHKX], M[G_CHKY])
  M[P_HP] = M[P_MAXHP]
  M[P_INV] = 2
  if M[G_BOSS] >= 0 and M[G_BOSSACTIVE] != 0: resetBoss()
  ev(EV_RESPAWN, M[P_X], M[P_Y] + 0.5, 0)

proc stompBounce(strong: bool) =
  M[P_VY] = (if M[IN_JUMPHELD] != 0: STOMP_BOUNCE + 3.5 else: STOMP_BOUNCE)
  if strong: M[P_VY] += 2
  M[P_JUMPS] = 1
  M[P_AIRDASH] = 0
  M[P_GND] = 0
  M[P_DASHT] = 0

proc updatePlayer(dt: float64) =
  let ax = M[IN_AX]
  M[P_PREVY] = M[P_Y]
  M[P_INV] = fmax(0, M[P_INV] - dt)
  M[P_DASHCD] -= dt
  M[P_WALLLOCK] -= dt
  M[P_BUFFER] -= dt
  M[P_DROPT] -= dt
  M[P_WALLGRACE] -= dt

  if M[IN_JUMPPRESS] != 0:
    M[IN_JUMPPRESS] = 0
    M[P_BUFFER] = BUFFER
  let wantDash = M[IN_DASHPRESS] != 0
  if wantDash: M[IN_DASHPRESS] = 0

  let rid = ii(M[P_RIDING])
  if rid >= 0:
    let p = plBase(rid)
    if M[p + PT_SOLID] != 0 and M[P_VY] <= 0:
      M[P_X] += M[p + PT_DX]
      M[P_Y] = M[p + PT_TOP]

  if M[P_GND] != 0:
    M[P_COYOTE] = COYOTE
    M[P_JUMPS] = 1
    M[P_AIRDASH] = 0
  else:
    M[P_COYOTE] -= dt

  if wantDash and M[P_DASHCD] <= 0 and M[P_AIRDASH] == 0:
    M[P_DASHT] = DASH_TIME
    M[P_DASHCD] = DASH_CD
    M[P_DASHDIR] = (if ax != 0: ax else: M[P_FACE])
    M[P_FACE] = M[P_DASHDIR]
    if M[P_GND] == 0: M[P_AIRDASH] = 1
    ev(EV_DASH, M[P_X], M[P_Y] + 0.4, M[P_DASHDIR])

  if M[P_DASHT] > 0:
    M[P_DASHT] -= dt
    M[P_VX] = M[P_DASHDIR] * DASH_SPEED
    M[P_VY] = 0
    if M[P_DASHT] <= 0: M[P_VX] = M[P_DASHDIR] * MAX_SPEED
  else:
    if M[P_WALLLOCK] <= 0:
      let target = ax * MAX_SPEED
      var acc: float64
      if ax == 0:
        acc = (if M[P_GND] != 0: DECEL_G else: DECEL_A)
      else:
        acc = (if M[P_GND] != 0: ACCEL_G else: ACCEL_A)
        if sign(M[P_VX]) == -ax: acc *= 1.6
      M[P_VX] = approach(M[P_VX], target, acc * dt)
      if ax != 0: M[P_FACE] = ax

    M[P_WALLDIR] = 0
    if M[P_GND] == 0 and ax != 0:
      let tx = fi(M[P_X] + ax * (PLW / 2 + 0.08))
      if isSolid(tileAt(tx, fi(M[P_Y] + 0.3))) or isSolid(tileAt(tx, fi(M[P_Y] + 0.75))):
        M[P_WALLDIR] = ax
        M[P_WALLGRACE] = 0.1
        M[P_WALLMEM] = ax

    if M[P_BUFFER] > 0:
      if M[IN_DOWNHELD] != 0 and M[P_GND] != 0 and tileAt(fi(M[P_X]), fi(M[P_Y] - 0.05)) == 2:
        M[P_DROPT] = 0.22
        M[P_BUFFER] = 0
        M[P_Y] -= 0.06
        M[P_GND] = 0
      elif M[P_COYOTE] > 0:
        M[P_VY] = JUMP_V
        M[P_BUFFER] = 0
        M[P_COYOTE] = 0
        M[P_GND] = 0
        ev(EV_JUMP, M[P_X], M[P_Y] + 0.05, 0)
      elif M[P_WALLGRACE] > 0 and M[P_WALLMEM] != 0:
        M[P_VX] = -M[P_WALLMEM] * WALL_JUMP_X
        M[P_VY] = WALL_JUMP_Y
        M[P_WALLLOCK] = WALL_LOCK
        M[P_FACE] = -M[P_WALLMEM]
        M[P_BUFFER] = 0
        M[P_WALLGRACE] = 0
        M[P_JUMPS] = 1
        M[P_AIRDASH] = 0
        ev(EV_WALLJUMP, M[P_X] + M[P_WALLMEM] * 0.3, M[P_Y] + 0.5, M[P_WALLMEM])
      elif M[P_JUMPS] > 0:
        M[P_VY] = DOUBLE_V
        M[P_JUMPS] -= 1
        M[P_BUFFER] = 0
        ev(EV_DOUBLE, M[P_X], M[P_Y] + 0.05, 0)

    let g = (if M[P_VY] > 0: (if M[IN_JUMPHELD] != 0: GRAVITY else: GRAVITY * 2.4) else: FALL_GRAVITY)
    M[P_VY] = fmax(M[P_VY] - g * dt, -MAX_FALL)
    if M[P_WALLDIR] != 0 and M[P_VY] < -WALL_SLIDE: M[P_VY] = -WALL_SLIDE

  let vyPre = M[P_VY]
  moveBody(P_X, dt, M[P_DROPT] > 0)

  M[P_RIDING] = -1
  if vyPre <= 0 and M[P_DASHT] <= 0:
    let np = ii(M[G_NPLT])
    for i in 0 ..< np:
      let p = plBase(i)
      if M[p + PT_SOLID] == 0: continue
      if fabs(M[P_X] - M[p + PT_X]) < M[p + PT_W] / 2 + PLW / 2 - 0.08 and M[P_PREVY] >= M[p + PT_PREVTOP] - 0.12 and
          M[P_Y] <= M[p + PT_TOP] + 0.02 and M[P_Y] >= M[p + PT_TOP] - 0.6:
        M[P_Y] = M[p + PT_TOP]
        M[P_VY] = 0
        M[P_GND] = 1
        M[P_RIDING] = f(i)
        if M[p + PT_KIND] == 2 and M[p + PT_STATE] == f(CS_IDLE):
          M[p + PT_STATE] = f(CS_SHAKE)
          M[p + PT_TIMER] = 0.45
        break

  if M[P_GND] != 0 and M[P_WASGND] == 0 and vyPre < -9: ev(EV_LAND, M[P_X], M[P_Y] + 0.05, 0)
  if M[P_HY] == 1 and M[P_VY] == 0: M[P_VY] = -1
  M[P_WASGND] = M[P_GND]

  if M[P_GND] != 0 and M[P_RIDING] < 0:
    let ty = fi(M[P_Y]) - 1
    if isSolid(tileAt(fi(M[P_X] - 0.6), ty)) and isSolid(tileAt(fi(M[P_X] + 0.6), ty)) and
        isSolid(tileAt(fi(M[P_X] - 1.6), ty)) and isSolid(tileAt(fi(M[P_X] + 1.6), ty)) and
        tileAt(fi(M[P_X]), fi(M[P_Y])) != 3:
      M[P_SAFET] += dt
      if M[P_SAFET] > 0.35:
        M[P_SAFEX] = M[P_X]
        M[P_SAFEY] = M[P_Y]
    else:
      M[P_SAFET] = 0
  else:
    M[P_SAFET] = 0

  if M[P_INV] <= 0 and M[P_DASHT] <= 0:
    let x0 = fi(M[P_X] - PLW / 2)
    let x1 = fi(M[P_X] + PLW / 2)
    let y0 = fi(M[P_Y])
    let y1 = fi(M[P_Y] + PLH * 0.5)
    var done = false
    var ty = y0
    while ty <= y1 and not done:
      for tx in x0 .. x1:
        if tileAt(tx, ty) == 3:
          if M[P_X] + PLW / 2 > f(tx) + 0.15 and M[P_X] - PLW / 2 < f(tx) + 0.85 and M[P_Y] < f(ty) + 0.5:
            if hurtPlayer(f(tx) + 0.5):
              M[P_VY] = 15
              M[P_VX] = (if M[P_X] < f(tx) + 0.5: -1.0 else: 1.0) * 5
            done = true
            break
      inc ty

  if M[P_Y] < -2.5: pitFall()

# ------------------------------------------------------------- platforms
proc updatePlatforms(dt: float64) =
  let n = ii(M[G_NPLT])
  for i in 0 ..< n:
    let p = plBase(i)
    M[p + PT_PREVTOP] = M[p + PT_TOP]
    M[p + PT_DX] = 0
    M[p + PT_DY] = 0
    M[p + PT_T] += dt
    let kind = ii(M[p + PT_KIND])
    if kind == 0:
      let nx = M[p + PT_X0] + fsin(M[p + PT_T] * 1.05) * 2.4
      M[p + PT_DX] = nx - M[p + PT_X]
      M[p + PT_X] = nx
    elif kind == 1:
      let nt = M[p + PT_TOP0] + (1 - fcos(M[p + PT_T] * 0.95)) / 2 * 5.0
      M[p + PT_DY] = nt - M[p + PT_TOP]
      M[p + PT_TOP] = nt
    else:
      let st = ii(M[p + PT_STATE])
      if st == CS_SHAKE:
        M[p + PT_TIMER] -= dt
        if M[p + PT_TIMER] <= 0:
          M[p + PT_STATE] = f(CS_FALL)
          M[p + PT_SOLID] = 0
          M[p + PT_VY] = 0
          ev(EV_CRUMBLE, M[p + PT_X], M[p + PT_TOP], 0)
      elif st == CS_FALL:
        M[p + PT_VY] -= 32 * dt
        M[p + PT_TOP] += M[p + PT_VY] * dt
        if M[p + PT_TOP] < -3:
          M[p + PT_STATE] = f(CS_GONE)
          M[p + PT_TIMER] = 2.6
      elif st == CS_GONE:
        M[p + PT_TIMER] -= dt
        if M[p + PT_TIMER] <= 0:
          M[p + PT_STATE] = f(CS_IDLE)
          M[p + PT_SOLID] = 1
          M[p + PT_TOP] = M[p + PT_TOP0]
          M[p + PT_PREVTOP] = M[p + PT_TOP]

# ----------------------------------------------------------------- shots
proc shoot(x, y, vx, vy, g, life, size, purple: float64) =
  let i = ii(M[G_NSH])
  if i >= SH_MAX: return
  M[G_NSH] = f(i + 1)
  let s = shBase(i)
  M[s + S_X] = x
  M[s + S_Y] = y
  M[s + S_VX] = vx
  M[s + S_VY] = vy
  M[s + S_G] = g
  M[s + S_LIFE] = life
  M[s + S_R] = size * 0.36
  M[s + S_PURPLE] = purple
  M[s + S_SIZE] = size

proc removeShot(i: int) =
  let last = ii(M[G_NSH]) - 1
  if i != last:
    let a = shBase(i)
    let b = shBase(last)
    for k in 0 ..< SH_N: M[a + k] = M[b + k]
  M[G_NSH] = f(last)

proc updateShots(dt: float64) =
  var i = ii(M[G_NSH]) - 1
  while i >= 0:
    let s = shBase(i)
    M[s + S_LIFE] -= dt
    M[s + S_VY] -= M[s + S_G] * dt
    M[s + S_X] += M[s + S_VX] * dt
    M[s + S_Y] += M[s + S_VY] * dt
    var dead = M[s + S_LIFE] <= 0 or M[s + S_Y] < -3
    if not dead and isSolid(tileAt(fi(M[s + S_X]), fi(M[s + S_Y]))):
      dead = true
      ev(EV_SHOTHIT, M[s + S_X], M[s + S_Y], M[s + S_PURPLE])
    if not dead and M[P_DEAD] == 0 and fabs(M[P_X] - M[s + S_X]) < PLW / 2 + M[s + S_R] and
        M[s + S_Y] > M[P_Y] - M[s + S_R] and M[s + S_Y] < M[P_Y] + PLH + M[s + S_R]:
      if hurtPlayer(M[s + S_X]): dead = true
    if dead: removeShot(i)
    dec i

# --------------------------------------------------------------- enemies
proc killEnemy(b: int, byDash: bool) =
  M[b + E_ALIVE] = 0
  M[G_KILLS] += 1
  M[G_SCORE] += 150
  ev(EV_KILL, M[b + B_X], M[b + B_Y] + M[b + B_H] / 2, M[b + E_KIND] + (if byDash: 10.0 else: 0.0))

proc startBoss() =
  let b = enBase(ii(M[G_BOSS]))
  M[G_BOSSACTIVE] = 1
  setDoor(true)
  M[b + B_X] = M[G_BSPECX]
  M[b + B_Y] = f(LEVEL_H + 1)
  M[b + B_VX] = 0
  M[b + B_VY] = 0
  M[b + E_STATE] = f(BS_INTRO)
  M[b + E_HP] = M[b + E_HP0]
  M[b + E_INV] = 0
  M[b + E_ALIVE] = 1
  ev(EV_BOSSSTART, M[b + B_X], M[b + B_Y], 0)

proc resetBoss() =
  let b = enBase(ii(M[G_BOSS]))
  M[G_BOSSACTIVE] = 0
  M[b + E_STATE] = f(BS_SLEEP)
  M[b + E_HP] = M[b + E_HP0]
  M[b + E_ALIVE] = 1
  M[b + E_INV] = 0
  M[b + B_X] = M[G_BSPECX]
  M[b + B_Y] = M[G_BSPECY]
  setDoor(false)
  M[G_NSH] = 0

proc damageBoss(): bool =
  let b = enBase(ii(M[G_BOSS]))
  let st = ii(M[b + E_STATE])
  if M[b + E_INV] > 0 or st == BS_INTRO or st == BS_DYING: return false
  M[b + E_HP] -= 1
  M[b + E_INV] = 1.1
  M[G_FREEZE] = 0.1
  ev(EV_BOSSHIT, M[b + B_X], M[b + B_Y], M[b + E_HP])
  if M[b + E_HP] <= 0:
    M[b + E_STATE] = f(BS_DYING)
    M[b + E_ST] = 1.8
    M[b + B_VX] = 0
    M[G_NSH] = 0
  return true

proc bossLand(b: int, variant: float64) = ev(EV_BOOM, M[b + B_X], M[b + B_Y], variant)

proc updateBoss(b: int, dt: float64) =
  let state = ii(M[b + E_STATE])
  if state == BS_SLEEP: return
  M[b + E_INV] = fmax(0, M[b + E_INV] - dt)
  M[b + E_ST] -= dt
  let dx = M[P_X] - M[b + B_X]
  let lowHp = M[b + E_HP] <= 3
  if state == BS_INTRO:
    M[b + B_VY] = fmax(M[b + B_VY] - 60 * dt, -30)
    moveBody(b, dt, false)
    if M[b + B_GND] != 0:
      M[b + E_STATE] = f(BS_IDLE)
      M[b + E_ST] = 1.0
      bossLand(b, 1)
  elif state == BS_IDLE:
    let d = sign(dx)
    M[b + E_DIR] = (if d != 0: d else: 1.0)
    M[b + B_VX] = 0
    M[b + B_VY] = -2
    moveBody(b, dt, false)
    if M[b + E_ST] <= 0:
      let r = rnd()
      if r < 0.5:
        M[b + E_STATE] = f(BS_JUMP)
        M[b + B_VY] = 25
        M[b + B_VX] = clamp(dx / 1.2, -11, 11)
        ev(EV_SPRING, M[b + B_X], M[b + B_Y], 1)
      elif r < 0.8:
        M[b + E_STATE] = f(BS_SHOOT)
        M[b + E_ST] = 0.5
        M[b + E_SHOTS] = (if lowHp: 5.0 else: 3.0)
      else:
        M[b + E_STATE] = f(BS_CHARGE)
        M[b + E_ST] = 1.3
        M[b + E_DIR] = (if d != 0: d else: 1.0)
  elif state == BS_JUMP:
    M[b + B_VY] = fmax(M[b + B_VY] - 62 * dt, -30)
    moveBody(b, dt, false)
    if M[b + B_HX] != 0: M[b + B_VX] = 0
    if M[b + B_GND] != 0:
      M[b + E_STATE] = f(BS_RECOVER)
      M[b + E_ST] = (if lowHp: 0.8 else: 1.2)
      M[b + B_VX] = 0
      bossLand(b, 0)
      shoot(M[b + B_X] - 1.3, M[b + B_Y] + 0.35, -7, 0, 0, 4, 0.7, 1)
      shoot(M[b + B_X] + 1.3, M[b + B_Y] + 0.35, 7, 0, 0, 4, 0.7, 1)
  elif state == BS_RECOVER:
    M[b + B_VX] = 0
    M[b + B_VY] = -2
    moveBody(b, dt, false)
    if M[b + E_ST] <= 0:
      M[b + E_STATE] = f(BS_IDLE)
      M[b + E_ST] = (if lowHp: 0.35 else: 0.7)
  elif state == BS_SHOOT:
    M[b + B_VX] = 0
    M[b + B_VY] = -2
    moveBody(b, dt, false)
    let d = sign(dx)
    M[b + E_DIR] = (if d != 0: d else: 1.0)
    if M[b + E_ST] <= 0 and M[b + E_SHOTS] > 0:
      var ux = M[P_X] - M[b + B_X]
      var uy = M[P_Y] + 0.4 - (M[b + B_Y] + 1.4)
      var len = sqrtC(ux * ux + uy * uy)
      if len < 1e-6:
        ux = 1
        uy = 0
        len = 1
      ux /= len
      uy /= len
      let off = (rnd() - 0.5) * 0.35
      var vx = ux - uy * off
      var vy = uy + ux * off
      let l2 = sqrtC(vx * vx + vy * vy)
      vx = vx / l2 * 7.5
      vy = vy / l2 * 7.5
      shoot(M[b + B_X] + M[b + E_DIR] * 1.1, M[b + B_Y] + 1.4, vx, vy, 0, 5, 0.6, 1)
      ev(EV_SHOOT, M[b + B_X] + M[b + E_DIR] * 1.1, M[b + B_Y] + 1.4, M[b + E_DIR])
      M[b + E_SHOTS] -= 1
      M[b + E_ST] = 0.3
    elif M[b + E_SHOTS] <= 0 and M[b + E_ST] <= 0:
      M[b + E_STATE] = f(BS_IDLE)
      M[b + E_ST] = 0.7
  elif state == BS_CHARGE:
    M[b + B_VY] = fmax(M[b + B_VY] - 60 * dt, -30)
    if M[b + E_ST] > 0.75: M[b + B_VX] = 0
    else: M[b + B_VX] = M[b + E_DIR] * (if lowHp: 15.0 else: 12.0)
    moveBody(b, dt, false)
    if M[b + E_ST] <= 0.75 and M[b + B_HX] != 0:
      M[b + E_STATE] = f(BS_RECOVER)
      M[b + E_ST] = 1.3
      M[b + B_VX] = 0
      bossLand(b, 2)
    elif M[b + E_ST] <= 0:
      M[b + E_STATE] = f(BS_RECOVER)
      M[b + E_ST] = 0.8
      M[b + B_VX] = 0
  elif state == BS_DYING:
    M[b + B_VX] = 0
    if rnd() < 0.5:
      let rx = rndr(-1.2, 1.2)
      let ry = rndr(0, 2.4)
      ev(EV_BOSSEXPLODE, M[b + B_X] + rx, M[b + B_Y] + ry, 0)
    if M[b + E_ST] <= 0:
      M[b + E_ALIVE] = 0
      M[G_SCORE] += 3000
      M[G_BOSSKILLED] = 1
      M[G_BOSSACTIVE] = 0
      setDoor(false)
      addGoal(M[b + B_X], 2)
      ev(EV_BOSSDEAD, M[b + B_X], M[b + B_Y], 0)

proc updateEnemies(dt: float64) =
  let n = ii(M[G_NEN])
  for i in 0 ..< n:
    let b = enBase(i)
    if M[b + E_ALIVE] == 0: continue
    M[b + E_T] += dt
    let kind = ii(M[b + E_KIND])
    if kind == EK_SLIME:
      M[b + B_VY] = fmax(M[b + B_VY] - 55 * dt, -25)
      M[b + B_VX] = M[b + E_DIR] * 1.9
      moveBody(b, dt, false)
      if M[b + B_HX] != 0:
        M[b + E_DIR] = -M[b + E_DIR]
      elif M[b + B_GND] != 0:
        let tx = fi(M[b + B_X] + M[b + E_DIR] * (M[b + B_W] / 2 + 0.12))
        let ty = fi(M[b + B_Y] - 0.1)
        let t = tileAt(tx, ty)
        if not isSolid(t) and t != 2:
          M[b + E_DIR] = -M[b + E_DIR]
        elif tileAt(tx, fi(M[b + B_Y] + 0.1)) == 3:
          M[b + E_DIR] = -M[b + E_DIR]
    elif kind == EK_BAT:
      let near = fabs(M[P_X] - M[b + E_OX]) < 8
      if near and M[P_DEAD] == 0: M[b + E_OX] += sign(M[P_X] - M[b + E_OX]) * 0.9 * dt
      let px = M[b + B_X]
      M[b + B_X] = M[b + E_OX] + fsin(M[b + E_T] * 1.3) * 3.2
      M[b + B_Y] = M[b + E_OY] + fsin(M[b + E_T] * 2.4) * 0.6
      let d = sign(M[b + B_X] - px)
      if d != 0: M[b + E_DIR] = d
    elif kind == EK_SAW:
      M[b + B_X] += M[b + B_VX] * dt
      let ahead = fi(M[b + B_X] + sign(M[b + B_VX]) * 0.5)
      let floorAhead = tileAt(ahead, fi(M[b + B_Y] - 0.2))
      if isSolid(tileAt(ahead, fi(M[b + B_Y] + 0.4))) or (not isSolid(floorAhead) and floorAhead != 2) or
          fabs(M[b + B_X] - M[b + E_OX]) > 3:
        M[b + B_VX] = -M[b + B_VX]
        M[b + B_X] += M[b + B_VX] * dt * 2
    elif kind == EK_TURRET:
      let d = sign(M[P_X] - M[b + B_X])
      M[b + E_DIR] = (if d != 0: d else: 1.0)
      M[b + E_CD] -= dt
      let dx = fabs(M[P_X] - M[b + B_X])
      let dy = fabs(M[P_Y] - M[b + B_Y])
      if M[b + E_CD] <= 0 and dx < fmin(13, M[G_VIEWW] / 2 + 1) and dy < 7 and M[P_DEAD] == 0:
        M[b + E_CD] = 2.2
        shoot(M[b + B_X] + M[b + E_DIR] * 0.6, M[b + B_Y] + 0.5, M[b + E_DIR] * 6.5, 0, 0, 6, 0.6, 0)
        ev(EV_SHOOT, M[b + B_X] + M[b + E_DIR] * 0.7, M[b + B_Y] + 0.5, M[b + E_DIR])
    elif kind == EK_BOSS:
      updateBoss(b, dt)

# ---------------------------------------------------------- interactions
proc updateInteractions(dt: float64) =
  if M[P_DEAD] != 0: return
  let nen = ii(M[G_NEN])
  for i in 0 ..< nen:
    let e = enBase(i)
    if M[e + E_ALIVE] == 0: continue
    let kind = ii(M[e + E_KIND])
    if kind == EK_BOSS and (M[e + E_STATE] == f(BS_SLEEP) or M[e + E_STATE] == f(BS_DYING)): continue
    if not boxHit(P_X, e): continue
    if kind == EK_BOSS:
      if M[P_VY] < 0 and M[P_PREVY] >= M[e + B_Y] + M[e + B_H] * 0.55 and M[P_DASHT] <= 0:
        if damageBoss(): stompBounce(true)
        else: M[P_VY] = fmax(M[P_VY], 8)
      else:
        discard hurtPlayer(M[e + B_X])
    elif M[e + E_STOMP] == 0:
      discard hurtPlayer(M[e + B_X])
    elif M[P_DASHT] > 0:
      killEnemy(e, true)
    elif M[P_VY] < 0 and M[P_PREVY] >= M[e + B_Y] + M[e + B_H] * 0.5:
      killEnemy(e, false)
      stompBounce(false)
      M[G_FREEZE] = 0.04
    else:
      discard hurtPlayer(M[e + B_X])

  let nc = ii(M[G_NCOINS])
  for i in 0 ..< nc:
    let c = COIN_BASE + i * COIN_N
    if M[c + C_GOT] != 0: continue
    let r = M[c + C_R]
    if fabs(M[P_X] - M[c + C_X]) < r + PLW / 2 and fabs(M[P_Y] + 0.45 - M[c + C_Y]) < r + 0.45:
      let kind = ii(M[c + C_KIND])
      if kind == SK_HEART:
        if M[P_HP] >= M[P_MAXHP]:
          M[G_SCORE] += 250
          ev(EV_HEART, M[c + C_X], M[c + C_Y], 0)
        else:
          M[P_HP] += 1
          ev(EV_HEART, M[c + C_X], M[c + C_Y], 1)
      elif kind == SK_GEM:
        M[G_COINS] += 5
        M[G_SCORE] += 500
        ev(EV_GEM, M[c + C_X], M[c + C_Y], 0)
      else:
        M[G_COINS] += 1
        M[G_SCORE] += 100
        ev(EV_COIN, M[c + C_X], M[c + C_Y], 0)
      M[c + C_GOT] = 1

  let ns = ii(M[G_NSPR])
  for i in 0 ..< ns:
    let s = SPR_BASE + i * SPR_N
    M[s + SP_T] = fmax(0, M[s + SP_T] - dt)
    if fabs(M[P_X] - M[s + SP_X]) < 0.7 and M[P_Y] >= M[s + SP_Y] - 0.1 and M[P_Y] < M[s + SP_Y] + 0.55 and M[P_VY] <= 0.5:
      M[P_VY] = 31
      M[P_GND] = 0
      M[P_JUMPS] = 1
      M[P_AIRDASH] = 0
      M[P_DASHT] = 0
      M[P_COYOTE] = 0
      M[s + SP_T] = 0.3
      ev(EV_SPRING, M[s + SP_X], M[s + SP_Y] + 0.4, 0)

  let nk = ii(M[G_NCK])
  for i in 0 ..< nk:
    let c = CKT_BASE + i * CKT_N
    if M[c + CK_ON] == 0 and fabs(M[P_X] - M[c + CK_X]) < 1.3 and fabs(M[P_Y] - M[c + CK_Y]) < 2:
      for j in 0 ..< nk: M[CKT_BASE + j * CKT_N + CK_ON] = 0
      M[c + CK_ON] = 1
      M[G_CHKX] = M[c + CK_X]
      M[G_CHKY] = M[c + CK_Y]
      ev(EV_CHECK, M[c + CK_X], M[c + CK_Y], 0)

  let bi = ii(M[G_BOSS])
  if bi >= 0:
    let b = enBase(bi)
    if M[b + E_STATE] == f(BS_SLEEP) and M[b + E_ALIVE] != 0 and M[P_X] > M[G_BTRIG]: startBoss()

  if M[G_GOALON] != 0 and fabs(M[P_X] - M[G_GOALX]) < 0.9 and fabs(M[P_Y] + 0.5 - (M[G_GOALY] + 1.1)) < 1.5:
    M[G_MODE] = f(MODE_CLEAR)
    ev(EV_COMPLETE, M[P_X], M[P_Y], 0)

proc step(dt: float64) =
  if M[G_FREEZE] > 0:
    M[G_FREEZE] -= dt
    return
  if ii(M[G_MODE]) != MODE_PLAY:
    updatePlatforms(dt)
    updateEnemies(dt)
    updateShots(dt)
    return
  M[G_TIME] += dt
  updatePlatforms(dt)
  if M[P_DEAD] == 0:
    updatePlayer(dt)
  else:
    M[P_DEADT] += dt
    if M[P_DEADT] > 1.4: respawn()
  updateEnemies(dt)
  updateShots(dt)
  updateInteractions(dt)

proc advance(rdt: float64): int32 {.exportc: "advance", cdecl.} =
  M[G_EVN] = 0
  var acc = M[G_ACC] + rdt
  var n = 0
  while acc >= DT and n < 12:
    step(DT)
    acc -= DT
    inc n
  if n >= 12: acc = 0
  M[G_ACC] = acc
  M[G_STEPS] = f(n)
  if n > 0:
    M[IN_JUMPPRESS] = 0
    M[IN_DASHPRESS] = 0
  return int32(n)

proc memGet(i: int32): float64 {.exportc: "mem_get", cdecl.} = M[i]
proc memSet(i: int32, v: float64) {.exportc: "mem_set", cdecl.} = M[i] = v
proc memPtr(): int32 {.exportc: "mem_ptr", cdecl.} = cast[int32](addr M[0])
