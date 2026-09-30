// Slop Runner simulation core - Go port (GOOS=wasip1 reactor, //go:wasmexport).
// Mirrors core/js/core.js line by line; all state lives in the flat array M.
package main

import (
	"math"
	"unsafe"
)

const LEVEL_H = 14

// Package-level vars (not consts): keeps every float op a runtime IEEE-754 op,
// exactly like the other ports (Go folds untyped constant expressions exactly).
var (
	DT                                                   float64 = 1.0 / 120.0
	PW, PH                                               float64 = 0.62, 0.86
	MAX_SPEED, ACCEL_G, ACCEL_A, DECEL_G, DECEL_A        float64 = 8.2, 95.0, 62.0, 110.0, 26.0
	GRAVITY, FALL_GRAVITY, MAX_FALL                      float64 = 62.0, 74.0, 27.0
	JUMP_V, DOUBLE_V                                     float64 = 19.6, 17.2
	COYOTE, BUFFER                                       float64 = 0.11, 0.13
	WALL_SLIDE, WALL_JUMP_X, WALL_JUMP_Y, WALL_LOCK      float64 = 3.4, 9.5, 18.2, 0.16
	DASH_TIME, DASH_SPEED, DASH_CD                       float64 = 0.17, 24.0, 0.75
	INVULN, STOMP_BOUNCE                                 float64 = 1.5, 15.5
	M                                                    [MEM_SIZE]float64
)

func sign(v float64) float64 {
	if v > 0 {
		return 1
	}
	if v < 0 {
		return -1
	}
	return 0
}
func fmin(a, b float64) float64 {
	if a < b {
		return a
	}
	return b
}
func fmax(a, b float64) float64 {
	if a > b {
		return a
	}
	return b
}
func fabs(a float64) float64 {
	if a < 0 {
		return -a
	}
	return a
}
func clamp(v, a, b float64) float64 {
	if v < a {
		return a
	}
	if v > b {
		return b
	}
	return v
}
func approach(v, t, s float64) float64 {
	if v < t {
		return fmin(v+s, t)
	}
	return fmax(v-s, t)
}
func fi(x float64) int   { return int(math.Floor(x)) }
func ii(x float64) int   { return int(x) }
func f(x int) float64    { return float64(x) }
func b2f(b bool) float64 {
	if b {
		return 1
	}
	return 0
}

func fsin(x float64) float64 {
	k := math.Floor(x*0.15915494309189535 + 0.5)
	r := x - k*6.283185307179586
	if r > 1.5707963267948966 {
		r = 3.141592653589793 - r
	} else if r < -1.5707963267948966 {
		r = -3.141592653589793 - r
	}
	r2 := r * r
	return r * (1.0 + r2*(-0.16666666666666666+r2*(0.008333333333333333+r2*(-0.0001984126984126984+
		r2*(2.7557319223985893e-6+r2*(-2.505210838544172e-8+r2*1.6059043836821613e-10))))))
}
func fcos(x float64) float64 { return fsin(x + 1.5707963267948966) }

func rnd() float64 {
	s := M[G_RNG]*1664525.0 + 1013904223.0
	s = s - math.Floor(s/4294967296.0)*4294967296.0
	M[G_RNG] = s
	return s / 4294967296.0
}
func rndr(a, b float64) float64 { return a + rnd()*(b-a) }
func ev(t int, x, y, a float64) {
	n := ii(M[G_EVN])
	if n < EV_MAX {
		i := EV_BASE + n*EV_N
		M[i] = f(t)
		M[i+1] = x
		M[i+2] = y
		M[i+3] = a
		M[G_EVN] = f(n + 1)
	}
}

func tileAt(tx, ty int) float64 {
	lw := ii(M[G_LW])
	if tx < 0 || tx >= lw {
		return 1
	}
	if ty < 0 || ty >= LEVEL_H {
		return 0
	}
	return M[TILE_BASE+ty*lw+tx]
}
func isSolid(t float64) bool { return t == 1 || t == 4 }
func setTile(tx, ty int, v float64) {
	M[TILE_BASE+ty*ii(M[G_LW])+tx] = v
}

func moveBody(b int, dt float64, drop bool) {
	M[b+B_HX] = 0
	M[b+B_HY] = 0
	hw := M[b+B_W] / 2
	M[b+B_X] += M[b+B_VX] * dt
	y0 := fi(M[b+B_Y] + 0.02)
	y1 := fi(M[b+B_Y] + M[b+B_H] - 0.02)
	vx := M[b+B_VX]
	if vx > 0 {
		tx := fi(M[b+B_X] + hw)
		for ty := y0; ty <= y1; ty++ {
			if isSolid(tileAt(tx, ty)) {
				M[b+B_X] = f(tx) - hw - 1e-4
				M[b+B_VX] = 0
				M[b+B_HX] = 1
				break
			}
		}
	} else if vx < 0 {
		tx := fi(M[b+B_X] - hw)
		for ty := y0; ty <= y1; ty++ {
			if isSolid(tileAt(tx, ty)) {
				M[b+B_X] = f(tx) + 1 + hw + 1e-4
				M[b+B_VX] = 0
				M[b+B_HX] = -1
				break
			}
		}
	}
	prevY := M[b+B_Y]
	M[b+B_Y] += M[b+B_VY] * dt
	x0 := fi(M[b+B_X] - hw + 1e-3)
	x1 := fi(M[b+B_X] + hw - 1e-3)
	M[b+B_GND] = 0
	if M[b+B_VY] <= 0 {
		ty := fi(M[b+B_Y])
		for tx := x0; tx <= x1; tx++ {
			t := tileAt(tx, ty)
			if isSolid(t) || (t == 2 && !drop && prevY >= f(ty)+1-0.02) {
				M[b+B_Y] = f(ty) + 1
				M[b+B_VY] = 0
				M[b+B_GND] = 1
				M[b+B_HY] = -1
				break
			}
		}
	} else {
		ty := fi(M[b+B_Y] + M[b+B_H])
		for tx := x0; tx <= x1; tx++ {
			if isSolid(tileAt(tx, ty)) {
				M[b+B_Y] = f(ty) - M[b+B_H] - 1e-4
				M[b+B_VY] = 0
				M[b+B_HY] = 1
				break
			}
		}
	}
}
func boxHit(a, b int) bool {
	return fabs(M[a+B_X]-M[b+B_X]) < (M[a+B_W]+M[b+B_W])/2 &&
		M[a+B_Y] < M[b+B_Y]+M[b+B_H] && M[a+B_Y]+M[a+B_H] > M[b+B_Y]
}
func enBase(i int) int { return EN_BASE + i*EN_N }
func plBase(i int) int { return PLT_BASE + i*PLT_N }
func shBase(i int) int { return SH_BASE + i*SH_N }

func setDoor(closed bool) {
	if M[G_DOORON] == 0 {
		return
	}
	for ty := ii(M[G_DOORY0]); ty <= ii(M[G_DOORY1]); ty++ {
		v := 0.0
		if closed {
			v = 4
		}
		setTile(ii(M[G_DOORX]), ty, v)
	}
	M[G_DOORCLOSED] = b2f(closed)
	ev(EV_DOOR, M[G_DOORX], 0, b2f(closed))
}

func addEnemy(kind int, x, y float64) {
	i := ii(M[G_NEN])
	if i >= EN_MAX {
		return
	}
	M[G_NEN] = f(i + 1)
	b := enBase(i)
	for k := 0; k < EN_N; k++ {
		M[b+k] = 0
	}
	M[b+B_X] = x
	M[b+B_Y] = y
	M[b+B_W] = 0.8
	M[b+B_H] = 0.62
	M[b+E_KIND] = f(kind)
	M[b+E_ALIVE] = 1
	M[b+E_STOMP] = 1
	if rnd() < 0.5 {
		M[b+E_DIR] = -1
	} else {
		M[b+E_DIR] = 1
	}
	M[b+E_T] = rnd() * 10
	M[b+E_OX] = x
	M[b+E_OY] = y
	M[b+E_CD] = 1 + rnd()
	if kind == EK_BAT {
		M[b+B_W] = 0.75
		M[b+B_H] = 0.5
	} else if kind == EK_SAW {
		M[b+B_W] = 0.78
		M[b+B_H] = 0.78
		M[b+E_STOMP] = 0
		M[b+E_DIR] = 1
		M[b+B_VX] = 3
	} else if kind == EK_TURRET {
		M[b+B_W] = 0.9
		M[b+B_H] = 0.8
		M[b+E_STOMP] = 0
	} else if kind == EK_BOSS {
		M[b+B_W] = 2.5
		M[b+B_H] = 2.3
		M[b+E_HP] = 8
		M[b+E_HP0] = 8
		M[b+E_STATE] = BS_SLEEP
		M[b+E_DIR] = -1
		M[G_BOSS] = f(i)
	}
}
func addPlatform(kind int, x, ty float64) {
	i := ii(M[G_NPLT])
	if i >= PLT_MAX {
		return
	}
	M[G_NPLT] = f(i + 1)
	p := plBase(i)
	for k := 0; k < PLT_N; k++ {
		M[p+k] = 0
	}
	M[p+PT_KIND] = f(kind)
	M[p+PT_X] = x
	M[p+PT_X0] = x
	M[p+PT_TOP] = ty + 0.75
	M[p+PT_TOP0] = ty + 0.75
	M[p+PT_PREVTOP] = ty + 0.75
	if kind == 2 {
		M[p+PT_W] = 1
	} else {
		M[p+PT_W] = 3
	}
	M[p+PT_SOLID] = 1
	M[p+PT_T] = rnd() * 6
	M[p+PT_STATE] = CS_IDLE
}
func addGoal(x, y float64) {
	M[G_GOALON] = 1
	M[G_GOALX] = x
	M[G_GOALY] = y
}

func resetPlayer(x, y float64) {
	M[P_X], M[P_Y], M[P_VX], M[P_VY], M[P_FACE], M[P_GND] = x, y, 0, 0, 1, 0
	M[P_COYOTE], M[P_BUFFER], M[P_JUMPS], M[P_WALLDIR], M[P_WALLGRACE], M[P_WALLLOCK] = 0, 0, 1, 0, 0, 0
	M[P_DASHT], M[P_DASHCD], M[P_AIRDASH], M[P_INV], M[P_DROPT], M[P_DEAD], M[P_DEADT] = 0, 0, 0, 0, 0, 0, 0
	M[P_RIDING], M[P_SAFEX], M[P_SAFEY], M[P_SAFET], M[P_PREVY], M[P_WASGND] = -1, x, y, 0, y, 0
}

//go:wasmexport init
func coreInit() {
	for i := 0; i < MEM_SIZE; i++ {
		M[i] = 0
	}
	M[G_RNG] = 12345
	M[G_VIEWW] = 20
	M[P_W] = PW
	M[P_H] = PH
	M[P_MAXHP] = 3
	M[P_HP] = 3
	M[P_RIDING] = -1
	M[G_BOSS] = -1
}

//go:wasmexport load_level
func loadLevel() {
	M[G_NCOINS], M[G_NEN], M[G_NPLT], M[G_NSH], M[G_NSPR], M[G_NCK], M[G_BOSS] = 0, 0, 0, 0, 0, 0, -1
	M[G_TIME], M[G_COINS], M[G_KILLS], M[G_DEATHS], M[G_SCORE], M[G_BOSSKILLED] = 0, 0, 0, 0, 0, 0
	M[G_BOSSACTIVE], M[G_FREEZE], M[G_COINSTOTAL], M[G_DOORCLOSED], M[G_ACC], M[G_EVN] = 0, 0, 0, 0, 0, 0
	n := ii(M[G_NSPEC])
	for s := 0; s < n; s++ {
		kind := ii(M[SPEC_BASE+s*SPEC_N])
		x := M[SPEC_BASE+s*SPEC_N+1]
		y := M[SPEC_BASE+s*SPEC_N+2]
		if kind <= SK_HEART {
			i := ii(M[G_NCOINS])
			if i < COIN_MAX {
				M[G_NCOINS] = f(i + 1)
				c := COIN_BASE + i*COIN_N
				M[c+C_KIND] = f(kind)
				M[c+C_X] = x
				M[c+C_Y] = y + 0.5
				M[c+C_GOT] = 0
				if kind == SK_COIN {
					M[c+C_R] = 0.55
				} else {
					M[c+C_R] = 0.65
				}
				if kind == SK_COIN {
					M[G_COINSTOTAL] += 1
				} else if kind == SK_GEM {
					M[G_COINSTOTAL] += 5
				}
			}
		} else if kind == SK_SLIME {
			addEnemy(EK_SLIME, x, y)
		} else if kind == SK_BAT {
			addEnemy(EK_BAT, x, y+0.5)
		} else if kind == SK_SAW {
			addEnemy(EK_SAW, x, y+0.02)
		} else if kind == SK_TURRET {
			addEnemy(EK_TURRET, x, y)
		} else if kind == SK_SPRING {
			i := ii(M[G_NSPR])
			if i < SPR_MAX {
				M[G_NSPR] = f(i + 1)
				q := SPR_BASE + i*SPR_N
				M[q+SP_X] = x
				M[q+SP_Y] = y
				M[q+SP_T] = 0
			}
		} else if kind == SK_PLAT_H {
			addPlatform(0, x, y)
		} else if kind == SK_PLAT_V {
			addPlatform(1, x, y)
		} else if kind == SK_CRUMBLE {
			addPlatform(2, x, y)
		} else if kind == SK_CHECK {
			i := ii(M[G_NCK])
			if i < CKT_MAX {
				M[G_NCK] = f(i + 1)
				q := CKT_BASE + i*CKT_N
				M[q+CK_X] = x
				M[q+CK_Y] = y
				M[q+CK_ON] = 0
			}
		} else if kind == SK_BOSS {
			addEnemy(EK_BOSS, x, y)
		}
	}
	M[P_MAXHP] = 3
	M[P_HP] = 3
	M[G_CHKX] = M[G_SPAWNX]
	M[G_CHKY] = M[G_SPAWNY]
	resetPlayer(M[G_SPAWNX], M[G_SPAWNY])
}

// ---------------------------------------------------------------- player
func killPlayer() {
	M[P_DEAD] = 1
	M[P_DEADT] = 0
	M[P_HP] = 0
	M[G_DEATHS] += 1
	ev(EV_DIE, M[P_X], M[P_Y]+0.5, 0)
}
func hurtPlayer(srcX float64) bool {
	if M[P_INV] > 0 || M[P_DASHT] > 0 || M[P_DEAD] != 0 {
		return false
	}
	M[P_HP] -= 1
	M[P_INV] = INVULN
	M[G_FREEZE] = 0.08
	ev(EV_HURT, M[P_X], M[P_Y]+0.5, 0)
	dir := 1.0
	if M[P_X] < srcX {
		dir = -1.0
	}
	M[P_VX] = dir * 8.5
	M[P_VY] = 12
	M[P_WALLLOCK] = 0.18
	M[P_DASHT] = 0
	if M[P_HP] <= 0 {
		killPlayer()
	}
	return true
}
func pitFall() {
	if M[P_DEAD] != 0 {
		return
	}
	M[P_HP] -= 1
	ev(EV_PIT, M[P_X], M[P_Y], 0)
	if M[P_HP] <= 0 {
		killPlayer()
		return
	}
	M[P_X] = M[P_SAFEX]
	M[P_Y] = M[P_SAFEY] + 0.05
	M[P_VX] = 0
	M[P_VY] = 0
	M[P_INV] = 2
	ev(EV_RESPAWN, M[P_X], M[P_Y]+0.4, 1)
}
func respawn() {
	resetPlayer(M[G_CHKX], M[G_CHKY])
	M[P_HP] = M[P_MAXHP]
	M[P_INV] = 2
	if M[G_BOSS] >= 0 && M[G_BOSSACTIVE] != 0 {
		resetBoss()
	}
	ev(EV_RESPAWN, M[P_X], M[P_Y]+0.5, 0)
}
func stompBounce(strong bool) {
	if M[IN_JUMPHELD] != 0 {
		M[P_VY] = STOMP_BOUNCE + 3.5
	} else {
		M[P_VY] = STOMP_BOUNCE
	}
	if strong {
		M[P_VY] += 2
	}
	M[P_JUMPS] = 1
	M[P_AIRDASH] = 0
	M[P_GND] = 0
	M[P_DASHT] = 0
}

func updatePlayer(dt float64) {
	ax := M[IN_AX]
	M[P_PREVY] = M[P_Y]
	M[P_INV] = fmax(0, M[P_INV]-dt)
	M[P_DASHCD] -= dt
	M[P_WALLLOCK] -= dt
	M[P_BUFFER] -= dt
	M[P_DROPT] -= dt
	M[P_WALLGRACE] -= dt

	if M[IN_JUMPPRESS] != 0 {
		M[IN_JUMPPRESS] = 0
		M[P_BUFFER] = BUFFER
	}
	wantDash := M[IN_DASHPRESS] != 0
	if wantDash {
		M[IN_DASHPRESS] = 0
	}

	rid := ii(M[P_RIDING])
	if rid >= 0 {
		p := plBase(rid)
		if M[p+PT_SOLID] != 0 && M[P_VY] <= 0 {
			M[P_X] += M[p+PT_DX]
			M[P_Y] = M[p+PT_TOP]
		}
	}

	if M[P_GND] != 0 {
		M[P_COYOTE] = COYOTE
		M[P_JUMPS] = 1
		M[P_AIRDASH] = 0
	} else {
		M[P_COYOTE] -= dt
	}

	if wantDash && M[P_DASHCD] <= 0 && M[P_AIRDASH] == 0 {
		M[P_DASHT] = DASH_TIME
		M[P_DASHCD] = DASH_CD
		if ax != 0 {
			M[P_DASHDIR] = ax
		} else {
			M[P_DASHDIR] = M[P_FACE]
		}
		M[P_FACE] = M[P_DASHDIR]
		if M[P_GND] == 0 {
			M[P_AIRDASH] = 1
		}
		ev(EV_DASH, M[P_X], M[P_Y]+0.4, M[P_DASHDIR])
	}

	if M[P_DASHT] > 0 {
		M[P_DASHT] -= dt
		M[P_VX] = M[P_DASHDIR] * DASH_SPEED
		M[P_VY] = 0
		if M[P_DASHT] <= 0 {
			M[P_VX] = M[P_DASHDIR] * MAX_SPEED
		}
	} else {
		if M[P_WALLLOCK] <= 0 {
			target := ax * MAX_SPEED
			var acc float64
			if ax == 0 {
				if M[P_GND] != 0 {
					acc = DECEL_G
				} else {
					acc = DECEL_A
				}
			} else {
				if M[P_GND] != 0 {
					acc = ACCEL_G
				} else {
					acc = ACCEL_A
				}
				if sign(M[P_VX]) == -ax {
					acc *= 1.6
				}
			}
			M[P_VX] = approach(M[P_VX], target, acc*dt)
			if ax != 0 {
				M[P_FACE] = ax
			}
		}

		M[P_WALLDIR] = 0
		if M[P_GND] == 0 && ax != 0 {
			tx := fi(M[P_X] + ax*(PW/2+0.08))
			if isSolid(tileAt(tx, fi(M[P_Y]+0.3))) || isSolid(tileAt(tx, fi(M[P_Y]+0.75))) {
				M[P_WALLDIR] = ax
				M[P_WALLGRACE] = 0.1
				M[P_WALLMEM] = ax
			}
		}

		if M[P_BUFFER] > 0 {
			if M[IN_DOWNHELD] != 0 && M[P_GND] != 0 && tileAt(fi(M[P_X]), fi(M[P_Y]-0.05)) == 2 {
				M[P_DROPT] = 0.22
				M[P_BUFFER] = 0
				M[P_Y] -= 0.06
				M[P_GND] = 0
			} else if M[P_COYOTE] > 0 {
				M[P_VY] = JUMP_V
				M[P_BUFFER] = 0
				M[P_COYOTE] = 0
				M[P_GND] = 0
				ev(EV_JUMP, M[P_X], M[P_Y]+0.05, 0)
			} else if M[P_WALLGRACE] > 0 && M[P_WALLMEM] != 0 {
				M[P_VX] = -M[P_WALLMEM] * WALL_JUMP_X
				M[P_VY] = WALL_JUMP_Y
				M[P_WALLLOCK] = WALL_LOCK
				M[P_FACE] = -M[P_WALLMEM]
				M[P_BUFFER] = 0
				M[P_WALLGRACE] = 0
				M[P_JUMPS] = 1
				M[P_AIRDASH] = 0
				ev(EV_WALLJUMP, M[P_X]+M[P_WALLMEM]*0.3, M[P_Y]+0.5, M[P_WALLMEM])
			} else if M[P_JUMPS] > 0 {
				M[P_VY] = DOUBLE_V
				M[P_JUMPS] -= 1
				M[P_BUFFER] = 0
				ev(EV_DOUBLE, M[P_X], M[P_Y]+0.05, 0)
			}
		}

		var g float64
		if M[P_VY] > 0 {
			if M[IN_JUMPHELD] != 0 {
				g = GRAVITY
			} else {
				g = GRAVITY * 2.4
			}
		} else {
			g = FALL_GRAVITY
		}
		M[P_VY] = fmax(M[P_VY]-g*dt, -MAX_FALL)
		if M[P_WALLDIR] != 0 && M[P_VY] < -WALL_SLIDE {
			M[P_VY] = -WALL_SLIDE
		}
	}

	vyPre := M[P_VY]
	moveBody(P_X, dt, M[P_DROPT] > 0)

	M[P_RIDING] = -1
	if vyPre <= 0 && M[P_DASHT] <= 0 {
		np := ii(M[G_NPLT])
		for i := 0; i < np; i++ {
			p := plBase(i)
			if M[p+PT_SOLID] == 0 {
				continue
			}
			if fabs(M[P_X]-M[p+PT_X]) < M[p+PT_W]/2+PW/2-0.08 && M[P_PREVY] >= M[p+PT_PREVTOP]-0.12 &&
				M[P_Y] <= M[p+PT_TOP]+0.02 && M[P_Y] >= M[p+PT_TOP]-0.6 {
				M[P_Y] = M[p+PT_TOP]
				M[P_VY] = 0
				M[P_GND] = 1
				M[P_RIDING] = f(i)
				if M[p+PT_KIND] == 2 && M[p+PT_STATE] == CS_IDLE {
					M[p+PT_STATE] = CS_SHAKE
					M[p+PT_TIMER] = 0.45
				}
				break
			}
		}
	}

	if M[P_GND] != 0 && M[P_WASGND] == 0 && vyPre < -9 {
		ev(EV_LAND, M[P_X], M[P_Y]+0.05, 0)
	}
	if M[P_HY] == 1 && M[P_VY] == 0 {
		M[P_VY] = -1
	}
	M[P_WASGND] = M[P_GND]

	if M[P_GND] != 0 && M[P_RIDING] < 0 {
		ty := fi(M[P_Y]) - 1
		if isSolid(tileAt(fi(M[P_X]-0.6), ty)) && isSolid(tileAt(fi(M[P_X]+0.6), ty)) &&
			isSolid(tileAt(fi(M[P_X]-1.6), ty)) && isSolid(tileAt(fi(M[P_X]+1.6), ty)) &&
			tileAt(fi(M[P_X]), fi(M[P_Y])) != 3 {
			M[P_SAFET] += dt
			if M[P_SAFET] > 0.35 {
				M[P_SAFEX] = M[P_X]
				M[P_SAFEY] = M[P_Y]
			}
		} else {
			M[P_SAFET] = 0
		}
	} else {
		M[P_SAFET] = 0
	}

	if M[P_INV] <= 0 && M[P_DASHT] <= 0 {
		x0 := fi(M[P_X] - PW/2)
		x1 := fi(M[P_X] + PW/2)
		y0 := fi(M[P_Y])
		y1 := fi(M[P_Y] + PH*0.5)
		done := false
		for ty := y0; ty <= y1 && !done; ty++ {
			for tx := x0; tx <= x1; tx++ {
				if tileAt(tx, ty) == 3 {
					if M[P_X]+PW/2 > f(tx)+0.15 && M[P_X]-PW/2 < f(tx)+0.85 && M[P_Y] < f(ty)+0.5 {
						if hurtPlayer(f(tx) + 0.5) {
							M[P_VY] = 15
							d := 1.0
							if M[P_X] < f(tx)+0.5 {
								d = -1.0
							}
							M[P_VX] = d * 5
						}
						done = true
						break
					}
				}
			}
		}
	}

	if M[P_Y] < -2.5 {
		pitFall()
	}
}

// ------------------------------------------------------------- platforms
func updatePlatforms(dt float64) {
	n := ii(M[G_NPLT])
	for i := 0; i < n; i++ {
		p := plBase(i)
		M[p+PT_PREVTOP] = M[p+PT_TOP]
		M[p+PT_DX] = 0
		M[p+PT_DY] = 0
		M[p+PT_T] += dt
		kind := ii(M[p+PT_KIND])
		if kind == 0 {
			nx := M[p+PT_X0] + fsin(M[p+PT_T]*1.05)*2.4
			M[p+PT_DX] = nx - M[p+PT_X]
			M[p+PT_X] = nx
		} else if kind == 1 {
			nt := M[p+PT_TOP0] + (1-fcos(M[p+PT_T]*0.95))/2*5.0
			M[p+PT_DY] = nt - M[p+PT_TOP]
			M[p+PT_TOP] = nt
		} else {
			st := ii(M[p+PT_STATE])
			if st == CS_SHAKE {
				M[p+PT_TIMER] -= dt
				if M[p+PT_TIMER] <= 0 {
					M[p+PT_STATE] = CS_FALL
					M[p+PT_SOLID] = 0
					M[p+PT_VY] = 0
					ev(EV_CRUMBLE, M[p+PT_X], M[p+PT_TOP], 0)
				}
			} else if st == CS_FALL {
				M[p+PT_VY] -= 32 * dt
				M[p+PT_TOP] += M[p+PT_VY] * dt
				if M[p+PT_TOP] < -3 {
					M[p+PT_STATE] = CS_GONE
					M[p+PT_TIMER] = 2.6
				}
			} else if st == CS_GONE {
				M[p+PT_TIMER] -= dt
				if M[p+PT_TIMER] <= 0 {
					M[p+PT_STATE] = CS_IDLE
					M[p+PT_SOLID] = 1
					M[p+PT_TOP] = M[p+PT_TOP0]
					M[p+PT_PREVTOP] = M[p+PT_TOP]
				}
			}
		}
	}
}

// ----------------------------------------------------------------- shots
func shoot(x, y, vx, vy, g, life, size, purple float64) {
	i := ii(M[G_NSH])
	if i >= SH_MAX {
		return
	}
	M[G_NSH] = f(i + 1)
	s := shBase(i)
	M[s+S_X] = x
	M[s+S_Y] = y
	M[s+S_VX] = vx
	M[s+S_VY] = vy
	M[s+S_G] = g
	M[s+S_LIFE] = life
	M[s+S_R] = size * 0.36
	M[s+S_PURPLE] = purple
	M[s+S_SIZE] = size
}
func removeShot(i int) {
	last := ii(M[G_NSH]) - 1
	if i != last {
		a, b := shBase(i), shBase(last)
		for k := 0; k < SH_N; k++ {
			M[a+k] = M[b+k]
		}
	}
	M[G_NSH] = f(last)
}
func updateShots(dt float64) {
	for i := ii(M[G_NSH]) - 1; i >= 0; i-- {
		s := shBase(i)
		M[s+S_LIFE] -= dt
		M[s+S_VY] -= M[s+S_G] * dt
		M[s+S_X] += M[s+S_VX] * dt
		M[s+S_Y] += M[s+S_VY] * dt
		dead := M[s+S_LIFE] <= 0 || M[s+S_Y] < -3
		if !dead && isSolid(tileAt(fi(M[s+S_X]), fi(M[s+S_Y]))) {
			dead = true
			ev(EV_SHOTHIT, M[s+S_X], M[s+S_Y], M[s+S_PURPLE])
		}
		if !dead && M[P_DEAD] == 0 && fabs(M[P_X]-M[s+S_X]) < PW/2+M[s+S_R] &&
			M[s+S_Y] > M[P_Y]-M[s+S_R] && M[s+S_Y] < M[P_Y]+PH+M[s+S_R] {
			if hurtPlayer(M[s+S_X]) {
				dead = true
			}
		}
		if dead {
			removeShot(i)
		}
	}
}

// --------------------------------------------------------------- enemies
func killEnemy(b int, byDash bool) {
	M[b+E_ALIVE] = 0
	M[G_KILLS] += 1
	M[G_SCORE] += 150
	extra := 0.0
	if byDash {
		extra = 10
	}
	ev(EV_KILL, M[b+B_X], M[b+B_Y]+M[b+B_H]/2, M[b+E_KIND]+extra)
}

func startBoss() {
	b := enBase(ii(M[G_BOSS]))
	M[G_BOSSACTIVE] = 1
	setDoor(true)
	M[b+B_X] = M[G_BSPECX]
	M[b+B_Y] = f(LEVEL_H + 1)
	M[b+B_VX] = 0
	M[b+B_VY] = 0
	M[b+E_STATE] = BS_INTRO
	M[b+E_HP] = M[b+E_HP0]
	M[b+E_INV] = 0
	M[b+E_ALIVE] = 1
	ev(EV_BOSSSTART, M[b+B_X], M[b+B_Y], 0)
}
func resetBoss() {
	b := enBase(ii(M[G_BOSS]))
	M[G_BOSSACTIVE] = 0
	M[b+E_STATE] = BS_SLEEP
	M[b+E_HP] = M[b+E_HP0]
	M[b+E_ALIVE] = 1
	M[b+E_INV] = 0
	M[b+B_X] = M[G_BSPECX]
	M[b+B_Y] = M[G_BSPECY]
	setDoor(false)
	M[G_NSH] = 0
}
func damageBoss() bool {
	b := enBase(ii(M[G_BOSS]))
	st := ii(M[b+E_STATE])
	if M[b+E_INV] > 0 || st == BS_INTRO || st == BS_DYING {
		return false
	}
	M[b+E_HP] -= 1
	M[b+E_INV] = 1.1
	M[G_FREEZE] = 0.1
	ev(EV_BOSSHIT, M[b+B_X], M[b+B_Y], M[b+E_HP])
	if M[b+E_HP] <= 0 {
		M[b+E_STATE] = BS_DYING
		M[b+E_ST] = 1.8
		M[b+B_VX] = 0
		M[G_NSH] = 0
	}
	return true
}
func bossLand(b int, variant float64) { ev(EV_BOOM, M[b+B_X], M[b+B_Y], variant) }

func updateBoss(b int, dt float64) {
	state := ii(M[b+E_STATE])
	if state == BS_SLEEP {
		return
	}
	M[b+E_INV] = fmax(0, M[b+E_INV]-dt)
	M[b+E_ST] -= dt
	dx := M[P_X] - M[b+B_X]
	lowHp := M[b+E_HP] <= 3
	if state == BS_INTRO {
		M[b+B_VY] = fmax(M[b+B_VY]-60*dt, -30)
		moveBody(b, dt, false)
		if M[b+B_GND] != 0 {
			M[b+E_STATE] = BS_IDLE
			M[b+E_ST] = 1.0
			bossLand(b, 1)
		}
	} else if state == BS_IDLE {
		d := sign(dx)
		if d != 0 {
			M[b+E_DIR] = d
		} else {
			M[b+E_DIR] = 1
		}
		M[b+B_VX] = 0
		M[b+B_VY] = -2
		moveBody(b, dt, false)
		if M[b+E_ST] <= 0 {
			r := rnd()
			if r < 0.5 {
				M[b+E_STATE] = BS_JUMP
				M[b+B_VY] = 25
				M[b+B_VX] = clamp(dx/1.2, -11, 11)
				ev(EV_SPRING, M[b+B_X], M[b+B_Y], 1)
			} else if r < 0.8 {
				M[b+E_STATE] = BS_SHOOT
				M[b+E_ST] = 0.5
				if lowHp {
					M[b+E_SHOTS] = 5
				} else {
					M[b+E_SHOTS] = 3
				}
			} else {
				M[b+E_STATE] = BS_CHARGE
				M[b+E_ST] = 1.3
				if d != 0 {
					M[b+E_DIR] = d
				} else {
					M[b+E_DIR] = 1
				}
			}
		}
	} else if state == BS_JUMP {
		M[b+B_VY] = fmax(M[b+B_VY]-62*dt, -30)
		moveBody(b, dt, false)
		if M[b+B_HX] != 0 {
			M[b+B_VX] = 0
		}
		if M[b+B_GND] != 0 {
			M[b+E_STATE] = BS_RECOVER
			if lowHp {
				M[b+E_ST] = 0.8
			} else {
				M[b+E_ST] = 1.2
			}
			M[b+B_VX] = 0
			bossLand(b, 0)
			shoot(M[b+B_X]-1.3, M[b+B_Y]+0.35, -7, 0, 0, 4, 0.7, 1)
			shoot(M[b+B_X]+1.3, M[b+B_Y]+0.35, 7, 0, 0, 4, 0.7, 1)
		}
	} else if state == BS_RECOVER {
		M[b+B_VX] = 0
		M[b+B_VY] = -2
		moveBody(b, dt, false)
		if M[b+E_ST] <= 0 {
			M[b+E_STATE] = BS_IDLE
			if lowHp {
				M[b+E_ST] = 0.35
			} else {
				M[b+E_ST] = 0.7
			}
		}
	} else if state == BS_SHOOT {
		M[b+B_VX] = 0
		M[b+B_VY] = -2
		moveBody(b, dt, false)
		d := sign(dx)
		if d != 0 {
			M[b+E_DIR] = d
		} else {
			M[b+E_DIR] = 1
		}
		if M[b+E_ST] <= 0 && M[b+E_SHOTS] > 0 {
			ux := M[P_X] - M[b+B_X]
			uy := M[P_Y] + 0.4 - (M[b+B_Y] + 1.4)
			ln := math.Sqrt(ux*ux + uy*uy)
			if ln < 1e-6 {
				ux = 1
				uy = 0
				ln = 1
			}
			ux /= ln
			uy /= ln
			off := (rnd() - 0.5) * 0.35
			vx := ux - uy*off
			vy := uy + ux*off
			l2 := math.Sqrt(vx*vx + vy*vy)
			vx = vx / l2 * 7.5
			vy = vy / l2 * 7.5
			shoot(M[b+B_X]+M[b+E_DIR]*1.1, M[b+B_Y]+1.4, vx, vy, 0, 5, 0.6, 1)
			ev(EV_SHOOT, M[b+B_X]+M[b+E_DIR]*1.1, M[b+B_Y]+1.4, M[b+E_DIR])
			M[b+E_SHOTS] -= 1
			M[b+E_ST] = 0.3
		} else if M[b+E_SHOTS] <= 0 && M[b+E_ST] <= 0 {
			M[b+E_STATE] = BS_IDLE
			M[b+E_ST] = 0.7
		}
	} else if state == BS_CHARGE {
		M[b+B_VY] = fmax(M[b+B_VY]-60*dt, -30)
		if M[b+E_ST] > 0.75 {
			M[b+B_VX] = 0
		} else {
			sp := 12.0
			if lowHp {
				sp = 15.0
			}
			M[b+B_VX] = M[b+E_DIR] * sp
		}
		moveBody(b, dt, false)
		if M[b+E_ST] <= 0.75 && M[b+B_HX] != 0 {
			M[b+E_STATE] = BS_RECOVER
			M[b+E_ST] = 1.3
			M[b+B_VX] = 0
			bossLand(b, 2)
		} else if M[b+E_ST] <= 0 {
			M[b+E_STATE] = BS_RECOVER
			M[b+E_ST] = 0.8
			M[b+B_VX] = 0
		}
	} else if state == BS_DYING {
		M[b+B_VX] = 0
		if rnd() < 0.5 {
			rx := rndr(-1.2, 1.2)
			ry := rndr(0, 2.4)
			ev(EV_BOSSEXPLODE, M[b+B_X]+rx, M[b+B_Y]+ry, 0)
		}
		if M[b+E_ST] <= 0 {
			M[b+E_ALIVE] = 0
			M[G_SCORE] += 3000
			M[G_BOSSKILLED] = 1
			M[G_BOSSACTIVE] = 0
			setDoor(false)
			addGoal(M[b+B_X], 2)
			ev(EV_BOSSDEAD, M[b+B_X], M[b+B_Y], 0)
		}
	}
}

func updateEnemies(dt float64) {
	n := ii(M[G_NEN])
	for i := 0; i < n; i++ {
		b := enBase(i)
		if M[b+E_ALIVE] == 0 {
			continue
		}
		M[b+E_T] += dt
		kind := ii(M[b+E_KIND])
		if kind == EK_SLIME {
			M[b+B_VY] = fmax(M[b+B_VY]-55*dt, -25)
			M[b+B_VX] = M[b+E_DIR] * 1.9
			moveBody(b, dt, false)
			if M[b+B_HX] != 0 {
				M[b+E_DIR] = -M[b+E_DIR]
			} else if M[b+B_GND] != 0 {
				tx := fi(M[b+B_X] + M[b+E_DIR]*(M[b+B_W]/2+0.12))
				ty := fi(M[b+B_Y] - 0.1)
				t := tileAt(tx, ty)
				if !isSolid(t) && t != 2 {
					M[b+E_DIR] = -M[b+E_DIR]
				} else if tileAt(tx, fi(M[b+B_Y]+0.1)) == 3 {
					M[b+E_DIR] = -M[b+E_DIR]
				}
			}
		} else if kind == EK_BAT {
			near := fabs(M[P_X]-M[b+E_OX]) < 8
			if near && M[P_DEAD] == 0 {
				M[b+E_OX] += sign(M[P_X]-M[b+E_OX]) * 0.9 * dt
			}
			px := M[b+B_X]
			M[b+B_X] = M[b+E_OX] + fsin(M[b+E_T]*1.3)*3.2
			M[b+B_Y] = M[b+E_OY] + fsin(M[b+E_T]*2.4)*0.6
			d := sign(M[b+B_X] - px)
			if d != 0 {
				M[b+E_DIR] = d
			}
		} else if kind == EK_SAW {
			M[b+B_X] += M[b+B_VX] * dt
			ahead := fi(M[b+B_X] + sign(M[b+B_VX])*0.5)
			floorAhead := tileAt(ahead, fi(M[b+B_Y]-0.2))
			if isSolid(tileAt(ahead, fi(M[b+B_Y]+0.4))) || (!isSolid(floorAhead) && floorAhead != 2) ||
				fabs(M[b+B_X]-M[b+E_OX]) > 3 {
				M[b+B_VX] = -M[b+B_VX]
				M[b+B_X] += M[b+B_VX] * dt * 2
			}
		} else if kind == EK_TURRET {
			d := sign(M[P_X] - M[b+B_X])
			if d != 0 {
				M[b+E_DIR] = d
			} else {
				M[b+E_DIR] = 1
			}
			M[b+E_CD] -= dt
			dx := fabs(M[P_X] - M[b+B_X])
			dy := fabs(M[P_Y] - M[b+B_Y])
			if M[b+E_CD] <= 0 && dx < fmin(13, M[G_VIEWW]/2+1) && dy < 7 && M[P_DEAD] == 0 {
				M[b+E_CD] = 2.2
				shoot(M[b+B_X]+M[b+E_DIR]*0.6, M[b+B_Y]+0.5, M[b+E_DIR]*6.5, 0, 0, 6, 0.6, 0)
				ev(EV_SHOOT, M[b+B_X]+M[b+E_DIR]*0.7, M[b+B_Y]+0.5, M[b+E_DIR])
			}
		} else if kind == EK_BOSS {
			updateBoss(b, dt)
		}
	}
}

// ---------------------------------------------------------- interactions
func updateInteractions(dt float64) {
	if M[P_DEAD] != 0 {
		return
	}
	nen := ii(M[G_NEN])
	for i := 0; i < nen; i++ {
		e := enBase(i)
		if M[e+E_ALIVE] == 0 {
			continue
		}
		kind := ii(M[e+E_KIND])
		if kind == EK_BOSS && (M[e+E_STATE] == BS_SLEEP || M[e+E_STATE] == BS_DYING) {
			continue
		}
		if !boxHit(P_X, e) {
			continue
		}
		if kind == EK_BOSS {
			if M[P_VY] < 0 && M[P_PREVY] >= M[e+B_Y]+M[e+B_H]*0.55 && M[P_DASHT] <= 0 {
				if damageBoss() {
					stompBounce(true)
				} else {
					M[P_VY] = fmax(M[P_VY], 8)
				}
			} else {
				hurtPlayer(M[e+B_X])
			}
		} else if M[e+E_STOMP] == 0 {
			hurtPlayer(M[e+B_X])
		} else if M[P_DASHT] > 0 {
			killEnemy(e, true)
		} else if M[P_VY] < 0 && M[P_PREVY] >= M[e+B_Y]+M[e+B_H]*0.5 {
			killEnemy(e, false)
			stompBounce(false)
			M[G_FREEZE] = 0.04
		} else {
			hurtPlayer(M[e+B_X])
		}
	}

	nc := ii(M[G_NCOINS])
	for i := 0; i < nc; i++ {
		c := COIN_BASE + i*COIN_N
		if M[c+C_GOT] != 0 {
			continue
		}
		r := M[c+C_R]
		if fabs(M[P_X]-M[c+C_X]) < r+PW/2 && fabs(M[P_Y]+0.45-M[c+C_Y]) < r+0.45 {
			kind := ii(M[c+C_KIND])
			if kind == SK_HEART {
				if M[P_HP] >= M[P_MAXHP] {
					M[G_SCORE] += 250
					ev(EV_HEART, M[c+C_X], M[c+C_Y], 0)
				} else {
					M[P_HP] += 1
					ev(EV_HEART, M[c+C_X], M[c+C_Y], 1)
				}
			} else if kind == SK_GEM {
				M[G_COINS] += 5
				M[G_SCORE] += 500
				ev(EV_GEM, M[c+C_X], M[c+C_Y], 0)
			} else {
				M[G_COINS] += 1
				M[G_SCORE] += 100
				ev(EV_COIN, M[c+C_X], M[c+C_Y], 0)
			}
			M[c+C_GOT] = 1
		}
	}

	ns := ii(M[G_NSPR])
	for i := 0; i < ns; i++ {
		s := SPR_BASE + i*SPR_N
		M[s+SP_T] = fmax(0, M[s+SP_T]-dt)
		if fabs(M[P_X]-M[s+SP_X]) < 0.7 && M[P_Y] >= M[s+SP_Y]-0.1 && M[P_Y] < M[s+SP_Y]+0.55 && M[P_VY] <= 0.5 {
			M[P_VY] = 31
			M[P_GND] = 0
			M[P_JUMPS] = 1
			M[P_AIRDASH] = 0
			M[P_DASHT] = 0
			M[P_COYOTE] = 0
			M[s+SP_T] = 0.3
			ev(EV_SPRING, M[s+SP_X], M[s+SP_Y]+0.4, 0)
		}
	}

	nk := ii(M[G_NCK])
	for i := 0; i < nk; i++ {
		c := CKT_BASE + i*CKT_N
		if M[c+CK_ON] == 0 && fabs(M[P_X]-M[c+CK_X]) < 1.3 && fabs(M[P_Y]-M[c+CK_Y]) < 2 {
			for j := 0; j < nk; j++ {
				M[CKT_BASE+j*CKT_N+CK_ON] = 0
			}
			M[c+CK_ON] = 1
			M[G_CHKX] = M[c+CK_X]
			M[G_CHKY] = M[c+CK_Y]
			ev(EV_CHECK, M[c+CK_X], M[c+CK_Y], 0)
		}
	}

	bi := ii(M[G_BOSS])
	if bi >= 0 {
		b := enBase(bi)
		if M[b+E_STATE] == BS_SLEEP && M[b+E_ALIVE] != 0 && M[P_X] > M[G_BTRIG] {
			startBoss()
		}
	}

	if M[G_GOALON] != 0 && fabs(M[P_X]-M[G_GOALX]) < 0.9 && fabs(M[P_Y]+0.5-(M[G_GOALY]+1.1)) < 1.5 {
		M[G_MODE] = MODE_CLEAR
		ev(EV_COMPLETE, M[P_X], M[P_Y], 0)
	}
}

func step(dt float64) {
	if M[G_FREEZE] > 0 {
		M[G_FREEZE] -= dt
		return
	}
	if ii(M[G_MODE]) != MODE_PLAY {
		updatePlatforms(dt)
		updateEnemies(dt)
		updateShots(dt)
		return
	}
	M[G_TIME] += dt
	updatePlatforms(dt)
	if M[P_DEAD] == 0 {
		updatePlayer(dt)
	} else {
		M[P_DEADT] += dt
		if M[P_DEADT] > 1.4 {
			respawn()
		}
	}
	updateEnemies(dt)
	updateShots(dt)
	updateInteractions(dt)
}

//go:wasmexport advance
func advance(rdt float64) int32 {
	M[G_EVN] = 0
	acc := M[G_ACC] + rdt
	n := 0
	for acc >= DT && n < 12 {
		step(DT)
		acc -= DT
		n++
	}
	if n >= 12 {
		acc = 0
	}
	M[G_ACC] = acc
	M[G_STEPS] = f(n)
	if n > 0 {
		M[IN_JUMPPRESS] = 0
		M[IN_DASHPRESS] = 0
	}
	return int32(n)
}

//go:wasmexport mem_get
func memGet(i int32) float64 { return M[i] }

//go:wasmexport mem_set
func memSet(i int32, v float64) { M[i] = v }

//go:wasmexport mem_ptr
func memPtr() int32 { return int32(uintptr(unsafe.Pointer(&M[0]))) }

func main() {}
