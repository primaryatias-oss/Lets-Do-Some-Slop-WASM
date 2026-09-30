{-# LANGUAGE BangPatterns #-}
-- | Slop Runner simulation core - Haskell port.
--   Mirrors core/js/core.js line by line; all state lives in one flat Double array
--   (a malloc'ed block, so a WebAssembly host can also read it straight out of linear memory).
module Core where

import Abi
import Control.Monad
import Data.Int (Int32)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

levelH :: Int
levelH = 14

dtStep, pw, ph, maxSpeed, accelG, accelA, decelG, decelA, gravity, fallGravity, maxFall :: Double
dtStep = 1.0 / 120.0
pw = 0.62
ph = 0.86
maxSpeed = 8.2
accelG = 95.0
accelA = 62.0
decelG = 110.0
decelA = 26.0
gravity = 62.0
fallGravity = 74.0
maxFall = 27.0

jumpV, doubleV, coyoteT, bufferT, wallSlide, wallJumpX, wallJumpY, wallLock, dashTime, dashSpeed, dashCd, invuln, stompBounceV :: Double
jumpV = 19.6
doubleV = 17.2
coyoteT = 0.11
bufferT = 0.13
wallSlide = 3.4
wallJumpX = 9.5
wallJumpY = 18.2
wallLock = 0.16
dashTime = 0.17
dashSpeed = 24.0
dashCd = 0.75
invuln = 1.5
stompBounceV = 15.5

{-# NOINLINE mptr #-}
mptr :: Ptr Double
mptr = unsafePerformIO (callocBytes (mem_size * 8))

memPtrAddr :: Int
memPtrAddr = mptr `minusPtr` nullPtr

r :: Int -> IO Double
r i = peekElemOff mptr i
{-# INLINE r #-}

w :: Int -> Double -> IO ()
w i v = pokeElemOff mptr i v
{-# INLINE w #-}

addTo :: Int -> Double -> IO ()
addTo i v = do { x <- r i; w i (x + v) }
{-# INLINE addTo #-}

f :: Int -> Double
f = fromIntegral
{-# INLINE f #-}

fi :: Double -> Int
fi = floor
{-# INLINE fi #-}

ii :: Double -> Int
ii = truncate
{-# INLINE ii #-}

b2f :: Bool -> Double
b2f b = if b then 1.0 else 0.0

sign :: Double -> Double
sign v = if v > 0 then 1.0 else if v < 0 then -1.0 else 0.0

fmin, fmax :: Double -> Double -> Double
fmin a b = if a < b then a else b
fmax a b = if a > b then a else b

fabs :: Double -> Double
fabs a = if a < 0 then negate a else a

clamp :: Double -> Double -> Double -> Double
clamp v a b = if v < a then a else if v > b then b else v

approach :: Double -> Double -> Double -> Double
approach v t s = if v < t then fmin (v + s) t else fmax (v - s) t

fsin :: Double -> Double
fsin x =
  let k = fromIntegral (floor (x * 0.15915494309189535 + 0.5) :: Int) :: Double
      r0 = x - k * 6.283185307179586
      r1 | r0 > 1.5707963267948966 = 3.141592653589793 - r0
         | r0 < -1.5707963267948966 = -3.141592653589793 - r0
         | otherwise = r0
      r2 = r1 * r1
  in r1 * (1.0 + r2 * (-0.16666666666666666 + r2 * (0.008333333333333333 + r2 * (-0.0001984126984126984 +
        r2 * (2.7557319223985893e-6 + r2 * (-2.505210838544172e-8 + r2 * 1.6059043836821613e-10))))))

fcos :: Double -> Double
fcos x = fsin (x + 1.5707963267948966)

rnd :: IO Double
rnd = do
  g <- r g_rng
  let s0 = g * 1664525.0 + 1013904223.0
      s = s0 - fromIntegral (floor (s0 / 4294967296.0) :: Int) * 4294967296.0
  w g_rng s
  return (s / 4294967296.0)

rndr :: Double -> Double -> IO Double
rndr a b = do { x <- rnd; return (a + x * (b - a)) }

ev :: Int -> Double -> Double -> Double -> IO ()
ev t x y a = do
  n <- ii <$> r g_evn
  when (n < ev_max) $ do
    let i = ev_base + n * ev_n
    w i (f t); w (i + 1) x; w (i + 2) y; w (i + 3) a
    w g_evn (f (n + 1))

tileAt :: Int -> Int -> IO Double
tileAt tx ty = do
  lw <- ii <$> r g_lw
  if tx < 0 || tx >= lw then return 1.0
  else if ty < 0 || ty >= levelH then return 0.0
  else r (tile_base + ty * lw + tx)

isSolid :: Double -> Bool
isSolid t = t == 1.0 || t == 4.0

setTile :: Int -> Int -> Double -> IO ()
setTile tx ty v = do { lw <- ii <$> r g_lw; w (tile_base + ty * lw + tx) v }

-- run body for i in [a..b]; body returns True to stop early
forStop :: Int -> Int -> (Int -> IO Bool) -> IO Bool
forStop a b body = go a
  where go i | i > b = return False
             | otherwise = do { s <- body i; if s then return True else go (i + 1) }

forN :: Int -> (Int -> IO ()) -> IO ()
forN n body = forM_ [0 .. n - 1] body

moveBody :: Int -> Double -> Bool -> IO ()
moveBody b dt dropT = do
  w (b + b_hx) 0.0; w (b + b_hy) 0.0
  bw <- r (b + b_w)
  let hw = bw / 2.0
  vx0 <- r (b + b_vx)
  addTo (b + b_x) (vx0 * dt)
  by <- r (b + b_y); bh <- r (b + b_h)
  let y0 = fi (by + 0.02); y1 = fi (by + bh - 0.02)
  vx <- r (b + b_vx)
  bx <- r (b + b_x)
  if vx > 0 then do
    let tx = fi (bx + hw)
    void $ forStop y0 y1 $ \ty -> do
      t <- tileAt tx ty
      if isSolid t then do { w (b + b_x) (f tx - hw - 1e-4); w (b + b_vx) 0.0; w (b + b_hx) 1.0; return True } else return False
  else when (vx < 0) $ do
    let tx = fi (bx - hw)
    void $ forStop y0 y1 $ \ty -> do
      t <- tileAt tx ty
      if isSolid t then do { w (b + b_x) (f tx + 1 + hw + 1e-4); w (b + b_vx) 0.0; w (b + b_hx) (-1.0); return True } else return False
  prevY <- r (b + b_y)
  vy <- r (b + b_vy)
  addTo (b + b_y) (vy * dt)
  bx2 <- r (b + b_x)
  let x0 = fi (bx2 - hw + 1e-3); x1 = fi (bx2 + hw - 1e-3)
  w (b + b_gnd) 0.0
  vy2 <- r (b + b_vy)
  ny <- r (b + b_y)
  if vy2 <= 0 then do
    let ty = fi ny
    void $ forStop x0 x1 $ \tx -> do
      t <- tileAt tx ty
      if isSolid t || (t == 2.0 && not dropT && prevY >= f ty + 1 - 0.02) then do
        w (b + b_y) (f ty + 1); w (b + b_vy) 0.0; w (b + b_gnd) 1.0; w (b + b_hy) (-1.0); return True
      else return False
  else do
    let ty = fi (ny + bh)
    void $ forStop x0 x1 $ \tx -> do
      t <- tileAt tx ty
      if isSolid t then do { w (b + b_y) (f ty - bh - 1e-4); w (b + b_vy) 0.0; w (b + b_hy) 1.0; return True } else return False

boxHit :: Int -> Int -> IO Bool
boxHit a b = do
  ax <- r (a + b_x); bx <- r (b + b_x); aw <- r (a + b_w); bw <- r (b + b_w)
  ay <- r (a + b_y); by <- r (b + b_y); ah <- r (a + b_h); bh <- r (b + b_h)
  return (fabs (ax - bx) < (aw + bw) / 2 && ay < by + bh && ay + ah > by)

enBase, plBase, shBase :: Int -> Int
enBase i = en_base + i * en_n
plBase i = plt_base + i * plt_n
shBase i = sh_base + i * sh_n

setDoor :: Bool -> IO ()
setDoor closed = do
  on <- r g_dooron
  when (on /= 0) $ do
    y0 <- ii <$> r g_doory0; y1 <- ii <$> r g_doory1; dx <- ii <$> r g_doorx
    forM_ [y0 .. y1] $ \ty -> setTile dx ty (if closed then 4.0 else 0.0)
    w g_doorclosed (b2f closed)
    dxf <- r g_doorx
    ev ev_door dxf 0.0 (b2f closed)

addEnemy :: Int -> Double -> Double -> IO ()
addEnemy kind x y = do
  i <- ii <$> r g_nen
  when (i < en_max) $ do
    w g_nen (f (i + 1))
    let b = enBase i
    forM_ [0 .. en_n - 1] $ \k -> w (b + k) 0.0
    w (b + b_x) x; w (b + b_y) y; w (b + b_w) 0.8; w (b + b_h) 0.62
    w (b + e_kind) (f kind); w (b + e_alive) 1.0; w (b + e_stomp) 1.0
    r0 <- rnd
    w (b + e_dir) (if r0 < 0.5 then -1.0 else 1.0)
    r1 <- rnd
    w (b + e_t) (r1 * 10.0)
    w (b + e_ox) x; w (b + e_oy) y
    r2 <- rnd
    w (b + e_cd) (1.0 + r2)
    if kind == ek_bat then do { w (b + b_w) 0.75; w (b + b_h) 0.5 }
    else if kind == ek_saw then do
      w (b + b_w) 0.78; w (b + b_h) 0.78; w (b + e_stomp) 0.0; w (b + e_dir) 1.0; w (b + b_vx) 3.0
    else if kind == ek_turret then do
      w (b + b_w) 0.9; w (b + b_h) 0.8; w (b + e_stomp) 0.0
    else when (kind == ek_boss) $ do
      w (b + b_w) 2.5; w (b + b_h) 2.3; w (b + e_hp) 8.0; w (b + e_hp0) 8.0
      w (b + e_state) (f bs_sleep); w (b + e_dir) (-1.0)
      w g_boss (f i)

addPlatform :: Int -> Double -> Double -> IO ()
addPlatform kind x ty = do
  i <- ii <$> r g_nplt
  when (i < plt_max) $ do
    w g_nplt (f (i + 1))
    let p = plBase i
    forM_ [0 .. plt_n - 1] $ \k -> w (p + k) 0.0
    w (p + pt_kind) (f kind); w (p + pt_x) x; w (p + pt_x0) x
    w (p + pt_top) (ty + 0.75); w (p + pt_top0) (ty + 0.75); w (p + pt_prevtop) (ty + 0.75)
    w (p + pt_w) (if kind == 2 then 1.0 else 3.0); w (p + pt_solid) 1.0
    rr <- rnd
    w (p + pt_t) (rr * 6.0); w (p + pt_state) (f cs_idle)

addGoal :: Double -> Double -> IO ()
addGoal x y = do { w g_goalon 1.0; w g_goalx x; w g_goaly y }

resetPlayer :: Double -> Double -> IO ()
resetPlayer x y = do
  w p_x x; w p_y y; w p_vx 0.0; w p_vy 0.0; w p_face 1.0; w p_gnd 0.0
  w p_coyote 0.0; w p_buffer 0.0; w p_jumps 1.0; w p_walldir 0.0; w p_wallgrace 0.0; w p_walllock 0.0
  w p_dasht 0.0; w p_dashcd 0.0; w p_airdash 0.0; w p_inv 0.0; w p_dropt 0.0; w p_dead 0.0; w p_deadt 0.0
  w p_riding (-1.0); w p_safex x; w p_safey y; w p_safet 0.0; w p_prevy y; w p_wasgnd 0.0

initCore :: IO ()
initCore = do
  forM_ [0 .. mem_size - 1] $ \i -> w i 0.0
  w g_rng 12345.0; w g_vieww 20.0
  w p_w pw; w p_h ph; w p_maxhp 3.0; w p_hp 3.0; w p_riding (-1.0); w g_boss (-1.0)

loadLevel :: IO ()
loadLevel = do
  mapM_ (\i -> w i 0.0) [g_ncoins, g_nen, g_nplt, g_nsh, g_nspr, g_nck]
  w g_boss (-1.0)
  mapM_ (\i -> w i 0.0) [g_time, g_coins, g_kills, g_deaths, g_score, g_bosskilled, g_bossactive, g_freeze, g_coinstotal, g_doorclosed, g_acc, g_evn]
  n <- ii <$> r g_nspec
  forM_ [0 .. n - 1] $ \s -> do
    kind <- ii <$> r (spec_base + s * spec_n)
    x <- r (spec_base + s * spec_n + 1)
    y <- r (spec_base + s * spec_n + 2)
    if kind <= sk_heart then do
      i <- ii <$> r g_ncoins
      when (i < coin_max) $ do
        w g_ncoins (f (i + 1))
        let c = coin_base + i * coin_n
        w (c + c_kind) (f kind); w (c + c_x) x; w (c + c_y) (y + 0.5); w (c + c_got) 0.0
        w (c + c_r) (if kind == sk_coin then 0.55 else 0.65)
        if kind == sk_coin then addTo g_coinstotal 1.0
        else when (kind == sk_gem) $ addTo g_coinstotal 5.0
    else if kind == sk_slime then addEnemy ek_slime x y
    else if kind == sk_bat then addEnemy ek_bat x (y + 0.5)
    else if kind == sk_saw then addEnemy ek_saw x (y + 0.02)
    else if kind == sk_turret then addEnemy ek_turret x y
    else if kind == sk_spring then do
      i <- ii <$> r g_nspr
      when (i < spr_max) $ do
        w g_nspr (f (i + 1))
        let q = spr_base + i * spr_n
        w (q + sp_x) x; w (q + sp_y) y; w (q + sp_t) 0.0
    else if kind == sk_plat_h then addPlatform 0 x y
    else if kind == sk_plat_v then addPlatform 1 x y
    else if kind == sk_crumble then addPlatform 2 x y
    else if kind == sk_check then do
      i <- ii <$> r g_nck
      when (i < ckt_max) $ do
        w g_nck (f (i + 1))
        let q = ckt_base + i * ckt_n
        w (q + ck_x) x; w (q + ck_y) y; w (q + ck_on) 0.0
    else when (kind == sk_boss) $ addEnemy ek_boss x y
  w p_maxhp 3.0; w p_hp 3.0
  sx <- r g_spawnx; sy <- r g_spawny
  w g_chkx sx; w g_chky sy
  resetPlayer sx sy

-- ------------------------------------------------------------ player
killPlayer :: IO ()
killPlayer = do
  w p_dead 1.0; w p_deadt 0.0; w p_hp 0.0
  addTo g_deaths 1.0
  px <- r p_x; py <- r p_y
  ev ev_die px (py + 0.5) 0.0

hurtPlayer :: Double -> IO Bool
hurtPlayer srcX = do
  inv <- r p_inv; dasht <- r p_dasht; dead <- r p_dead
  if inv > 0 || dasht > 0 || dead /= 0 then return False
  else do
    addTo p_hp (-1.0)
    w p_inv invuln
    w g_freeze 0.08
    px <- r p_x; py <- r p_y
    ev ev_hurt px (py + 0.5) 0.0
    w p_vx ((if px < srcX then -1.0 else 1.0) * 8.5); w p_vy 12.0
    w p_walllock 0.18; w p_dasht 0.0
    hp <- r p_hp
    when (hp <= 0) killPlayer
    return True

resetBoss :: IO ()
resetBoss = do
  bi <- ii <$> r g_boss
  let b = enBase bi
  w g_bossactive 0.0
  hp0 <- r (b + e_hp0)
  w (b + e_state) (f bs_sleep); w (b + e_hp) hp0; w (b + e_alive) 1.0; w (b + e_inv) 0.0
  sx <- r g_bspecx; sy <- r g_bspecy
  w (b + b_x) sx; w (b + b_y) sy
  setDoor False
  w g_nsh 0.0

pitFall :: IO ()
pitFall = do
  dead <- r p_dead
  when (dead == 0) $ do
    addTo p_hp (-1.0)
    px <- r p_x; py <- r p_y
    ev ev_pit px py 0.0
    hp <- r p_hp
    if hp <= 0 then killPlayer
    else do
      sx <- r p_safex; sy <- r p_safey
      w p_x sx; w p_y (sy + 0.05); w p_vx 0.0; w p_vy 0.0; w p_inv 2.0
      ev ev_respawn sx (sy + 0.05 + 0.4) 1.0

respawn :: IO ()
respawn = do
  cx <- r g_chkx; cy <- r g_chky
  resetPlayer cx cy
  mh <- r p_maxhp
  w p_hp mh; w p_inv 2.0
  bo <- r g_boss; ba <- r g_bossactive
  when (bo >= 0 && ba /= 0) resetBoss
  px <- r p_x; py <- r p_y
  ev ev_respawn px (py + 0.5) 0.0

stompBounce :: Bool -> IO ()
stompBounce strong = do
  jh <- r in_jumpheld
  w p_vy (if jh /= 0 then stompBounceV + 3.5 else stompBounceV)
  when strong $ addTo p_vy 2.0
  w p_jumps 1.0; w p_airdash 0.0; w p_gnd 0.0; w p_dasht 0.0

updatePlayer :: Double -> IO ()
updatePlayer dt = do
  ax <- r in_ax
  r p_y >>= w p_prevy
  r p_inv >>= \v -> w p_inv (fmax 0 (v - dt))
  addTo p_dashcd (-dt); addTo p_walllock (-dt); addTo p_buffer (-dt); addTo p_dropt (-dt); addTo p_wallgrace (-dt)

  jp <- r in_jumppress
  when (jp /= 0) $ do { w in_jumppress 0.0; w p_buffer bufferT }
  dp <- r in_dashpress
  let wantDash = dp /= 0
  when wantDash $ w in_dashpress 0.0

  rid <- ii <$> r p_riding
  when (rid >= 0) $ do
    let p = plBase rid
    sol <- r (p + pt_solid); vy <- r p_vy
    when (sol /= 0 && vy <= 0) $ do
      dx <- r (p + pt_dx); top <- r (p + pt_top)
      addTo p_x dx; w p_y top

  gnd0 <- r p_gnd
  if gnd0 /= 0 then do { w p_coyote coyoteT; w p_jumps 1.0; w p_airdash 0.0 } else addTo p_coyote (-dt)

  dcd <- r p_dashcd; air <- r p_airdash
  when (wantDash && dcd <= 0 && air == 0) $ do
    w p_dasht dashTime; w p_dashcd dashCd
    face <- r p_face
    w p_dashdir (if ax /= 0 then ax else face)
    dd <- r p_dashdir
    w p_face dd
    g <- r p_gnd
    when (g == 0) $ w p_airdash 1.0
    px <- r p_x; py <- r p_y
    ev ev_dash px (py + 0.4) dd

  dasht <- r p_dasht
  if dasht > 0 then do
    addTo p_dasht (-dt)
    dd <- r p_dashdir
    w p_vx (dd * dashSpeed); w p_vy 0.0
    dt2 <- r p_dasht
    when (dt2 <= 0) $ w p_vx (dd * maxSpeed)
  else do
    wl <- r p_walllock
    when (wl <= 0) $ do
      let target = ax * maxSpeed
      g <- r p_gnd
      vx <- r p_vx
      let acc | ax == 0 = if g /= 0 then decelG else decelA
              | otherwise = let a0 = if g /= 0 then accelG else accelA
                            in if sign vx == negate ax then a0 * 1.6 else a0
      w p_vx (approach vx target (acc * dt))
      when (ax /= 0) $ w p_face ax

    w p_walldir 0.0
    g1 <- r p_gnd
    when (g1 == 0 && ax /= 0) $ do
      px <- r p_x; py <- r p_y
      let tx = fi (px + ax * (pw / 2 + 0.08))
      a <- tileAt tx (fi (py + 0.3)); b <- tileAt tx (fi (py + 0.75))
      when (isSolid a || isSolid b) $ do { w p_walldir ax; w p_wallgrace 0.1; w p_wallmem ax }

    buf <- r p_buffer
    when (buf > 0) $ do
      dh <- r in_downheld; g2 <- r p_gnd
      px <- r p_x; py <- r p_y
      under <- tileAt (fi px) (fi (py - 0.05))
      coy <- r p_coyote; wg <- r p_wallgrace; wm <- r p_wallmem; jm <- r p_jumps
      if dh /= 0 && g2 /= 0 && under == 2.0 then do
        w p_dropt 0.22; w p_buffer 0.0; w p_y (py - 0.06); w p_gnd 0.0
      else if coy > 0 then do
        w p_vy jumpV; w p_buffer 0.0; w p_coyote 0.0; w p_gnd 0.0
        ev ev_jump px (py + 0.05) 0.0
      else if wg > 0 && wm /= 0 then do
        w p_vx (negate wm * wallJumpX); w p_vy wallJumpY
        w p_walllock wallLock; w p_face (negate wm); w p_buffer 0.0; w p_wallgrace 0.0
        w p_jumps 1.0; w p_airdash 0.0
        ev ev_walljump (px + wm * 0.3) (py + 0.5) wm
      else when (jm > 0) $ do
        w p_vy doubleV; addTo p_jumps (-1.0); w p_buffer 0.0
        ev ev_double px (py + 0.05) 0.0

    vy <- r p_vy; jh <- r in_jumpheld
    let g = if vy > 0 then (if jh /= 0 then gravity else gravity * 2.4) else fallGravity
    w p_vy (fmax (vy - g * dt) (negate maxFall))
    wd <- r p_walldir; vy2 <- r p_vy
    when (wd /= 0 && vy2 < negate wallSlide) $ w p_vy (negate wallSlide)

  vyPre <- r p_vy
  dropt <- r p_dropt
  moveBody p_x dt (dropt > 0)

  w p_riding (-1.0)
  dasht2 <- r p_dasht
  when (vyPre <= 0 && dasht2 <= 0) $ do
    np <- ii <$> r g_nplt
    void $ forStop 0 (np - 1) $ \i -> do
      let p = plBase i
      sol <- r (p + pt_solid)
      if sol == 0 then return False else do
        px <- r p_x; ppx <- r (p + pt_x); pww <- r (p + pt_w)
        pv <- r p_prevy; pt <- r (p + pt_prevtop); py <- r p_y; top <- r (p + pt_top)
        if fabs (px - ppx) < pww / 2 + pw / 2 - 0.08 && pv >= pt - 0.12 && py <= top + 0.02 && py >= top - 0.6 then do
          w p_y top; w p_vy 0.0; w p_gnd 1.0; w p_riding (f i)
          kd <- r (p + pt_kind); st <- r (p + pt_state)
          when (kd == 2 && st == f cs_idle) $ do { w (p + pt_state) (f cs_shake); w (p + pt_timer) 0.45 }
          return True
        else return False

  gnd <- r p_gnd; wasg <- r p_wasgnd
  when (gnd /= 0 && wasg == 0 && vyPre < -9) $ do { px <- r p_x; py <- r p_y; ev ev_land px (py + 0.05) 0.0 }
  hy <- r p_hy; vyN <- r p_vy
  when (hy == 1 && vyN == 0) $ w p_vy (-1.0)
  r p_gnd >>= w p_wasgnd

  gnd2 <- r p_gnd; rid2 <- r p_riding
  if gnd2 /= 0 && rid2 < 0 then do
    py <- r p_y; px <- r p_x
    let ty = fi py - 1
    a <- tileAt (fi (px - 0.6)) ty; b <- tileAt (fi (px + 0.6)) ty
    c <- tileAt (fi (px - 1.6)) ty; d <- tileAt (fi (px + 1.6)) ty
    e <- tileAt (fi px) (fi py)
    if isSolid a && isSolid b && isSolid c && isSolid d && e /= 3.0 then do
      addTo p_safet dt
      st <- r p_safet
      when (st > 0.35) $ do { w p_safex px; w p_safey py }
    else w p_safet 0.0
  else w p_safet 0.0

  inv <- r p_inv; dasht3 <- r p_dasht
  when (inv <= 0 && dasht3 <= 0) $ do
    px <- r p_x; py <- r p_y
    let x0 = fi (px - pw / 2); x1 = fi (px + pw / 2); y0 = fi py; y1 = fi (py + ph * 0.5)
    void $ forStop y0 y1 $ \ty -> forStop x0 x1 $ \tx -> do
      t <- tileAt tx ty
      if t == 3.0 then do
        px' <- r p_x; py' <- r p_y
        if px' + pw / 2 > f tx + 0.15 && px' - pw / 2 < f tx + 0.85 && py' < f ty + 0.5 then do
          h <- hurtPlayer (f tx + 0.5)
          when h $ do { w p_vy 15.0; px2 <- r p_x; w p_vx ((if px2 < f tx + 0.5 then -1.0 else 1.0) * 5.0) }
          return True
        else return False
      else return False

  py <- r p_y
  when (py < -2.5) pitFall

-- --------------------------------------------------------- platforms
updatePlatforms :: Double -> IO ()
updatePlatforms dt = do
  n <- ii <$> r g_nplt
  forN n $ \i -> do
    let p = plBase i
    r (p + pt_top) >>= w (p + pt_prevtop); w (p + pt_dx) 0.0; w (p + pt_dy) 0.0
    addTo (p + pt_t) dt
    kind <- ii <$> r (p + pt_kind)
    t <- r (p + pt_t)
    if kind == 0 then do
      x0 <- r (p + pt_x0); x <- r (p + pt_x)
      let nx = x0 + fsin (t * 1.05) * 2.4
      w (p + pt_dx) (nx - x); w (p + pt_x) nx
    else if kind == 1 then do
      t0 <- r (p + pt_top0); top <- r (p + pt_top)
      let nt = t0 + (1 - fcos (t * 0.95)) / 2 * 5.0
      w (p + pt_dy) (nt - top); w (p + pt_top) nt
    else do
      st <- ii <$> r (p + pt_state)
      if st == cs_shake then do
        addTo (p + pt_timer) (-dt)
        tm <- r (p + pt_timer)
        when (tm <= 0) $ do
          w (p + pt_state) (f cs_fall); w (p + pt_solid) 0.0; w (p + pt_vy) 0.0
          x <- r (p + pt_x); top <- r (p + pt_top)
          ev ev_crumble x top 0.0
      else if st == cs_fall then do
        addTo (p + pt_vy) (-32 * dt)
        vy <- r (p + pt_vy)
        addTo (p + pt_top) (vy * dt)
        top <- r (p + pt_top)
        when (top < -3) $ do { w (p + pt_state) (f cs_gone); w (p + pt_timer) 2.6 }
      else when (st == cs_gone) $ do
        addTo (p + pt_timer) (-dt)
        tm <- r (p + pt_timer)
        when (tm <= 0) $ do
          w (p + pt_state) (f cs_idle); w (p + pt_solid) 1.0
          t0 <- r (p + pt_top0)
          w (p + pt_top) t0; w (p + pt_prevtop) t0

-- ------------------------------------------------------------- shots
shoot :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
shoot x y vx vy g life size purple = do
  i <- ii <$> r g_nsh
  when (i < sh_max) $ do
    w g_nsh (f (i + 1))
    let s = shBase i
    w (s + s_x) x; w (s + s_y) y; w (s + s_vx) vx; w (s + s_vy) vy; w (s + s_g) g; w (s + s_life) life
    w (s + s_r) (size * 0.36); w (s + s_purple) purple; w (s + s_size) size

removeShot :: Int -> IO ()
removeShot i = do
  lastI <- subtract 1 . ii <$> r g_nsh
  when (i /= lastI) $ do
    let a = shBase i; b = shBase lastI
    forM_ [0 .. sh_n - 1] $ \k -> r (b + k) >>= w (a + k)
  w g_nsh (f lastI)

updateShots :: Double -> IO ()
updateShots dt = do
  n <- subtract 1 . ii <$> r g_nsh
  forM_ [n, n - 1 .. 0] $ \i -> do
    let s = shBase i
    addTo (s + s_life) (-dt)
    g <- r (s + s_g)
    addTo (s + s_vy) (negate g * dt)
    vx <- r (s + s_vx); vy <- r (s + s_vy)
    addTo (s + s_x) (vx * dt); addTo (s + s_y) (vy * dt)
    life <- r (s + s_life); sy <- r (s + s_y); sx <- r (s + s_x)
    let dead0 = life <= 0 || sy < -3
    (dead1) <- if not dead0 then do
        t <- tileAt (fi sx) (fi sy)
        if isSolid t then do { pu <- r (s + s_purple); ev ev_shothit sx sy pu; return True } else return False
      else return True
    dead2 <- if not dead1 then do
        pd <- r p_dead; px <- r p_x; py <- r p_y; sr <- r (s + s_r)
        if pd == 0 && fabs (px - sx) < pw / 2 + sr && sy > py - sr && sy < py + ph + sr then hurtPlayer sx else return False
      else return True
    when dead2 $ removeShot i

-- ----------------------------------------------------------- enemies
killEnemy :: Int -> Bool -> IO ()
killEnemy b byDash = do
  w (b + e_alive) 0.0
  addTo g_kills 1.0; addTo g_score 150.0
  x <- r (b + b_x); y <- r (b + b_y); h <- r (b + b_h); k <- r (b + e_kind)
  ev ev_kill x (y + h / 2) (k + (if byDash then 10.0 else 0.0))

startBoss :: IO ()
startBoss = do
  b <- enBase . ii <$> r g_boss
  w g_bossactive 1.0
  setDoor True
  sx <- r g_bspecx
  w (b + b_x) sx; w (b + b_y) (f (levelH + 1)); w (b + b_vx) 0.0; w (b + b_vy) 0.0
  hp0 <- r (b + e_hp0)
  w (b + e_state) (f bs_intro); w (b + e_hp) hp0; w (b + e_inv) 0.0; w (b + e_alive) 1.0
  ev ev_bossstart sx (f (levelH + 1)) 0.0

damageBoss :: IO Bool
damageBoss = do
  b <- enBase . ii <$> r g_boss
  st <- ii <$> r (b + e_state); inv <- r (b + e_inv)
  if inv > 0 || st == bs_intro || st == bs_dying then return False
  else do
    addTo (b + e_hp) (-1.0); w (b + e_inv) 1.1
    w g_freeze 0.1
    x <- r (b + b_x); y <- r (b + b_y); hp <- r (b + e_hp)
    ev ev_bosshit x y hp
    when (hp <= 0) $ do
      w (b + e_state) (f bs_dying); w (b + e_st) 1.8; w (b + b_vx) 0.0
      w g_nsh 0.0
    return True

bossLand :: Int -> Double -> IO ()
bossLand b variant = do { x <- r (b + b_x); y <- r (b + b_y); ev ev_boom x y variant }

updateBoss :: Int -> Double -> IO ()
updateBoss b dt = do
  state <- ii <$> r (b + e_state)
  when (state /= bs_sleep) $ do
    inv <- r (b + e_inv)
    w (b + e_inv) (fmax 0 (inv - dt))
    addTo (b + e_st) (-dt)
    px <- r p_x; bx <- r (b + b_x)
    let dx = px - bx
    hp <- r (b + e_hp)
    let lowHp = hp <= 3
    if state == bs_intro then do
      vy <- r (b + b_vy)
      w (b + b_vy) (fmax (vy - 60 * dt) (-30))
      moveBody b dt False
      g <- r (b + b_gnd)
      when (g /= 0) $ do { w (b + e_state) (f bs_idle); w (b + e_st) 1.0; bossLand b 1.0 }
    else if state == bs_idle then do
      let d = sign dx
      w (b + e_dir) (if d /= 0 then d else 1.0)
      w (b + b_vx) 0.0; w (b + b_vy) (-2.0)
      moveBody b dt False
      st <- r (b + e_st)
      when (st <= 0) $ do
        rr <- rnd
        if rr < 0.5 then do
          w (b + e_state) (f bs_jump); w (b + b_vy) 25.0; w (b + b_vx) (clamp (dx / 1.2) (-11) 11)
          x <- r (b + b_x); y <- r (b + b_y)
          ev ev_spring x y 1.0
        else if rr < 0.8 then do
          w (b + e_state) (f bs_shoot); w (b + e_st) 0.5; w (b + e_shots) (if lowHp then 5.0 else 3.0)
        else do
          w (b + e_state) (f bs_charge); w (b + e_st) 1.3; w (b + e_dir) (if d /= 0 then d else 1.0)
    else if state == bs_jump then do
      vy <- r (b + b_vy)
      w (b + b_vy) (fmax (vy - 62 * dt) (-30))
      moveBody b dt False
      hx <- r (b + b_hx)
      when (hx /= 0) $ w (b + b_vx) 0.0
      g <- r (b + b_gnd)
      when (g /= 0) $ do
        w (b + e_state) (f bs_recover); w (b + e_st) (if lowHp then 0.8 else 1.2); w (b + b_vx) 0.0
        bossLand b 0.0
        x <- r (b + b_x); y <- r (b + b_y)
        shoot (x - 1.3) (y + 0.35) (-7) 0 0 4 0.7 1
        shoot (x + 1.3) (y + 0.35) 7 0 0 4 0.7 1
    else if state == bs_recover then do
      w (b + b_vx) 0.0; w (b + b_vy) (-2.0)
      moveBody b dt False
      st <- r (b + e_st)
      when (st <= 0) $ do { w (b + e_state) (f bs_idle); w (b + e_st) (if lowHp then 0.35 else 0.7) }
    else if state == bs_shoot then do
      w (b + b_vx) 0.0; w (b + b_vy) (-2.0); moveBody b dt False
      let d = sign dx
      w (b + e_dir) (if d /= 0 then d else 1.0)
      st <- r (b + e_st); shots <- r (b + e_shots)
      if st <= 0 && shots > 0 then do
        px' <- r p_x; py' <- r p_y; bx' <- r (b + b_x); by' <- r (b + b_y)
        let ux0 = px' - bx'; uy0 = py' + 0.4 - (by' + 1.4)
            len0 = sqrt (ux0 * ux0 + uy0 * uy0)
            (ux1, uy1, len1) = if len0 < 1e-6 then (1.0, 0.0, 1.0) else (ux0, uy0, len0)
            ux = ux1 / len1; uy = uy1 / len1
        rr <- rnd
        let off = (rr - 0.5) * 0.35
            vx0 = ux - uy * off; vy0 = uy + ux * off
            l2 = sqrt (vx0 * vx0 + vy0 * vy0)
            vx = vx0 / l2 * 7.5; vy = vy0 / l2 * 7.5
        dir <- r (b + e_dir)
        shoot (bx' + dir * 1.1) (by' + 1.4) vx vy 0 5 0.6 1
        ev ev_shoot (bx' + dir * 1.1) (by' + 1.4) dir
        addTo (b + e_shots) (-1.0); w (b + e_st) 0.3
      else when (shots <= 0 && st <= 0) $ do { w (b + e_state) (f bs_idle); w (b + e_st) 0.7 }
    else if state == bs_charge then do
      vy <- r (b + b_vy)
      w (b + b_vy) (fmax (vy - 60 * dt) (-30))
      st <- r (b + e_st)
      if st > 0.75 then w (b + b_vx) 0.0
      else do { dir <- r (b + e_dir); w (b + b_vx) (dir * (if lowHp then 15.0 else 12.0)) }
      moveBody b dt False
      st2 <- r (b + e_st); hx <- r (b + b_hx)
      if st2 <= 0.75 && hx /= 0 then do
        w (b + e_state) (f bs_recover); w (b + e_st) 1.3; w (b + b_vx) 0.0
        bossLand b 2.0
      else when (st2 <= 0) $ do { w (b + e_state) (f bs_recover); w (b + e_st) 0.8; w (b + b_vx) 0.0 }
    else when (state == bs_dying) $ do
      w (b + b_vx) 0.0
      rr <- rnd
      when (rr < 0.5) $ do
        rx <- rndr (-1.2) 1.2
        ry <- rndr 0 2.4
        x <- r (b + b_x); y <- r (b + b_y)
        ev ev_bossexplode (x + rx) (y + ry) 0.0
      st <- r (b + e_st)
      when (st <= 0) $ do
        w (b + e_alive) 0.0
        addTo g_score 3000.0; w g_bosskilled 1.0; w g_bossactive 0.0
        setDoor False
        x <- r (b + b_x); y <- r (b + b_y)
        addGoal x 2.0
        ev ev_bossdead x y 0.0

updateEnemies :: Double -> IO ()
updateEnemies dt = do
  n <- ii <$> r g_nen
  forN n $ \i -> do
    let b = enBase i
    alive <- r (b + e_alive)
    when (alive /= 0) $ do
      addTo (b + e_t) dt
      kind <- ii <$> r (b + e_kind)
      if kind == ek_slime then do
        vy <- r (b + b_vy)
        w (b + b_vy) (fmax (vy - 55 * dt) (-25))
        dir <- r (b + e_dir)
        w (b + b_vx) (dir * 1.9)
        moveBody b dt False
        hx <- r (b + b_hx); g <- r (b + b_gnd)
        if hx /= 0 then w (b + e_dir) (negate dir)
        else when (g /= 0) $ do
          bx <- r (b + b_x); bw <- r (b + b_w); by <- r (b + b_y)
          let tx = fi (bx + dir * (bw / 2 + 0.12)); ty = fi (by - 0.1)
          t <- tileAt tx ty
          if not (isSolid t) && t /= 2.0 then w (b + e_dir) (negate dir)
          else do
            t2 <- tileAt tx (fi (by + 0.1))
            when (t2 == 3.0) $ w (b + e_dir) (negate dir)
      else if kind == ek_bat then do
        px <- r p_x; ox <- r (b + e_ox); pd <- r p_dead
        let near = fabs (px - ox) < 8
        when (near && pd == 0) $ w (b + e_ox) (ox + sign (px - ox) * 0.9 * dt)
        ox' <- r (b + e_ox); oy <- r (b + e_oy); t <- r (b + e_t)
        oldx <- r (b + b_x)
        let nx = ox' + fsin (t * 1.3) * 3.2
        w (b + b_x) nx
        w (b + b_y) (oy + fsin (t * 2.4) * 0.6)
        let d = sign (nx - oldx)
        when (d /= 0) $ w (b + e_dir) d
      else if kind == ek_saw then do
        vx <- r (b + b_vx)
        addTo (b + b_x) (vx * dt)
        bx <- r (b + b_x); by <- r (b + b_y); ox <- r (b + e_ox)
        let ahead = fi (bx + sign vx * 0.5)
        floorAhead <- tileAt ahead (fi (by - 0.2))
        wallAhead <- tileAt ahead (fi (by + 0.4))
        when (isSolid wallAhead || (not (isSolid floorAhead) && floorAhead /= 2.0) || fabs (bx - ox) > 3) $ do
          w (b + b_vx) (negate vx)
          addTo (b + b_x) (negate vx * dt * 2)
      else if kind == ek_turret then do
        px <- r p_x; py <- r p_y; bx <- r (b + b_x); by <- r (b + b_y)
        let d = sign (px - bx)
        w (b + e_dir) (if d /= 0 then d else 1.0)
        addTo (b + e_cd) (-dt)
        cd <- r (b + e_cd); vw <- r g_vieww; pd <- r p_dead
        let dx = fabs (px - bx); dy = fabs (py - by)
        when (cd <= 0 && dx < fmin 13 (vw / 2 + 1) && dy < 7 && pd == 0) $ do
          w (b + e_cd) 2.2
          dir <- r (b + e_dir)
          shoot (bx + dir * 0.6) (by + 0.5) (dir * 6.5) 0 0 6 0.6 0
          ev ev_shoot (bx + dir * 0.7) (by + 0.5) dir
      else when (kind == ek_boss) $ updateBoss b dt

updateInteractions :: Double -> IO ()
updateInteractions dt = do
  pd <- r p_dead
  when (pd == 0) $ do
    nen <- ii <$> r g_nen
    forN nen $ \i -> do
      let e = enBase i
      alive <- r (e + e_alive)
      when (alive /= 0) $ do
        kind <- ii <$> r (e + e_kind)
        st <- r (e + e_state)
        if kind == ek_boss && (st == f bs_sleep || st == f bs_dying) then return ()
        else do
          hit <- boxHit p_x e
          when hit $ do
            ex <- r (e + b_x); ey <- r (e + b_y); eh <- r (e + b_h)
            vy <- r p_vy; pv <- r p_prevy; dsh <- r p_dasht; stomp <- r (e + e_stomp)
            if kind == ek_boss then
              if vy < 0 && pv >= ey + eh * 0.55 && dsh <= 0 then do
                ok <- damageBoss
                if ok then stompBounce True else w p_vy (fmax vy 8)
              else void (hurtPlayer ex)
            else if stomp == 0 then void (hurtPlayer ex)
            else if dsh > 0 then killEnemy e True
            else if vy < 0 && pv >= ey + eh * 0.5 then do
              killEnemy e False; stompBounce False; w g_freeze 0.04
            else void (hurtPlayer ex)

    nc <- ii <$> r g_ncoins
    forN nc $ \i -> do
      let c = coin_base + i * coin_n
      got <- r (c + c_got)
      when (got == 0) $ do
        rad <- r (c + c_r); px <- r p_x; py <- r p_y; cx <- r (c + c_x); cy <- r (c + c_y)
        when (fabs (px - cx) < rad + pw / 2 && fabs (py + 0.45 - cy) < rad + 0.45) $ do
          kind <- ii <$> r (c + c_kind)
          if kind == sk_heart then do
            hp <- r p_hp; mh <- r p_maxhp
            if hp >= mh then do { addTo g_score 250.0; ev ev_heart cx cy 0.0 }
            else do { addTo p_hp 1.0; ev ev_heart cx cy 1.0 }
          else if kind == sk_gem then do
            addTo g_coins 5.0; addTo g_score 500.0; ev ev_gem cx cy 0.0
          else do
            addTo g_coins 1.0; addTo g_score 100.0; ev ev_coin cx cy 0.0
          w (c + c_got) 1.0

    ns <- ii <$> r g_nspr
    forN ns $ \i -> do
      let s = spr_base + i * spr_n
      t <- r (s + sp_t)
      w (s + sp_t) (fmax 0 (t - dt))
      px <- r p_x; py <- r p_y; vy <- r p_vy; sx <- r (s + sp_x); sy <- r (s + sp_y)
      when (fabs (px - sx) < 0.7 && py >= sy - 0.1 && py < sy + 0.55 && vy <= 0.5) $ do
        w p_vy 31.0; w p_gnd 0.0; w p_jumps 1.0; w p_airdash 0.0; w p_dasht 0.0; w p_coyote 0.0
        w (s + sp_t) 0.3
        ev ev_spring sx (sy + 0.4) 0.0

    nk <- ii <$> r g_nck
    forN nk $ \i -> do
      let c = ckt_base + i * ckt_n
      on <- r (c + ck_on); px <- r p_x; py <- r p_y; cx <- r (c + ck_x); cy <- r (c + ck_y)
      when (on == 0 && fabs (px - cx) < 1.3 && fabs (py - cy) < 2) $ do
        forN nk $ \j -> w (ckt_base + j * ckt_n + ck_on) 0.0
        w (c + ck_on) 1.0
        w g_chkx cx; w g_chky cy
        ev ev_check cx cy 0.0

    bi <- ii <$> r g_boss
    when (bi >= 0) $ do
      let b = enBase bi
      st <- r (b + e_state); al <- r (b + e_alive); px <- r p_x; tr <- r g_btrig
      when (st == f bs_sleep && al /= 0 && px > tr) startBoss

    gon <- r g_goalon; px <- r p_x; py <- r p_y; gx <- r g_goalx; gy <- r g_goaly
    when (gon /= 0 && fabs (px - gx) < 0.9 && fabs (py + 0.5 - (gy + 1.1)) < 1.5) $ do
      w g_mode (f mode_clear)
      ev ev_complete px py 0.0

step :: Double -> IO ()
step dt = do
  fr <- r g_freeze
  if fr > 0 then w g_freeze (fr - dt)
  else do
    mode <- ii <$> r g_mode
    if mode /= mode_play then do { updatePlatforms dt; updateEnemies dt; updateShots dt }
    else do
      addTo g_time dt
      updatePlatforms dt
      dead <- r p_dead
      if dead == 0 then updatePlayer dt
      else do
        addTo p_deadt dt
        dt' <- r p_deadt
        when (dt' > 1.4) respawn
      updateEnemies dt
      updateShots dt
      updateInteractions dt

advanceCore :: Double -> IO Int
advanceCore rdt = do
  w g_evn 0.0
  a0 <- r g_acc
  let loop !acc !n
        | acc >= dtStep && n < 12 = do { step dtStep; loop (acc - dtStep) (n + 1) }
        | otherwise = return (acc, n)
  (acc, n) <- loop (a0 + rdt) (0 :: Int)
  w g_acc (if n >= 12 then 0.0 else acc)
  w g_steps (f n)
  when (n > 0) $ do { w in_jumppress 0.0; w in_dashpress 0.0 }
  return n

memGet :: Int -> IO Double
memGet = r

memSet :: Int -> Double -> IO ()
memSet = w

_unusedInt32 :: Int32
_unusedInt32 = 0
