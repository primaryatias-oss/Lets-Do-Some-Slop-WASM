(* Slop Runner simulation core - OCaml port.
   Mirrors core/js/core.js line by line; all state lives in the flat float array [m].
   Pure OCaml (no Js_of_ocaml here) so it can also be tested natively. *)
open Abi

let level_h = 14
let dt_step = 1.0 /. 120.0
let pw = 0.62
let ph = 0.86
let max_speed = 8.2
let accel_g = 95.0
let accel_a = 62.0
let decel_g = 110.0
let decel_a = 26.0
let gravity = 62.0
let fall_gravity = 74.0
let max_fall = 27.0
let jump_v = 19.6
let double_v = 17.2
let coyote = 0.11
let buffer = 0.13
let wall_slide = 3.4
let wall_jump_x = 9.5
let wall_jump_y = 18.2
let wall_lock = 0.16
let dash_time = 0.17
let dash_speed = 24.0
let dash_cd = 0.75
let invuln = 1.5
let stomp_bounce = 15.5

let m : float array = Array.make mem_size 0.0
let ( .%() ) (a : float array) (i : int) = Array.unsafe_get a i
let ( .%()<- ) (a : float array) (i : int) (v : float) = Array.unsafe_set a i v

let f (x : int) : float = float_of_int x
let fi (x : float) : int = int_of_float (floor x)
let ii (x : float) : int = int_of_float x
let b2f (b : bool) : float = if b then 1.0 else 0.0
let sign (v : float) : float = if v > 0.0 then 1.0 else if v < 0.0 then -1.0 else 0.0
let fmin (a : float) (b : float) : float = if a < b then a else b
let fmax (a : float) (b : float) : float = if a > b then a else b
let fabs (a : float) : float = if a < 0.0 then -.a else a
let clamp (v : float) (a : float) (b : float) : float = if v < a then a else if v > b then b else v
let approach (v : float) (t : float) (s : float) : float = if v < t then fmin (v +. s) t else fmax (v -. s) t

let fsin (x : float) : float =
  let k = floor (x *. 0.15915494309189535 +. 0.5) in
  let r = ref (x -. k *. 6.283185307179586) in
  if !r > 1.5707963267948966 then r := 3.141592653589793 -. !r
  else if !r < -1.5707963267948966 then r := -3.141592653589793 -. !r;
  let r = !r in
  let r2 = r *. r in
  r *. (1.0 +. r2 *. (-0.16666666666666666 +. r2 *. (0.008333333333333333 +. r2 *. (-0.0001984126984126984 +.
        r2 *. (2.7557319223985893e-6 +. r2 *. (-2.505210838544172e-8 +. r2 *. 1.6059043836821613e-10))))))
let fcos (x : float) : float = fsin (x +. 1.5707963267948966)

let rnd () : float =
  let s = m.%(g_rng) *. 1664525.0 +. 1013904223.0 in
  let s = s -. floor (s /. 4294967296.0) *. 4294967296.0 in
  m.%(g_rng) <- s;
  s /. 4294967296.0
let rndr (a : float) (b : float) : float = a +. rnd () *. (b -. a)

let ev (t : int) (x : float) (y : float) (a : float) : unit =
  let n = ii m.%(g_evn) in
  if n < ev_max then begin
    let i = ev_base + n * ev_n in
    m.%(i) <- f t; m.%(i + 1) <- x; m.%(i + 2) <- y; m.%(i + 3) <- a;
    m.%(g_evn) <- f (n + 1)
  end

let tile_at (tx : int) (ty : int) : float =
  let lw = ii m.%(g_lw) in
  if tx < 0 || tx >= lw then 1.0
  else if ty < 0 || ty >= level_h then 0.0
  else m.%(tile_base + ty * lw + tx)
let is_solid (t : float) : bool = t = 1.0 || t = 4.0
let set_tile (tx : int) (ty : int) (v : float) : unit = m.%(tile_base + ty * ii m.%(g_lw) + tx) <- v

let move_body (b : int) (dt : float) (drop : bool) : unit =
  m.%(b + b_hx) <- 0.0; m.%(b + b_hy) <- 0.0;
  let hw = m.%(b + b_w) /. 2.0 in
  m.%(b + b_x) <- m.%(b + b_x) +. m.%(b + b_vx) *. dt;
  let y0 = fi (m.%(b + b_y) +. 0.02) and y1 = fi (m.%(b + b_y) +. m.%(b + b_h) -. 0.02) in
  let vx = m.%(b + b_vx) in
  if vx > 0.0 then begin
    let tx = fi (m.%(b + b_x) +. hw) in
    let ty = ref y0 and stop = ref false in
    while !ty <= y1 && not !stop do
      if is_solid (tile_at tx !ty) then begin
        m.%(b + b_x) <- f tx -. hw -. 1e-4; m.%(b + b_vx) <- 0.0; m.%(b + b_hx) <- 1.0; stop := true
      end;
      incr ty
    done
  end else if vx < 0.0 then begin
    let tx = fi (m.%(b + b_x) -. hw) in
    let ty = ref y0 and stop = ref false in
    while !ty <= y1 && not !stop do
      if is_solid (tile_at tx !ty) then begin
        m.%(b + b_x) <- f tx +. 1.0 +. hw +. 1e-4; m.%(b + b_vx) <- 0.0; m.%(b + b_hx) <- -1.0; stop := true
      end;
      incr ty
    done
  end;
  let prev_y = m.%(b + b_y) in
  m.%(b + b_y) <- m.%(b + b_y) +. m.%(b + b_vy) *. dt;
  let x0 = fi (m.%(b + b_x) -. hw +. 1e-3) and x1 = fi (m.%(b + b_x) +. hw -. 1e-3) in
  m.%(b + b_gnd) <- 0.0;
  if m.%(b + b_vy) <= 0.0 then begin
    let ty = fi m.%(b + b_y) in
    let tx = ref x0 and stop = ref false in
    while !tx <= x1 && not !stop do
      let t = tile_at !tx ty in
      if is_solid t || (t = 2.0 && (not drop) && prev_y >= f ty +. 1.0 -. 0.02) then begin
        m.%(b + b_y) <- f ty +. 1.0; m.%(b + b_vy) <- 0.0; m.%(b + b_gnd) <- 1.0; m.%(b + b_hy) <- -1.0; stop := true
      end;
      incr tx
    done
  end else begin
    let ty = fi (m.%(b + b_y) +. m.%(b + b_h)) in
    let tx = ref x0 and stop = ref false in
    while !tx <= x1 && not !stop do
      if is_solid (tile_at !tx ty) then begin
        m.%(b + b_y) <- f ty -. m.%(b + b_h) -. 1e-4; m.%(b + b_vy) <- 0.0; m.%(b + b_hy) <- 1.0; stop := true
      end;
      incr tx
    done
  end

let box_hit (a : int) (b : int) : bool =
  fabs (m.%(a + b_x) -. m.%(b + b_x)) < (m.%(a + b_w) +. m.%(b + b_w)) /. 2.0
  && m.%(a + b_y) < m.%(b + b_y) +. m.%(b + b_h)
  && m.%(a + b_y) +. m.%(a + b_h) > m.%(b + b_y)

let en_base i = en_base + i * en_n
let pl_base i = plt_base + i * plt_n
let sh_base i = sh_base + i * sh_n

let set_door (closed : bool) : unit =
  if m.%(g_dooron) <> 0.0 then begin
    for ty = ii m.%(g_doory0) to ii m.%(g_doory1) do
      set_tile (ii m.%(g_doorx)) ty (if closed then 4.0 else 0.0)
    done;
    m.%(g_doorclosed) <- b2f closed;
    ev ev_door m.%(g_doorx) 0.0 (b2f closed)
  end

let add_enemy (kind : int) (x : float) (y : float) : unit =
  let i = ii m.%(g_nen) in
  if i < en_max then begin
    m.%(g_nen) <- f (i + 1);
    let b = en_base i in
    for k = 0 to en_n - 1 do m.%(b + k) <- 0.0 done;
    m.%(b + b_x) <- x; m.%(b + b_y) <- y; m.%(b + b_w) <- 0.8; m.%(b + b_h) <- 0.62;
    m.%(b + e_kind) <- f kind; m.%(b + e_alive) <- 1.0; m.%(b + e_stomp) <- 1.0;
    let r0 = rnd () in
    m.%(b + e_dir) <- (if r0 < 0.5 then -1.0 else 1.0);
    let r1 = rnd () in
    m.%(b + e_t) <- r1 *. 10.0;
    m.%(b + e_ox) <- x; m.%(b + e_oy) <- y;
    let r2 = rnd () in
    m.%(b + e_cd) <- 1.0 +. r2;
    if kind = ek_bat then begin m.%(b + b_w) <- 0.75; m.%(b + b_h) <- 0.5 end
    else if kind = ek_saw then begin
      m.%(b + b_w) <- 0.78; m.%(b + b_h) <- 0.78; m.%(b + e_stomp) <- 0.0; m.%(b + e_dir) <- 1.0; m.%(b + b_vx) <- 3.0
    end else if kind = ek_turret then begin
      m.%(b + b_w) <- 0.9; m.%(b + b_h) <- 0.8; m.%(b + e_stomp) <- 0.0
    end else if kind = ek_boss then begin
      m.%(b + b_w) <- 2.5; m.%(b + b_h) <- 2.3; m.%(b + e_hp) <- 8.0; m.%(b + e_hp0) <- 8.0;
      m.%(b + e_state) <- f bs_sleep; m.%(b + e_dir) <- -1.0;
      m.%(g_boss) <- f i
    end
  end

let add_platform (kind : int) (x : float) (ty : float) : unit =
  let i = ii m.%(g_nplt) in
  if i < plt_max then begin
    m.%(g_nplt) <- f (i + 1);
    let p = pl_base i in
    for k = 0 to plt_n - 1 do m.%(p + k) <- 0.0 done;
    m.%(p + pt_kind) <- f kind; m.%(p + pt_x) <- x; m.%(p + pt_x0) <- x;
    m.%(p + pt_top) <- ty +. 0.75; m.%(p + pt_top0) <- ty +. 0.75; m.%(p + pt_prevtop) <- ty +. 0.75;
    m.%(p + pt_w) <- (if kind = 2 then 1.0 else 3.0); m.%(p + pt_solid) <- 1.0;
    let r = rnd () in
    m.%(p + pt_t) <- r *. 6.0; m.%(p + pt_state) <- f cs_idle
  end

let add_goal (x : float) (y : float) : unit =
  m.%(g_goalon) <- 1.0; m.%(g_goalx) <- x; m.%(g_goaly) <- y

let reset_player (x : float) (y : float) : unit =
  m.%(p_x) <- x; m.%(p_y) <- y; m.%(p_vx) <- 0.0; m.%(p_vy) <- 0.0; m.%(p_face) <- 1.0; m.%(p_gnd) <- 0.0;
  m.%(p_coyote) <- 0.0; m.%(p_buffer) <- 0.0; m.%(p_jumps) <- 1.0; m.%(p_walldir) <- 0.0; m.%(p_wallgrace) <- 0.0; m.%(p_walllock) <- 0.0;
  m.%(p_dasht) <- 0.0; m.%(p_dashcd) <- 0.0; m.%(p_airdash) <- 0.0; m.%(p_inv) <- 0.0; m.%(p_dropt) <- 0.0; m.%(p_dead) <- 0.0; m.%(p_deadt) <- 0.0;
  m.%(p_riding) <- -1.0; m.%(p_safex) <- x; m.%(p_safey) <- y; m.%(p_safet) <- 0.0; m.%(p_prevy) <- y; m.%(p_wasgnd) <- 0.0

let init () : unit =
  for i = 0 to mem_size - 1 do m.%(i) <- 0.0 done;
  m.%(g_rng) <- 12345.0; m.%(g_vieww) <- 20.0;
  m.%(p_w) <- pw; m.%(p_h) <- ph; m.%(p_maxhp) <- 3.0; m.%(p_hp) <- 3.0; m.%(p_riding) <- -1.0; m.%(g_boss) <- -1.0

let load_level () : unit =
  m.%(g_ncoins) <- 0.0; m.%(g_nen) <- 0.0; m.%(g_nplt) <- 0.0; m.%(g_nsh) <- 0.0; m.%(g_nspr) <- 0.0; m.%(g_nck) <- 0.0; m.%(g_boss) <- -1.0;
  m.%(g_time) <- 0.0; m.%(g_coins) <- 0.0; m.%(g_kills) <- 0.0; m.%(g_deaths) <- 0.0; m.%(g_score) <- 0.0; m.%(g_bosskilled) <- 0.0;
  m.%(g_bossactive) <- 0.0; m.%(g_freeze) <- 0.0; m.%(g_coinstotal) <- 0.0; m.%(g_doorclosed) <- 0.0; m.%(g_acc) <- 0.0; m.%(g_evn) <- 0.0;
  let n = ii m.%(g_nspec) in
  for s = 0 to n - 1 do
    let kind = ii m.%(spec_base + s * spec_n) in
    let x = m.%(spec_base + s * spec_n + 1) and y = m.%(spec_base + s * spec_n + 2) in
    if kind <= sk_heart then begin
      let i = ii m.%(g_ncoins) in
      if i < coin_max then begin
        m.%(g_ncoins) <- f (i + 1);
        let c = coin_base + i * coin_n in
        m.%(c + c_kind) <- f kind; m.%(c + c_x) <- x; m.%(c + c_y) <- y +. 0.5; m.%(c + c_got) <- 0.0;
        m.%(c + c_r) <- (if kind = sk_coin then 0.55 else 0.65);
        if kind = sk_coin then m.%(g_coinstotal) <- m.%(g_coinstotal) +. 1.0
        else if kind = sk_gem then m.%(g_coinstotal) <- m.%(g_coinstotal) +. 5.0
      end
    end
    else if kind = sk_slime then add_enemy ek_slime x y
    else if kind = sk_bat then add_enemy ek_bat x (y +. 0.5)
    else if kind = sk_saw then add_enemy ek_saw x (y +. 0.02)
    else if kind = sk_turret then add_enemy ek_turret x y
    else if kind = sk_spring then begin
      let i = ii m.%(g_nspr) in
      if i < spr_max then begin
        m.%(g_nspr) <- f (i + 1);
        let q = spr_base + i * spr_n in
        m.%(q + sp_x) <- x; m.%(q + sp_y) <- y; m.%(q + sp_t) <- 0.0
      end
    end
    else if kind = sk_plat_h then add_platform 0 x y
    else if kind = sk_plat_v then add_platform 1 x y
    else if kind = sk_crumble then add_platform 2 x y
    else if kind = sk_check then begin
      let i = ii m.%(g_nck) in
      if i < ckt_max then begin
        m.%(g_nck) <- f (i + 1);
        let q = ckt_base + i * ckt_n in
        m.%(q + ck_x) <- x; m.%(q + ck_y) <- y; m.%(q + ck_on) <- 0.0
      end
    end
    else if kind = sk_boss then add_enemy ek_boss x y
  done;
  m.%(p_maxhp) <- 3.0; m.%(p_hp) <- 3.0;
  m.%(g_chkx) <- m.%(g_spawnx); m.%(g_chky) <- m.%(g_spawny);
  reset_player m.%(g_spawnx) m.%(g_spawny)

(* ------------------------------------------------------------ player *)
let kill_player () : unit =
  m.%(p_dead) <- 1.0; m.%(p_deadt) <- 0.0; m.%(p_hp) <- 0.0;
  m.%(g_deaths) <- m.%(g_deaths) +. 1.0;
  ev ev_die m.%(p_x) (m.%(p_y) +. 0.5) 0.0

let hurt_player (src_x : float) : bool =
  if m.%(p_inv) > 0.0 || m.%(p_dasht) > 0.0 || m.%(p_dead) <> 0.0 then false
  else begin
    m.%(p_hp) <- m.%(p_hp) -. 1.0;
    m.%(p_inv) <- invuln;
    m.%(g_freeze) <- 0.08;
    ev ev_hurt m.%(p_x) (m.%(p_y) +. 0.5) 0.0;
    m.%(p_vx) <- (if m.%(p_x) < src_x then -1.0 else 1.0) *. 8.5; m.%(p_vy) <- 12.0;
    m.%(p_walllock) <- 0.18; m.%(p_dasht) <- 0.0;
    if m.%(p_hp) <= 0.0 then kill_player ();
    true
  end

let reset_boss () : unit =
  let b = en_base (ii m.%(g_boss)) in
  m.%(g_bossactive) <- 0.0;
  m.%(b + e_state) <- f bs_sleep; m.%(b + e_hp) <- m.%(b + e_hp0); m.%(b + e_alive) <- 1.0; m.%(b + e_inv) <- 0.0;
  m.%(b + b_x) <- m.%(g_bspecx); m.%(b + b_y) <- m.%(g_bspecy);
  set_door false;
  m.%(g_nsh) <- 0.0

let pit_fall () : unit =
  if m.%(p_dead) = 0.0 then begin
    m.%(p_hp) <- m.%(p_hp) -. 1.0;
    ev ev_pit m.%(p_x) m.%(p_y) 0.0;
    if m.%(p_hp) <= 0.0 then kill_player ()
    else begin
      m.%(p_x) <- m.%(p_safex); m.%(p_y) <- m.%(p_safey) +. 0.05; m.%(p_vx) <- 0.0; m.%(p_vy) <- 0.0; m.%(p_inv) <- 2.0;
      ev ev_respawn m.%(p_x) (m.%(p_y) +. 0.4) 1.0
    end
  end

let respawn () : unit =
  reset_player m.%(g_chkx) m.%(g_chky);
  m.%(p_hp) <- m.%(p_maxhp); m.%(p_inv) <- 2.0;
  if m.%(g_boss) >= 0.0 && m.%(g_bossactive) <> 0.0 then reset_boss ();
  ev ev_respawn m.%(p_x) (m.%(p_y) +. 0.5) 0.0

let stomp_bounce (strong : bool) : unit =
  m.%(p_vy) <- (if m.%(in_jumpheld) <> 0.0 then stomp_bounce +. 3.5 else stomp_bounce);
  if strong then m.%(p_vy) <- m.%(p_vy) +. 2.0;
  m.%(p_jumps) <- 1.0; m.%(p_airdash) <- 0.0; m.%(p_gnd) <- 0.0; m.%(p_dasht) <- 0.0

let update_player (dt : float) : unit =
  let ax = m.%(in_ax) in
  m.%(p_prevy) <- m.%(p_y);
  m.%(p_inv) <- fmax 0.0 (m.%(p_inv) -. dt);
  m.%(p_dashcd) <- m.%(p_dashcd) -. dt; m.%(p_walllock) <- m.%(p_walllock) -. dt; m.%(p_buffer) <- m.%(p_buffer) -. dt;
  m.%(p_dropt) <- m.%(p_dropt) -. dt; m.%(p_wallgrace) <- m.%(p_wallgrace) -. dt;

  if m.%(in_jumppress) <> 0.0 then begin m.%(in_jumppress) <- 0.0; m.%(p_buffer) <- buffer end;
  let want_dash = m.%(in_dashpress) <> 0.0 in
  if want_dash then m.%(in_dashpress) <- 0.0;

  let rid = ii m.%(p_riding) in
  if rid >= 0 then begin
    let p = pl_base rid in
    if m.%(p + pt_solid) <> 0.0 && m.%(p_vy) <= 0.0 then begin
      m.%(p_x) <- m.%(p_x) +. m.%(p + pt_dx); m.%(p_y) <- m.%(p + pt_top)
    end
  end;

  if m.%(p_gnd) <> 0.0 then begin m.%(p_coyote) <- coyote; m.%(p_jumps) <- 1.0; m.%(p_airdash) <- 0.0 end
  else m.%(p_coyote) <- m.%(p_coyote) -. dt;

  if want_dash && m.%(p_dashcd) <= 0.0 && m.%(p_airdash) = 0.0 then begin
    m.%(p_dasht) <- dash_time; m.%(p_dashcd) <- dash_cd;
    m.%(p_dashdir) <- (if ax <> 0.0 then ax else m.%(p_face)); m.%(p_face) <- m.%(p_dashdir);
    if m.%(p_gnd) = 0.0 then m.%(p_airdash) <- 1.0;
    ev ev_dash m.%(p_x) (m.%(p_y) +. 0.4) m.%(p_dashdir)
  end;

  if m.%(p_dasht) > 0.0 then begin
    m.%(p_dasht) <- m.%(p_dasht) -. dt;
    m.%(p_vx) <- m.%(p_dashdir) *. dash_speed;
    m.%(p_vy) <- 0.0;
    if m.%(p_dasht) <= 0.0 then m.%(p_vx) <- m.%(p_dashdir) *. max_speed
  end else begin
    if m.%(p_walllock) <= 0.0 then begin
      let target = ax *. max_speed in
      let acc = ref 0.0 in
      if ax = 0.0 then acc := (if m.%(p_gnd) <> 0.0 then decel_g else decel_a)
      else begin
        acc := (if m.%(p_gnd) <> 0.0 then accel_g else accel_a);
        if sign m.%(p_vx) = -.ax then acc := !acc *. 1.6
      end;
      m.%(p_vx) <- approach m.%(p_vx) target (!acc *. dt);
      if ax <> 0.0 then m.%(p_face) <- ax
    end;

    m.%(p_walldir) <- 0.0;
    if m.%(p_gnd) = 0.0 && ax <> 0.0 then begin
      let tx = fi (m.%(p_x) +. ax *. (pw /. 2.0 +. 0.08)) in
      if is_solid (tile_at tx (fi (m.%(p_y) +. 0.3))) || is_solid (tile_at tx (fi (m.%(p_y) +. 0.75))) then begin
        m.%(p_walldir) <- ax; m.%(p_wallgrace) <- 0.1; m.%(p_wallmem) <- ax
      end
    end;

    if m.%(p_buffer) > 0.0 then begin
      if m.%(in_downheld) <> 0.0 && m.%(p_gnd) <> 0.0 && tile_at (fi m.%(p_x)) (fi (m.%(p_y) -. 0.05)) = 2.0 then begin
        m.%(p_dropt) <- 0.22; m.%(p_buffer) <- 0.0; m.%(p_y) <- m.%(p_y) -. 0.06; m.%(p_gnd) <- 0.0
      end else if m.%(p_coyote) > 0.0 then begin
        m.%(p_vy) <- jump_v; m.%(p_buffer) <- 0.0; m.%(p_coyote) <- 0.0; m.%(p_gnd) <- 0.0;
        ev ev_jump m.%(p_x) (m.%(p_y) +. 0.05) 0.0
      end else if m.%(p_wallgrace) > 0.0 && m.%(p_wallmem) <> 0.0 then begin
        m.%(p_vx) <- -.m.%(p_wallmem) *. wall_jump_x; m.%(p_vy) <- wall_jump_y;
        m.%(p_walllock) <- wall_lock; m.%(p_face) <- -.m.%(p_wallmem); m.%(p_buffer) <- 0.0; m.%(p_wallgrace) <- 0.0;
        m.%(p_jumps) <- 1.0; m.%(p_airdash) <- 0.0;
        ev ev_walljump (m.%(p_x) +. m.%(p_wallmem) *. 0.3) (m.%(p_y) +. 0.5) m.%(p_wallmem)
      end else if m.%(p_jumps) > 0.0 then begin
        m.%(p_vy) <- double_v; m.%(p_jumps) <- m.%(p_jumps) -. 1.0; m.%(p_buffer) <- 0.0;
        ev ev_double m.%(p_x) (m.%(p_y) +. 0.05) 0.0
      end
    end;

    let g = if m.%(p_vy) > 0.0 then (if m.%(in_jumpheld) <> 0.0 then gravity else gravity *. 2.4) else fall_gravity in
    m.%(p_vy) <- fmax (m.%(p_vy) -. g *. dt) (-.max_fall);
    if m.%(p_walldir) <> 0.0 && m.%(p_vy) < -.wall_slide then m.%(p_vy) <- -.wall_slide
  end;

  let vy_pre = m.%(p_vy) in
  move_body p_x dt (m.%(p_dropt) > 0.0);

  m.%(p_riding) <- -1.0;
  if vy_pre <= 0.0 && m.%(p_dasht) <= 0.0 then begin
    let np = ii m.%(g_nplt) in
    let i = ref 0 and stop = ref false in
    while !i < np && not !stop do
      let p = pl_base !i in
      if m.%(p + pt_solid) <> 0.0 then begin
        if fabs (m.%(p_x) -. m.%(p + pt_x)) < m.%(p + pt_w) /. 2.0 +. pw /. 2.0 -. 0.08 && m.%(p_prevy) >= m.%(p + pt_prevtop) -. 0.12
           && m.%(p_y) <= m.%(p + pt_top) +. 0.02 && m.%(p_y) >= m.%(p + pt_top) -. 0.6 then begin
          m.%(p_y) <- m.%(p + pt_top); m.%(p_vy) <- 0.0; m.%(p_gnd) <- 1.0; m.%(p_riding) <- f !i;
          if m.%(p + pt_kind) = 2.0 && m.%(p + pt_state) = f cs_idle then begin
            m.%(p + pt_state) <- f cs_shake; m.%(p + pt_timer) <- 0.45
          end;
          stop := true
        end
      end;
      incr i
    done
  end;

  if m.%(p_gnd) <> 0.0 && m.%(p_wasgnd) = 0.0 && vy_pre < -9.0 then ev ev_land m.%(p_x) (m.%(p_y) +. 0.05) 0.0;
  if m.%(p_hy) = 1.0 && m.%(p_vy) = 0.0 then m.%(p_vy) <- -1.0;
  m.%(p_wasgnd) <- m.%(p_gnd);

  if m.%(p_gnd) <> 0.0 && m.%(p_riding) < 0.0 then begin
    let ty = fi m.%(p_y) - 1 in
    if is_solid (tile_at (fi (m.%(p_x) -. 0.6)) ty) && is_solid (tile_at (fi (m.%(p_x) +. 0.6)) ty)
       && is_solid (tile_at (fi (m.%(p_x) -. 1.6)) ty) && is_solid (tile_at (fi (m.%(p_x) +. 1.6)) ty)
       && tile_at (fi m.%(p_x)) (fi m.%(p_y)) <> 3.0 then begin
      m.%(p_safet) <- m.%(p_safet) +. dt;
      if m.%(p_safet) > 0.35 then begin m.%(p_safex) <- m.%(p_x); m.%(p_safey) <- m.%(p_y) end
    end else m.%(p_safet) <- 0.0
  end else m.%(p_safet) <- 0.0;

  if m.%(p_inv) <= 0.0 && m.%(p_dasht) <= 0.0 then begin
    let x0 = fi (m.%(p_x) -. pw /. 2.0) and x1 = fi (m.%(p_x) +. pw /. 2.0) in
    let y0 = fi m.%(p_y) and y1 = fi (m.%(p_y) +. ph *. 0.5) in
    let fin = ref false in
    let ty = ref y0 in
    while !ty <= y1 && not !fin do
      let tx = ref x0 in
      while !tx <= x1 && not !fin do
        if tile_at !tx !ty = 3.0 then begin
          if m.%(p_x) +. pw /. 2.0 > f !tx +. 0.15 && m.%(p_x) -. pw /. 2.0 < f !tx +. 0.85 && m.%(p_y) < f !ty +. 0.5 then begin
            if hurt_player (f !tx +. 0.5) then begin
              m.%(p_vy) <- 15.0;
              m.%(p_vx) <- (if m.%(p_x) < f !tx +. 0.5 then -1.0 else 1.0) *. 5.0
            end;
            fin := true
          end
        end;
        incr tx
      done;
      incr ty
    done
  end;

  if m.%(p_y) < -2.5 then pit_fall ()

(* --------------------------------------------------------- platforms *)
let update_platforms (dt : float) : unit =
  let n = ii m.%(g_nplt) in
  for i = 0 to n - 1 do
    let p = pl_base i in
    m.%(p + pt_prevtop) <- m.%(p + pt_top); m.%(p + pt_dx) <- 0.0; m.%(p + pt_dy) <- 0.0;
    m.%(p + pt_t) <- m.%(p + pt_t) +. dt;
    let kind = ii m.%(p + pt_kind) in
    if kind = 0 then begin
      let nx = m.%(p + pt_x0) +. fsin (m.%(p + pt_t) *. 1.05) *. 2.4 in
      m.%(p + pt_dx) <- nx -. m.%(p + pt_x); m.%(p + pt_x) <- nx
    end else if kind = 1 then begin
      let nt = m.%(p + pt_top0) +. (1.0 -. fcos (m.%(p + pt_t) *. 0.95)) /. 2.0 *. 5.0 in
      m.%(p + pt_dy) <- nt -. m.%(p + pt_top); m.%(p + pt_top) <- nt
    end else begin
      let st = ii m.%(p + pt_state) in
      if st = cs_shake then begin
        m.%(p + pt_timer) <- m.%(p + pt_timer) -. dt;
        if m.%(p + pt_timer) <= 0.0 then begin
          m.%(p + pt_state) <- f cs_fall; m.%(p + pt_solid) <- 0.0; m.%(p + pt_vy) <- 0.0;
          ev ev_crumble m.%(p + pt_x) m.%(p + pt_top) 0.0
        end
      end else if st = cs_fall then begin
        m.%(p + pt_vy) <- m.%(p + pt_vy) -. 32.0 *. dt; m.%(p + pt_top) <- m.%(p + pt_top) +. m.%(p + pt_vy) *. dt;
        if m.%(p + pt_top) < -3.0 then begin m.%(p + pt_state) <- f cs_gone; m.%(p + pt_timer) <- 2.6 end
      end else if st = cs_gone then begin
        m.%(p + pt_timer) <- m.%(p + pt_timer) -. dt;
        if m.%(p + pt_timer) <= 0.0 then begin
          m.%(p + pt_state) <- f cs_idle; m.%(p + pt_solid) <- 1.0; m.%(p + pt_top) <- m.%(p + pt_top0); m.%(p + pt_prevtop) <- m.%(p + pt_top)
        end
      end
    end
  done

(* -------------------------------------------------------------- shots *)
let shoot (x : float) (y : float) (vx : float) (vy : float) (g : float) (life : float) (size : float) (purple : float) : unit =
  let i = ii m.%(g_nsh) in
  if i < sh_max then begin
    m.%(g_nsh) <- f (i + 1);
    let s = sh_base i in
    m.%(s + s_x) <- x; m.%(s + s_y) <- y; m.%(s + s_vx) <- vx; m.%(s + s_vy) <- vy; m.%(s + s_g) <- g; m.%(s + s_life) <- life;
    m.%(s + s_r) <- size *. 0.36; m.%(s + s_purple) <- purple; m.%(s + s_size) <- size
  end

let remove_shot (i : int) : unit =
  let last = ii m.%(g_nsh) - 1 in
  if i <> last then begin
    let a = sh_base i and b = sh_base last in
    for k = 0 to sh_n - 1 do m.%(a + k) <- m.%(b + k) done
  end;
  m.%(g_nsh) <- f last

let update_shots (dt : float) : unit =
  let i = ref (ii m.%(g_nsh) - 1) in
  while !i >= 0 do
    let s = sh_base !i in
    m.%(s + s_life) <- m.%(s + s_life) -. dt;
    m.%(s + s_vy) <- m.%(s + s_vy) -. m.%(s + s_g) *. dt;
    m.%(s + s_x) <- m.%(s + s_x) +. m.%(s + s_vx) *. dt; m.%(s + s_y) <- m.%(s + s_y) +. m.%(s + s_vy) *. dt;
    let dead = ref (m.%(s + s_life) <= 0.0 || m.%(s + s_y) < -3.0) in
    if (not !dead) && is_solid (tile_at (fi m.%(s + s_x)) (fi m.%(s + s_y))) then begin
      dead := true;
      ev ev_shothit m.%(s + s_x) m.%(s + s_y) m.%(s + s_purple)
    end;
    if (not !dead) && m.%(p_dead) = 0.0 && fabs (m.%(p_x) -. m.%(s + s_x)) < pw /. 2.0 +. m.%(s + s_r)
       && m.%(s + s_y) > m.%(p_y) -. m.%(s + s_r) && m.%(s + s_y) < m.%(p_y) +. ph +. m.%(s + s_r) then begin
      if hurt_player m.%(s + s_x) then dead := true
    end;
    if !dead then remove_shot !i;
    decr i
  done

(* ------------------------------------------------------------ enemies *)
let kill_enemy (b : int) (by_dash : bool) : unit =
  m.%(b + e_alive) <- 0.0;
  m.%(g_kills) <- m.%(g_kills) +. 1.0; m.%(g_score) <- m.%(g_score) +. 150.0;
  ev ev_kill m.%(b + b_x) (m.%(b + b_y) +. m.%(b + b_h) /. 2.0) (m.%(b + e_kind) +. (if by_dash then 10.0 else 0.0))

let start_boss () : unit =
  let b = en_base (ii m.%(g_boss)) in
  m.%(g_bossactive) <- 1.0;
  set_door true;
  m.%(b + b_x) <- m.%(g_bspecx); m.%(b + b_y) <- f (level_h + 1); m.%(b + b_vx) <- 0.0; m.%(b + b_vy) <- 0.0;
  m.%(b + e_state) <- f bs_intro; m.%(b + e_hp) <- m.%(b + e_hp0); m.%(b + e_inv) <- 0.0; m.%(b + e_alive) <- 1.0;
  ev ev_bossstart m.%(b + b_x) m.%(b + b_y) 0.0

let damage_boss () : bool =
  let b = en_base (ii m.%(g_boss)) in
  let st = ii m.%(b + e_state) in
  if m.%(b + e_inv) > 0.0 || st = bs_intro || st = bs_dying then false
  else begin
    m.%(b + e_hp) <- m.%(b + e_hp) -. 1.0; m.%(b + e_inv) <- 1.1;
    m.%(g_freeze) <- 0.1;
    ev ev_bosshit m.%(b + b_x) m.%(b + b_y) m.%(b + e_hp);
    if m.%(b + e_hp) <= 0.0 then begin
      m.%(b + e_state) <- f bs_dying; m.%(b + e_st) <- 1.8; m.%(b + b_vx) <- 0.0;
      m.%(g_nsh) <- 0.0
    end;
    true
  end

let boss_land (b : int) (variant : float) : unit = ev ev_boom m.%(b + b_x) m.%(b + b_y) variant

let update_boss (b : int) (dt : float) : unit =
  let state = ii m.%(b + e_state) in
  if state <> bs_sleep then begin
    m.%(b + e_inv) <- fmax 0.0 (m.%(b + e_inv) -. dt);
    m.%(b + e_st) <- m.%(b + e_st) -. dt;
    let dx = m.%(p_x) -. m.%(b + b_x) in
    let low_hp = m.%(b + e_hp) <= 3.0 in
    if state = bs_intro then begin
      m.%(b + b_vy) <- fmax (m.%(b + b_vy) -. 60.0 *. dt) (-30.0);
      move_body b dt false;
      if m.%(b + b_gnd) <> 0.0 then begin m.%(b + e_state) <- f bs_idle; m.%(b + e_st) <- 1.0; boss_land b 1.0 end
    end else if state = bs_idle then begin
      let d = sign dx in
      m.%(b + e_dir) <- (if d <> 0.0 then d else 1.0);
      m.%(b + b_vx) <- 0.0; m.%(b + b_vy) <- -2.0;
      move_body b dt false;
      if m.%(b + e_st) <= 0.0 then begin
        let r = rnd () in
        if r < 0.5 then begin
          m.%(b + e_state) <- f bs_jump; m.%(b + b_vy) <- 25.0; m.%(b + b_vx) <- clamp (dx /. 1.2) (-11.0) 11.0;
          ev ev_spring m.%(b + b_x) m.%(b + b_y) 1.0
        end else if r < 0.8 then begin
          m.%(b + e_state) <- f bs_shoot; m.%(b + e_st) <- 0.5; m.%(b + e_shots) <- (if low_hp then 5.0 else 3.0)
        end else begin
          m.%(b + e_state) <- f bs_charge; m.%(b + e_st) <- 1.3; m.%(b + e_dir) <- (if d <> 0.0 then d else 1.0)
        end
      end
    end else if state = bs_jump then begin
      m.%(b + b_vy) <- fmax (m.%(b + b_vy) -. 62.0 *. dt) (-30.0);
      move_body b dt false;
      if m.%(b + b_hx) <> 0.0 then m.%(b + b_vx) <- 0.0;
      if m.%(b + b_gnd) <> 0.0 then begin
        m.%(b + e_state) <- f bs_recover; m.%(b + e_st) <- (if low_hp then 0.8 else 1.2); m.%(b + b_vx) <- 0.0;
        boss_land b 0.0;
        shoot (m.%(b + b_x) -. 1.3) (m.%(b + b_y) +. 0.35) (-7.0) 0.0 0.0 4.0 0.7 1.0;
        shoot (m.%(b + b_x) +. 1.3) (m.%(b + b_y) +. 0.35) 7.0 0.0 0.0 4.0 0.7 1.0
      end
    end else if state = bs_recover then begin
      m.%(b + b_vx) <- 0.0; m.%(b + b_vy) <- -2.0;
      move_body b dt false;
      if m.%(b + e_st) <= 0.0 then begin m.%(b + e_state) <- f bs_idle; m.%(b + e_st) <- (if low_hp then 0.35 else 0.7) end
    end else if state = bs_shoot then begin
      m.%(b + b_vx) <- 0.0; m.%(b + b_vy) <- -2.0; move_body b dt false;
      let d = sign dx in
      m.%(b + e_dir) <- (if d <> 0.0 then d else 1.0);
      if m.%(b + e_st) <= 0.0 && m.%(b + e_shots) > 0.0 then begin
        let ux = ref (m.%(p_x) -. m.%(b + b_x)) and uy = ref (m.%(p_y) +. 0.4 -. (m.%(b + b_y) +. 1.4)) in
        let len = ref (sqrt (!ux *. !ux +. !uy *. !uy)) in
        if !len < 1e-6 then begin ux := 1.0; uy := 0.0; len := 1.0 end;
        ux := !ux /. !len; uy := !uy /. !len;
        let off = (rnd () -. 0.5) *. 0.35 in
        let vx = !ux -. !uy *. off and vy = !uy +. !ux *. off in
        let l2 = sqrt (vx *. vx +. vy *. vy) in
        let vx = vx /. l2 *. 7.5 and vy = vy /. l2 *. 7.5 in
        shoot (m.%(b + b_x) +. m.%(b + e_dir) *. 1.1) (m.%(b + b_y) +. 1.4) vx vy 0.0 5.0 0.6 1.0;
        ev ev_shoot (m.%(b + b_x) +. m.%(b + e_dir) *. 1.1) (m.%(b + b_y) +. 1.4) m.%(b + e_dir);
        m.%(b + e_shots) <- m.%(b + e_shots) -. 1.0; m.%(b + e_st) <- 0.3
      end else if m.%(b + e_shots) <= 0.0 && m.%(b + e_st) <= 0.0 then begin
        m.%(b + e_state) <- f bs_idle; m.%(b + e_st) <- 0.7
      end
    end else if state = bs_charge then begin
      m.%(b + b_vy) <- fmax (m.%(b + b_vy) -. 60.0 *. dt) (-30.0);
      if m.%(b + e_st) > 0.75 then m.%(b + b_vx) <- 0.0
      else m.%(b + b_vx) <- m.%(b + e_dir) *. (if low_hp then 15.0 else 12.0);
      move_body b dt false;
      if m.%(b + e_st) <= 0.75 && m.%(b + b_hx) <> 0.0 then begin
        m.%(b + e_state) <- f bs_recover; m.%(b + e_st) <- 1.3; m.%(b + b_vx) <- 0.0;
        boss_land b 2.0
      end else if m.%(b + e_st) <= 0.0 then begin
        m.%(b + e_state) <- f bs_recover; m.%(b + e_st) <- 0.8; m.%(b + b_vx) <- 0.0
      end
    end else if state = bs_dying then begin
      m.%(b + b_vx) <- 0.0;
      if rnd () < 0.5 then begin
        let rx = rndr (-1.2) 1.2 in
        let ry = rndr 0.0 2.4 in
        ev ev_bossexplode (m.%(b + b_x) +. rx) (m.%(b + b_y) +. ry) 0.0
      end;
      if m.%(b + e_st) <= 0.0 then begin
        m.%(b + e_alive) <- 0.0;
        m.%(g_score) <- m.%(g_score) +. 3000.0; m.%(g_bosskilled) <- 1.0; m.%(g_bossactive) <- 0.0;
        set_door false;
        add_goal m.%(b + b_x) 2.0;
        ev ev_bossdead m.%(b + b_x) m.%(b + b_y) 0.0
      end
    end
  end

let update_enemies (dt : float) : unit =
  let n = ii m.%(g_nen) in
  for i = 0 to n - 1 do
    let b = en_base i in
    if m.%(b + e_alive) <> 0.0 then begin
      m.%(b + e_t) <- m.%(b + e_t) +. dt;
      let kind = ii m.%(b + e_kind) in
      if kind = ek_slime then begin
        m.%(b + b_vy) <- fmax (m.%(b + b_vy) -. 55.0 *. dt) (-25.0);
        m.%(b + b_vx) <- m.%(b + e_dir) *. 1.9;
        move_body b dt false;
        if m.%(b + b_hx) <> 0.0 then m.%(b + e_dir) <- -.m.%(b + e_dir)
        else if m.%(b + b_gnd) <> 0.0 then begin
          let tx = fi (m.%(b + b_x) +. m.%(b + e_dir) *. (m.%(b + b_w) /. 2.0 +. 0.12)) and ty = fi (m.%(b + b_y) -. 0.1) in
          let t = tile_at tx ty in
          if (not (is_solid t)) && t <> 2.0 then m.%(b + e_dir) <- -.m.%(b + e_dir)
          else if tile_at tx (fi (m.%(b + b_y) +. 0.1)) = 3.0 then m.%(b + e_dir) <- -.m.%(b + e_dir)
        end
      end else if kind = ek_bat then begin
        let near = fabs (m.%(p_x) -. m.%(b + e_ox)) < 8.0 in
        if near && m.%(p_dead) = 0.0 then m.%(b + e_ox) <- m.%(b + e_ox) +. sign (m.%(p_x) -. m.%(b + e_ox)) *. 0.9 *. dt;
        let px = m.%(b + b_x) in
        m.%(b + b_x) <- m.%(b + e_ox) +. fsin (m.%(b + e_t) *. 1.3) *. 3.2;
        m.%(b + b_y) <- m.%(b + e_oy) +. fsin (m.%(b + e_t) *. 2.4) *. 0.6;
        let d = sign (m.%(b + b_x) -. px) in
        if d <> 0.0 then m.%(b + e_dir) <- d
      end else if kind = ek_saw then begin
        m.%(b + b_x) <- m.%(b + b_x) +. m.%(b + b_vx) *. dt;
        let ahead = fi (m.%(b + b_x) +. sign m.%(b + b_vx) *. 0.5) in
        let floor_ahead = tile_at ahead (fi (m.%(b + b_y) -. 0.2)) in
        if is_solid (tile_at ahead (fi (m.%(b + b_y) +. 0.4))) || ((not (is_solid floor_ahead)) && floor_ahead <> 2.0)
           || fabs (m.%(b + b_x) -. m.%(b + e_ox)) > 3.0 then begin
          m.%(b + b_vx) <- -.m.%(b + b_vx); m.%(b + b_x) <- m.%(b + b_x) +. m.%(b + b_vx) *. dt *. 2.0
        end
      end else if kind = ek_turret then begin
        let d = sign (m.%(p_x) -. m.%(b + b_x)) in
        m.%(b + e_dir) <- (if d <> 0.0 then d else 1.0);
        m.%(b + e_cd) <- m.%(b + e_cd) -. dt;
        let dx = fabs (m.%(p_x) -. m.%(b + b_x)) and dy = fabs (m.%(p_y) -. m.%(b + b_y)) in
        if m.%(b + e_cd) <= 0.0 && dx < fmin 13.0 (m.%(g_vieww) /. 2.0 +. 1.0) && dy < 7.0 && m.%(p_dead) = 0.0 then begin
          m.%(b + e_cd) <- 2.2;
          shoot (m.%(b + b_x) +. m.%(b + e_dir) *. 0.6) (m.%(b + b_y) +. 0.5) (m.%(b + e_dir) *. 6.5) 0.0 0.0 6.0 0.6 0.0;
          ev ev_shoot (m.%(b + b_x) +. m.%(b + e_dir) *. 0.7) (m.%(b + b_y) +. 0.5) m.%(b + e_dir)
        end
      end else if kind = ek_boss then update_boss b dt
    end
  done

(* ------------------------------------------------------ interactions *)
let update_interactions (dt : float) : unit =
  if m.%(p_dead) = 0.0 then begin
    let nen = ii m.%(g_nen) in
    for i = 0 to nen - 1 do
      let e = en_base i in
      if m.%(e + e_alive) <> 0.0 then begin
        let kind = ii m.%(e + e_kind) in
        if kind = ek_boss && (m.%(e + e_state) = f bs_sleep || m.%(e + e_state) = f bs_dying) then ()
        else if box_hit p_x e then begin
          if kind = ek_boss then begin
            if m.%(p_vy) < 0.0 && m.%(p_prevy) >= m.%(e + b_y) +. m.%(e + b_h) *. 0.55 && m.%(p_dasht) <= 0.0 then begin
              if damage_boss () then stomp_bounce true
              else m.%(p_vy) <- fmax m.%(p_vy) 8.0
            end else ignore (hurt_player m.%(e + b_x))
          end else if m.%(e + e_stomp) = 0.0 then ignore (hurt_player m.%(e + b_x))
          else if m.%(p_dasht) > 0.0 then kill_enemy e true
          else if m.%(p_vy) < 0.0 && m.%(p_prevy) >= m.%(e + b_y) +. m.%(e + b_h) *. 0.5 then begin
            kill_enemy e false; stomp_bounce false; m.%(g_freeze) <- 0.04
          end else ignore (hurt_player m.%(e + b_x))
        end
      end
    done;

    let nc = ii m.%(g_ncoins) in
    for i = 0 to nc - 1 do
      let c = coin_base + i * coin_n in
      if m.%(c + c_got) = 0.0 then begin
        let r = m.%(c + c_r) in
        if fabs (m.%(p_x) -. m.%(c + c_x)) < r +. pw /. 2.0 && fabs (m.%(p_y) +. 0.45 -. m.%(c + c_y)) < r +. 0.45 then begin
          let kind = ii m.%(c + c_kind) in
          if kind = sk_heart then begin
            if m.%(p_hp) >= m.%(p_maxhp) then begin m.%(g_score) <- m.%(g_score) +. 250.0; ev ev_heart m.%(c + c_x) m.%(c + c_y) 0.0 end
            else begin m.%(p_hp) <- m.%(p_hp) +. 1.0; ev ev_heart m.%(c + c_x) m.%(c + c_y) 1.0 end
          end else if kind = sk_gem then begin
            m.%(g_coins) <- m.%(g_coins) +. 5.0; m.%(g_score) <- m.%(g_score) +. 500.0; ev ev_gem m.%(c + c_x) m.%(c + c_y) 0.0
          end else begin
            m.%(g_coins) <- m.%(g_coins) +. 1.0; m.%(g_score) <- m.%(g_score) +. 100.0; ev ev_coin m.%(c + c_x) m.%(c + c_y) 0.0
          end;
          m.%(c + c_got) <- 1.0
        end
      end
    done;

    let ns = ii m.%(g_nspr) in
    for i = 0 to ns - 1 do
      let s = spr_base + i * spr_n in
      m.%(s + sp_t) <- fmax 0.0 (m.%(s + sp_t) -. dt);
      if fabs (m.%(p_x) -. m.%(s + sp_x)) < 0.7 && m.%(p_y) >= m.%(s + sp_y) -. 0.1 && m.%(p_y) < m.%(s + sp_y) +. 0.55 && m.%(p_vy) <= 0.5 then begin
        m.%(p_vy) <- 31.0; m.%(p_gnd) <- 0.0; m.%(p_jumps) <- 1.0; m.%(p_airdash) <- 0.0; m.%(p_dasht) <- 0.0; m.%(p_coyote) <- 0.0;
        m.%(s + sp_t) <- 0.3;
        ev ev_spring m.%(s + sp_x) (m.%(s + sp_y) +. 0.4) 0.0
      end
    done;

    let nk = ii m.%(g_nck) in
    for i = 0 to nk - 1 do
      let c = ckt_base + i * ckt_n in
      if m.%(c + ck_on) = 0.0 && fabs (m.%(p_x) -. m.%(c + ck_x)) < 1.3 && fabs (m.%(p_y) -. m.%(c + ck_y)) < 2.0 then begin
        for j = 0 to nk - 1 do m.%(ckt_base + j * ckt_n + ck_on) <- 0.0 done;
        m.%(c + ck_on) <- 1.0;
        m.%(g_chkx) <- m.%(c + ck_x); m.%(g_chky) <- m.%(c + ck_y);
        ev ev_check m.%(c + ck_x) m.%(c + ck_y) 0.0
      end
    done;

    let bi = ii m.%(g_boss) in
    if bi >= 0 then begin
      let b = en_base bi in
      if m.%(b + e_state) = f bs_sleep && m.%(b + e_alive) <> 0.0 && m.%(p_x) > m.%(g_btrig) then start_boss ()
    end;

    if m.%(g_goalon) <> 0.0 && fabs (m.%(p_x) -. m.%(g_goalx)) < 0.9 && fabs (m.%(p_y) +. 0.5 -. (m.%(g_goaly) +. 1.1)) < 1.5 then begin
      m.%(g_mode) <- f mode_clear;
      ev ev_complete m.%(p_x) m.%(p_y) 0.0
    end
  end

let step (dt : float) : unit =
  if m.%(g_freeze) > 0.0 then m.%(g_freeze) <- m.%(g_freeze) -. dt
  else if ii m.%(g_mode) <> mode_play then begin
    update_platforms dt; update_enemies dt; update_shots dt
  end else begin
    m.%(g_time) <- m.%(g_time) +. dt;
    update_platforms dt;
    if m.%(p_dead) = 0.0 then update_player dt
    else begin
      m.%(p_deadt) <- m.%(p_deadt) +. dt;
      if m.%(p_deadt) > 1.4 then respawn ()
    end;
    update_enemies dt;
    update_shots dt;
    update_interactions dt
  end

let advance (rdt : float) : int =
  m.%(g_evn) <- 0.0;
  let acc = ref (m.%(g_acc) +. rdt) in
  let n = ref 0 in
  while !acc >= dt_step && !n < 12 do
    step dt_step; acc := !acc -. dt_step; incr n
  done;
  if !n >= 12 then acc := 0.0;
  m.%(g_acc) <- !acc; m.%(g_steps) <- f !n;
  if !n > 0 then begin m.%(in_jumppress) <- 0.0; m.%(in_dashpress) <- 0.0 end;
  !n

let mem_get (i : int) : float = m.%(i)
let mem_set (i : int) (v : float) : unit = m.%(i) <- v
