module Core
(* Slop Runner simulation core - F* port.
   Mirrors core/js/core.js line by line.  All state lives in one flat float memory (Prim.mget/mset);
   this file is written in F*'s ML effect and extracted to OCaml (then wasm_of_ocaml). *)
open FStar.All
open Prim
open Abi

let k (s:string) : f64 = lit s
let ( +. ) (a b:f64) : f64 = fadd a b
let ( -. ) (a b:f64) : f64 = fsub a b
let ( *. ) (a b:f64) : f64 = fmul a b
let ( /. ) (a b:f64) : f64 = fdiv a b
let ( <. ) (a b:f64) : bool = flt a b
let ( <=. ) (a b:f64) : bool = fle a b
let ( >. ) (a b:f64) : bool = fgt a b
let ( >=. ) (a b:f64) : bool = fge a b

let level_h : int = 14
let dt_step : f64 = k "1.0" /. k "120.0"
let pw : f64 = k "0.62"
let ph : f64 = k "0.86"
let max_speed : f64 = k "8.2"
let accel_g : f64 = k "95.0"
let accel_a : f64 = k "62.0"
let decel_g : f64 = k "110.0"
let decel_a : f64 = k "26.0"
let gravity : f64 = k "62.0"
let fall_gravity : f64 = k "74.0"
let max_fall : f64 = k "27.0"
let jump_v : f64 = k "19.6"
let double_v : f64 = k "17.2"
let coyote_t : f64 = k "0.11"
let buffer_t : f64 = k "0.13"
let wall_slide : f64 = k "3.4"
let wall_jump_x : f64 = k "9.5"
let wall_jump_y : f64 = k "18.2"
let wall_lock : f64 = k "0.16"
let dash_time : f64 = k "0.17"
let dash_speed : f64 = k "24.0"
let dash_cd : f64 = k "0.75"
let invuln : f64 = k "1.5"
let stomp_bounce_v : f64 = k "15.5"

let zero : f64 = k "0.0"
let one : f64 = k "1.0"
let two : f64 = k "2.0"
let half : f64 = k "0.5"
let m1 : f64 = fneg (k "1.0")

let rd (i:int) : ML f64 = mget i
let wr (i:int) (v:f64) : ML unit = mset i v
let add_to (i:int) (v:f64) : ML unit = wr i (rd i +. v)
let f (i:int) : f64 = i2f i
let fi (x:f64) : int = floor_int x
let ii (x:f64) : int = f2i x
let b2f (b:bool) : f64 = if b then one else zero
let is0 (x:f64) : bool = feq x zero
let nz (x:f64) : bool = not (feq x zero)

let sign (v:f64) : f64 = if v >. zero then one else if v <. zero then m1 else zero
let fmin_ (a b:f64) : f64 = if a <. b then a else b
let fmax_ (a b:f64) : f64 = if a >. b then a else b
let fabs_ (a:f64) : f64 = if a <. zero then fneg a else a
let clamp (v a b:f64) : f64 = if v <. a then a else if v >. b then b else v
let approach (v t s:f64) : f64 = if v <. t then fmin_ (v +. s) t else fmax_ (v -. s) t

let fsin (x:f64) : f64 =
  let kk = ffloor (x *. k "0.15915494309189535" +. half) in
  let r0 = x -. kk *. k "6.283185307179586" in
  let r1 =
    if r0 >. k "1.5707963267948966" then k "3.141592653589793" -. r0
    else if r0 <. fneg (k "1.5707963267948966") then fneg (k "3.141592653589793") -. r0
    else r0 in
  let r2 = r1 *. r1 in
  r1 *. (one +. r2 *. (fneg (k "0.16666666666666666") +. r2 *. (k "0.008333333333333333" +. r2 *. (fneg (k "0.0001984126984126984") +.
        r2 *. (k "2.7557319223985893e-6" +. r2 *. (fneg (k "2.505210838544172e-8") +. r2 *. k "1.6059043836821613e-10"))))))
let fcos (x:f64) : f64 = fsin (x +. k "1.5707963267948966")

let rnd () : ML f64 =
  let s0 = rd g_rng *. k "1664525.0" +. k "1013904223.0" in
  let s = s0 -. ffloor (s0 /. k "4294967296.0") *. k "4294967296.0" in
  wr g_rng s;
  s /. k "4294967296.0"

let rndr (a b:f64) : ML f64 =
  let x = rnd () in
  a +. x *. (b -. a)

let ev (t:int) (x y a:f64) : ML unit =
  let n = ii (rd g_evn) in
  if n < ev_max then begin
    let i = ev_base + n * ev_n in
    wr i (f t); wr (i + 1) x; wr (i + 2) y; wr (i + 3) a;
    wr g_evn (f (n + 1))
  end

let tile_at (tx ty:int) : ML f64 =
  let lw = ii (rd g_lw) in
  if tx < 0 || tx >= lw then one
  else if ty < 0 || ty >= level_h then zero
  else rd (tile_base + ty * lw + tx)

let is_solid (t:f64) : bool = feq t one || feq t (k "4.0")

let set_tile (tx ty:int) (v:f64) : ML unit =
  let lw = ii (rd g_lw) in
  wr (tile_base + ty * lw + tx) v

(* loops: run [body i] for i in [a..b]; forStop stops as soon as body returns true *)
let rec for_stop (a b:int) (body:int -> ML bool) : ML bool =
  if a > b then false
  else if body a then true
  else for_stop (a + 1) b body

let rec for_n (i n:int) (body:int -> ML unit) : ML unit =
  if i >= n then () else begin body i; for_n (i + 1) n body end

let rec for_down (i:int) (body:int -> ML unit) : ML unit =
  if i < 0 then () else begin body i; for_down (i - 1) body end

let move_body (b:int) (dt:f64) (drop:bool) : ML unit =
  wr (b + b_hx) zero; wr (b + b_hy) zero;
  let hw = rd (b + b_w) /. two in
  add_to (b + b_x) (rd (b + b_vx) *. dt);
  let y0 = fi (rd (b + b_y) +. k "0.02") in
  let y1 = fi (rd (b + b_y) +. rd (b + b_h) -. k "0.02") in
  let vx = rd (b + b_vx) in
  if vx >. zero then begin
    let tx = fi (rd (b + b_x) +. hw) in
    let _ = for_stop y0 y1 (fun ty ->
      if is_solid (tile_at tx ty) then begin
        wr (b + b_x) (f tx -. hw -. k "1e-4"); wr (b + b_vx) zero; wr (b + b_hx) one; true
      end else false) in ()
  end else if vx <. zero then begin
    let tx = fi (rd (b + b_x) -. hw) in
    let _ = for_stop y0 y1 (fun ty ->
      if is_solid (tile_at tx ty) then begin
        wr (b + b_x) (f tx +. one +. hw +. k "1e-4"); wr (b + b_vx) zero; wr (b + b_hx) m1; true
      end else false) in ()
  end;
  let prev_y = rd (b + b_y) in
  add_to (b + b_y) (rd (b + b_vy) *. dt);
  let x0 = fi (rd (b + b_x) -. hw +. k "1e-3") in
  let x1 = fi (rd (b + b_x) +. hw -. k "1e-3") in
  wr (b + b_gnd) zero;
  if rd (b + b_vy) <=. zero then begin
    let ty = fi (rd (b + b_y)) in
    let _ = for_stop x0 x1 (fun tx ->
      let t = tile_at tx ty in
      if is_solid t || (feq t two && not drop && prev_y >=. f ty +. one -. k "0.02") then begin
        wr (b + b_y) (f ty +. one); wr (b + b_vy) zero; wr (b + b_gnd) one; wr (b + b_hy) m1; true
      end else false) in ()
  end else begin
    let ty = fi (rd (b + b_y) +. rd (b + b_h)) in
    let _ = for_stop x0 x1 (fun tx ->
      if is_solid (tile_at tx ty) then begin
        wr (b + b_y) (f ty -. rd (b + b_h) -. k "1e-4"); wr (b + b_vy) zero; wr (b + b_hy) one; true
      end else false) in ()
  end

let box_hit (a b:int) : ML bool =
  fabs_ (rd (a + b_x) -. rd (b + b_x)) <. (rd (a + b_w) +. rd (b + b_w)) /. two
  && rd (a + b_y) <. rd (b + b_y) +. rd (b + b_h)
  && rd (a + b_y) +. rd (a + b_h) >. rd (b + b_y)

let en_base_ (i:int) : int = en_base + i * en_n
let pl_base_ (i:int) : int = plt_base + i * plt_n
let sh_base_ (i:int) : int = sh_base + i * sh_n

let set_door (closed:bool) : ML unit =
  if nz (rd g_dooron) then begin
    let y0 = ii (rd g_doory0) in
    let y1 = ii (rd g_doory1) in
    let dx = ii (rd g_doorx) in
    let rec go (ty:int) : ML unit =
      if ty > y1 then () else begin set_tile dx ty (if closed then k "4.0" else zero); go (ty + 1) end in
    go y0;
    wr g_doorclosed (b2f closed);
    ev ev_door (rd g_doorx) zero (b2f closed)
  end

let add_enemy (kind:int) (x y:f64) : ML unit =
  let i = ii (rd g_nen) in
  if i < en_max then begin
    wr g_nen (f (i + 1));
    let b = en_base_ i in
    for_n 0 en_n (fun kk -> wr (b + kk) zero);
    wr (b + b_x) x; wr (b + b_y) y; wr (b + b_w) (k "0.8"); wr (b + b_h) (k "0.62");
    wr (b + e_kind) (f kind); wr (b + e_alive) one; wr (b + e_stomp) one;
    let r0 = rnd () in
    wr (b + e_dir) (if r0 <. half then m1 else one);
    let r1 = rnd () in
    wr (b + e_t) (r1 *. k "10.0");
    wr (b + e_ox) x; wr (b + e_oy) y;
    let r2 = rnd () in
    wr (b + e_cd) (one +. r2);
    if kind = ek_bat then begin wr (b + b_w) (k "0.75"); wr (b + b_h) half end
    else if kind = ek_saw then begin
      wr (b + b_w) (k "0.78"); wr (b + b_h) (k "0.78"); wr (b + e_stomp) zero; wr (b + e_dir) one; wr (b + b_vx) (k "3.0")
    end else if kind = ek_turret then begin
      wr (b + b_w) (k "0.9"); wr (b + b_h) (k "0.8"); wr (b + e_stomp) zero
    end else if kind = ek_boss then begin
      wr (b + b_w) (k "2.5"); wr (b + b_h) (k "2.3"); wr (b + e_hp) (k "8.0"); wr (b + e_hp0) (k "8.0");
      wr (b + e_state) (f bs_sleep); wr (b + e_dir) m1;
      wr g_boss (f i)
    end
  end

let add_platform (kind:int) (x ty:f64) : ML unit =
  let i = ii (rd g_nplt) in
  if i < plt_max then begin
    wr g_nplt (f (i + 1));
    let p = pl_base_ i in
    for_n 0 plt_n (fun kk -> wr (p + kk) zero);
    wr (p + pt_kind) (f kind); wr (p + pt_x) x; wr (p + pt_x0) x;
    wr (p + pt_top) (ty +. k "0.75"); wr (p + pt_top0) (ty +. k "0.75"); wr (p + pt_prevtop) (ty +. k "0.75");
    wr (p + pt_w) (if kind = 2 then one else k "3.0"); wr (p + pt_solid) one;
    let rr = rnd () in
    wr (p + pt_t) (rr *. k "6.0"); wr (p + pt_state) (f cs_idle)
  end

let add_goal (x y:f64) : ML unit = wr g_goalon one; wr g_goalx x; wr g_goaly y

let reset_player (x y:f64) : ML unit =
  wr p_x x; wr p_y y; wr p_vx zero; wr p_vy zero; wr p_face one; wr p_gnd zero;
  wr p_coyote zero; wr p_buffer zero; wr p_jumps one; wr p_walldir zero; wr p_wallgrace zero; wr p_walllock zero;
  wr p_dasht zero; wr p_dashcd zero; wr p_airdash zero; wr p_inv zero; wr p_dropt zero; wr p_dead zero; wr p_deadt zero;
  wr p_riding m1; wr p_safex x; wr p_safey y; wr p_safet zero; wr p_prevy y; wr p_wasgnd zero

let init () : ML unit =
  for_n 0 mem_size (fun i -> wr i zero);
  wr g_rng (k "12345.0"); wr g_vieww (k "20.0");
  wr p_w pw; wr p_h ph; wr p_maxhp (k "3.0"); wr p_hp (k "3.0"); wr p_riding m1; wr g_boss m1

let load_level () : ML unit =
  wr g_ncoins zero; wr g_nen zero; wr g_nplt zero; wr g_nsh zero; wr g_nspr zero; wr g_nck zero; wr g_boss m1;
  wr g_time zero; wr g_coins zero; wr g_kills zero; wr g_deaths zero; wr g_score zero; wr g_bosskilled zero;
  wr g_bossactive zero; wr g_freeze zero; wr g_coinstotal zero; wr g_doorclosed zero; wr g_acc zero; wr g_evn zero;
  let n = ii (rd g_nspec) in
  for_n 0 n (fun s ->
    let kind = ii (rd (spec_base + s * spec_n)) in
    let x = rd (spec_base + s * spec_n + 1) in
    let y = rd (spec_base + s * spec_n + 2) in
    if kind <= sk_heart then begin
      let i = ii (rd g_ncoins) in
      if i < coin_max then begin
        wr g_ncoins (f (i + 1));
        let c = coin_base + i * coin_n in
        wr (c + c_kind) (f kind); wr (c + c_x) x; wr (c + c_y) (y +. half); wr (c + c_got) zero;
        wr (c + c_r) (if kind = sk_coin then k "0.55" else k "0.65");
        if kind = sk_coin then add_to g_coinstotal one
        else if kind = sk_gem then add_to g_coinstotal (k "5.0")
      end
    end
    else if kind = sk_slime then add_enemy ek_slime x y
    else if kind = sk_bat then add_enemy ek_bat x (y +. half)
    else if kind = sk_saw then add_enemy ek_saw x (y +. k "0.02")
    else if kind = sk_turret then add_enemy ek_turret x y
    else if kind = sk_spring then begin
      let i = ii (rd g_nspr) in
      if i < spr_max then begin
        wr g_nspr (f (i + 1));
        let q = spr_base + i * spr_n in
        wr (q + sp_x) x; wr (q + sp_y) y; wr (q + sp_t) zero
      end
    end
    else if kind = sk_plat_h then add_platform 0 x y
    else if kind = sk_plat_v then add_platform 1 x y
    else if kind = sk_crumble then add_platform 2 x y
    else if kind = sk_check then begin
      let i = ii (rd g_nck) in
      if i < ckt_max then begin
        wr g_nck (f (i + 1));
        let q = ckt_base + i * ckt_n in
        wr (q + ck_x) x; wr (q + ck_y) y; wr (q + ck_on) zero
      end
    end
    else if kind = sk_boss then add_enemy ek_boss x y
    else ());
  wr p_maxhp (k "3.0"); wr p_hp (k "3.0");
  wr g_chkx (rd g_spawnx); wr g_chky (rd g_spawny);
  reset_player (rd g_spawnx) (rd g_spawny)

(* ------------------------------------------------------------- player *)
let kill_player () : ML unit =
  wr p_dead one; wr p_deadt zero; wr p_hp zero;
  add_to g_deaths one;
  ev ev_die (rd p_x) (rd p_y +. half) zero

let hurt_player (src_x:f64) : ML bool =
  if rd p_inv >. zero || rd p_dasht >. zero || nz (rd p_dead) then false
  else begin
    add_to p_hp m1;
    wr p_inv invuln;
    wr g_freeze (k "0.08");
    ev ev_hurt (rd p_x) (rd p_y +. half) zero;
    wr p_vx ((if rd p_x <. src_x then m1 else one) *. k "8.5"); wr p_vy (k "12.0");
    wr p_walllock (k "0.18"); wr p_dasht zero;
    if rd p_hp <=. zero then kill_player ();
    true
  end

let reset_boss () : ML unit =
  let b = en_base_ (ii (rd g_boss)) in
  wr g_bossactive zero;
  wr (b + e_state) (f bs_sleep); wr (b + e_hp) (rd (b + e_hp0)); wr (b + e_alive) one; wr (b + e_inv) zero;
  wr (b + b_x) (rd g_bspecx); wr (b + b_y) (rd g_bspecy);
  set_door false;
  wr g_nsh zero

let pit_fall () : ML unit =
  if is0 (rd p_dead) then begin
    add_to p_hp m1;
    ev ev_pit (rd p_x) (rd p_y) zero;
    if rd p_hp <=. zero then kill_player ()
    else begin
      wr p_x (rd p_safex); wr p_y (rd p_safey +. k "0.05"); wr p_vx zero; wr p_vy zero; wr p_inv two;
      ev ev_respawn (rd p_x) (rd p_y +. k "0.4") one
    end
  end

let respawn () : ML unit =
  reset_player (rd g_chkx) (rd g_chky);
  wr p_hp (rd p_maxhp); wr p_inv two;
  if rd g_boss >=. zero && nz (rd g_bossactive) then reset_boss ();
  ev ev_respawn (rd p_x) (rd p_y +. half) zero

let stomp_bounce (strong:bool) : ML unit =
  wr p_vy (if nz (rd in_jumpheld) then stomp_bounce_v +. k "3.5" else stomp_bounce_v);
  if strong then add_to p_vy two;
  wr p_jumps one; wr p_airdash zero; wr p_gnd zero; wr p_dasht zero

let update_player (dt:f64) : ML unit =
  let ax = rd in_ax in
  wr p_prevy (rd p_y);
  wr p_inv (fmax_ zero (rd p_inv -. dt));
  add_to p_dashcd (fneg dt); add_to p_walllock (fneg dt); add_to p_buffer (fneg dt); add_to p_dropt (fneg dt); add_to p_wallgrace (fneg dt);

  if nz (rd in_jumppress) then begin wr in_jumppress zero; wr p_buffer buffer_t end;
  let want_dash = nz (rd in_dashpress) in
  if want_dash then wr in_dashpress zero;

  let rid = ii (rd p_riding) in
  if rid >= 0 then begin
    let p = pl_base_ rid in
    if nz (rd (p + pt_solid)) && rd p_vy <=. zero then begin
      add_to p_x (rd (p + pt_dx)); wr p_y (rd (p + pt_top))
    end
  end;

  if nz (rd p_gnd) then begin wr p_coyote coyote_t; wr p_jumps one; wr p_airdash zero end
  else add_to p_coyote (fneg dt);

  if want_dash && rd p_dashcd <=. zero && is0 (rd p_airdash) then begin
    wr p_dasht dash_time; wr p_dashcd dash_cd;
    wr p_dashdir (if nz ax then ax else rd p_face); wr p_face (rd p_dashdir);
    if is0 (rd p_gnd) then wr p_airdash one;
    ev ev_dash (rd p_x) (rd p_y +. k "0.4") (rd p_dashdir)
  end;

  if rd p_dasht >. zero then begin
    add_to p_dasht (fneg dt);
    wr p_vx (rd p_dashdir *. dash_speed);
    wr p_vy zero;
    if rd p_dasht <=. zero then wr p_vx (rd p_dashdir *. max_speed)
  end else begin
    if rd p_walllock <=. zero then begin
      let target = ax *. max_speed in
      let acc =
        if is0 ax then (if nz (rd p_gnd) then decel_g else decel_a)
        else begin
          let a0 = if nz (rd p_gnd) then accel_g else accel_a in
          if feq (sign (rd p_vx)) (fneg ax) then a0 *. k "1.6" else a0
        end in
      wr p_vx (approach (rd p_vx) target (acc *. dt));
      if nz ax then wr p_face ax
    end;

    wr p_walldir zero;
    if is0 (rd p_gnd) && nz ax then begin
      let tx = fi (rd p_x +. ax *. (pw /. two +. k "0.08")) in
      if is_solid (tile_at tx (fi (rd p_y +. k "0.3"))) || is_solid (tile_at tx (fi (rd p_y +. k "0.75"))) then begin
        wr p_walldir ax; wr p_wallgrace (k "0.1"); wr p_wallmem ax
      end
    end;

    if rd p_buffer >. zero then begin
      if nz (rd in_downheld) && nz (rd p_gnd) && feq (tile_at (fi (rd p_x)) (fi (rd p_y -. k "0.05"))) two then begin
        wr p_dropt (k "0.22"); wr p_buffer zero; wr p_y (rd p_y -. k "0.06"); wr p_gnd zero
      end else if rd p_coyote >. zero then begin
        wr p_vy jump_v; wr p_buffer zero; wr p_coyote zero; wr p_gnd zero;
        ev ev_jump (rd p_x) (rd p_y +. k "0.05") zero
      end else if rd p_wallgrace >. zero && nz (rd p_wallmem) then begin
        wr p_vx (fneg (rd p_wallmem) *. wall_jump_x); wr p_vy wall_jump_y;
        wr p_walllock wall_lock; wr p_face (fneg (rd p_wallmem)); wr p_buffer zero; wr p_wallgrace zero;
        wr p_jumps one; wr p_airdash zero;
        ev ev_walljump (rd p_x +. rd p_wallmem *. k "0.3") (rd p_y +. half) (rd p_wallmem)
      end else if rd p_jumps >. zero then begin
        wr p_vy double_v; add_to p_jumps m1; wr p_buffer zero;
        ev ev_double (rd p_x) (rd p_y +. k "0.05") zero
      end
    end;

    let g = if rd p_vy >. zero then (if nz (rd in_jumpheld) then gravity else gravity *. k "2.4") else fall_gravity in
    wr p_vy (fmax_ (rd p_vy -. g *. dt) (fneg max_fall));
    if nz (rd p_walldir) && rd p_vy <. fneg wall_slide then wr p_vy (fneg wall_slide)
  end;

  let vy_pre = rd p_vy in
  move_body p_x dt (rd p_dropt >. zero);

  wr p_riding m1;
  if vy_pre <=. zero && rd p_dasht <=. zero then begin
    let np = ii (rd g_nplt) in
    let _ = for_stop 0 (np - 1) (fun i ->
      let p = pl_base_ i in
      if is0 (rd (p + pt_solid)) then false
      else if fabs_ (rd p_x -. rd (p + pt_x)) <. rd (p + pt_w) /. two +. pw /. two -. k "0.08"
              && rd p_prevy >=. rd (p + pt_prevtop) -. k "0.12"
              && rd p_y <=. rd (p + pt_top) +. k "0.02" && rd p_y >=. rd (p + pt_top) -. k "0.6" then begin
        wr p_y (rd (p + pt_top)); wr p_vy zero; wr p_gnd one; wr p_riding (f i);
        if feq (rd (p + pt_kind)) two && feq (rd (p + pt_state)) (f cs_idle) then begin
          wr (p + pt_state) (f cs_shake); wr (p + pt_timer) (k "0.45")
        end;
        true
      end else false) in ()
  end;

  if nz (rd p_gnd) && is0 (rd p_wasgnd) && vy_pre <. fneg (k "9.0") then ev ev_land (rd p_x) (rd p_y +. k "0.05") zero;
  if feq (rd p_hy) one && is0 (rd p_vy) then wr p_vy m1;
  wr p_wasgnd (rd p_gnd);

  if nz (rd p_gnd) && rd p_riding <. zero then begin
    let ty = fi (rd p_y) - 1 in
    if is_solid (tile_at (fi (rd p_x -. k "0.6")) ty) && is_solid (tile_at (fi (rd p_x +. k "0.6")) ty)
       && is_solid (tile_at (fi (rd p_x -. k "1.6")) ty) && is_solid (tile_at (fi (rd p_x +. k "1.6")) ty)
       && not (feq (tile_at (fi (rd p_x)) (fi (rd p_y))) (k "3.0")) then begin
      add_to p_safet dt;
      if rd p_safet >. k "0.35" then begin wr p_safex (rd p_x); wr p_safey (rd p_y) end
    end else wr p_safet zero
  end else wr p_safet zero;

  if rd p_inv <=. zero && rd p_dasht <=. zero then begin
    let x0 = fi (rd p_x -. pw /. two) in
    let x1 = fi (rd p_x +. pw /. two) in
    let y0 = fi (rd p_y) in
    let y1 = fi (rd p_y +. ph *. half) in
    let _ = for_stop y0 y1 (fun ty ->
      for_stop x0 x1 (fun tx ->
        if feq (tile_at tx ty) (k "3.0") then begin
          if rd p_x +. pw /. two >. f tx +. k "0.15" && rd p_x -. pw /. two <. f tx +. k "0.85" && rd p_y <. f ty +. half then begin
            if hurt_player (f tx +. half) then begin
              wr p_vy (k "15.0");
              wr p_vx ((if rd p_x <. f tx +. half then m1 else one) *. k "5.0")
            end;
            true
          end else false
        end else false)) in ()
  end;

  if rd p_y <. fneg (k "2.5") then pit_fall ()

(* ---------------------------------------------------------- platforms *)
let update_platforms (dt:f64) : ML unit =
  let n = ii (rd g_nplt) in
  for_n 0 n (fun i ->
    let p = pl_base_ i in
    wr (p + pt_prevtop) (rd (p + pt_top)); wr (p + pt_dx) zero; wr (p + pt_dy) zero;
    add_to (p + pt_t) dt;
    let kind = ii (rd (p + pt_kind)) in
    if kind = 0 then begin
      let nx = rd (p + pt_x0) +. fsin (rd (p + pt_t) *. k "1.05") *. k "2.4" in
      wr (p + pt_dx) (nx -. rd (p + pt_x)); wr (p + pt_x) nx
    end else if kind = 1 then begin
      let nt = rd (p + pt_top0) +. (one -. fcos (rd (p + pt_t) *. k "0.95")) /. two *. k "5.0" in
      wr (p + pt_dy) (nt -. rd (p + pt_top)); wr (p + pt_top) nt
    end else begin
      let st = ii (rd (p + pt_state)) in
      if st = cs_shake then begin
        add_to (p + pt_timer) (fneg dt);
        if rd (p + pt_timer) <=. zero then begin
          wr (p + pt_state) (f cs_fall); wr (p + pt_solid) zero; wr (p + pt_vy) zero;
          ev ev_crumble (rd (p + pt_x)) (rd (p + pt_top)) zero
        end
      end else if st = cs_fall then begin
        add_to (p + pt_vy) (fneg (k "32.0" *. dt)); add_to (p + pt_top) (rd (p + pt_vy) *. dt);
        if rd (p + pt_top) <. fneg (k "3.0") then begin wr (p + pt_state) (f cs_gone); wr (p + pt_timer) (k "2.6") end
      end else if st = cs_gone then begin
        add_to (p + pt_timer) (fneg dt);
        if rd (p + pt_timer) <=. zero then begin
          wr (p + pt_state) (f cs_idle); wr (p + pt_solid) one; wr (p + pt_top) (rd (p + pt_top0)); wr (p + pt_prevtop) (rd (p + pt_top))
        end
      end
    end)

(* -------------------------------------------------------------- shots *)
let shoot (x y vx vy g life size purple:f64) : ML unit =
  let i = ii (rd g_nsh) in
  if i < sh_max then begin
    wr g_nsh (f (i + 1));
    let s = sh_base_ i in
    wr (s + s_x) x; wr (s + s_y) y; wr (s + s_vx) vx; wr (s + s_vy) vy; wr (s + s_g) g; wr (s + s_life) life;
    wr (s + s_r) (size *. k "0.36"); wr (s + s_purple) purple; wr (s + s_size) size
  end

let remove_shot (i:int) : ML unit =
  let last = ii (rd g_nsh) - 1 in
  if i <> last then begin
    let a = sh_base_ i in
    let b = sh_base_ last in
    for_n 0 sh_n (fun kk -> wr (a + kk) (rd (b + kk)))
  end;
  wr g_nsh (f last)

let update_shots (dt:f64) : ML unit =
  for_down (ii (rd g_nsh) - 1) (fun i ->
    let s = sh_base_ i in
    add_to (s + s_life) (fneg dt);
    add_to (s + s_vy) (fneg (rd (s + s_g) *. dt));
    add_to (s + s_x) (rd (s + s_vx) *. dt); add_to (s + s_y) (rd (s + s_vy) *. dt);
    let dead0 = rd (s + s_life) <=. zero || rd (s + s_y) <. fneg (k "3.0") in
    let dead1 =
      if not dead0 && is_solid (tile_at (fi (rd (s + s_x))) (fi (rd (s + s_y)))) then begin
        ev ev_shothit (rd (s + s_x)) (rd (s + s_y)) (rd (s + s_purple)); true
      end else dead0 in
    let dead2 =
      if not dead1 && is0 (rd p_dead) && fabs_ (rd p_x -. rd (s + s_x)) <. pw /. two +. rd (s + s_r)
         && rd (s + s_y) >. rd p_y -. rd (s + s_r) && rd (s + s_y) <. rd p_y +. ph +. rd (s + s_r) then begin
        if hurt_player (rd (s + s_x)) then true else dead1
      end else dead1 in
    if dead2 then remove_shot i)

(* ------------------------------------------------------------ enemies *)
let kill_enemy (b:int) (by_dash:bool) : ML unit =
  wr (b + e_alive) zero;
  add_to g_kills one; add_to g_score (k "150.0");
  ev ev_kill (rd (b + b_x)) (rd (b + b_y) +. rd (b + b_h) /. two) (rd (b + e_kind) +. (if by_dash then k "10.0" else zero))

let start_boss () : ML unit =
  let b = en_base_ (ii (rd g_boss)) in
  wr g_bossactive one;
  set_door true;
  wr (b + b_x) (rd g_bspecx); wr (b + b_y) (f (level_h + 1)); wr (b + b_vx) zero; wr (b + b_vy) zero;
  wr (b + e_state) (f bs_intro); wr (b + e_hp) (rd (b + e_hp0)); wr (b + e_inv) zero; wr (b + e_alive) one;
  ev ev_bossstart (rd (b + b_x)) (rd (b + b_y)) zero

let damage_boss () : ML bool =
  let b = en_base_ (ii (rd g_boss)) in
  let st = ii (rd (b + e_state)) in
  if rd (b + e_inv) >. zero || st = bs_intro || st = bs_dying then false
  else begin
    add_to (b + e_hp) m1; wr (b + e_inv) (k "1.1");
    wr g_freeze (k "0.1");
    ev ev_bosshit (rd (b + b_x)) (rd (b + b_y)) (rd (b + e_hp));
    if rd (b + e_hp) <=. zero then begin
      wr (b + e_state) (f bs_dying); wr (b + e_st) (k "1.8"); wr (b + b_vx) zero;
      wr g_nsh zero
    end;
    true
  end

let boss_land (b:int) (variant:f64) : ML unit = ev ev_boom (rd (b + b_x)) (rd (b + b_y)) variant

let update_boss (b:int) (dt:f64) : ML unit =
  let state = ii (rd (b + e_state)) in
  if state <> bs_sleep then begin
    wr (b + e_inv) (fmax_ zero (rd (b + e_inv) -. dt));
    add_to (b + e_st) (fneg dt);
    let dx = rd p_x -. rd (b + b_x) in
    let low_hp = rd (b + e_hp) <=. k "3.0" in
    if state = bs_intro then begin
      wr (b + b_vy) (fmax_ (rd (b + b_vy) -. k "60.0" *. dt) (fneg (k "30.0")));
      move_body b dt false;
      if nz (rd (b + b_gnd)) then begin wr (b + e_state) (f bs_idle); wr (b + e_st) one; boss_land b one end
    end else if state = bs_idle then begin
      let d = sign dx in
      wr (b + e_dir) (if nz d then d else one);
      wr (b + b_vx) zero; wr (b + b_vy) (fneg two);
      move_body b dt false;
      if rd (b + e_st) <=. zero then begin
        let rr = rnd () in
        if rr <. half then begin
          wr (b + e_state) (f bs_jump); wr (b + b_vy) (k "25.0"); wr (b + b_vx) (clamp (dx /. k "1.2") (fneg (k "11.0")) (k "11.0"));
          ev ev_spring (rd (b + b_x)) (rd (b + b_y)) one
        end else if rr <. k "0.8" then begin
          wr (b + e_state) (f bs_shoot); wr (b + e_st) half; wr (b + e_shots) (if low_hp then k "5.0" else k "3.0")
        end else begin
          wr (b + e_state) (f bs_charge); wr (b + e_st) (k "1.3"); wr (b + e_dir) (if nz d then d else one)
        end
      end
    end else if state = bs_jump then begin
      wr (b + b_vy) (fmax_ (rd (b + b_vy) -. k "62.0" *. dt) (fneg (k "30.0")));
      move_body b dt false;
      if nz (rd (b + b_hx)) then wr (b + b_vx) zero;
      if nz (rd (b + b_gnd)) then begin
        wr (b + e_state) (f bs_recover); wr (b + e_st) (if low_hp then k "0.8" else k "1.2"); wr (b + b_vx) zero;
        boss_land b zero;
        shoot (rd (b + b_x) -. k "1.3") (rd (b + b_y) +. k "0.35") (fneg (k "7.0")) zero zero (k "4.0") (k "0.7") one;
        shoot (rd (b + b_x) +. k "1.3") (rd (b + b_y) +. k "0.35") (k "7.0") zero zero (k "4.0") (k "0.7") one
      end
    end else if state = bs_recover then begin
      wr (b + b_vx) zero; wr (b + b_vy) (fneg two);
      move_body b dt false;
      if rd (b + e_st) <=. zero then begin wr (b + e_state) (f bs_idle); wr (b + e_st) (if low_hp then k "0.35" else k "0.7") end
    end else if state = bs_shoot then begin
      wr (b + b_vx) zero; wr (b + b_vy) (fneg two); move_body b dt false;
      let d = sign dx in
      wr (b + e_dir) (if nz d then d else one);
      if rd (b + e_st) <=. zero && rd (b + e_shots) >. zero then begin
        let ux0 = rd p_x -. rd (b + b_x) in
        let uy0 = rd p_y +. k "0.4" -. (rd (b + b_y) +. k "1.4") in
        let len0 = fsqrt (ux0 *. ux0 +. uy0 *. uy0) in
        let small = len0 <. k "1e-6" in
        let ux1 = if small then one else ux0 in
        let uy1 = if small then zero else uy0 in
        let len1 = if small then one else len0 in
        let ux = ux1 /. len1 in
        let uy = uy1 /. len1 in
        let rr = rnd () in
        let off = (rr -. half) *. k "0.35" in
        let vx0 = ux -. uy *. off in
        let vy0 = uy +. ux *. off in
        let l2 = fsqrt (vx0 *. vx0 +. vy0 *. vy0) in
        let vx = vx0 /. l2 *. k "7.5" in
        let vy = vy0 /. l2 *. k "7.5" in
        shoot (rd (b + b_x) +. rd (b + e_dir) *. k "1.1") (rd (b + b_y) +. k "1.4") vx vy zero (k "5.0") (k "0.6") one;
        ev ev_shoot (rd (b + b_x) +. rd (b + e_dir) *. k "1.1") (rd (b + b_y) +. k "1.4") (rd (b + e_dir));
        add_to (b + e_shots) m1; wr (b + e_st) (k "0.3")
      end else if rd (b + e_shots) <=. zero && rd (b + e_st) <=. zero then begin
        wr (b + e_state) (f bs_idle); wr (b + e_st) (k "0.7")
      end
    end else if state = bs_charge then begin
      wr (b + b_vy) (fmax_ (rd (b + b_vy) -. k "60.0" *. dt) (fneg (k "30.0")));
      if rd (b + e_st) >. k "0.75" then wr (b + b_vx) zero
      else wr (b + b_vx) (rd (b + e_dir) *. (if low_hp then k "15.0" else k "12.0"));
      move_body b dt false;
      if rd (b + e_st) <=. k "0.75" && nz (rd (b + b_hx)) then begin
        wr (b + e_state) (f bs_recover); wr (b + e_st) (k "1.3"); wr (b + b_vx) zero;
        boss_land b two
      end else if rd (b + e_st) <=. zero then begin
        wr (b + e_state) (f bs_recover); wr (b + e_st) (k "0.8"); wr (b + b_vx) zero
      end
    end else if state = bs_dying then begin
      wr (b + b_vx) zero;
      let rr = rnd () in
      if rr <. half then begin
        let rx = rndr (fneg (k "1.2")) (k "1.2") in
        let ry = rndr zero (k "2.4") in
        ev ev_bossexplode (rd (b + b_x) +. rx) (rd (b + b_y) +. ry) zero
      end;
      if rd (b + e_st) <=. zero then begin
        wr (b + e_alive) zero;
        add_to g_score (k "3000.0"); wr g_bosskilled one; wr g_bossactive zero;
        set_door false;
        add_goal (rd (b + b_x)) two;
        ev ev_bossdead (rd (b + b_x)) (rd (b + b_y)) zero
      end
    end
  end

let update_enemies (dt:f64) : ML unit =
  let n = ii (rd g_nen) in
  for_n 0 n (fun i ->
    let b = en_base_ i in
    if nz (rd (b + e_alive)) then begin
      add_to (b + e_t) dt;
      let kind = ii (rd (b + e_kind)) in
      if kind = ek_slime then begin
        wr (b + b_vy) (fmax_ (rd (b + b_vy) -. k "55.0" *. dt) (fneg (k "25.0")));
        wr (b + b_vx) (rd (b + e_dir) *. k "1.9");
        move_body b dt false;
        if nz (rd (b + b_hx)) then wr (b + e_dir) (fneg (rd (b + e_dir)))
        else if nz (rd (b + b_gnd)) then begin
          let tx = fi (rd (b + b_x) +. rd (b + e_dir) *. (rd (b + b_w) /. two +. k "0.12")) in
          let ty = fi (rd (b + b_y) -. k "0.1") in
          let t = tile_at tx ty in
          if not (is_solid t) && not (feq t two) then wr (b + e_dir) (fneg (rd (b + e_dir)))
          else if feq (tile_at tx (fi (rd (b + b_y) +. k "0.1"))) (k "3.0") then wr (b + e_dir) (fneg (rd (b + e_dir)))
        end
      end else if kind = ek_bat then begin
        let near = fabs_ (rd p_x -. rd (b + e_ox)) <. k "8.0" in
        if near && is0 (rd p_dead) then wr (b + e_ox) (rd (b + e_ox) +. sign (rd p_x -. rd (b + e_ox)) *. k "0.9" *. dt);
        let px = rd (b + b_x) in
        wr (b + b_x) (rd (b + e_ox) +. fsin (rd (b + e_t) *. k "1.3") *. k "3.2");
        wr (b + b_y) (rd (b + e_oy) +. fsin (rd (b + e_t) *. k "2.4") *. k "0.6");
        let d = sign (rd (b + b_x) -. px) in
        if nz d then wr (b + e_dir) d
      end else if kind = ek_saw then begin
        add_to (b + b_x) (rd (b + b_vx) *. dt);
        let ahead = fi (rd (b + b_x) +. sign (rd (b + b_vx)) *. half) in
        let floor_ahead = tile_at ahead (fi (rd (b + b_y) -. k "0.2")) in
        if is_solid (tile_at ahead (fi (rd (b + b_y) +. k "0.4"))) || (not (is_solid floor_ahead) && not (feq floor_ahead two))
           || fabs_ (rd (b + b_x) -. rd (b + e_ox)) >. k "3.0" then begin
          wr (b + b_vx) (fneg (rd (b + b_vx))); add_to (b + b_x) (rd (b + b_vx) *. dt *. two)
        end
      end else if kind = ek_turret then begin
        let d = sign (rd p_x -. rd (b + b_x)) in
        wr (b + e_dir) (if nz d then d else one);
        add_to (b + e_cd) (fneg dt);
        let dx = fabs_ (rd p_x -. rd (b + b_x)) in
        let dy = fabs_ (rd p_y -. rd (b + b_y)) in
        if rd (b + e_cd) <=. zero && dx <. fmin_ (k "13.0") (rd g_vieww /. two +. one) && dy <. k "7.0" && is0 (rd p_dead) then begin
          wr (b + e_cd) (k "2.2");
          shoot (rd (b + b_x) +. rd (b + e_dir) *. k "0.6") (rd (b + b_y) +. half) (rd (b + e_dir) *. k "6.5") zero zero (k "6.0") (k "0.6") zero;
          ev ev_shoot (rd (b + b_x) +. rd (b + e_dir) *. k "0.7") (rd (b + b_y) +. half) (rd (b + e_dir))
        end
      end else if kind = ek_boss then update_boss b dt
    end)

(* ------------------------------------------------------- interactions *)
let update_interactions (dt:f64) : ML unit =
  if is0 (rd p_dead) then begin
    let nen = ii (rd g_nen) in
    for_n 0 nen (fun i ->
      let e = en_base_ i in
      if nz (rd (e + e_alive)) then begin
        let kind = ii (rd (e + e_kind)) in
        if kind = ek_boss && (feq (rd (e + e_state)) (f bs_sleep) || feq (rd (e + e_state)) (f bs_dying)) then ()
        else if box_hit p_x e then begin
          if kind = ek_boss then begin
            if rd p_vy <. zero && rd p_prevy >=. rd (e + b_y) +. rd (e + b_h) *. k "0.55" && rd p_dasht <=. zero then begin
              if damage_boss () then stomp_bounce true
              else wr p_vy (fmax_ (rd p_vy) (k "8.0"))
            end else (let _ = hurt_player (rd (e + b_x)) in ())
          end else if is0 (rd (e + e_stomp)) then (let _ = hurt_player (rd (e + b_x)) in ())
          else if rd p_dasht >. zero then kill_enemy e true
          else if rd p_vy <. zero && rd p_prevy >=. rd (e + b_y) +. rd (e + b_h) *. half then begin
            kill_enemy e false; stomp_bounce false; wr g_freeze (k "0.04")
          end else (let _ = hurt_player (rd (e + b_x)) in ())
        end
      end);

    let nc = ii (rd g_ncoins) in
    for_n 0 nc (fun i ->
      let c = coin_base + i * coin_n in
      if is0 (rd (c + c_got)) then begin
        let r = rd (c + c_r) in
        if fabs_ (rd p_x -. rd (c + c_x)) <. r +. pw /. two && fabs_ (rd p_y +. k "0.45" -. rd (c + c_y)) <. r +. k "0.45" then begin
          let kind = ii (rd (c + c_kind)) in
          if kind = sk_heart then begin
            if rd p_hp >=. rd p_maxhp then begin add_to g_score (k "250.0"); ev ev_heart (rd (c + c_x)) (rd (c + c_y)) zero end
            else begin add_to p_hp one; ev ev_heart (rd (c + c_x)) (rd (c + c_y)) one end
          end else if kind = sk_gem then begin
            add_to g_coins (k "5.0"); add_to g_score (k "500.0"); ev ev_gem (rd (c + c_x)) (rd (c + c_y)) zero
          end else begin
            add_to g_coins one; add_to g_score (k "100.0"); ev ev_coin (rd (c + c_x)) (rd (c + c_y)) zero
          end;
          wr (c + c_got) one
        end
      end);

    let ns = ii (rd g_nspr) in
    for_n 0 ns (fun i ->
      let s = spr_base + i * spr_n in
      wr (s + sp_t) (fmax_ zero (rd (s + sp_t) -. dt));
      if fabs_ (rd p_x -. rd (s + sp_x)) <. k "0.7" && rd p_y >=. rd (s + sp_y) -. k "0.1" && rd p_y <. rd (s + sp_y) +. k "0.55" && rd p_vy <=. half then begin
        wr p_vy (k "31.0"); wr p_gnd zero; wr p_jumps one; wr p_airdash zero; wr p_dasht zero; wr p_coyote zero;
        wr (s + sp_t) (k "0.3");
        ev ev_spring (rd (s + sp_x)) (rd (s + sp_y) +. k "0.4") zero
      end);

    let nk = ii (rd g_nck) in
    for_n 0 nk (fun i ->
      let c = ckt_base + i * ckt_n in
      if is0 (rd (c + ck_on)) && fabs_ (rd p_x -. rd (c + ck_x)) <. k "1.3" && fabs_ (rd p_y -. rd (c + ck_y)) <. two then begin
        for_n 0 nk (fun j -> wr (ckt_base + j * ckt_n + ck_on) zero);
        wr (c + ck_on) one;
        wr g_chkx (rd (c + ck_x)); wr g_chky (rd (c + ck_y));
        ev ev_check (rd (c + ck_x)) (rd (c + ck_y)) zero
      end);

    let bi = ii (rd g_boss) in
    if bi >= 0 then begin
      let b = en_base_ bi in
      if feq (rd (b + e_state)) (f bs_sleep) && nz (rd (b + e_alive)) && rd p_x >. rd g_btrig then start_boss ()
    end;

    if nz (rd g_goalon) && fabs_ (rd p_x -. rd g_goalx) <. k "0.9" && fabs_ (rd p_y +. half -. (rd g_goaly +. k "1.1")) <. k "1.5" then begin
      wr g_mode (f mode_clear);
      ev ev_complete (rd p_x) (rd p_y) zero
    end
  end

let step (dt:f64) : ML unit =
  if rd g_freeze >. zero then wr g_freeze (rd g_freeze -. dt)
  else if ii (rd g_mode) <> mode_play then begin
    update_platforms dt; update_enemies dt; update_shots dt
  end else begin
    add_to g_time dt;
    update_platforms dt;
    if is0 (rd p_dead) then update_player dt
    else begin
      add_to p_deadt dt;
      if rd p_deadt >. k "1.4" then respawn ()
    end;
    update_enemies dt;
    update_shots dt;
    update_interactions dt
  end

let rec run_steps (n:int) : ML int =
  if rd g_acc >=. dt_step && n < 12 then begin
    step dt_step;
    wr g_acc (rd g_acc -. dt_step);
    run_steps (n + 1)
  end else n

let advance (rdt:f64) : ML int =
  wr g_evn zero;
  wr g_acc (rd g_acc +. rdt);
  let n = run_steps 0 in
  if n >= 12 then wr g_acc zero;
  wr g_steps (f n);
  if n > 0 then begin wr in_jumppress zero; wr in_dashpress zero end;
  n

let mem_get (i:int) : ML f64 = rd i
let mem_set (i:int) (v:f64) : ML unit = wr i v
