(* Native open-loop driver: same scenario + trace format as tools/ol_node.js. *)
open Abi
let () =
  let lvl = int_of_string Sys.argv.(1) and frames = int_of_string Sys.argv.(2) in
  let tp = if Array.length Sys.argv > 3 then Some (float_of_string Sys.argv.(3)) else None in
  let ic = open_in (Printf.sprintf "%s/level%d.txt" (if Array.length Sys.argv > 4 then Sys.argv.(4) else "core/test") lvl) in
  let buf = Buffer.create 65536 in
  (try while true do Buffer.add_channel buf ic 1 done with End_of_file -> ());
  let toks = List.filter (fun s -> s <> "") (String.split_on_char '\n' (String.map (fun c -> if c = ' ' || c = '\t' || c = '\r' then '\n' else c) (Buffer.contents buf))) in
  let toks = ref toks in
  let nx () = match !toks with x :: r -> toks := r; x | [] -> failwith "eof" in
  let nf () = float_of_string (nx ()) in
  let ni () = int_of_string (nx ()) in
  let m = Core.m in
  Core.init ();
  ignore (nx ()); let w = ni () in m.(g_lw) <- float_of_int w;
  ignore (nx ()); m.(g_spawnx) <- nf (); m.(g_spawny) <- nf ();
  ignore (nx ()); (let x = nf () in let y = nf () in let on = ni () in if on <> 0 then begin m.(g_goalon) <- 1.0; m.(g_goalx) <- x; m.(g_goaly) <- y end);
  ignore (nx ()); (let x = nf () in let y0 = nf () in let y1 = nf () in let on = ni () in if on <> 0 then begin m.(g_dooron) <- 1.0; m.(g_doorx) <- x; m.(g_doory0) <- y0; m.(g_doory1) <- y1 end);
  ignore (nx ()); (let x = nf () in let y = nf () in let tr = nf () in let on = ni () in if on <> 0 then begin m.(g_bspecx) <- x; m.(g_bspecy) <- y; m.(g_btrig) <- tr end);
  ignore (nx ()); m.(g_rng) <- nf ();
  ignore (nx ()); (let n = ni () in for _ = 1 to n do let x = ni () in let y = ni () in let v = nf () in m.(tile_base + y * w + x) <- v done);
  ignore (nx ()); (let n = ni () in for i = 0 to n - 1 do let k = nf () in let x = nf () in let y = nf () in
    m.(spec_base + i * 3) <- k; m.(spec_base + i * 3 + 1) <- x; m.(spec_base + i * 3 + 2) <- y done; m.(g_nspec) <- float_of_int n);
  m.(g_mode) <- float_of_int mode_play; m.(g_vieww) <- 20.0;
  Core.load_level ();
  (match tp with Some x -> m.(p_x) <- x; m.(p_y) <- 3.0 | None -> ());
  let s = ref 7.0 in
  let lcg () = let v = !s *. 1664525.0 +. 1013904223.0 in let v = v -. floor (v /. 4294967296.0) *. 4294967296.0 in s := v; v /. 4294967296.0 in
  let bits x = Printf.sprintf "%016Lx" (Int64.bits_of_float x) in
  let ev_total = ref 0.0 in
  for f = 0 to frames - 1 do
    let r = lcg () in let ax = if r < 0.2 then -1.0 else if r < 0.9 then 1.0 else 0.0 in
    let r = lcg () in let jp = r < 0.05 in
    let r = lcg () in let jh = if r < 0.5 then 1.0 else 0.0 in
    let r = lcg () in let dp = r < 0.01 in
    let r = lcg () in let dn = if r < 0.03 then 1.0 else 0.0 in
    m.(in_ax) <- ax; m.(in_jumpheld) <- jh; m.(in_downheld) <- dn;
    if jp then m.(in_jumppress) <- 1.0;
    if dp then m.(in_dashpress) <- 1.0;
    ignore (Core.advance (1.0 /. 60.0));
    ev_total := !ev_total +. m.(g_evn);
    if m.(p_hp) < 2.0 then m.(p_hp) <- 3.0;
    if f mod 15 = 0 then begin
      let h = ref 0.0 in
      for i = 0 to spec_base - 1 do h := !h +. m.(i) *. float_of_int (1 + (i mod 7)) done;
      print_string (string_of_int f);
      List.iter (fun v -> print_string (" " ^ bits v))
        [m.(p_x); m.(p_y); m.(p_vx); m.(p_vy); m.(p_hp); m.(g_score); m.(g_coins); m.(g_kills); m.(g_mode); m.(g_nsh); m.(g_rng); !ev_total; !h];
      print_newline ()
    end
  done
