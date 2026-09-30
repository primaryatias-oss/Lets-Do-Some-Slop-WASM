module Test
open Prim

let p_x : int = 0

let rec loop (i:int) (n:int) : ML unit =
  if i >= n then ()
  else begin
    mset (p_x + i) (fadd (mget (p_x + i)) (lit "0.62"));
    loop (i + 1) n
  end

let step (dt: f64) : ML bool =
  let x = mget p_x in
  if flt x (lit "1.5") && p_x < 3 then (mset p_x (fadd x dt); true)
  else (mset (p_x + 1) (fmul x dt); loop 0 4; false)
