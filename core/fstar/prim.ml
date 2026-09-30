(* Hand-written runtime for Prim.fst *)
type f64 = float
let tbl : (string, float) Hashtbl.t = Hashtbl.create 512
let lit (s : string) : float =
  match Hashtbl.find_opt tbl s with
  | Some v -> v
  | None -> let v = float_of_string s in Hashtbl.add tbl s v; v
let fadd (a : float) (b : float) : float = a +. b
let fsub (a : float) (b : float) : float = a -. b
let fmul (a : float) (b : float) : float = a *. b
let fdiv (a : float) (b : float) : float = a /. b
let fneg (a : float) : float = -. a
let flt (a : float) (b : float) : bool = a < b
let fle (a : float) (b : float) : bool = a <= b
let fgt (a : float) (b : float) : bool = a > b
let fge (a : float) (b : float) : bool = a >= b
let feq (a : float) (b : float) : bool = a = b
let ffloor (a : float) : float = floor a
let fsqrt (a : float) : float = sqrt a
let i2f (i : int) : float = float_of_int i
let f2i (a : float) : int = int_of_float a
let floor_int (a : float) : int = int_of_float (floor a)
let m : float array = Array.make 13000 0.0
let mget (i : int) : float = Array.unsafe_get m i
let mset (i : int) (v : float) : unit = Array.unsafe_set m i v
let mptr () : int = 0
