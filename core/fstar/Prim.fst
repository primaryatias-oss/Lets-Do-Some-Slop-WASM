module Prim
open FStar.All

(* The only trusted base of the F* port: opaque IEEE-754 doubles and one flat float memory.
   Implemented by hand in prim.ml (OCaml floats / float array), everything else is F* code. *)
assume new type f64
assume val lit : string -> f64            (* decimal literal, parsed once and memoised *)
assume val fadd : f64 -> f64 -> f64
assume val fsub : f64 -> f64 -> f64
assume val fmul : f64 -> f64 -> f64
assume val fdiv : f64 -> f64 -> f64
assume val fneg : f64 -> f64
assume val flt : f64 -> f64 -> bool
assume val fle : f64 -> f64 -> bool
assume val fgt : f64 -> f64 -> bool
assume val fge : f64 -> f64 -> bool
assume val feq : f64 -> f64 -> bool
assume val ffloor : f64 -> f64
assume val fsqrt : f64 -> f64
assume val i2f : int -> f64
assume val f2i : f64 -> int              (* truncate toward zero *)
assume val floor_int : f64 -> int
assume val mget : int -> ML f64
assume val mset : int -> f64 -> ML unit
assume val mptr : unit -> ML int
