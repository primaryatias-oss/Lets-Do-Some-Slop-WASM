module Prim
open FStar.All
(* opaque machine floats + a flat float memory, implemented by hand in prim.ml *)
assume new type f64
assume val lit : string -> f64
assume val fadd : f64 -> f64 -> f64
assume val fmul : f64 -> f64 -> f64
assume val flt : f64 -> f64 -> bool
assume val mget : int -> ML f64
assume val mset : int -> f64 -> ML unit
