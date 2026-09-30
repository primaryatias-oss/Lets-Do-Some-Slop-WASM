(* Minimal replacement for F*'s OCaml Prims: F* [int] becomes a native OCaml int
   (so no zarith/GMP is needed and the result compiles with wasm_of_ocaml). *)
type nonrec int = int
type nonrec bool = bool
type nonrec unit = unit
type nat = int
type nonrec string = string
let of_int (x : int) : int = x
let int_zero : int = 0
let int_one : int = 1
let parse_int (s : string) : int = int_of_string s
let to_string (x : int) : string = string_of_int x
let string_of_int (x : int) : string = string_of_int x
let string_of_bool (b : bool) : string = string_of_bool b
let op_Negation (b : bool) : bool = not b
let op_AmpAmp (a : bool) (b : bool) : bool = a && b
let op_BarBar (a : bool) (b : bool) : bool = a || b
let op_Addition (a : int) (b : int) : int = a + b
let op_Subtraction (a : int) (b : int) : int = a - b
let op_Multiply (a : int) (b : int) : int = a * b
let op_Minus (a : int) : int = - a
let op_LessThan (a : int) (b : int) : bool = a < b
let op_LessThanOrEqual (a : int) (b : int) : bool = a <= b
let op_GreaterThan (a : int) (b : int) : bool = a > b
let op_GreaterThanOrEqual (a : int) (b : int) : bool = a >= b
let op_Equality (a : int) (b : int) : bool = a = b
let op_disEquality (a : int) (b : int) : bool = a <> b
let not (b : bool) : bool = Stdlib.not b
let op_Modulus (a : int) (b : int) : int = a mod b
let op_Division (a : int) (b : int) : int = a / b
let op_Percent (a : int) (b : int) : int = a mod b
let op_Slash (a : int) (b : int) : int = a / b
let op_Star (a : int) (b : int) : int = a * b
let op_Plus (a : int) (b : int) : int = a + b
let op_Less (a : int) (b : int) : bool = a < b
let op_Greater (a : int) (b : int) : bool = a > b
let op_Less_Equals (a : int) (b : int) : bool = a <= b
let op_Greater_Equals (a : int) (b : int) : bool = a >= b
let op_Equals (a : int) (b : int) : bool = a = b
let op_Tilde_Minus (a : int) : int = - a
let min (a : int) (b : int) : int = if a <= b then a else b
let abs (a : int) : int = if a >= 0 then a else - a
let fst = Stdlib.fst
let snd = Stdlib.snd
let admit () = failwith "Prims.admit"
let magic () = failwith "Prims.magic"
let unsafe_coerce (x : 'a) : 'b = Obj.magic x
let strcat (a : string) (b : string) : string = a ^ b
let op_Hat (a : string) (b : string) : string = a ^ b
