(* Minimal replacement for F*'s OCaml Prims: F* [int] becomes a native OCaml int
   (so no zarith/GMP is needed and the result compiles with wasm_of_ocaml). *)
type int = Stdlib.int
type bool = Stdlib.bool
type unit = Stdlib.unit
type nat = Stdlib.int
type string = Stdlib.string
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
