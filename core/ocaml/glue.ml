(* wasm_of_ocaml entry point: exposes the core as globalThis.SlopCore_ocaml
   (init, load_level, advance, mem_get, mem_set).  The host adapter polls for [ready]. *)
open Js_of_ocaml

let num (x : float) : Js.number Js.t = Js.number_of_float x
let flt (x : Js.number Js.t) : float = Js.float_of_number x

let () =
  let o = Js.Unsafe.obj [||] in
  let set name fn = Js.Unsafe.set o (Js.string name) (Js.wrap_callback fn) in
  set "init" (fun () -> Core.init ());
  set "load_level" (fun () -> Core.load_level ());
  set "advance" (fun (dt : Js.number Js.t) -> num (float_of_int (Core.advance (flt dt))));
  set "mem_get" (fun (i : Js.number Js.t) -> num (Core.mem_get (int_of_float (flt i))));
  set "mem_set" (fun (i : Js.number Js.t) (v : Js.number Js.t) -> Core.mem_set (int_of_float (flt i)) (flt v));
  Js.Unsafe.set o (Js.string "ready") Js._true;
  Js.Unsafe.set Js.Unsafe.global (Js.string "SlopCore_ocaml") o
