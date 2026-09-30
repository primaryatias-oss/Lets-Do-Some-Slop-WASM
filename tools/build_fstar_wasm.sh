#!/bin/sh
# F* -> OCaml (tools/build_fstar.sh) -> wasm_of_ocaml.  Needs $FSTAR and an opam switch with dune,
# js_of_ocaml and wasm_of_ocaml-compiler (+ binaryen >= 119 on PATH).
set -e
cd "$(dirname "$0")/.."
tools/build_fstar.sh
W=core/fstar/wasm
rm -rf $W && mkdir -p $W
cp core/fstar/out/Abi.ml core/fstar/out/Core.ml core/fstar/prims.ml core/fstar/prim.ml core/fstar/glue_fstar.ml $W/
echo "(lang dune 3.17)" > $W/dune-project
cat > $W/dune <<'DUNE'
(executable
 (name glue_fstar)
 (modes wasm)
 (libraries js_of_ocaml)
 (flags (:standard -w -a -unsafe)))
DUNE
cd $W
opam exec -- dune build ./glue_fstar.bc.wasm.js --profile release
ls -la _build/default | head -20
