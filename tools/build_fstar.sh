#!/bin/sh
# F* -> OCaml extraction (lax: the port has no proofs, it exercises F*'s ML effect), then a native
# OCaml build of the extracted code for testing.  $FSTAR must point at the unpacked release (fstar/).
set -e
cd "$(dirname "$0")/../core/fstar"
python3 ../../tools/abi.py >/dev/null
F="${FSTAR:-$HOME/fst/fstar}/bin/fstar.exe --lax --include . --cache_checked_modules --cache_dir _cache"
rm -rf _cache out && mkdir -p out
$F Prim.fst
$F Abi.fst
$F Core.fst
for m in Abi Core; do $F --codegen OCaml --extract_module $m --odir out $m.fst; done
ls out
