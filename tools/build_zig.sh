#!/bin/sh
set -e
cd "$(dirname "$0")/../core/zig"
mkdir -p ../../wasm
python3 -m ziglang build-exe core.zig -target wasm32-freestanding -O ReleaseFast -fstrip -fno-entry -rdynamic \
  -femit-bin=../../wasm/zig.wasm 2>&1 | head -50
rm -f ../../wasm/zig.wasm.o
ls -l ../../wasm/zig.wasm
