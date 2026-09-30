#!/bin/sh
set -e
cd "$(dirname "$0")/.."
mkdir -p wasm
clang --target=wasm32 -O2 -nostdlib -ffreestanding -fno-math-errno -mbulk-memory -Icore/c \
  -Wl,--strip-all -Wl,--no-entry -Wl,--export-memory -Wl,--initial-memory=262144 -Wl,-z,stack-size=16384 \
  -o wasm/c.wasm core/c/core.c
ls -l wasm/c.wasm
