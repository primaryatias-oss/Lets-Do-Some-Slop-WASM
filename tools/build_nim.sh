#!/bin/sh
# Nim -> C (32-bit, standalone, no GC) -> clang wasm32 -> wasm-ld
set -e
cd "$(dirname "$0")/../core/nim"
rm -rf build && mkdir -p build ../../wasm
nim c --compileOnly:on --nimcache:build --cpu:i386 --os:standalone --gc:none --mm:none -d:danger \
  --noMain:on --threads:off --stackTrace:off --lineTrace:off --checks:off --panics:off -d:noSignalHandler \
  --hints:off --warnings:off core.nim 2>&1 | tail -20
NIMLIB=/usr/lib/nim/lib
mkdir -p build/stub && : > build/stub/string.h
for f in build/*.c; do
  clang --target=wasm32 -O2 -ffreestanding -fno-math-errno -mbulk-memory -fno-builtin-printf -I"$NIMLIB" -Ibuild/stub -c "$f" -o "${f%.c}.o" 2>&1 | grep -E "error" | head -5 || true
done
wasm-ld --no-entry --export-memory --strip-all --initial-memory=262144 -z stack-size=16384 \
  --export=init --export=load_level --export=advance --export=mem_get --export=mem_set --export=mem_ptr \
  build/*.o -o ../../wasm/nim.wasm
ls -l ../../wasm/nim.wasm
