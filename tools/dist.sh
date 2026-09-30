#!/bin/sh
# Assemble the static site into ./dist from whatever is built in ./wasm
set -e
cd "$(dirname "$0")/.."
python3 tools/abi.py >/dev/null
rm -rf dist && mkdir -p dist/wasm
cp web/index.html web/backends.js core/js/abi.js core/js/core.js dist/
[ -d wasm ] && cp -r wasm/. dist/wasm/ || true
echo "dist ready: $(ls dist/wasm | tr '\n' ' ')"
