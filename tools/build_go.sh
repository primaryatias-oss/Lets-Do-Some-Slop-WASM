#!/bin/sh
set -e
cd "$(dirname "$0")/../core/go"
mkdir -p ../../wasm
GOOS=wasip1 GOARCH=wasm go build -buildmode=c-shared -ldflags="-s -w" -o ../../wasm/go.wasm .
ls -l ../../wasm/go.wasm
