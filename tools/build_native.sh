#!/bin/sh
# Build every backend that only needs common toolchains (C, Rust, Zig, Go, Nim).
set -e
cd "$(dirname "$0")/.."
python3 tools/abi.py >/dev/null
tools/build_c.sh
tools/build_rust.sh
tools/build_zig.sh
tools/build_go.sh
tools/build_nim.sh
